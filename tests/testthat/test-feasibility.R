feasibility_env <- new.env(parent = globalenv())
sys.source(here::here("R", "feasibility.R"), envir = feasibility_env)

database_env <- new.env(parent = globalenv())
sys.source(here::here("R", "database.R"), envir = database_env)

open_local_eunomia_for_feasibility <- function() {
  database_file <- database_env$get_eunomia_database_path()
  testthat::skip_if_not(
    file.exists(database_file),
    "The project-local Eunomia database is not available."
  )
  database_env$connect_eunomia(database_file = database_file)
}

feasibility_test_config <- function() {
  list(
    project = list(database_schema = "main"),
    design = list(
      washout_days = 180L,
      minimum_prior_observation_days = 180L,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    ),
    feasibility = list(
      minimum_subjects_per_arm = 100L,
      minimum_total_outcomes = 20L,
      minimum_outcomes_per_arm = 5L
    )
  )
}

testthat::test_that("candidate concepts are validated and normalized", {
  candidates <- data.frame(
    concept_id = c(10L, 20L),
    concept_name = factor(c(" First drug ", "Second drug")),
    count = c(12L, 8L)
  )

  result <- feasibility_env$validate_candidate_concepts(candidates, "drug")
  empty_result <- feasibility_env$validate_candidate_concepts(
    candidates[FALSE, ],
    "drug"
  )

  testthat::expect_named(result, c("concept_id", "concept_name"))
  testthat::expect_identical(result$concept_name, c("First drug", "Second drug"))
  testthat::expect_type(result$concept_id, "double")
  testthat::expect_equal(nrow(empty_result), 0L)
})

testthat::test_that("candidate validation rejects malformed inputs", {
  valid <- data.frame(
    concept_id = c(10L, 20L),
    concept_name = c("First", "Second")
  )
  invalid_candidates <- list(
    "not a data frame",
    data.frame(concept_name = "Missing ID"),
    data.frame(concept_id = 10L),
    transform(valid, concept_id = c(10L, 10L)),
    transform(valid, concept_id = c(0L, 20L)),
    transform(valid, concept_id = c(NA_integer_, 20L)),
    transform(valid, concept_name = c(NA_character_, "Second")),
    transform(valid, concept_name = c(" ", "Second"))
  )

  for (candidates in invalid_candidates) {
    testthat::expect_error(
      feasibility_env$validate_candidate_concepts(candidates, "drug"),
      class = "feasibility_candidate_error"
    )
  }
})

testthat::test_that("candidate queries return ordered Eunomia aggregates", {
  connection <- open_local_eunomia_for_feasibility()
  on.exit(database_env$disconnect_safely(connection), add = TRUE)

  drugs <- feasibility_env$get_candidate_drugs(
    connection,
    database_schema = "main",
    maximum_candidates = 12L
  )
  outcomes <- feasibility_env$get_candidate_outcomes(
    connection,
    database_schema = "main",
    maximum_candidates = 12L
  )

  testthat::expect_named(
    drugs,
    c("concept_id", "concept_name", "exposure_count", "exposed_person_count")
  )
  testthat::expect_named(
    outcomes,
    c(
      "concept_id",
      "concept_name",
      "occurrence_count",
      "affected_person_count"
    )
  )
  testthat::expect_lte(nrow(drugs), 12L)
  testthat::expect_lte(nrow(outcomes), 12L)
  testthat::expect_false(any(drugs$concept_id == 0))
  testthat::expect_false(any(outcomes$concept_id == 0))
  testthat::expect_identical(
    order(
      -drugs$exposed_person_count,
      -drugs$exposure_count,
      drugs$concept_id
    ),
    seq_len(nrow(drugs))
  )
  testthat::expect_identical(
    order(
      -outcomes$affected_person_count,
      -outcomes$occurrence_count,
      outcomes$concept_id
    ),
    seq_len(nrow(outcomes))
  )
})

testthat::test_that("one Eunomia combination returns one safe aggregate row", {
  connection <- open_local_eunomia_for_feasibility()
  on.exit(database_env$disconnect_safely(connection), add = TRUE)
  drugs <- feasibility_env$get_candidate_drugs(connection, maximum_candidates = 2L)
  outcomes <- feasibility_env$get_candidate_outcomes(connection, maximum_candidates = 1L)

  result <- feasibility_env$evaluate_feasibility_combination(
    connection = connection,
    target_concept_id = drugs$concept_id[[1L]],
    comparator_concept_id = drugs$concept_id[[2L]],
    outcome_concept_id = outcomes$concept_id[[1L]]
  )
  required_columns <- c(
    "target_concept_id",
    "comparator_concept_id",
    "outcome_concept_id",
    "target_subject_count",
    "comparator_subject_count",
    "target_outcome_count",
    "comparator_outcome_count",
    "median_prior_observation_days"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_named(result, required_columns)
  testthat::expect_false(
    any(tolower(names(result)) %in% c("person_id", "subject_id"))
  )
  testthat::expect_false(any(grepl("date", tolower(names(result)))))
})

testthat::test_that("duplicate indexes and risk-window boundaries stay aggregate", {
  database_file <- tempfile(fileext = ".sqlite")
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = database_file
  )
  connection <- DatabaseConnector::connect(connection_details)
  on.exit(
    {
      database_env$disconnect_safely(connection)
      unlink(database_file)
    },
    add = TRUE
  )
  setup_sql <- c(
    paste(
      "CREATE TABLE observation_period (",
      "observation_period_id INTEGER, person_id INTEGER,",
      "observation_period_start_date DATE, observation_period_end_date DATE);"
    ),
    paste(
      "CREATE TABLE drug_exposure (",
      "drug_exposure_id INTEGER, person_id INTEGER, drug_concept_id INTEGER,",
      "drug_exposure_start_date DATE);"
    ),
    paste(
      "CREATE TABLE condition_occurrence (",
      "condition_occurrence_id INTEGER, person_id INTEGER,",
      "condition_concept_id INTEGER, condition_start_date DATE);"
    ),
    paste(
      "INSERT INTO observation_period VALUES",
      "(1, 1, '2019-01-01', '2020-12-31'),",
      "(2, 2, '2019-01-01', '2020-12-31'),",
      "(3, 3, '2019-01-01', '2020-12-31'),",
      "(4, 4, '2019-01-01', '2020-12-31'),",
      "(5, 5, '2020-01-03', '2020-12-31'),",
      "(6, 6, '2020-01-04', '2020-12-31'),",
      "(7, 7, '2019-09-01', '2020-12-31'),",
      "(8, 8, '2019-09-01', '2020-07-01');"
    ),
    paste(
      "INSERT INTO drug_exposure VALUES",
      "(1, 1, 10, '2020-07-01'), (2, 1, 10, '2020-07-01'),",
      "(3, 2, 20, '2020-07-01'), (4, 3, 10, '2020-07-01'),",
      "(5, 4, 20, '2020-07-01'), (6, 5, 10, '2020-07-01'),",
      "(7, 6, 10, '2020-07-01'), (8, 7, 20, '2020-01-03'),",
      "(9, 7, 10, '2020-07-01'), (10, 8, 20, '2020-01-02'),",
      "(11, 8, 10, '2020-07-01');"
    ),
    paste(
      "INSERT INTO condition_occurrence VALUES",
      "(1, 1, 30, '2020-07-02'), (2, 1, 30, '2020-07-31'),",
      "(3, 2, 30, '2020-07-31'), (4, 3, 30, '2020-08-01'),",
      "(5, 4, 30, '2020-07-01'), (6, 5, 30, '2020-07-02'),",
      "(7, 8, 30, '2020-07-02');"
    ),
    paste(
      "UPDATE observation_period SET",
      "observation_period_start_date = STRFTIME('%s',",
      "observation_period_start_date),",
      "observation_period_end_date = STRFTIME('%s', observation_period_end_date);"
    ),
    paste(
      "UPDATE drug_exposure SET drug_exposure_start_date =",
      "STRFTIME('%s', drug_exposure_start_date);"
    ),
    paste(
      "UPDATE condition_occurrence SET condition_start_date =",
      "STRFTIME('%s', condition_start_date);"
    )
  )
  for (statement in setup_sql) {
    DatabaseConnector::executeSql(connection, statement)
  }

  result <- feasibility_env$evaluate_feasibility_combination(
    connection = connection,
    target_concept_id = 10L,
    comparator_concept_id = 20L,
    outcome_concept_id = 30L,
    washout_days = 180L,
    minimum_prior_observation_days = 180L,
    risk_window_start_days = 1L,
    risk_window_end_days = 30L
  )

  testthat::expect_equal(result$target_subject_count, 4)
  testthat::expect_equal(result$comparator_subject_count, 1)
  testthat::expect_equal(result$target_outcome_count, 2)
  testthat::expect_equal(result$comparator_outcome_count, 1)
  testthat::expect_false(any(grepl("date", tolower(names(result)))))
})

testthat::test_that("combination validation rejects invalid parameters", {
  testthat::expect_error(
    feasibility_env$evaluate_feasibility_combination(
      connection = NULL,
      target_concept_id = 10L,
      comparator_concept_id = 10L,
      outcome_concept_id = 30L
    ),
    class = "feasibility_argument_error"
  )
  testthat::expect_error(
    feasibility_env$evaluate_feasibility_combination(
      connection = NULL,
      target_concept_id = 10L,
      comparator_concept_id = 20L,
      outcome_concept_id = 30L,
      risk_window_start_days = 30L,
      risk_window_end_days = 1L
    ),
    class = "feasibility_argument_error"
  )
})

testthat::test_that("thresholds and deterministic matrix ordering are applied", {
  original_evaluator <- feasibility_env$evaluate_feasibility_combination
  on.exit(
    assign(
      "evaluate_feasibility_combination",
      original_evaluator,
      envir = feasibility_env
    ),
    add = TRUE
  )
  fake_evaluator <- function(connection,
                             target_concept_id,
                             comparator_concept_id,
                             outcome_concept_id,
                             ...) {
    key <- paste(target_concept_id, comparator_concept_id, outcome_concept_id)
    values <- switch(
      key,
      "10 20 100" = c(120, 110, 6, 15),
      "20 30 100" = c(115, 105, 15, 15),
      "10 30 100" = c(130, 90, 10, 10),
      c(100, 100, 4, 6)
    )
    data.frame(
      target_concept_id = target_concept_id,
      comparator_concept_id = comparator_concept_id,
      outcome_concept_id = outcome_concept_id,
      target_subject_count = values[[1L]],
      comparator_subject_count = values[[2L]],
      target_outcome_count = values[[3L]],
      comparator_outcome_count = values[[4L]],
      median_prior_observation_days = 365,
      stringsAsFactors = FALSE
    )
  }
  assign(
    "evaluate_feasibility_combination",
    fake_evaluator,
    envir = feasibility_env
  )
  drugs <- data.frame(
    concept_id = c(30L, 10L, 20L),
    concept_name = c("Drug 30", "Drug 10", "Drug 20")
  )
  outcomes <- data.frame(
    concept_id = c(200L, 100L),
    concept_name = c("Outcome 200", "Outcome 100")
  )

  result <- feasibility_env$build_feasibility_matrix(
    connection = NULL,
    drug_candidates = drugs,
    outcome_candidates = outcomes,
    config = feasibility_test_config()
  )

  testthat::expect_equal(nrow(result), 6L)
  testthat::expect_identical(result$feasible[1:2], c(TRUE, TRUE))
  testthat::expect_equal(result$target_concept_id[1:2], c(10, 20))
  testthat::expect_equal(result$comparator_concept_id[1:2], c(20, 30))
  testthat::expect_equal(result$outcome_concept_id[1:2], c(100, 100))
  testthat::expect_identical(
    result$feasibility_reason[1:2],
    rep("All engineering thresholds passed", 2L)
  )
  testthat::expect_true(
    any(grepl("minimum_subjects_per_arm", result$feasibility_reason))
  )
  testthat::expect_true(
    any(grepl("minimum_total_outcomes", result$feasibility_reason))
  )
  testthat::expect_true(
    any(grepl("minimum_outcomes_per_arm", result$feasibility_reason))
  )
})

testthat::test_that("empty candidate sets return an empty aggregate matrix", {
  drugs <- data.frame(concept_id = numeric(), concept_name = character())
  outcomes <- data.frame(concept_id = numeric(), concept_name = character())

  result <- feasibility_env$build_feasibility_matrix(
    connection = NULL,
    drug_candidates = drugs,
    outcome_candidates = outcomes,
    config = feasibility_test_config()
  )

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_named(result, names(feasibility_env$empty_feasibility_matrix()))
})

testthat::test_that("aggregate feasibility CSV output is written", {
  output_directory <- tempfile("feasibility-output-")
  output_path <- file.path(output_directory, "nested", "matrix.csv")
  on.exit(unlink(output_directory, recursive = TRUE), add = TRUE)
  matrix <- feasibility_env$empty_feasibility_matrix()
  matrix[1L, ] <- list(
    10,
    "Target drug",
    20,
    "Comparator drug",
    30,
    "Outcome",
    120,
    115,
    8,
    7,
    365,
    15,
    FALSE,
    "Failed engineering threshold(s): minimum_total_outcomes"
  )

  returned_path <- feasibility_env$write_feasibility_output(matrix, output_path)
  written <- readr::read_csv(output_path, show_col_types = FALSE)

  testthat::expect_identical(returned_path, output_path)
  testthat::expect_true(file.exists(output_path))
  testthat::expect_gt(file.info(output_path)$size, 0)
  testthat::expect_named(written, names(matrix))
})

testthat::test_that("CSV output rejects person identifiers and dates", {
  output_path <- tempfile(fileext = ".csv")
  on.exit(unlink(output_path), add = TRUE)
  person_data <- data.frame(person_id = 1L, aggregate_count = 2L)
  date_data <- data.frame(index_date = as.Date("2020-01-01"), aggregate_count = 2L)
  typed_date_data <- data.frame(index = as.Date("2020-01-01"), aggregate_count = 2L)

  for (unsafe_data in list(person_data, date_data, typed_date_data)) {
    testthat::expect_error(
      feasibility_env$write_feasibility_output(unsafe_data, output_path),
      class = "feasibility_output_error"
    )
  }
  testthat::expect_false(file.exists(output_path))
})
