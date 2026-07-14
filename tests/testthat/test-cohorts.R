cohort_test_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = cohort_test_env)
sys.source(here::here("R", "database.R"), envir = cohort_test_env)
sys.source(here::here("R", "cohorts.R"), envir = cohort_test_env)

open_local_eunomia_for_cohorts <- function() {
  database_file <- cohort_test_env$get_eunomia_database_path()
  testthat::skip_if_not(
    file.exists(database_file),
    "The project-local Eunomia database is not available."
  )
  cohort_test_env$connect_eunomia(database_file = database_file)
}

query_cohort_test_sql <- function(connection, sql) {
  translated_sql <- SqlRender::translate(sql, targetDialect = "sqlite")
  result <- DatabaseConnector::querySql(
    connection = connection,
    sql = translated_sql,
    snakeCaseToCamelCase = FALSE
  )
  as.data.frame(result, stringsAsFactors = FALSE)
}

build_test_cohorts <- function(connection) {
  config <- cohort_test_env$read_study_config()
  tables <- cohort_test_env$create_study_cohorts(connection, config)
  list(config = config, tables = tables)
}

testthat::test_that("observation end dates do not influence treatment ranking", {
  treatment_sql <- paste(
    readLines(
      here::here("sql", "cohorts", "treatment_cohort.sql"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  row_number_pattern <- paste0(
    "ROW_NUMBER\\(\\)\\s+OVER\\s*",
    "\\((?:[^()]|\\([^()]*\\))*\\)"
  )
  row_number_windows <- regmatches(
    treatment_sql,
    gregexpr(row_number_pattern, treatment_sql, perl = TRUE)
  )[[1L]]
  treatment_entry_window <- regmatches(
    treatment_sql,
    regexpr(
      paste0(row_number_pattern, "\\s+AS\\s+treatment_entry_number"),
      treatment_sql,
      perl = TRUE
    )
  )
  cohort_end_expression <- regmatches(
    treatment_sql,
    regexpr(
      "CASE[\\s\\S]*?END\\s+AS\\s+cohort_end_date",
      treatment_sql,
      perl = TRUE
    )
  )

  testthat::expect_match(
    treatment_sql,
    paste0(
      "exposure\\.drug_exposure_start_date\\s*>=\\s*",
      "observation\\.observation_period_start_date[\\s\\S]*?",
      "exposure\\.drug_exposure_start_date\\s*<=\\s*",
      "observation\\.observation_period_end_date"
    ),
    perl = TRUE
  )
  testthat::expect_length(row_number_windows, 2L)
  testthat::expect_false(
    any(grepl(
      "observation_period_end_date",
      row_number_windows,
      fixed = TRUE
    ))
  )
  testthat::expect_length(treatment_entry_window, 1L)
  testthat::expect_match(
    treatment_entry_window,
    paste0(
      "ORDER BY\\s+eligible_exposures\\.drug_exposure_start_date ASC,",
      "\\s+eligible_exposures\\.drug_exposure_id ASC"
    ),
    perl = TRUE
  )
  testthat::expect_length(cohort_end_expression, 1L)
  testthat::expect_match(
    cohort_end_expression,
    paste0(
      "DATEADD\\([\\s\\S]*?",
      "selected_treatment_entries\\.drug_exposure_start_date",
      "[\\s\\S]*?<\\s*",
      "selected_observation\\.observation_period_end_date"
    ),
    perl = TRUE
  )
  testthat::expect_match(
    cohort_end_expression,
    paste0(
      "ELSE\\s+selected_observation\\.",
      "observation_period_end_date\\s+END"
    ),
    perl = TRUE
  )
})

testthat::test_that("temporary cohort table names are validated", {
  valid_names <- c("cohort", "cohort_1", "_temporary", "A1")
  invalid_names <- list(
    NULL,
    character(),
    c("one", "two"),
    NA_character_,
    "",
    " ",
    "1cohort",
    "cohort-name",
    "cohort.name",
    "cohort name"
  )

  for (table_name in valid_names) {
    testthat::expect_identical(
      cohort_test_env$validate_cohort_table_name(table_name),
      table_name
    )
  }
  for (table_name in invalid_names) {
    testthat::expect_error(
      cohort_test_env$validate_cohort_table_name(table_name),
      class = "cohort_argument_error"
    )
  }
})

testthat::test_that("cohort SQL rendering and creation validate inputs", {
  testthat::expect_error(
    cohort_test_env$render_cohort_sql("../outcome_cohort.sql", list()),
    class = "cohort_argument_error"
  )
  testthat::expect_error(
    cohort_test_env$render_cohort_sql("missing.sql", list(unused = 1L)),
    class = "cohort_sql_file_error"
  )
  testthat::expect_error(
    cohort_test_env$render_cohort_sql("outcome_cohort.sql", list(1L)),
    class = "cohort_argument_error"
  )

  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  config <- cohort_test_env$read_study_config()
  testthat::expect_error(
    cohort_test_env$create_treatment_cohort(
      connection = connection,
      config = config,
      treatment_concept_id = 999L,
      cohort_definition_id = 1L,
      table_name = "invalid_treatment"
    ),
    class = "cohort_argument_error"
  )
  testthat::expect_error(
    cohort_test_env$create_outcome_cohort(
      connection = connection,
      config = config,
      cohort_definition_id = 0L
    ),
    class = "cohort_argument_error"
  )
})

testthat::test_that("study cohort tables have only standard columns", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  standard_columns <- c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  for (table_name in unname(unlist(study$tables))) {
    table_details <- DatabaseConnector::querySql(
      connection = connection,
      sql = paste0("PRAGMA table_info(", table_name, ")"),
      snakeCaseToCamelCase = FALSE
    )
    testthat::expect_identical(table_details$name, standard_columns)
  }
})

testthat::test_that("treatment cohorts are unique and match feasibility", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  counts <- cohort_test_env$summarize_study_cohorts(
    connection,
    study$tables
  )

  testthat::expect_equal(counts$target_subject_count, 1800)
  testthat::expect_equal(counts$target_entry_count, 1800)
  testthat::expect_equal(counts$comparator_subject_count, 830)
  testthat::expect_equal(counts$comparator_entry_count, 830)

  feasibility_review <- readr::read_csv(
    here::here("results", "tables", "targeted_feasibility_review.csv"),
    show_col_types = FALSE,
    progress = FALSE
  )
  selected_row <- feasibility_review[
    feasibility_review$target_concept_id ==
      study$config$cohorts$target$concept_id &
      feasibility_review$comparator_concept_id ==
        study$config$cohorts$comparator$concept_id &
      feasibility_review$outcome_concept_id ==
        study$config$cohorts$outcome$concept_id,
    ,
    drop = FALSE
  ]
  testthat::expect_equal(nrow(selected_row), 1L)
  testthat::expect_equal(
    counts$target_subject_count,
    selected_row$target_subject_count
  )
  testthat::expect_equal(
    counts$comparator_subject_count,
    selected_row$comparator_subject_count
  )
})

testthat::test_that("treatment entries satisfy prespecified eligibility", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  config <- study$config
  treatment_union <- paste(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM target_cohort",
    "UNION ALL",
    paste(
      "SELECT subject_id, cohort_start_date, cohort_end_date",
      "FROM comparator_cohort"
    )
  )

  observation_sql <- paste(
    "SELECT COUNT(*) AS invalid_count",
    paste0("FROM (", treatment_union, ") cohort"),
    "WHERE NOT EXISTS (",
    "  SELECT 1 FROM main.observation_period observation",
    "  WHERE observation.person_id = cohort.subject_id",
    "    AND cohort.cohort_start_date >= observation.observation_period_start_date",
    "    AND cohort.cohort_start_date <= observation.observation_period_end_date",
    paste0(
      "    AND DATEDIFF(day, observation.observation_period_start_date, ",
      "cohort.cohort_start_date) >= ",
      config$design$minimum_prior_observation_days
    ),
    ")"
  )
  observation_result <- query_cohort_test_sql(connection, observation_sql)
  testthat::expect_equal(observation_result$invalid_count, 0)

  washout_sql <- paste(
    "SELECT COUNT(*) AS invalid_count",
    paste0("FROM (", treatment_union, ") cohort"),
    "WHERE EXISTS (",
    "  SELECT 1 FROM main.drug_exposure prior_exposure",
    "  WHERE prior_exposure.person_id = cohort.subject_id",
    paste0(
      "    AND prior_exposure.drug_concept_id IN (",
      config$cohorts$target$concept_id,
      ", ",
      config$cohorts$comparator$concept_id,
      ")"
    ),
    paste0(
      "    AND prior_exposure.drug_exposure_start_date >= DATEADD(day, -",
      config$design$washout_days,
      ", cohort.cohort_start_date)"
    ),
    "    AND prior_exposure.drug_exposure_start_date < cohort.cohort_start_date",
    ")"
  )
  washout_result <- query_cohort_test_sql(connection, washout_sql)
  testthat::expect_equal(washout_result$invalid_count, 0)

  prior_outcome_sql <- paste(
    "SELECT COUNT(*) AS invalid_count",
    paste0("FROM (", treatment_union, ") cohort"),
    "WHERE EXISTS (",
    "  SELECT 1 FROM main.condition_occurrence prior_outcome",
    "  WHERE prior_outcome.person_id = cohort.subject_id",
    paste0(
      "    AND prior_outcome.condition_concept_id = ",
      config$cohorts$outcome$concept_id
    ),
    "    AND prior_outcome.condition_start_date <= cohort.cohort_start_date",
    ")"
  )
  prior_outcome_result <- query_cohort_test_sql(connection, prior_outcome_sql)
  testthat::expect_equal(prior_outcome_result$invalid_count, 0)
})

testthat::test_that("treatment end dates respect observation and risk boundaries", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  treatment_union <- paste(
    "SELECT subject_id, cohort_start_date, cohort_end_date FROM target_cohort",
    "UNION ALL",
    paste(
      "SELECT subject_id, cohort_start_date, cohort_end_date",
      "FROM comparator_cohort"
    )
  )
  end_date_sql <- paste(
    "SELECT COUNT(*) AS invalid_count",
    paste0("FROM (", treatment_union, ") cohort"),
    "WHERE NOT EXISTS (",
    "  SELECT 1 FROM main.observation_period observation",
    "  WHERE observation.person_id = cohort.subject_id",
    "    AND cohort.cohort_start_date >= observation.observation_period_start_date",
    "    AND cohort.cohort_start_date <= observation.observation_period_end_date",
    "    AND cohort.cohort_end_date <= observation.observation_period_end_date",
    paste0(
      "    AND cohort.cohort_end_date = CASE WHEN DATEADD(day, ",
      study$config$design$risk_window_end_days,
      ", cohort.cohort_start_date) < observation.observation_period_end_date"
    ),
    paste0(
      "      THEN DATEADD(day, ",
      study$config$design$risk_window_end_days,
      ", cohort.cohort_start_date)"
    ),
    "      ELSE observation.observation_period_end_date END",
    ")"
  )
  end_date_result <- query_cohort_test_sql(connection, end_date_sql)
  testthat::expect_equal(end_date_result$invalid_count, 0)
})

testthat::test_that("outcome occurrences use the concept and observed dates", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  outcome_sql <- paste(
    "SELECT COUNT(*) AS invalid_count FROM outcome_cohort outcome",
    "WHERE outcome.cohort_start_date <> outcome.cohort_end_date",
    "  OR NOT EXISTS (",
    "    SELECT 1 FROM main.condition_occurrence condition_occurrence",
    "    WHERE condition_occurrence.person_id = outcome.subject_id",
    paste0(
      "      AND condition_occurrence.condition_concept_id = ",
      study$config$cohorts$outcome$concept_id
    ),
    "      AND condition_occurrence.condition_start_date =",
    "        outcome.cohort_start_date",
    "      AND EXISTS (",
    "        SELECT 1 FROM main.observation_period observation",
    "        WHERE observation.person_id = condition_occurrence.person_id",
    "          AND condition_occurrence.condition_start_date >=",
    "            observation.observation_period_start_date",
    "          AND condition_occurrence.condition_start_date <=",
    "            observation.observation_period_end_date",
    "      )",
    "  )"
  )
  outcome_result <- query_cohort_test_sql(connection, outcome_sql)
  testthat::expect_equal(outcome_result$invalid_count, 0)

  expected_count_sql <- paste(
    "SELECT COUNT(*) AS expected_count",
    "FROM main.condition_occurrence condition_occurrence",
    paste0(
      "WHERE condition_occurrence.condition_concept_id = ",
      study$config$cohorts$outcome$concept_id
    ),
    "  AND EXISTS (",
    "    SELECT 1 FROM main.observation_period observation",
    "    WHERE observation.person_id = condition_occurrence.person_id",
    "      AND condition_occurrence.condition_start_date >=",
    "        observation.observation_period_start_date",
    "      AND condition_occurrence.condition_start_date <=",
    "        observation.observation_period_end_date",
    "  )"
  )
  expected_count <- query_cohort_test_sql(connection, expected_count_sql)
  cohort_count <- query_cohort_test_sql(
    connection,
    "SELECT COUNT(*) AS outcome_entry_count FROM outcome_cohort"
  )
  testthat::expect_equal(
    cohort_count$outcome_entry_count,
    expected_count$expected_count
  )
})

testthat::test_that("cohort count exports contain aggregate counts only", {
  connection <- open_local_eunomia_for_cohorts()
  on.exit(cohort_test_env$disconnect_safely(connection), add = TRUE)
  study <- build_test_cohorts(connection)
  counts <- cohort_test_env$summarize_study_cohorts(connection, study$tables)
  output_path <- tempfile(fileext = ".csv")
  on.exit(unlink(output_path), add = TRUE)

  returned_path <- cohort_test_env$write_cohort_counts(counts, output_path)
  exported_counts <- readr::read_csv(
    returned_path,
    show_col_types = FALSE,
    progress = FALSE
  )
  testthat::expect_identical(returned_path, output_path)
  testthat::expect_equal(nrow(exported_counts), 1L)
  testthat::expect_identical(names(exported_counts), names(counts))
  testthat::expect_false(any(grepl("_id$|date", tolower(names(exported_counts)))))

  testthat::expect_error(
    cohort_test_env$write_cohort_counts(counts[FALSE, ], output_path),
    class = "cohort_output_error"
  )
  testthat::expect_error(
    cohort_test_env$write_cohort_counts(rbind(counts, counts), output_path),
    class = "cohort_output_error"
  )
  invalid_counts <- counts
  names(invalid_counts)[[1L]] <- "subject_id"
  testthat::expect_error(
    cohort_test_env$write_cohort_counts(invalid_counts, output_path),
    class = "cohort_output_error"
  )
})
