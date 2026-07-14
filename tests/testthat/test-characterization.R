characterization_env <- new.env(parent = globalenv())
sys.source(
  here::here("R", "characterization.R"),
  envir = characterization_env
)

database_env <- new.env(parent = globalenv())
sys.source(here::here("R", "database.R"), envir = database_env)

open_local_eunomia <- function() {
  database_file <- database_env$get_eunomia_database_path()
  testthat::skip_if_not(
    file.exists(database_file),
    "The project-local Eunomia database is not available."
  )
  database_env$connect_eunomia(database_file = database_file)
}

testthat::test_that("read_sql_file rejects a missing SQL file", {
  missing_path <- tempfile(fileext = ".sql")

  testthat::expect_error(
    characterization_env$read_sql_file(missing_path),
    class = "characterization_sql_file_error"
  )
})

testthat::test_that("read_sql_file rejects an empty SQL file", {
  empty_path <- tempfile(fileext = ".sql")
  file.create(empty_path)
  on.exit(unlink(empty_path), add = TRUE)

  testthat::expect_error(
    characterization_env$read_sql_file(empty_path),
    class = "characterization_empty_sql_error"
  )
})

testthat::test_that("table-count SQL executes successfully against Eunomia", {
  connection <- open_local_eunomia()
  on.exit(database_env$disconnect_safely(connection), add = TRUE)

  result <- characterization_env$run_sql_file(
    connection = connection,
    sql_path = here::here(
      "sql",
      "characterization",
      "table_counts.sql"
    ),
    parameters = list(cdm_database_schema = "main")
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_named(
    result,
    c("table_name", "record_count", "person_count")
  )
  testthat::expect_identical(
    result$table_name,
    c(
      "person",
      "observation_period",
      "drug_exposure",
      "condition_occurrence",
      "visit_occurrence",
      "measurement"
    )
  )
  person_row <- result[result$table_name == "person", , drop = FALSE]
  testthat::expect_equal(person_row$person_count, person_row$record_count)
})

testthat::test_that("SQL schema parameters are rendered", {
  sql <- characterization_env$read_sql_file(
    here::here("sql", "characterization", "table_counts.sql")
  )
  rendered_sql <- SqlRender::render(
    sql = sql,
    cdm_database_schema = "main"
  )

  testthat::expect_match(rendered_sql, "FROM main\\.person")
  testthat::expect_match(rendered_sql, "FROM main\\.measurement")
})

testthat::test_that("all characterization queries return safe aggregates", {
  connection <- open_local_eunomia()
  on.exit(database_env$disconnect_safely(connection), add = TRUE)

  characterization <- characterization_env$characterize_omop(
    connection = connection,
    database_schema = "main"
  )
  expected_columns <- list(
    table_counts = c("table_name", "record_count", "person_count"),
    population_summary = c(
      "metric",
      "category",
      "person_count",
      "category_order"
    ),
    top_drugs = c(
      "concept_id",
      "concept_name",
      "exposure_count",
      "exposed_person_count"
    ),
    top_conditions = c(
      "concept_id",
      "concept_name",
      "occurrence_count",
      "affected_person_count"
    )
  )

  testthat::expect_named(characterization, names(expected_columns))
  for (result_name in names(expected_columns)) {
    testthat::expect_named(
      characterization[[result_name]],
      expected_columns[[result_name]]
    )
    testthat::expect_false(
      any(
        tolower(names(characterization[[result_name]])) %in%
          c("person_id", "subject_id")
      )
    )
  }
  testthat::expect_setequal(
    unique(characterization$population_summary$metric),
    c("gender", "age_group", "follow_up_group")
  )
  testthat::expect_false(any(characterization$top_drugs$concept_id == 0))
  testthat::expect_false(any(characterization$top_conditions$concept_id == 0))
})

testthat::test_that("result validation rejects missing columns", {
  incomplete_result <- data.frame(table_name = "person")

  testthat::expect_error(
    characterization_env$validate_characterization_result(
      data = incomplete_result,
      required_columns = c("table_name", "record_count", "person_count"),
      result_name = "table_counts"
    ),
    class = "characterization_result_error"
  )
})

testthat::test_that("result validation rejects person-level identifiers", {
  person_result <- data.frame(
    metric = "gender",
    category = "Unknown",
    person_count = 1,
    category_order = 1,
    person_id = 1001
  )
  subject_result <- person_result
  names(subject_result)[[5L]] <- "subject_id"

  for (result in list(person_result, subject_result)) {
    testthat::expect_error(
      characterization_env$validate_characterization_result(
        data = result,
        required_columns = c(
          "metric",
          "category",
          "person_count",
          "category_order"
        ),
        result_name = "population_summary"
      ),
      class = "characterization_result_error"
    )
  }
})

testthat::test_that("result validation rejects duplicated column names", {
  duplicated_result <- data.frame(
    table_name = "person",
    record_count = 1,
    person_count = 1
  )
  names(duplicated_result)[[3L]] <- "record_count"

  testthat::expect_error(
    characterization_env$validate_characterization_result(
      data = duplicated_result,
      required_columns = c("table_name", "record_count", "person_count"),
      result_name = "table_counts"
    ),
    class = "characterization_result_error"
  )
})

testthat::test_that("run_sql_file rejects invalid target dialects", {
  sql_path <- here::here(
    "sql",
    "characterization",
    "table_counts.sql"
  )

  for (target_dialect in list("", " ", NA_character_, c("sqlite", "oracle"))) {
    testthat::expect_error(
      characterization_env$run_sql_file(
        connection = NULL,
        sql_path = sql_path,
        target_dialect = target_dialect
      ),
      class = "characterization_argument_error"
    )
  }
})

testthat::test_that("aggregate CSV and PNG outputs are written", {
  output_root <- tempfile("characterization-output-")
  on.exit(unlink(output_root, recursive = TRUE), add = TRUE)
  tables_directory <- file.path(output_root, "tables")
  figures_directory <- file.path(output_root, "figures")
  characterization <- list(
    table_counts = data.frame(
      table_name = "person",
      record_count = 10,
      person_count = 10
    ),
    population_summary = data.frame(
      metric = c("gender", "age_group", "age_group", "follow_up_group"),
      category = c("Female", "18-34", "35-49", "365-729 days"),
      person_count = c(6, 4, 6, 10),
      category_order = c(2, 2, 3, 3)
    ),
    top_drugs = data.frame(
      concept_id = 100,
      concept_name = "Aggregate drug concept",
      exposure_count = 12,
      exposed_person_count = 8
    ),
    top_conditions = data.frame(
      concept_id = 200,
      concept_name = "Aggregate condition concept",
      occurrence_count = 9,
      affected_person_count = 7
    )
  )

  paths <- characterization_env$write_characterization_outputs(
    characterization = characterization,
    tables_directory = tables_directory,
    figures_directory = figures_directory
  )

  testthat::expect_named(
    paths,
    c(
      "table_counts",
      "population_summary",
      "top_drugs",
      "top_conditions",
      "age_distribution",
      "follow_up_distribution"
    )
  )
  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_true(all(file.info(paths)$size > 0))
})
