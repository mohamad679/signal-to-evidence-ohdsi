covariate_test_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = covariate_test_env)
sys.source(here::here("R", "database.R"), envir = covariate_test_env)
sys.source(here::here("R", "cohorts.R"), envir = covariate_test_env)
sys.source(here::here("R", "covariates.R"), envir = covariate_test_env)

local_eunomia_file_for_covariates <- function() {
  database_file <- covariate_test_env$get_eunomia_database_path()
  testthat::skip_if_not(
    isTRUE(fs::is_file(database_file)),
    "The project-local Eunomia database is not available."
  )
  database_file
}

open_local_eunomia_for_covariates <- function() {
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = local_eunomia_file_for_covariates()
  )
  suppressMessages(
    DatabaseConnector::connect(connection_details)
  )
}

testthat::test_that("baseline covariate windows are strictly pre-index", {
  testthat::expect_identical(
    covariate_test_env$validate_covariate_window(-365, -1),
    c(start_days = -365L, end_days = -1L)
  )
  testthat::expect_identical(
    covariate_test_env$validate_covariate_window(-30L, -1L),
    c(start_days = -30L, end_days = -1L)
  )

  invalid_windows <- list(
    list(NULL, -1L),
    list(numeric(), -1L),
    list(c(-30L, -10L), -1L),
    list(NA_real_, -1L),
    list(Inf, -1L),
    list(-30.5, -1L),
    list(-1L, -30L),
    list(-30L, 0L),
    list(-30L, 1L)
  )
  for (window in invalid_windows) {
    testthat::expect_error(
      covariate_test_env$validate_covariate_window(window[[1L]], window[[2L]]),
      class = "covariate_argument_error"
    )
  }
})

testthat::test_that("configured clinical-history windows end before index", {
  settings <- covariate_test_env$create_baseline_covariate_settings()
  testthat::expect_s3_class(settings, "covariateSettings")

  start_fields <- c(
    "longTermStartDays",
    "mediumTermStartDays",
    "shortTermStartDays"
  )
  for (start_field in start_fields) {
    window <- covariate_test_env$validate_covariate_window(
      settings[[start_field]],
      settings$endDays
    )
    testthat::expect_lte(window[["end_days"]], -1L)
  }
  testthat::expect_identical(as.integer(settings$endDays), -1L)
  testthat::expect_true(isTRUE(settings$DemographicsGender))
  testthat::expect_true(isTRUE(settings$DemographicsAge))
  testthat::expect_identical(
    attr(settings, "fun", exact = TRUE),
    "getDbDefaultCovariateData"
  )
  other_demographics <- c(
    "DemographicsAgeGroup",
    "DemographicsRace",
    "DemographicsEthnicity",
    "DemographicsIndexYear",
    "DemographicsIndexMonth",
    "DemographicsPriorObservationTime",
    "DemographicsIndexYearMonth",
    "CareSiteId"
  )
  testthat::expect_false(any(other_demographics %in% names(settings)))
  testthat::expect_false(isTRUE(settings$DemographicsPostObservationTime))
  testthat::expect_false(isTRUE(settings$DemographicsTimeInCohort))
  testthat::expect_false(isTRUE(settings$ConditionEraOverlapping))
  testthat::expect_false(isTRUE(settings$DrugEraOverlapping))
  testthat::expect_invisible(
    covariate_test_env$validate_baseline_covariate_settings(settings)
  )

  future_settings <- settings
  future_settings$DemographicsPostObservationTime <- TRUE
  testthat::expect_error(
    covariate_test_env$validate_baseline_covariate_settings(future_settings),
    class = "covariate_argument_error"
  )
  overlapping_settings <- settings
  overlapping_settings$DrugEraOverlapping <- TRUE
  testthat::expect_error(
    covariate_test_env$validate_baseline_covariate_settings(overlapping_settings),
    class = "covariate_argument_error"
  )
  index_day_settings <- settings
  index_day_settings$endDays <- 0L
  testthat::expect_error(
    covariate_test_env$validate_baseline_covariate_settings(index_day_settings),
    class = "covariate_argument_error"
  )
})

testthat::test_that("working cohort table names and inputs are validated", {
  valid_names <- c("study_covariate_cohort", "cohort_1", "_cohort")
  invalid_names <- list(
    NULL,
    character(),
    c("one", "two"),
    NA_character_,
    "",
    "1cohort",
    "cohort-name",
    "cohort.name",
    "cohort name"
  )
  for (table_name in valid_names) {
    testthat::expect_identical(
      covariate_test_env$validate_covariate_table_name(table_name),
      table_name
    )
  }
  for (table_name in invalid_names) {
    testthat::expect_error(
      covariate_test_env$validate_covariate_table_name(table_name),
      class = "covariate_argument_error"
    )
  }

  connection <- open_local_eunomia_for_covariates()
  on.exit(covariate_test_env$disconnect_safely(connection), add = TRUE)
  testthat::expect_error(
    covariate_test_env$create_feature_extraction_cohort_table(
      connection = connection,
      cohort_tables = list(target = "target_cohort")
    ),
    class = "covariate_argument_error"
  )
})

testthat::test_that("duplicate person and index-date entries are rejected", {
  connection <- open_local_eunomia_for_covariates()
  source_tables <- c(
    target = "duplicate_target_covariate_test",
    comparator = "duplicate_comparator_covariate_test"
  )
  working_table <- "duplicate_working_covariate_test"
  cleanup_tables <- c(unname(source_tables), working_table)
  on.exit({
    for (table_name in cleanup_tables) {
      try(
        DatabaseConnector::executeSql(
          connection = connection,
          sql = paste("DROP TABLE IF EXISTS", table_name),
          progressBar = FALSE,
          reportOverallTime = FALSE
        ),
        silent = TRUE
      )
    }
    covariate_test_env$disconnect_safely(connection)
  }, add = TRUE)

  target_sql <- paste(
    paste0("CREATE TEMP TABLE ", source_tables[["target"]], " AS"),
    "SELECT 1 AS cohort_definition_id, 1001 AS subject_id,",
    "'2020-01-01' AS cohort_start_date, '2020-01-30' AS cohort_end_date",
    "UNION ALL",
    "SELECT 1, 1001, '2020-01-01', '2020-01-30'"
  )
  comparator_sql <- paste(
    paste0("CREATE TEMP TABLE ", source_tables[["comparator"]], " AS"),
    "SELECT 2 AS cohort_definition_id, 1002 AS subject_id,",
    "'2020-01-01' AS cohort_start_date, '2020-01-30' AS cohort_end_date"
  )
  DatabaseConnector::executeSql(
    connection,
    target_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  DatabaseConnector::executeSql(
    connection,
    comparator_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  testthat::expect_error(
    covariate_test_env$create_feature_extraction_cohort_table(
      connection = connection,
      cohort_tables = list(
        target = source_tables[["target"]],
        comparator = source_tables[["comparator"]],
        outcome = "unused_outcome"
      ),
      table_name = working_table
    )
  )
  table_names <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = "main"
  )
  testthat::expect_false(working_table %in% tolower(table_names))
})

testthat::test_that("baseline extraction is aggregate-safe and reproducible", {
  working_table <- "study_covariate_cohort_test"
  database_file <- local_eunomia_file_for_covariates()
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = database_file
  )

  extract_fixture <- function() {
    connection <- open_local_eunomia_for_covariates()
    on.exit(covariate_test_env$disconnect_safely(connection), add = TRUE)
    config <- covariate_test_env$read_study_config()
    cohort_tables <- covariate_test_env$create_study_cohorts(connection, config)
    suppressMessages(covariate_test_env$extract_study_baseline_covariates(
      connection = connection,
      connection_details = connection_details,
      cdm_database_schema = "main",
      cohort_tables = cohort_tables,
      covariate_settings =
        covariate_test_env$create_baseline_covariate_settings(),
      cohort_table = working_table
    ))
  }

  covariate_data <- extract_fixture()
  on.exit(
    if (Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    },
    add = TRUE
  )

  check_connection <- open_local_eunomia_for_covariates()
  on.exit(covariate_test_env$disconnect_safely(check_connection), add = TRUE)
  table_names <- DatabaseConnector::getTableNames(
    connection = check_connection,
    databaseSchema = "main"
  )
  testthat::expect_false(working_table %in% tolower(table_names))

  testthat::expect_invisible(
    covariate_test_env$validate_covariate_data(covariate_data)
  )
  covariate_ref <- dplyr::collect(covariate_data$covariateRef)
  analysis_ref <- dplyr::collect(covariate_data$analysisRef)
  testthat::expect_gt(nrow(covariate_ref), 0L)
  testthat::expect_gt(nrow(analysis_ref), 0L)

  normalized_names <- tolower(gsub(
    "[^a-z0-9]+",
    " ",
    c(covariate_ref$covariateName, analysis_ref$analysisName)
  ))
  prohibited_pattern <- paste(
    "post index|after index|risk window|outcome after index|future follow up|",
    "future observation|post observation|time in cohort|cohort end",
    sep = ""
  )
  testthat::expect_false(any(grepl(prohibited_pattern, normalized_names)))

  summary <- covariate_test_env$summarize_covariates(covariate_data)
  testthat::expect_identical(summary$target_subject_count, 1800L)
  testthat::expect_identical(summary$comparator_subject_count, 830L)
  testthat::expect_gt(summary$covariate_count, 0L)
  testthat::expect_gt(summary$analysis_count, 0L)
  testthat::expect_identical(
    summary$binary_covariate_count + summary$continuous_covariate_count,
    summary$covariate_count
  )
  prohibited_columns <- c(
    "person_id",
    "subject_id",
    "row_id",
    "date",
    "covariate_value"
  )
  testthat::expect_false(any(names(summary) %in% prohibited_columns))

  output_path <- here::here(
    "results",
    "tables",
    "covariate_summary.csv"
  )
  testthat::expect_identical(
    covariate_test_env$write_covariate_summary(summary, output_path),
    output_path
  )
  testthat::expect_error(
    covariate_test_env$write_covariate_summary(summary, tempfile(fileext = ".csv")),
    class = "covariate_output_error"
  )
  testthat::expect_error(
    covariate_test_env$write_covariate_summary(
      transform(summary, subject_id = 1L),
      output_path
    ),
    class = "covariate_output_error"
  )
  testthat::expect_error(
    covariate_test_env$write_covariate_summary(summary[FALSE, ], output_path),
    class = "covariate_output_error"
  )

  local_path <- here::here("data", "derived", "baseline_covariates.rds")
  testthat::expect_error(
    covariate_test_env$save_local_covariate_data(
      covariate_data,
      here::here("data", "derived", "other_covariates.rds")
    ),
    class = "covariate_output_error"
  )
  testthat::expect_error(
    covariate_test_env$save_local_covariate_data(
      covariate_data,
      here::here("data", "derived", "..", "escaped_covariates.rds")
    ),
    class = "covariate_output_error"
  )
  testthat::expect_error(
    covariate_test_env$save_local_covariate_data(
      covariate_data,
      here::here("results", "baseline_covariates.rds")
    ),
    class = "covariate_output_error"
  )
  testthat::expect_identical(
    covariate_test_env$save_local_covariate_data(covariate_data, local_path),
    local_path
  )
  testthat::expect_true(fs::is_file(local_path))
  loaded_covariate_data <- FeatureExtraction::loadCovariateData(file = local_path)
  on.exit(
    if (Andromeda::isValidAndromeda(loaded_covariate_data)) {
      Andromeda::close(loaded_covariate_data)
    },
    add = TRUE
  )
  testthat::expect_true(
    FeatureExtraction::isCovariateData(loaded_covariate_data)
  )
})
