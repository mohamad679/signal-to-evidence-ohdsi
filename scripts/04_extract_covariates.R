covariate_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = covariate_env)
sys.source(here::here("R", "database.R"), envir = covariate_env)
sys.source(here::here("R", "cohorts.R"), envir = covariate_env)
sys.source(here::here("R", "covariates.R"), envir = covariate_env)

run_covariate_extraction <- function() {
  config <- covariate_env$read_study_config()
  dataset_name <- if (is.null(config$database$dataset_name)) {
    "GiBleed"
  } else {
    config$database$dataset_name
  }
  database_file <- covariate_env$get_eunomia_database_path(dataset_name)
  if (!isTRUE(fs::is_file(database_file))) {
    cli::cli_abort(
      "The existing project-local Eunomia database is unavailable.",
      class = "covariate_database_error"
    )
  }

  connection_details <- covariate_env$create_eunomia_connection_details(
    dataset_name = dataset_name,
    database_file = database_file
  )
  connection <- suppressMessages(
    DatabaseConnector::connect(connection_details)
  )
  on.exit(covariate_env$disconnect_safely(connection), add = TRUE)

  covariate_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )
  cohort_tables <- covariate_env$create_study_cohorts(
    connection = connection,
    config = config
  )

  covariate_settings <- covariate_env$create_baseline_covariate_settings()
  covariate_data <- suppressMessages(
    covariate_env$extract_study_baseline_covariates(
      connection = connection,
      connection_details = connection_details,
      cdm_database_schema = config$project$database_schema,
      cohort_tables = cohort_tables,
      covariate_settings = covariate_settings
    )
  )
  on.exit(
    if (Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    },
    add = TRUE,
    after = FALSE
  )
  covariate_env$validate_covariate_data(covariate_data)
  covariate_summary <- covariate_env$summarize_covariates(covariate_data)
  covariate_env$save_local_covariate_data(covariate_data)
  covariate_env$write_covariate_summary(covariate_summary)

  cat(
    "Target subjects: ", covariate_summary$target_subject_count, "\n",
    "Comparator subjects: ", covariate_summary$comparator_subject_count, "\n",
    "Covariates: ", covariate_summary$covariate_count, "\n",
    "Analyses: ", covariate_summary$analysis_count, "\n",
    "Local artifact: data/derived/baseline_covariates.rds\n",
    "PASS: Baseline covariates extracted successfully.\n",
    sep = ""
  )

  invisible(covariate_summary)
}

run_covariate_extraction()
