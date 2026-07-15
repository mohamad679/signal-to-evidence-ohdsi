ps_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = ps_env)
sys.source(here::here("R", "database.R"), envir = ps_env)
sys.source(here::here("R", "cohorts.R"), envir = ps_env)
sys.source(here::here("R", "covariates.R"), envir = ps_env)
sys.source(here::here("R", "propensity_score.R"), envir = ps_env)

run_propensity_score_adjustment <- function() {
  config <- ps_env$read_study_config()
  ps_env$validate_propensity_score_config(config)
  dataset_name <- if (is.null(config$database$dataset_name)) {
    "GiBleed"
  } else {
    config$database$dataset_name
  }
  database_file <- ps_env$get_eunomia_database_path(dataset_name)
  if (!isTRUE(fs::is_file(database_file))) {
    cli::cli_abort(
      "The existing project-local Eunomia database is unavailable.",
      class = "propensity_score_data_error"
    )
  }

  connection <- NULL
  covariate_data <- NULL
  working_table <- "study_ps_cohort"
  on.exit({
    if (!is.null(connection)) {
      tryCatch(
        {
          if (!is.null(covariate_data) &&
                Andromeda::isValidAndromeda(covariate_data)) {
            Andromeda::close(covariate_data)
          }
        },
        finally = tryCatch(
          ps_env$drop_feature_extraction_cohort_table(
            connection = connection,
            table_name = working_table
          ),
          finally = ps_env$disconnect_safely(connection)
        )
      )
    } else if (!is.null(covariate_data) &&
                 Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    }
  }, add = TRUE)

  connection_details <- ps_env$create_eunomia_connection_details(
    dataset_name = dataset_name,
    database_file = database_file
  )
  connection <- suppressMessages(
    DatabaseConnector::connect(connection_details)
  )
  ps_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )
  cohort_tables <- ps_env$create_study_cohorts(
    connection = connection,
    config = config
  )

  covariate_data <- ps_env$load_baseline_covariates()
  population <- ps_env$create_propensity_score_population(
    connection = connection,
    cohort_tables = cohort_tables,
    covariate_data = covariate_data,
    table_name = working_table
  )
  model_data <- ps_env$create_propensity_score_model_data(
    covariate_data = covariate_data,
    population = population
  )
  population_before <- ps_env$estimate_propensity_scores(
    model_data = model_data,
    population = population,
    config = config
  )
  population_before <- ps_env$calculate_preference_scores(population_before)
  population_trimmed <- ps_env$trim_propensity_score_population(
    population_before,
    trim_fraction = config$propensity_score$trim_fraction
  )
  population_matched <- ps_env$match_propensity_score_population(
    ps_population = population_trimmed,
    config = config
  )
  balance <- ps_env$compute_propensity_score_balance(
    covariate_data = covariate_data,
    population_before = population_before,
    population_after = population_matched,
    threshold = config$balance$absolute_smd_threshold
  )
  summary <- ps_env$summarize_propensity_score_adjustment(
    population_before = population_before,
    population_trimmed = population_trimmed,
    population_matched = population_matched,
    balance = balance,
    threshold = config$balance$absolute_smd_threshold
  )

  ps_env$save_local_ps_population(population_matched)
  ps_env$write_propensity_score_summary(summary)
  ps_env$write_covariate_balance(balance)
  ps_env$plot_propensity_score_overlap(
    population_before = population_before,
    population_after = population_matched
  )
  ps_env$plot_covariate_balance(balance)

  cat(
    "Target before: ", summary$target_before, "\n",
    "Comparator before: ", summary$comparator_before, "\n",
    "Target after trimming: ", summary$target_after_trimming, "\n",
    "Comparator after trimming: ", summary$comparator_after_trimming, "\n",
    "Matched pairs: ", summary$matched_pair_count, "\n",
    "Unbalanced before: ", summary$unbalanced_before_count, "\n",
    "Unbalanced after: ", summary$unbalanced_after_count, "\n",
    "Maximum absolute SMD after: ",
    summary$maximum_absolute_smd_after,
    "\n",
    "PASS: Propensity-score adjustment completed successfully.\n",
    sep = ""
  )

  invisible(summary)
}

run_propensity_score_adjustment()
