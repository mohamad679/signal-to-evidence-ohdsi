subgroup_env <- new.env(
  parent = globalenv()
)

source_files <- c(
  "config.R",
  "database.R",
  "cohorts.R",
  "covariates.R",
  "propensity_score.R",
  "outcome.R",
  "subgroup.R"
)

for (source_file in source_files) {
  sys.source(
    here::here(
      "R",
      source_file
    ),
    envir = subgroup_env
  )
}

rm(
  source_file,
  source_files
)

write_subgroup_summary <- function(
    output,
    path) {
  if (!is.data.frame(output) ||
        nrow(output) == 0L) {
    stop(
      "`output` must be a non-empty data frame.",
      call. = FALSE
    )
  }

  prohibited_fields <- c(
    "rowId",
    "matchId",
    "subjectId",
    "personId",
    "row_id",
    "match_id",
    "subject_id",
    "person_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  if (
    length(
      intersect(
        names(output),
        prohibited_fields
      )
    ) > 0L
  ) {
    stop(
      paste(
        "Aggregate subgroup summary",
        "contains person-level fields."
      ),
      call. = FALSE
    )
  }

  if (!is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !nzchar(trimws(path))) {
    stop(
      "`path` must be one non-empty string.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  utils::write.csv(
    output,
    file = path,
    row.names = FALSE,
    na = ""
  )

  invisible(path)
}

run_subgroup_analysis <- function(
    baseline_path = here::here(
      "data",
      "derived",
      "baseline_covariates.rds"
    ),
    matched_path = here::here(
      "data",
      "derived",
      "ps_matched_population.rds"
    ),
    summary_path = here::here(
      "results",
      "tables",
      "subgroup_analysis_summary.csv"
    ),
    risk_window_start_days = 1L,
    risk_window_end_days = 30L) {
  config <- subgroup_env$read_study_config()

  subgroup_env$validate_propensity_score_config(
    config
  )

  subgroup_env$validate_subgroup_config(
    config
  )

  age_cutoff <- as.integer(
    config$subgroups$age_cutoff
  )

  dataset_name <- if (
    is.null(
      config$database$dataset_name
    )
  ) {
    "GiBleed"
  } else {
    config$database$dataset_name
  }

  database_file <-
    subgroup_env$get_eunomia_database_path(
      dataset_name
    )

  if (!file.exists(database_file)) {
    stop(
      paste(
        "The project-local Eunomia database",
        "is unavailable."
      ),
      call. = FALSE
    )
  }

  connection <- NULL
  covariate_data <- NULL
  feature_table_created <- FALSE

  feature_table_name <- paste0(
    "study_subgroup_",
    Sys.getpid()
  )

  on.exit(
    {
      if (
        !is.null(connection) &&
          feature_table_created
      ) {
        subgroup_env$drop_feature_extraction_cohort_table(
          connection = connection,
          table_name =
            feature_table_name
        )
      }

      if (
        !is.null(covariate_data) &&
          Andromeda::isValidAndromeda(
            covariate_data
          )
      ) {
        Andromeda::close(
          covariate_data
        )
      }

      if (!is.null(connection)) {
        subgroup_env$disconnect_safely(
          connection
        )
      }
    },
    add = TRUE
  )

  connection_details <-
    subgroup_env$create_eunomia_connection_details(
      dataset_name =
      dataset_name,
      database_file =
      database_file
    )

  connection <- suppressMessages(
    DatabaseConnector::connect(
      connection_details
    )
  )

  subgroup_env$validate_required_omop_tables(
    connection = connection,
    database_schema =
      config$project$database_schema
  )

  cohort_tables <-
    subgroup_env$create_study_cohorts(
      connection = connection,
      config = config
    )

  subgroup_env$validate_outcome_cohort_tables(
    cohort_tables
  )

  matched_population <-
    subgroup_env$load_matched_population(
      path = matched_path
    )

  covariate_data <-
    subgroup_env$load_baseline_covariates(
      path = baseline_path
    )

  demographics <-
    subgroup_env$extract_subgroup_demographics(
      covariate_data =
      covariate_data,
      age_cutoff =
      age_cutoff
    )

  create_table <- function(...) {
    subgroup_env$create_feature_extraction_cohort_table(
      ...
    )

    feature_table_created <<- TRUE

    invisible(NULL)
  }

  drop_table <- function(...) {
    subgroup_env$drop_feature_extraction_cohort_table(
      ...
    )

    feature_table_created <<- FALSE

    invisible(NULL)
  }

  analysis_population <-
    subgroup_env$build_matched_outcome_from_tables(
      connection = connection,
      cohort_tables = cohort_tables,
      matched_population =
      matched_population,
      feature_table_name =
      feature_table_name,
      risk_window_start_days =
      risk_window_start_days,
      risk_window_end_days =
      risk_window_end_days,
      create_table =
      create_table,
      drop_table =
      drop_table
    )

  subgroup_population <-
    subgroup_env$attach_subgroup_demographics(
      analysis_population =
      analysis_population,
      matched_population =
      matched_population,
      demographics =
      demographics
    )

  model_results <-
    subgroup_env$run_prespecified_subgroup_models(
      subgroup_population =
      subgroup_population,
      risk_window_start_days =
      risk_window_start_days,
      risk_window_end_days =
      risk_window_end_days
    )

  balance_results <-
    subgroup_env$calculate_subgroup_balance_summary(
      covariate_data =
      covariate_data,
      subgroup_population =
      subgroup_population,
      threshold =
      config$balance$absolute_smd_threshold
    )

  model_key <- paste(
    model_results$subgroupType,
    model_results$subgroupLevel,
    sep = "\r"
  )

  balance_key <- paste(
    balance_results$subgroupType,
    balance_results$subgroupLevel,
    sep = "\r"
  )

  balance_position <- match(
    model_key,
    balance_key
  )

  if (
    anyNA(balance_position) ||
      anyDuplicated(model_key) > 0L ||
      anyDuplicated(balance_key) > 0L
  ) {
    stop(
      paste(
        "Subgroup model and balance results",
        "could not be linked one-to-one."
      ),
      call. = FALSE
    )
  }

  balance_columns <- setdiff(
    names(balance_results),
    c(
      "subgroupType",
      "subgroupLevel"
    )
  )

  output <- cbind(
    data.frame(
      riskWindowStartDays =
        risk_window_start_days,
      riskWindowEndDays =
        risk_window_end_days
    ),
    model_results,
    balance_results[
      balance_position,
      balance_columns,
      drop = FALSE
    ]
  )

  row.names(output) <- NULL

  write_subgroup_summary(
    output = output,
    path = summary_path
  )

  invisible(output)
}

if (sys.nframe() == 0L) {
  result <- run_subgroup_analysis()

  print(result)
}
