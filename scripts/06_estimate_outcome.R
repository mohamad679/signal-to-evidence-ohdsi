ps_env <- new.env(parent = globalenv())

source_files <- c(
  "config.R",
  "database.R",
  "cohorts.R",
  "covariates.R",
  "propensity_score.R",
  "outcome.R"
)

for (source_file in source_files) {
  sys.source(
    here::here("R", source_file),
    envir = ps_env
  )
}

rm(source_file, source_files)

write_outcome_effect_figure <- function(
  model_result,
  path
) {
  if (!is.data.frame(model_result) || nrow(model_result) != 1L) {
    stop(
      "`model_result` must contain exactly one row.",
      call. = FALSE
    )
  }

  values <- c(
    model_result$estimate,
    model_result$ciLower,
    model_result$ciUpper
  )

  if (anyNA(values) || any(!is.finite(values)) || any(values <= 0)) {
    stop(
      "Effect-estimate values must be finite and positive.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  log_limits <- range(
    log(c(values, 1))
  )

  x_limits <- exp(
    log_limits + c(-0.15, 0.15)
  )

  device_open <- FALSE

  on.exit(
    {
      if (device_open) {
        grDevices::dev.off()
      }
    },
    add = TRUE
  )

  grDevices::png(
    filename = path,
    width = 1200,
    height = 700,
    res = 150
  )

  device_open <- TRUE

  graphics::plot(
    NA_real_,
    NA_real_,
    xlim = x_limits,
    ylim = c(0.5, 1.5),
    log = "x",
    xaxt = "n",
    yaxt = "n",
    xlab = "Odds ratio (log scale)",
    ylab = "",
    bty = "n"
  )

  graphics::axis(1)

  graphics::axis(
    side = 2,
    at = 1,
    labels = "Celecoxib vs diclofenac",
    las = 1,
    tick = FALSE
  )

  graphics::abline(
    v = 1,
    lty = 2
  )

  graphics::segments(
    x0 = model_result$ciLower,
    y0 = 1,
    x1 = model_result$ciUpper,
    y1 = 1,
    lwd = 2
  )

  graphics::points(
    x = model_result$estimate,
    y = 1,
    pch = 19,
    cex = 1.2
  )

  graphics::title(
    main = "GI hemorrhage outcome, days 1-30",
    sub = paste0(
      "Odds ratio ",
      format(model_result$estimate, digits = 3),
      " (95% CI ",
      format(model_result$ciLower, digits = 3),
      "-",
      format(model_result$ciUpper, digits = 3),
      ")"
    )
  )

  grDevices::dev.off()
  device_open <- FALSE

  invisible(path)
}

run_outcome_analysis <- function(
  matched_path = here::here(
    "data",
    "derived",
    "ps_matched_population.rds"
  ),
  summary_path = here::here(
    "results",
    "tables",
    "outcome_analysis_summary.csv"
  ),
  figure_path = here::here(
    "figures",
    "outcome_effect_estimate.png"
  ),
  risk_window_start_days = 1L,
  risk_window_end_days = 30L
) {
  config <- ps_env$read_study_config()

  ps_env$validate_propensity_score_config(
    config
  )

  dataset_name <- if (
    is.null(config$database$dataset_name)
  ) {
    "GiBleed"
  } else {
    config$database$dataset_name
  }

  database_file <- ps_env$get_eunomia_database_path(
    dataset_name
  )

  if (!file.exists(database_file)) {
    stop(
      "The project-local Eunomia database is unavailable.",
      call. = FALSE
    )
  }

  connection <- NULL
  feature_table_created <- FALSE

  feature_table_name <- paste0(
    "study_outcome_",
    Sys.getpid()
  )

  on.exit(
    {
      if (!is.null(connection) && feature_table_created) {
        ps_env$drop_feature_extraction_cohort_table(
          connection = connection,
          table_name = feature_table_name
        )
      }

      if (!is.null(connection)) {
        ps_env$disconnect_safely(
          connection
        )
      }
    },
    add = TRUE
  )

  connection_details <-
    ps_env$create_eunomia_connection_details(
      dataset_name = dataset_name,
      database_file = database_file
    )

  connection <- suppressMessages(
    DatabaseConnector::connect(
      connection_details
    )
  )

  ps_env$validate_required_omop_tables(
    connection = connection,
    database_schema =
      config$project$database_schema
  )

  cohort_tables <- ps_env$create_study_cohorts(
    connection = connection,
    config = config
  )

  ps_env$validate_outcome_cohort_tables(
    cohort_tables
  )

  matched_population <-
    ps_env$load_matched_population(
      path = matched_path
    )

  create_table <- function(...) {
    ps_env$create_feature_extraction_cohort_table(
      ...
    )

    feature_table_created <<- TRUE

    invisible(NULL)
  }

  drop_table <- function(...) {
    ps_env$drop_feature_extraction_cohort_table(
      ...
    )

    feature_table_created <<- FALSE

    invisible(NULL)
  }

  analysis_population <-
    ps_env$build_matched_outcome_from_tables(
      connection = connection,
      cohort_tables = cohort_tables,
      matched_population = matched_population,
      feature_table_name = feature_table_name,
      risk_window_start_days = risk_window_start_days,
      risk_window_end_days = risk_window_end_days,
      create_table = create_table,
      drop_table = drop_table
    )

  model_result <-
    ps_env$fit_matched_outcome_model(
      analysis_population = analysis_population,
      risk_window_start_days = risk_window_start_days,
      risk_window_end_days = risk_window_end_days
    )

  output <- cbind(
    data.frame(
      riskWindowStartDays =
        risk_window_start_days,
      riskWindowEndDays =
        risk_window_end_days,
      residualImbalanceCount = 6L,
      balanceThreshold = 0.1
    ),
    model_result
  )

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

  if (length(intersect(
    names(output),
    prohibited_fields
  )) > 0L) {
    stop(
      "Aggregate output contains person-level fields.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(summary_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  utils::write.csv(
    output,
    file = summary_path,
    row.names = FALSE,
    na = ""
  )

  write_outcome_effect_figure(
    model_result = model_result,
    path = figure_path
  )

  invisible(output)
}

if (sys.nframe() == 0L) {
  result <- run_outcome_analysis()
  print(result)
}
