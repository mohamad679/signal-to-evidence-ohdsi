pipeline_project_root <- function() {
  root <- Sys.getenv(
    "PHASE18_PROJECT_ROOT",
    unset = getwd()
  )

  normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )
}

discover_pipeline_scripts <- function(project_root) {
  expected <- c(
    "01_characterize_data.R",
    "02_run_feasibility.R",
    "03_build_cohorts.R",
    "04_extract_covariates.R",
    "05_adjust_propensity_score.R",
    "06_estimate_outcome.R",
    "07_run_subgroups.R",
    "08_run_sensitivity.R",
    "09_export_results.R"
  )
  legacy_placeholders <- c(
    "04_build_covariates.R",
    "05_fit_propensity_score.R"
  )
  scripts_directory <- file.path(
    project_root,
    "scripts"
  )
  scripts <- file.path(
    scripts_directory,
    expected
  )
  missing <- expected[
    !file.exists(scripts)
  ]

  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Required executable pipeline stages are missing: %s.",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  placeholders <- file.path(
    scripts_directory,
    legacy_placeholders
  )
  non_empty_placeholders <- legacy_placeholders[
    file.exists(placeholders) &
      file.info(placeholders)$size != 0
  ]

  if (length(non_empty_placeholders) > 0L) {
    stop(
      sprintf(
        "Legacy pipeline placeholders must remain empty: %s.",
        paste(
          non_empty_placeholders,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  candidates <- basename(
    list.files(
      scripts_directory,
      pattern = "^0[1-9]_.*\\.R$",
      full.names = TRUE
    )
  )
  unexpected <- setdiff(
    candidates,
    c(
      expected,
      legacy_placeholders
    )
  )

  if (length(unexpected) > 0L) {
    stop(
      sprintf(
        "Unexpected numbered pipeline scripts were found: %s.",
        paste(
          unexpected,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  scripts
}

configure_pipeline_environment <- function(project_root) {
  rscript <- file.path(
    R.home("bin"),
    "Rscript"
  )
  quarto <- unname(
    Sys.which("quarto")
  )

  for (phase in seq_len(18L)) {
    prefix <- sprintf(
      "PHASE%d_",
      phase
    )
    values <- list(
      project_root,
      project_root,
      rscript,
      rscript,
      quarto,
      "false"
    )
    names(values) <- paste0(
      prefix,
      c(
        "PROJECT_ROOT",
        "IMPLEMENTATION_ROOT",
        "RSCRIPT",
        "R_SCRIPT",
        "QUARTO_BIN",
        "SKIP_RENDER"
      )
    )

    do.call(
      Sys.setenv,
      values
    )
  }

  Sys.setenv(
    PHASE18_PROJECT_ROOT = project_root,
    PHASE18_IMPLEMENTATION_ROOT = project_root
  )

  invisible(TRUE)
}

tracked_status <- function(project_root) {
  output <- system2(
    "git",
    c(
      "-C",
      project_root,
      "status",
      "--porcelain=v1",
      "--untracked-files=all"
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(
    output,
    "status"
  )

  if (
    !is.null(status) &&
      !identical(
        as.integer(status),
        0L
      )
  ) {
    stop(
      paste(
        output,
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  output
}

validate_pipeline_artifacts <- function(project_root) {
  environment <- new.env(
    parent = globalenv()
  )
  sys.source(
    file.path(
      project_root,
      "R",
      "plots.R"
    ),
    envir = environment
  )

  relative_paths <- environment$reporting_table_paths()
  csv_files <- file.path(
    project_root,
    unname(relative_paths)
  )
  missing <- unname(relative_paths)[
    !file.exists(csv_files)
  ]

  if (length(relative_paths) != 15L) {
    stop(
      sprintf(
        "Reporting contract must define exactly 15 aggregate CSV files; found %d.",
        length(relative_paths)
      ),
      call. = FALSE
    )
  }

  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Required aggregate CSV files are missing: %s.",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  empty <- csv_files[
    file.info(csv_files)$size == 0
  ]

  if (length(empty) > 0L) {
    stop(
      sprintf(
        "Pipeline artifacts must be non-empty: %s.",
        paste(
          basename(empty),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  reporting_inputs <- environment$validate_reporting_inputs(
    project_root
  )
  effect_plot_data <- environment$prepare_effect_estimate_plot_data(
    reporting_inputs
  )
  balance_plot_data <- environment$prepare_balance_plot_data(
    reporting_inputs
  )

  if (
    nrow(effect_plot_data) == 0L ||
      nrow(balance_plot_data) == 0L
  ) {
    stop(
      "Aggregate plot-data contracts must be non-empty.",
      call. = FALSE
    )
  }

  invisible(
    list(
      csv_files = sort(csv_files),
      reporting_inputs = reporting_inputs,
      effect_plot_rows = nrow(effect_plot_data),
      balance_plot_rows = nrow(balance_plot_data)
    )
  )
}

validate_pipeline_privacy <- function(artifacts) {
  prohibited <- c(
    "personid",
    "subjectid",
    "rowid",
    "indexdate",
    "cohortstartdate",
    "cohortenddate",
    "eventdate",
    "drugexposurestartdate"
  )

  for (path in artifacts$csv_files) {
    header <- readLines(
      path,
      n = 1L,
      warn = FALSE,
      encoding = "UTF-8"
    )
    normalized <- gsub(
      "[^a-z0-9]",
      "",
      tolower(header)
    )

    if (
      any(
        vapply(
          prohibited,
          function(field) {
            grepl(
              field,
              normalized,
              fixed = TRUE
            )
          },
          logical(1)
        )
      )
    ) {
      stop(
        sprintf(
          "Aggregate artifact contains a prohibited field: %s.",
          basename(path)
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

run_pipeline_script <- function(
    script,
    project_root) {
  rscript <- file.path(
    R.home("bin"),
    "Rscript"
  )
  old_directory <- setwd(project_root)
  on.exit(
    setwd(old_directory),
    add = TRUE
  )

  message(
    sprintf(
      "PIPELINE START %s",
      basename(script)
    )
  )
  status <- system2(
    rscript,
    script
  )

  if (!identical(as.integer(status), 0L)) {
    stop(
      sprintf(
        "Pipeline stage failed: %s.",
        basename(script)
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "PIPELINE PASS %s",
      basename(script)
    )
  )

  invisible(script)
}

run_full_pipeline <- function(
    project_root = pipeline_project_root(),
    execute = TRUE) {
  root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
  scripts <- discover_pipeline_scripts(root)

  if (!isTRUE(execute)) {
    return(
      invisible(
        list(
          project_root = root,
          scripts = scripts,
          executed = FALSE
        )
      )
    )
  }

  configure_pipeline_environment(root)

  invisible(
    lapply(
      scripts,
      run_pipeline_script,
      project_root = root
    )
  )

  artifacts <- validate_pipeline_artifacts(root)
  validate_pipeline_privacy(artifacts)

  invisible(
    list(
      project_root = root,
      scripts = scripts,
      artifacts = artifacts,
      status = tracked_status(root),
      executed = TRUE
    )
  )
}

main <- function() {
  arguments <- commandArgs(
    trailingOnly = TRUE
  )
  dry_run <- "--dry-run" %in% arguments

  result <- run_full_pipeline(
    execute = !dry_run
  )

  message(
    sprintf(
      "Full pipeline %s.",
      if (isTRUE(result$executed)) {
        "passed"
      } else {
        "inventory passed"
      }
    )
  )
}

if (sys.nframe() == 0L) {
  main()
}
