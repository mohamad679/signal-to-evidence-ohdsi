cohort_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = cohort_env)
sys.source(here::here("R", "database.R"), envir = cohort_env)
sys.source(here::here("R", "cohorts.R"), envir = cohort_env)

run_cohort_construction <- function() {
  config <- cohort_env$read_study_config()
  database_file <- cohort_env$get_eunomia_database_path(
    dataset_name = if (is.null(config$database$dataset_name)) {
      "GiBleed"
    } else {
      config$database$dataset_name
    }
  )
  if (!isTRUE(fs::is_file(database_file))) {
    cli::cli_abort(
      "The existing project-local Eunomia database is unavailable.",
      class = "cohort_database_error"
    )
  }

  connection <- suppressMessages(
    cohort_env$connect_eunomia(
      dataset_name = config$database$dataset_name,
      database_file = database_file
    )
  )
  on.exit(cohort_env$disconnect_safely(connection), add = TRUE)

  cohort_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )
  cohort_tables <- cohort_env$create_study_cohorts(
    connection = connection,
    config = config
  )
  cohort_counts <- cohort_env$summarize_study_cohorts(
    connection = connection,
    cohort_tables = cohort_tables
  )

  feasibility_path <- here::here(
    "results",
    "tables",
    "targeted_feasibility_review.csv"
  )
  if (!isTRUE(fs::is_file(feasibility_path))) {
    cli::cli_abort(
      "The targeted feasibility review is unavailable.",
      class = "cohort_feasibility_error"
    )
  }
  feasibility_review <- readr::read_csv(
    feasibility_path,
    show_col_types = FALSE,
    progress = FALSE
  )
  required_columns <- c(
    "target_concept_id",
    "comparator_concept_id",
    "outcome_concept_id",
    "target_subject_count",
    "comparator_subject_count"
  )
  if (!all(required_columns %in% names(feasibility_review))) {
    cli::cli_abort(
      "The targeted feasibility review is missing required aggregate columns.",
      class = "cohort_feasibility_error"
    )
  }

  selected_row <- feasibility_review[
    feasibility_review$target_concept_id == config$cohorts$target$concept_id &
      feasibility_review$comparator_concept_id ==
        config$cohorts$comparator$concept_id &
      feasibility_review$outcome_concept_id == config$cohorts$outcome$concept_id,
    required_columns,
    drop = FALSE
  ]
  if (nrow(selected_row) != 1L) {
    cli::cli_abort(
      "The targeted feasibility review must contain exactly one selected row.",
      class = "cohort_feasibility_error"
    )
  }

  expected_target_count <- 1800
  expected_comparator_count <- 830
  selected_counts_match <-
    selected_row$target_subject_count == expected_target_count &&
    selected_row$comparator_subject_count == expected_comparator_count
  cohort_counts_match <-
    cohort_counts$target_subject_count == selected_row$target_subject_count &&
    cohort_counts$comparator_subject_count ==
    selected_row$comparator_subject_count
  if (!selected_counts_match || !cohort_counts_match) {
    cli::cli_abort(
      paste0(
        "Treatment cohort counts must agree with the selected feasibility row ",
        "and the prespecified counts of 1800 and 830."
      ),
      class = "cohort_feasibility_error"
    )
  }

  cohort_env$write_cohort_counts(cohort_counts)
  cat(
    "Target subjects: ", cohort_counts$target_subject_count, "\n",
    "Comparator subjects: ", cohort_counts$comparator_subject_count, "\n",
    "Outcome subjects: ", cohort_counts$outcome_subject_count, "\n",
    "Outcome entries: ", cohort_counts$outcome_entry_count, "\n",
    "Feasibility count agreement: PASS\n",
    "PASS: Study cohorts constructed successfully.\n",
    sep = ""
  )

  invisible(cohort_counts)
}

run_cohort_construction()
