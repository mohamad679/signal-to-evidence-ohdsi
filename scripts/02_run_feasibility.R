feasibility_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = feasibility_env)
sys.source(here::here("R", "database.R"), envir = feasibility_env)
sys.source(here::here("R", "feasibility.R"), envir = feasibility_env)

run_feasibility <- function() {
  config <- feasibility_env$read_study_config()
  connection <- suppressMessages(
    feasibility_env$connect_eunomia(
      dataset_name = config$database$dataset_name
    )
  )
  on.exit(feasibility_env$disconnect_safely(connection), add = TRUE)

  feasibility_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )
  drug_candidates <- feasibility_env$get_candidate_drugs(
    connection = connection,
    database_schema = config$project$database_schema,
    maximum_candidates = 12L
  )
  outcome_candidates <- feasibility_env$get_candidate_outcomes(
    connection = connection,
    database_schema = config$project$database_schema,
    maximum_candidates = 12L
  )
  feasibility_matrix <- feasibility_env$build_feasibility_matrix(
    connection = connection,
    drug_candidates = drug_candidates,
    outcome_candidates = outcome_candidates,
    config = config
  )
  output_path <- feasibility_env$write_feasibility_output(feasibility_matrix)
  output_information <- file.info(output_path)
  valid_output <- nrow(feasibility_matrix) > 0L &&
    file.exists(output_path) &&
    !is.na(output_information$size) &&
    output_information$size > 0
  if (!valid_output) {
    cli::cli_abort(
      "The feasibility matrix output is missing or empty.",
      class = "feasibility_output_error"
    )
  }

  cat(
    "Drug candidates: ", nrow(drug_candidates), "\n",
    "Outcome candidates: ", nrow(outcome_candidates), "\n",
    "Evaluated combinations: ", nrow(feasibility_matrix), "\n",
    "Feasible combinations: ", sum(feasibility_matrix$feasible), "\n",
    "PASS: Feasibility analysis completed successfully.\n",
    sep = ""
  )

  invisible(feasibility_matrix)
}

run_feasibility()
