setup_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = setup_env)
sys.source(here::here("R", "database.R"), envir = setup_env)

run_setup_smoke_test <- function() {
  config <- setup_env$read_study_config()
  connection <- setup_env$connect_eunomia(
    dataset_name = config$database$dataset_name
  )
  on.exit(setup_env$disconnect_safely(connection), add = TRUE)

  setup_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )

  person_count_result <- DatabaseConnector::querySql(
    connection = connection,
    sql = "SELECT COUNT(*) AS person_count FROM main.person;"
  )
  valid_person_count <- nrow(person_count_result) == 1L &&
    "person_count" %in% names(person_count_result) &&
    length(person_count_result$person_count) == 1L &&
    is.numeric(person_count_result$person_count) &&
    !is.na(person_count_result$person_count) &&
    person_count_result$person_count >= 0

  if (!isTRUE(valid_person_count)) {
    cli::cli_abort(
      "Person count query must return exactly one non-negative {.field person_count}."
    )
  }

  print(person_count_result)
  message("PASS: Eunomia setup smoke test completed successfully.")
  invisible(person_count_result)
}

run_setup_smoke_test()
