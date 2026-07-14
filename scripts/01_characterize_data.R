characterization_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = characterization_env)
sys.source(here::here("R", "database.R"), envir = characterization_env)
sys.source(here::here("R", "characterization.R"), envir = characterization_env)

run_characterization <- function() {
  config <- characterization_env$read_study_config()
  connection <- characterization_env$connect_eunomia(
    dataset_name = config$database$dataset_name
  )
  on.exit(
    characterization_env$disconnect_safely(connection),
    add = TRUE
  )

  characterization_env$validate_required_omop_tables(
    connection = connection,
    database_schema = config$project$database_schema
  )
  characterization <- characterization_env$characterize_omop(
    connection = connection,
    database_schema = config$project$database_schema
  )
  output_paths <- characterization_env$write_characterization_outputs(
    characterization = characterization
  )

  output_information <- file.info(unname(output_paths))
  valid_outputs <- file.exists(unname(output_paths)) &
    !is.na(output_information$size) &
    output_information$size > 0
  if (!all(valid_outputs)) {
    invalid_outputs <- unname(output_paths)[!valid_outputs]
    cli::cli_abort(
      paste0(
        "Required characterization output(s) are missing or empty: ",
        paste(invalid_outputs, collapse = ", "),
        "."
      ),
      class = "characterization_output_error"
    )
  }

  get_record_count <- function(table_name) {
    count_rows <- characterization$table_counts[
      characterization$table_counts$table_name == table_name,
      "record_count",
      drop = TRUE
    ]
    if (length(count_rows) != 1L || is.na(count_rows)) {
      cli::cli_abort(
        "Expected exactly one aggregate count for OMOP table '{table_name}'.",
        class = "characterization_result_error"
      )
    }
    count_rows
  }

  compact_count <- function(value) {
    format(value, scientific = FALSE, trim = TRUE)
  }
  cat(
    "Person count: ", compact_count(get_record_count("person")), "\n",
    "Drug-exposure count: ",
    compact_count(get_record_count("drug_exposure")), "\n",
    "Condition-occurrence count: ",
    compact_count(get_record_count("condition_occurrence")), "\n",
    "Top-drug rows: ", nrow(characterization$top_drugs), "\n",
    "Top-condition rows: ", nrow(characterization$top_conditions), "\n",
    sep = ""
  )
  message("PASS: OMOP characterization completed successfully.")

  invisible(characterization)
}

run_characterization()
