#' Read a SQL file
#'
#' @param sql_path Path to a readable SQL file.
#'
#' @return The SQL contents as one character string.
read_sql_file <- function(sql_path) {
  valid_path <- checkmate::test_string(sql_path, min.chars = 1L) &&
    checkmate::test_file_exists(sql_path, access = "r")
  if (!valid_path) {
    cli::cli_abort(
      "SQL file does not exist or is not readable: {.path {sql_path}}.",
      class = "characterization_sql_file_error"
    )
  }

  sql <- paste(readLines(sql_path, warn = FALSE), collapse = "\n")
  if (!nzchar(trimws(sql))) {
    cli::cli_abort(
      "SQL file is empty: {.path {sql_path}}.",
      class = "characterization_empty_sql_error"
    )
  }

  sql
}

#' Render, translate, and execute a SQL file
#'
#' @param connection An open DatabaseConnector connection.
#' @param sql_path Path to a SQL file.
#' @param parameters Named SqlRender parameter values.
#' @param target_dialect Database dialect used to translate the rendered SQL.
#'
#' @return A data frame containing the query result.
run_sql_file <- function(connection,
                         sql_path,
                         parameters = list(),
                         target_dialect = "sqlite") {
  valid_dialect <- checkmate::test_string(target_dialect, min.chars = 1L) &&
    nzchar(trimws(target_dialect))
  if (!valid_dialect) {
    cli::cli_abort(
      "{.arg target_dialect} must be one non-empty character value.",
      class = "characterization_argument_error"
    )
  }
  if (!checkmate::test_list(parameters, names = "unique")) {
    cli::cli_abort(
      "{.arg parameters} must be a named list with unique names.",
      class = "characterization_argument_error"
    )
  }

  sql <- read_sql_file(sql_path)
  rendered_sql <- do.call(
    SqlRender::render,
    c(list(sql = sql), parameters)
  )
  translated_sql <- SqlRender::translate(
    sql = rendered_sql,
    targetDialect = target_dialect
  )

  result <- DatabaseConnector::querySql(
    connection = connection,
    sql = translated_sql,
    snakeCaseToCamelCase = FALSE
  )
  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Validate an aggregate characterization result
#'
#' @param data Query result to validate.
#' @param required_columns Expected column names.
#' @param result_name Human-readable result name for errors.
#'
#' @return `TRUE`, invisibly, when the result is valid.
validate_characterization_result <- function(data,
                                             required_columns,
                                             result_name) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.field {result_name}} must be a data frame.",
      class = "characterization_result_error"
    )
  }
  valid_columns <- checkmate::test_character(
    required_columns,
    min.len = 1L,
    any.missing = FALSE,
    unique = TRUE
  ) && all(nzchar(required_columns))
  if (!valid_columns) {
    cli::cli_abort(
      "{.arg required_columns} must contain unique, non-empty column names.",
      class = "characterization_argument_error"
    )
  }
  if (!checkmate::test_string(result_name, min.chars = 1L) ||
        !nzchar(trimws(result_name))) {
    cli::cli_abort(
      "{.arg result_name} must be one non-empty character value.",
      class = "characterization_argument_error"
    )
  }
  if (anyDuplicated(names(data)) > 0L) {
    cli::cli_abort(
      "{.field {result_name}} contains duplicated column names.",
      class = "characterization_result_error"
    )
  }

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      paste0(
        result_name,
        " is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      class = "characterization_result_error"
    )
  }

  prohibited_columns <- intersect(
    c("person_id", "subject_id"),
    tolower(names(data))
  )
  if (length(prohibited_columns) > 0L) {
    cli::cli_abort(
      paste0(
        result_name,
        " contains prohibited person-level identifier column(s): ",
        paste(prohibited_columns, collapse = ", "),
        "."
      ),
      class = "characterization_result_error"
    )
  }

  invisible(TRUE)
}

#' Characterize core OMOP CDM data using aggregate queries
#'
#' @param connection An open DatabaseConnector connection.
#' @param database_schema Schema containing the OMOP CDM tables.
#'
#' @return A named list of four aggregate characterization data frames.
characterize_omop <- function(connection, database_schema = "main") {
  valid_schema <- checkmate::test_string(database_schema, min.chars = 1L) &&
    nzchar(trimws(database_schema))
  if (!valid_schema) {
    cli::cli_abort(
      "{.arg database_schema} must be one non-empty character value.",
      class = "characterization_argument_error"
    )
  }

  result_columns <- list(
    table_counts = c("table_name", "record_count", "person_count"),
    population_summary = c(
      "metric",
      "category",
      "person_count",
      "category_order"
    ),
    top_drugs = c(
      "concept_id",
      "concept_name",
      "exposure_count",
      "exposed_person_count"
    ),
    top_conditions = c(
      "concept_id",
      "concept_name",
      "occurrence_count",
      "affected_person_count"
    )
  )
  parameters <- list(cdm_database_schema = database_schema)
  characterization <- lapply(names(result_columns), function(result_name) {
    result <- run_sql_file(
      connection = connection,
      sql_path = here::here(
        "sql",
        "characterization",
        paste0(result_name, ".sql")
      ),
      parameters = parameters
    )
    validate_characterization_result(
      data = result,
      required_columns = result_columns[[result_name]],
      result_name = result_name
    )
    result
  })
  names(characterization) <- names(result_columns)

  characterization
}

#' Write aggregate characterization tables and plots
#'
#' @param characterization Named characterization results from
#'   [characterize_omop()].
#' @param tables_directory Directory for aggregate CSV tables.
#' @param figures_directory Directory for aggregate PNG figures.
#'
#' @return A named character vector containing all generated paths.
write_characterization_outputs <- function(
    characterization,
    tables_directory = here::here("results", "tables"),
    figures_directory = here::here("figures")) {
  required_results <- c(
    "table_counts",
    "population_summary",
    "top_drugs",
    "top_conditions"
  )
  valid_characterization <- is.list(characterization) &&
    !is.null(names(characterization)) &&
    all(required_results %in% names(characterization))
  if (!valid_characterization) {
    cli::cli_abort(
      "{.arg characterization} must contain all four named aggregate results.",
      class = "characterization_argument_error"
    )
  }
  output_directories <- list(
    tables_directory = tables_directory,
    figures_directory = figures_directory
  )
  valid_directories <- vapply(output_directories, function(directory) {
    checkmate::test_string(directory, min.chars = 1L) &&
      nzchar(trimws(directory))
  }, logical(1L))
  if (!all(valid_directories)) {
    cli::cli_abort(
      "Output directories must each be one non-empty character value.",
      class = "characterization_argument_error"
    )
  }

  result_columns <- list(
    table_counts = c("table_name", "record_count", "person_count"),
    population_summary = c(
      "metric",
      "category",
      "person_count",
      "category_order"
    ),
    top_drugs = c(
      "concept_id",
      "concept_name",
      "exposure_count",
      "exposed_person_count"
    ),
    top_conditions = c(
      "concept_id",
      "concept_name",
      "occurrence_count",
      "affected_person_count"
    )
  )
  for (result_name in required_results) {
    validate_characterization_result(
      data = characterization[[result_name]],
      required_columns = result_columns[[result_name]],
      result_name = result_name
    )
  }

  fs::dir_create(tables_directory)
  fs::dir_create(figures_directory)

  output_paths <- c(
    table_counts = file.path(tables_directory, "table_counts.csv"),
    population_summary = file.path(
      tables_directory,
      "population_summary.csv"
    ),
    top_drugs = file.path(tables_directory, "top_drugs.csv"),
    top_conditions = file.path(tables_directory, "top_conditions.csv"),
    age_distribution = file.path(figures_directory, "age_distribution.png"),
    follow_up_distribution = file.path(
      figures_directory,
      "follow_up_distribution.png"
    )
  )

  for (result_name in required_results) {
    readr::write_csv(
      characterization[[result_name]],
      output_paths[[result_name]]
    )
  }

  create_distribution_plot <- function(metric, title, x_label) {
    plot_data <- characterization$population_summary[
      characterization$population_summary$metric == metric,
      c("category", "person_count", "category_order"),
      drop = FALSE
    ]
    if (nrow(plot_data) == 0L) {
      cli::cli_abort(
        "Population summary contains no rows for metric '{metric}'.",
        class = "characterization_result_error"
      )
    }
    ordered_rows <- order(plot_data$category_order, plot_data$category)
    plot_data <- plot_data[ordered_rows, , drop = FALSE]
    plot_data$category <- factor(
      plot_data$category,
      levels = unique(plot_data$category)
    )
    category <- NULL
    person_count <- NULL

    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = category,
        y = person_count
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::labs(
        title = title,
        x = x_label,
        y = "Person count"
      )
  }

  age_plot <- create_distribution_plot(
    metric = "age_group",
    title = "Age at First Observation",
    x_label = "Age group"
  )
  follow_up_plot <- create_distribution_plot(
    metric = "follow_up_group",
    title = "Total Follow-up",
    x_label = "Follow-up group"
  )
  ggplot2::ggsave(
    filename = output_paths[["age_distribution"]],
    plot = age_plot,
    width = 8,
    height = 5,
    dpi = 150
  )
  ggplot2::ggsave(
    filename = output_paths[["follow_up_distribution"]],
    plot = follow_up_plot,
    width = 8,
    height = 5,
    dpi = 150
  )

  output_paths
}
