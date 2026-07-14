#' Validate a temporary cohort table name
#'
#' @param table_name Temporary table name.
#'
#' @return The validated table name.
validate_cohort_table_name <- function(table_name) {
  valid_table_name <- checkmate::test_string(table_name, min.chars = 1L) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", table_name)

  if (!valid_table_name) {
    cli::cli_abort(
      paste0(
        "{.arg table_name} must be one non-empty character value containing only ",
        "letters, numbers, and underscores, and it must not begin with a number."
      ),
      class = "cohort_argument_error"
    )
  }

  table_name
}

#' Validate a database schema for cohort SQL
#'
#' @param database_schema OMOP CDM database schema.
#'
#' @return The validated schema.
validate_cohort_schema <- function(database_schema) {
  valid_schema <- checkmate::test_string(database_schema, min.chars = 1L) &&
    grepl(
      "^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*$",
      database_schema
    )
  if (!valid_schema) {
    cli::cli_abort(
      "{.arg database_schema} must be a valid schema identifier.",
      class = "cohort_argument_error"
    )
  }
  database_schema
}

#' Validate one whole-number cohort parameter
#'
#' @param value Value to validate.
#' @param argument Argument name used in errors.
#' @param lower Smallest permitted value.
#'
#' @return The validated value as a numeric whole number.
validate_cohort_integer <- function(value, argument, lower = 0L) {
  if (!checkmate::test_integerish(
    value,
    lower = lower,
    len = 1L,
    any.missing = FALSE
  )) {
    cli::cli_abort(
      "{.arg {argument}} must be one whole number at least {lower}.",
      class = "cohort_argument_error"
    )
  }
  as.numeric(value)
}

#' Validate an open DatabaseConnector connection
#'
#' @param connection DatabaseConnector connection.
#'
#' @return The connection, invisibly.
validate_cohort_connection <- function(connection) {
  if (!inherits(connection, "DatabaseConnectorDbiConnection")) {
    cli::cli_abort(
      "{.arg connection} must be an open DatabaseConnector connection.",
      class = "cohort_argument_error"
    )
  }
  invisible(connection)
}

#' Validate and extract cohort-construction configuration
#'
#' @param config Study configuration.
#'
#' @return Named validated cohort parameters.
validate_cohort_config <- function(config) {
  if (!checkmate::test_list(config, names = "unique") ||
        is.null(config$project) ||
        is.null(config$design) ||
        is.null(config$cohorts)) {
    cli::cli_abort(
      "{.arg config} must contain project, design, and cohorts sections.",
      class = "cohort_argument_error"
    )
  }

  cohort_names <- c("target", "comparator", "outcome")
  if (!all(cohort_names %in% names(config$cohorts))) {
    cli::cli_abort(
      "{.arg config} is missing a target, comparator, or outcome cohort.",
      class = "cohort_argument_error"
    )
  }

  design_names <- c(
    "washout_days",
    "minimum_prior_observation_days",
    "risk_window_start_days",
    "risk_window_end_days"
  )
  if (!all(design_names %in% names(config$design))) {
    cli::cli_abort(
      "{.arg config} is missing required cohort design values.",
      class = "cohort_argument_error"
    )
  }

  database_schema <- validate_cohort_schema(config$project$database_schema)
  target_concept_id <- validate_cohort_integer(
    config$cohorts$target$concept_id,
    "config$cohorts$target$concept_id",
    lower = 1L
  )
  comparator_concept_id <- validate_cohort_integer(
    config$cohorts$comparator$concept_id,
    "config$cohorts$comparator$concept_id",
    lower = 1L
  )
  outcome_concept_id <- validate_cohort_integer(
    config$cohorts$outcome$concept_id,
    "config$cohorts$outcome$concept_id",
    lower = 1L
  )
  if (target_concept_id == comparator_concept_id) {
    cli::cli_abort(
      "Target and comparator concept identifiers must be distinct.",
      class = "cohort_argument_error"
    )
  }

  washout_days <- validate_cohort_integer(
    config$design$washout_days,
    "config$design$washout_days"
  )
  prior_observation_days <- validate_cohort_integer(
    config$design$minimum_prior_observation_days,
    "config$design$minimum_prior_observation_days"
  )
  risk_window_start_days <- validate_cohort_integer(
    config$design$risk_window_start_days,
    "config$design$risk_window_start_days",
    lower = 1L
  )
  risk_window_end_days <- validate_cohort_integer(
    config$design$risk_window_end_days,
    "config$design$risk_window_end_days",
    lower = risk_window_start_days
  )

  list(
    database_schema = database_schema,
    target_concept_id = target_concept_id,
    comparator_concept_id = comparator_concept_id,
    outcome_concept_id = outcome_concept_id,
    washout_days = washout_days,
    prior_observation_days = prior_observation_days,
    risk_window_end_days = risk_window_end_days
  )
}

#' Render and translate cohort SQL for SQLite
#'
#' @param sql_file SQL filename below `sql/cohorts`.
#' @param parameters Named SqlRender parameters.
#'
#' @return One translated SQL string.
render_cohort_sql <- function(sql_file, parameters) {
  valid_sql_file <- checkmate::test_string(sql_file, min.chars = 1L) &&
    identical(basename(sql_file), sql_file) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*\\.sql$", sql_file)
  if (!valid_sql_file) {
    cli::cli_abort(
      "{.arg sql_file} must be one SQL filename below {.path sql/cohorts}.",
      class = "cohort_argument_error"
    )
  }
  if (!checkmate::test_list(parameters, names = "unique") ||
        is.null(names(parameters)) ||
        any(!nzchar(names(parameters)))) {
    cli::cli_abort(
      "{.arg parameters} must be a named list with unique, non-empty names.",
      class = "cohort_argument_error"
    )
  }

  sql_path <- here::here("sql", "cohorts", sql_file)
  if (!checkmate::test_file_exists(sql_path, access = "r")) {
    cli::cli_abort(
      "Cohort SQL file does not exist or is not readable: {.path {sql_path}}.",
      class = "cohort_sql_file_error"
    )
  }

  sql <- SqlRender::readSql(sql_path)
  rendered_sql <- do.call(
    SqlRender::render,
    c(list(sql = sql), parameters)
  )
  translated_sql <- SqlRender::translate(
    sql = rendered_sql,
    targetDialect = "sqlite"
  )
  paste(translated_sql, collapse = "\n")
}

#' Drop one validated temporary cohort table
#'
#' @param connection DatabaseConnector connection.
#' @param table_name Validated temporary table name.
#'
#' @return `NULL`, invisibly.
drop_temporary_cohort <- function(connection, table_name) {
  DatabaseConnector::executeSql(
    connection = connection,
    sql = paste("DROP TABLE IF EXISTS", table_name),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  invisible(NULL)
}

#' Create one treatment cohort as a SQLite temporary table
#'
#' @param connection Open DatabaseConnector connection.
#' @param config Validated study configuration.
#' @param treatment_concept_id Target or comparator concept identifier.
#' @param cohort_definition_id Cohort definition identifier.
#' @param table_name Destination temporary table name.
#'
#' @return The validated temporary table name.
create_treatment_cohort <- function(connection,
                                    config,
                                    treatment_concept_id,
                                    cohort_definition_id,
                                    table_name) {
  validate_cohort_connection(connection)
  cohort_config <- validate_cohort_config(config)
  treatment_concept_id <- validate_cohort_integer(
    treatment_concept_id,
    "treatment_concept_id",
    lower = 1L
  )
  cohort_definition_id <- validate_cohort_integer(
    cohort_definition_id,
    "cohort_definition_id",
    lower = 1L
  )
  table_name <- validate_cohort_table_name(table_name)
  study_concept_ids <- c(
    cohort_config$target_concept_id,
    cohort_config$comparator_concept_id
  )
  if (!treatment_concept_id %in% study_concept_ids) {
    cli::cli_abort(
      "{.arg treatment_concept_id} must identify the target or comparator.",
      class = "cohort_argument_error"
    )
  }

  drop_temporary_cohort(connection, table_name)
  sql <- render_cohort_sql(
    "treatment_cohort.sql",
    parameters = c(
      cohort_config,
      list(
        table_name = table_name,
        treatment_concept_id = treatment_concept_id,
        cohort_definition_id = cohort_definition_id
      )
    )
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  table_name
}

#' Create the outcome cohort as a SQLite temporary table
#'
#' @param connection Open DatabaseConnector connection.
#' @param config Validated study configuration.
#' @param cohort_definition_id Cohort definition identifier.
#' @param table_name Destination temporary table name.
#'
#' @return The validated temporary table name.
create_outcome_cohort <- function(connection,
                                  config,
                                  cohort_definition_id = 3L,
                                  table_name = "outcome_cohort") {
  validate_cohort_connection(connection)
  cohort_config <- validate_cohort_config(config)
  cohort_definition_id <- validate_cohort_integer(
    cohort_definition_id,
    "cohort_definition_id",
    lower = 1L
  )
  table_name <- validate_cohort_table_name(table_name)

  drop_temporary_cohort(connection, table_name)
  sql <- render_cohort_sql(
    "outcome_cohort.sql",
    parameters = list(
      database_schema = cohort_config$database_schema,
      table_name = table_name,
      outcome_concept_id = cohort_config$outcome_concept_id,
      cohort_definition_id = cohort_definition_id
    )
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  table_name
}

#' Create all prespecified study cohorts
#'
#' @param connection Open DatabaseConnector connection.
#' @param config Validated study configuration.
#'
#' @return Named temporary treatment and outcome table names.
create_study_cohorts <- function(connection, config) {
  cohort_config <- validate_cohort_config(config)
  target_table <- create_treatment_cohort(
    connection = connection,
    config = config,
    treatment_concept_id = cohort_config$target_concept_id,
    cohort_definition_id = 1L,
    table_name = "target_cohort"
  )
  comparator_table <- create_treatment_cohort(
    connection = connection,
    config = config,
    treatment_concept_id = cohort_config$comparator_concept_id,
    cohort_definition_id = 2L,
    table_name = "comparator_cohort"
  )
  outcome_table <- create_outcome_cohort(
    connection = connection,
    config = config,
    cohort_definition_id = 3L,
    table_name = "outcome_cohort"
  )

  list(
    target = validate_cohort_table_name(target_table),
    comparator = validate_cohort_table_name(comparator_table),
    outcome = validate_cohort_table_name(outcome_table)
  )
}

#' Summarize the prespecified study cohorts
#'
#' @param connection Open DatabaseConnector connection.
#' @param cohort_tables Named target, comparator, and outcome table names.
#'
#' @return Exactly one row of aggregate cohort counts.
summarize_study_cohorts <- function(connection, cohort_tables) {
  validate_cohort_connection(connection)
  expected_names <- c("target", "comparator", "outcome")
  if (!checkmate::test_list(cohort_tables, names = "unique") ||
        !identical(names(cohort_tables), expected_names)) {
    cli::cli_abort(
      "{.arg cohort_tables} must be named target, comparator, and outcome.",
      class = "cohort_argument_error"
    )
  }
  cohort_tables <- lapply(cohort_tables, validate_cohort_table_name)

  sql <- paste(
    "SELECT",
    paste0(
      "  (SELECT COUNT(DISTINCT subject_id) FROM ",
      cohort_tables$target,
      ") AS target_subject_count,"
    ),
    paste0(
      "  (SELECT COUNT(*) FROM ",
      cohort_tables$target,
      ") AS target_entry_count,"
    ),
    paste0(
      "  (SELECT COUNT(DISTINCT subject_id) FROM ",
      cohort_tables$comparator,
      ") AS comparator_subject_count,"
    ),
    paste0(
      "  (SELECT COUNT(*) FROM ",
      cohort_tables$comparator,
      ") AS comparator_entry_count,"
    ),
    paste0(
      "  (SELECT COUNT(DISTINCT subject_id) FROM ",
      cohort_tables$outcome,
      ") AS outcome_subject_count,"
    ),
    paste0(
      "  (SELECT COUNT(*) FROM ",
      cohort_tables$outcome,
      ") AS outcome_entry_count"
    )
  )
  counts <- DatabaseConnector::querySql(
    connection = connection,
    sql = sql,
    snakeCaseToCamelCase = FALSE
  )
  counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  expected_columns <- c(
    "target_subject_count",
    "target_entry_count",
    "comparator_subject_count",
    "comparator_entry_count",
    "outcome_subject_count",
    "outcome_entry_count"
  )
  if (nrow(counts) != 1L || !identical(names(counts), expected_columns)) {
    cli::cli_abort(
      "Cohort summary did not return the required single aggregate row.",
      class = "cohort_summary_error"
    )
  }
  if (counts$target_subject_count != counts$target_entry_count ||
        counts$comparator_subject_count != counts$comparator_entry_count) {
    cli::cli_abort(
      "Treatment cohorts must contain exactly one entry per subject.",
      class = "cohort_summary_error"
    )
  }
  counts
}

#' Write aggregate cohort counts
#'
#' @param cohort_counts Exactly one aggregate cohort-count row.
#' @param path Destination CSV path.
#'
#' @return The output path.
write_cohort_counts <- function(
    cohort_counts,
    path = here::here("results", "tables", "cohort_counts.csv")) {
  expected_columns <- c(
    "target_subject_count",
    "target_entry_count",
    "comparator_subject_count",
    "comparator_entry_count",
    "outcome_subject_count",
    "outcome_entry_count"
  )
  if (!is.data.frame(cohort_counts) ||
        nrow(cohort_counts) != 1L ||
        !identical(names(cohort_counts), expected_columns)) {
    cli::cli_abort(
      "{.arg cohort_counts} must be exactly one row of required aggregate counts.",
      class = "cohort_output_error"
    )
  }
  if (!checkmate::test_string(path, min.chars = 1L) || !nzchar(trimws(path))) {
    cli::cli_abort(
      "{.arg path} must be one non-empty character value.",
      class = "cohort_output_error"
    )
  }

  column_names <- tolower(names(cohort_counts))
  identifier_columns <- column_names[
    column_names == "id" |
      grepl("_id$", column_names) |
      column_names %in% c("person_id", "subject_id")
  ]
  date_columns <- column_names[grepl("date", column_names)]
  date_type_columns <- column_names[vapply(cohort_counts, function(column) {
    inherits(column, "Date") || inherits(column, "POSIXt")
  }, logical(1L))]
  prohibited_columns <- unique(c(
    identifier_columns,
    date_columns,
    date_type_columns
  ))
  if (length(prohibited_columns) > 0L) {
    cli::cli_abort(
      paste0(
        "Cohort counts contain prohibited identifier or date column(s): ",
        paste(prohibited_columns, collapse = ", "),
        "."
      ),
      class = "cohort_output_error"
    )
  }

  valid_counts <- vapply(cohort_counts, function(column) {
    is.numeric(column) &&
      checkmate::test_integerish(column, lower = 0, len = 1L, any.missing = FALSE)
  }, logical(1L))
  if (!all(valid_counts)) {
    cli::cli_abort(
      "All cohort counts must be non-negative whole numbers.",
      class = "cohort_output_error"
    )
  }

  fs::dir_create(dirname(path))
  readr::write_csv(cohort_counts, file = path)
  path
}
