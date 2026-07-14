#' Validate a Eunomia dataset name
#'
#' @param dataset_name Eunomia dataset name.
#'
#' @return The validated dataset name.
validate_eunomia_dataset_name <- function(dataset_name) {
  valid_dataset_name <-
    checkmate::test_string(dataset_name, min.chars = 1L) &&
    grepl("^[A-Za-z0-9_-]+$", dataset_name)

  if (!valid_dataset_name) {
    cli::cli_abort(
      paste0(
        "{.arg dataset_name} must be one non-empty character value containing ",
        "only letters, numbers, hyphens, and underscores."
      ),
      class = "eunomia_argument_error"
    )
  }

  dataset_name
}

#' Check whether a path contains a symbolic link
#'
#' @param path Path to inspect, including all of its ancestors.
#'
#' @return `TRUE` when the path or one of its ancestors is a symbolic link.
path_contains_symbolic_link <- function(path) {
  current_path <- fs::path_abs(path)

  repeat {
    if (isTRUE(fs::is_link(current_path))) {
      return(TRUE)
    }

    parent_path <- fs::path_dir(current_path)
    if (identical(as.character(parent_path), as.character(current_path))) {
      break
    }
    current_path <- parent_path
  }

  FALSE
}

#' Canonicalize a path that may not exist yet
#'
#' Resolves the nearest existing ancestor and appends any unresolved path
#' components in their original order.
#'
#' @param path Path to canonicalize.
#'
#' @return The canonical absolute path.
canonicalize_path <- function(path) {
  current_path <- fs::path_abs(path)
  unresolved_components <- character()

  while (!isTRUE(fs::file_exists(current_path))) {
    unresolved_components <- c(
      as.character(fs::path_file(current_path)),
      unresolved_components
    )
    current_path <- fs::path_dir(current_path)
  }

  canonical_path <- fs::path_real(current_path)
  for (component in unresolved_components) {
    canonical_path <- fs::path(canonical_path, component)
  }

  canonical_path
}

#' Validate a project-local Eunomia database file
#'
#' Canonicalizes the allowed directory and candidate path before checking that
#' the candidate is a file path strictly below the allowed data directory.
#'
#' @param database_file Candidate SQLite database path.
#'
#' @return The validated absolute database path.
validate_eunomia_database_file <- function(database_file) {
  valid_database_file <-
    checkmate::test_string(database_file, min.chars = 1L) &&
    nzchar(trimws(database_file))
  if (!valid_database_file) {
    cli::cli_abort(
      "{.arg database_file} must be one non-empty character value.",
      class = "eunomia_argument_error"
    )
  }

  allowed_directory_path <- fs::path_abs(
    here::here("data", "raw", "eunomia")
  )
  candidate_path <- fs::path_abs(database_file)
  contains_parent_traversal <- any(
    unlist(fs::path_split(database_file), use.names = FALSE) == ".."
  )
  contains_symbolic_link <-
    path_contains_symbolic_link(allowed_directory_path) ||
    path_contains_symbolic_link(candidate_path)

  if (contains_parent_traversal || contains_symbolic_link) {
    cli::cli_abort(
      "{.arg database_file} must not contain parent traversal or symbolic links.",
      class = "eunomia_argument_error"
    )
  }

  allowed_directory <- canonicalize_path(allowed_directory_path)
  candidate_path <- canonicalize_path(candidate_path)
  candidate_is_directory <- identical(
    as.character(candidate_path),
    as.character(allowed_directory)
  )
  candidate_is_allowed <- fs::path_has_parent(
    candidate_path,
    allowed_directory
  )

  if (candidate_is_directory || !candidate_is_allowed) {
    cli::cli_abort(
      paste0(
        "{.arg database_file} must be a file below {.path ",
        "{as.character(allowed_directory)}}."
      ),
      class = "eunomia_argument_error"
    )
  }

  as.character(candidate_path)
}

#' Get the project Eunomia database path
#'
#' @param dataset_name Eunomia dataset name.
#'
#' @return The path to the project-local dataset SQLite database.
get_eunomia_database_path <- function(dataset_name = "GiBleed") {
  dataset_name <- validate_eunomia_dataset_name(dataset_name)
  database_file <- here::here(
    "data",
    "raw",
    "eunomia",
    paste0(dataset_name, "_5.3.sqlite")
  )
  validate_eunomia_database_file(database_file)
}

#' Create Eunomia connection details
#'
#' Ensures the database directory exists and obtains the requested Eunomia
#' database without overwriting an existing database file.
#'
#' @param dataset_name Eunomia dataset name. `NULL` selects GiBleed.
#' @param database_file Destination path for the SQLite database. `NULL` uses
#'   the project Eunomia database path.
#'
#' @return DatabaseConnector connection details for a SQLite database.
create_eunomia_connection_details <- function(dataset_name = NULL,
                                              database_file = NULL) {
  resolved_dataset_name <- if (is.null(dataset_name)) {
    "GiBleed"
  } else {
    validate_eunomia_dataset_name(dataset_name)
  }

  if (is.null(database_file)) {
    database_file <- get_eunomia_database_path(resolved_dataset_name)
  } else {
    database_file <- validate_eunomia_database_file(database_file)
  }

  expected_filename <- basename(
    get_eunomia_database_path(resolved_dataset_name)
  )
  if (!identical(basename(database_file), expected_filename)) {
    cli::cli_abort(
      paste0(
        "{.arg database_file} must use the dataset-specific filename ",
        "{.file {expected_filename}}."
      ),
      class = "eunomia_argument_error"
    )
  }

  if (isTRUE(fs::is_file(database_file)) &&
        !isTRUE(fs::is_link(database_file))) {
    return(DatabaseConnector::createConnectionDetails(
      dbms = "sqlite",
      server = database_file
    ))
  }

  if (identical(resolved_dataset_name, "GiBleed")) {
    fs::dir_create(dirname(database_file))

    return(Eunomia::getEunomiaConnectionDetails(
      databaseFile = database_file,
      dbms = "sqlite",
      overwrite = FALSE
    ))
  }

  cache_database_file <- validate_eunomia_database_file(
    here::here(
      "data",
      "raw",
      "eunomia",
      ".cache",
      expected_filename
    )
  )
  cache_directory <- dirname(cache_database_file)
  if (isTRUE(fs::file_exists(cache_directory)) &&
        !isTRUE(fs::is_dir(cache_directory))) {
    cli::cli_abort(
      "The Eunomia cache path must be a directory.",
      class = "eunomia_argument_error"
    )
  }
  if (identical(cache_database_file, database_file)) {
    cli::cli_abort(
      "The Eunomia cache database and final database paths must differ.",
      class = "eunomia_argument_error"
    )
  }

  fs::dir_create(dirname(database_file))
  fs::dir_create(cache_directory)

  returned_database_file <- Eunomia::getDatabaseFile(
    datasetName = resolved_dataset_name,
    cdmVersion = "5.3",
    pathToData = cache_directory,
    dbms = "sqlite",
    databaseFile = database_file,
    overwrite = FALSE
  )
  returned_database_file <- validate_eunomia_database_file(
    returned_database_file
  )

  DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = returned_database_file
  )
}

#' Connect to a Eunomia database
#'
#' @inheritParams create_eunomia_connection_details
#'
#' @return An open DatabaseConnector connection.
connect_eunomia <- function(dataset_name = NULL, database_file = NULL) {
  connection_details <- create_eunomia_connection_details(
    dataset_name = dataset_name,
    database_file = database_file
  )
  DatabaseConnector::connect(connection_details)
}

#' Disconnect a database connection safely
#'
#' @param connection A DatabaseConnector connection or `NULL`.
#'
#' @return `NULL`, invisibly.
disconnect_safely <- function(connection) {
  if (!is.null(connection)) {
    DatabaseConnector::disconnect(connection)
  }
  invisible(NULL)
}

#' Get required OMOP CDM table names
#'
#' @return A character vector of required lowercase OMOP table names.
get_required_omop_tables <- function() {
  c(
    "person",
    "observation_period",
    "drug_exposure",
    "condition_occurrence",
    "visit_occurrence",
    "measurement",
    "concept",
    "concept_ancestor"
  )
}

#' Validate required OMOP CDM tables
#'
#' Checks table names case-insensitively and reports every required table that
#' is absent from the selected schema.
#'
#' @param connection An open DatabaseConnector connection.
#' @param database_schema Database schema containing the OMOP CDM tables.
#'
#' @return `TRUE`, invisibly, when every required table exists.
validate_required_omop_tables <- function(connection, database_schema = "main") {
  valid_database_schema <-
    checkmate::test_string(database_schema, min.chars = 1L) &&
    nzchar(trimws(database_schema))
  if (!valid_database_schema) {
    cli::cli_abort(
      "{.arg database_schema} must be one non-empty character value.",
      class = "eunomia_argument_error"
    )
  }

  table_names <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = database_schema
  )
  missing_tables <- setdiff(
    tolower(get_required_omop_tables()),
    tolower(table_names)
  )

  if (length(missing_tables) > 0L) {
    cli::cli_abort(
      paste0(
        "Required OMOP table(s) missing from schema '",
        database_schema,
        "': ",
        paste(missing_tables, collapse = ", "),
        "."
      ),
      class = "missing_omop_tables_error"
    )
  }

  invisible(TRUE)
}
