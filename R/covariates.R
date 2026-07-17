#' Validate a baseline covariate window
#'
#' @param start_days First day of the covariate window relative to index.
#' @param end_days Last day of the covariate window relative to index.
#'
#' @return A named integer vector containing the validated window.
validate_covariate_window <- function(start_days, end_days) {
  is_integer_scalar <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value == round(value) &&
      abs(value) <= .Machine$integer.max
  }

  if (!is_integer_scalar(start_days) || !is_integer_scalar(end_days)) {
    cli::cli_abort(
      "{.arg start_days} and {.arg end_days} must be finite whole-number scalars.",
      class = "covariate_argument_error"
    )
  }
  if (start_days > end_days) {
    cli::cli_abort(
      "{.arg start_days} must not be later than {.arg end_days}.",
      class = "covariate_argument_error"
    )
  }
  if (end_days > -1) {
    cli::cli_abort(
      "{.arg end_days} must be day -1 or earlier.",
      class = "covariate_argument_error"
    )
  }

  c(start_days = as.integer(start_days), end_days = as.integer(end_days))
}

#' Create prespecified baseline covariate settings
#'
#' Time-dependent clinical history stops on day -1. Demographic age and sex
#' are derived at index using the standard FeatureExtraction definitions.
#'
#' @return A FeatureExtraction covariate-settings object.
create_baseline_covariate_settings <- function() {
  long_term <- validate_covariate_window(-365L, -1L)
  medium_term <- validate_covariate_window(-180L, -1L)
  short_term <- validate_covariate_window(-30L, -1L)

  FeatureExtraction::createCovariateSettings(
    useDemographicsGender = TRUE,
    useDemographicsAge = TRUE,
    useDemographicsPostObservationTime = FALSE,
    useDemographicsTimeInCohort = FALSE,
    useConditionOccurrenceAnyTimePrior = TRUE,
    useConditionOccurrenceShortTerm = TRUE,
    useDrugExposureAnyTimePrior = TRUE,
    useDrugExposureShortTerm = TRUE,
    useMeasurementAnyTimePrior = TRUE,
    useMeasurementShortTerm = TRUE,
    useMeasurementRangeGroupLongTerm = TRUE,
    useMeasurementRangeGroupShortTerm = TRUE,
    useObservationAnyTimePrior = TRUE,
    useObservationShortTerm = TRUE,
    useObservationValueAsConceptLongTerm = TRUE,
    useObservationValueAsConceptShortTerm = TRUE,
    useDistinctConditionCountLongTerm = TRUE,
    useDistinctIngredientCountLongTerm = TRUE,
    useDistinctMeasurementCountLongTerm = TRUE,
    useDistinctObservationCountLongTerm = TRUE,
    useVisitCountLongTerm = TRUE,
    useVisitCountShortTerm = TRUE,
    useVisitConceptCountLongTerm = TRUE,
    useVisitConceptCountShortTerm = TRUE,
    longTermStartDays = long_term[["start_days"]],
    mediumTermStartDays = medium_term[["start_days"]],
    shortTermStartDays = short_term[["start_days"]],
    endDays = short_term[["end_days"]]
  )
}

#' Validate prespecified baseline covariate settings
#'
#' @param covariate_settings FeatureExtraction covariate settings.
#'
#' @return The validated settings object, invisibly.
validate_baseline_covariate_settings <- function(covariate_settings) {
  if (!inherits(covariate_settings, "covariateSettings")) {
    cli::cli_abort(
      "{.arg covariate_settings} must be a FeatureExtraction settings object.",
      class = "covariate_argument_error"
    )
  }

  for (start_field in c(
    "longTermStartDays",
    "mediumTermStartDays",
    "shortTermStartDays"
  )) {
    if (is.null(covariate_settings[[start_field]]) ||
          is.null(covariate_settings$endDays)) {
      cli::cli_abort(
        "{.arg covariate_settings} is missing required baseline windows.",
        class = "covariate_argument_error"
      )
    }
    validate_covariate_window(
      covariate_settings[[start_field]],
      covariate_settings$endDays
    )
  }

  prohibited_settings <- c(
    "DemographicsAgeGroup",
    "DemographicsRace",
    "DemographicsEthnicity",
    "DemographicsIndexYear",
    "DemographicsIndexMonth",
    "DemographicsPriorObservationTime",
    "DemographicsPostObservationTime",
    "DemographicsTimeInCohort",
    "DemographicsIndexYearMonth",
    "CareSiteId",
    "ConditionEraOverlapping",
    "ConditionGroupEraOverlapping",
    "DrugEraOverlapping",
    "DrugGroupEraOverlapping"
  )
  enabled_prohibited_settings <- vapply(
    prohibited_settings,
    function(setting) isTRUE(covariate_settings[[setting]]),
    logical(1L)
  )
  if (!identical(
    attr(covariate_settings, "fun", exact = TRUE),
    "getDbDefaultCovariateData"
  ) ||
    any(enabled_prohibited_settings) ||
    isTRUE(covariate_settings$temporal) ||
    isTRUE(covariate_settings$temporalSequence)) {
    cli::cli_abort(
      paste0(
        "{.arg covariate_settings} must use standard prespecified extraction, ",
        "age and sex only for demographics, and no post-index, cohort-end, ",
        "overlapping-era, or temporal-sequence covariates."
      ),
      class = "covariate_argument_error"
    )
  }

  invisible(covariate_settings)
}

#' Validate a persistent covariate cohort table name
#'
#' @param table_name Candidate unqualified SQLite table name.
#'
#' @return The validated table name.
validate_covariate_table_name <- function(table_name) {
  valid_name <- checkmate::test_string(table_name, min.chars = 1L) &&
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", table_name)
  if (!valid_name) {
    cli::cli_abort(
      paste0(
        "{.arg table_name} must contain only letters, numbers, and underscores, ",
        "and must not begin with a number."
      ),
      class = "covariate_argument_error"
    )
  }
  table_name
}

#' Drop the persistent FeatureExtraction cohort table
#'
#' @param connection Open DatabaseConnector connection.
#' @param table_name Persistent working table name.
#'
#' @return `NULL`, invisibly.
drop_feature_extraction_cohort_table <- function(connection, table_name) {
  validate_cohort_connection(connection)
  table_name <- validate_covariate_table_name(table_name)
  DatabaseConnector::executeSql(
    connection = connection,
    sql = paste("DROP TABLE IF EXISTS main.", table_name, sep = ""),
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  invisible(NULL)
}

#' Create a persistent combined treatment cohort table
#'
#' @param connection Open DatabaseConnector connection.
#' @param cohort_tables Named study cohort table names returned by
#'   `create_study_cohorts()`.
#' @param table_name Persistent working table name.
#'
#' @return The validated working table name.
create_feature_extraction_cohort_table <- function(
    connection,
    cohort_tables,
    table_name = "study_covariate_cohort") {
  validate_cohort_connection(connection)
  table_name <- validate_covariate_table_name(table_name)
  expected_names <- c("target", "comparator", "outcome")
  if (!checkmate::test_list(cohort_tables, names = "unique") ||
        !identical(names(cohort_tables), expected_names)) {
    cli::cli_abort(
      "{.arg cohort_tables} must be named target, comparator, and outcome.",
      class = "covariate_argument_error"
    )
  }
  target_table <- validate_cohort_table_name(cohort_tables$target)
  comparator_table <- validate_cohort_table_name(cohort_tables$comparator)

  drop_feature_extraction_cohort_table(connection, table_name)
  creation_succeeded <- FALSE
  on.exit(
    if (!creation_succeeded) {
      drop_feature_extraction_cohort_table(connection, table_name)
    },
    add = TRUE
  )

  combined_query <- paste(
    "SELECT cohort_definition_id, subject_id, cohort_start_date, cohort_end_date",
    "FROM (",
    paste0(
      "  SELECT cohort_definition_id, subject_id, cohort_start_date, ",
      "cohort_end_date FROM ",
      target_table,
      " WHERE cohort_definition_id = 1"
    ),
    "  UNION ALL",
    paste0(
      "  SELECT cohort_definition_id, subject_id, cohort_start_date, ",
      "cohort_end_date FROM ",
      comparator_table,
      " WHERE cohort_definition_id = 2"
    ),
    ") treatment_entries"
  )
  create_sql <- paste(
    paste0("CREATE TABLE main.", table_name, " AS"),
    "SELECT",
    paste0(
      "  ROW_NUMBER() OVER (ORDER BY cohort_definition_id, subject_id, ",
      "cohort_start_date, cohort_end_date) AS row_id,"
    ),
    "  cohort_definition_id,",
    "  subject_id,",
    "  cohort_start_date,",
    "  cohort_end_date",
    paste0("FROM (", combined_query, ") ordered_entries")
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = create_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  index_name <- paste0(table_name, "_entry_uq")
  index_sql <- paste0(
    "CREATE UNIQUE INDEX main.",
    index_name,
    " ON ",
    table_name,
    " (cohort_definition_id, subject_id, cohort_start_date)"
  )
  DatabaseConnector::executeSql(
    connection = connection,
    sql = index_sql,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )

  creation_succeeded <- TRUE
  table_name
}

#' Extract baseline covariates from the local CDM
#'
#' @param connection_details DatabaseConnector connection details.
#' @param cdm_database_schema Schema containing the OMOP CDM and working table.
#' @param cohort_table Persistent FeatureExtraction cohort table name.
#' @param covariate_settings FeatureExtraction covariate settings.
#'
#' @return A FeatureExtraction covariate-data object.
extract_baseline_covariates <- function(
    connection_details,
    cdm_database_schema,
    cohort_table,
    covariate_settings) {
  if (!inherits(connection_details, "ConnectionDetails")) {
    cli::cli_abort(
      "{.arg connection_details} must be DatabaseConnector connection details.",
      class = "covariate_argument_error"
    )
  }
  cdm_database_schema <- validate_cohort_schema(cdm_database_schema)
  cohort_table <- validate_covariate_table_name(cohort_table)
  validate_baseline_covariate_settings(covariate_settings)

  count_connection <- DatabaseConnector::connect(connection_details)
  on.exit(DatabaseConnector::disconnect(count_connection), add = TRUE)
  count_sql <- paste(
    "SELECT cohort_definition_id,",
    "COUNT(DISTINCT subject_id) AS subject_count",
    paste0("FROM ", cdm_database_schema, ".", cohort_table),
    "WHERE cohort_definition_id IN (1, 2)",
    "GROUP BY cohort_definition_id",
    "ORDER BY cohort_definition_id"
  )
  cohort_counts <- DatabaseConnector::querySql(
    connection = count_connection,
    sql = count_sql,
    snakeCaseToCamelCase = FALSE
  )
  cohort_counts <- as.data.frame(cohort_counts, stringsAsFactors = FALSE)
  if (!identical(as.integer(cohort_counts$cohort_definition_id), c(1L, 2L)) ||
        any(cohort_counts$subject_count <= 0)) {
    cli::cli_abort(
      "The working cohort table must contain non-empty cohort IDs 1 and 2.",
      class = "covariate_data_error"
    )
  }

  covariate_data <- FeatureExtraction::getDbCovariateData(
    connectionDetails = connection_details,
    cdmDatabaseSchema = cdm_database_schema,
    cdmVersion = "5",
    cohortTable = cohort_table,
    cohortDatabaseSchema = cdm_database_schema,
    cohortTableIsTemp = FALSE,
    cohortIds = c(1L, 2L),
    rowIdField = "row_id",
    covariateSettings = covariate_settings,
    aggregated = FALSE
  )

  metadata <- attr(covariate_data, "metaData")
  metadata$cohortSubjectCounts <- stats::setNames(
    as.integer(cohort_counts$subject_count),
    as.character(cohort_counts$cohort_definition_id)
  )
  attr(covariate_data, "metaData") <- metadata
  covariate_data
}

#' Create, use, and remove the persistent FeatureExtraction working table
#'
#' The table is registered for removal before creation so it is dropped after
#' successful extraction and on every error path.
#'
#' @param connection Open DatabaseConnector connection used to create cohorts.
#' @param connection_details DatabaseConnector connection details.
#' @param cdm_database_schema Schema containing the OMOP CDM.
#' @param cohort_tables Named study cohort table names.
#' @param covariate_settings FeatureExtraction covariate settings.
#' @param cohort_table Persistent FeatureExtraction working table name.
#'
#' @return A FeatureExtraction covariate-data object.
extract_study_baseline_covariates <- function(
    connection,
    connection_details,
    cdm_database_schema,
    cohort_tables,
    covariate_settings = create_baseline_covariate_settings(),
    cohort_table = "study_covariate_cohort") {
  validate_cohort_connection(connection)
  cohort_table <- validate_covariate_table_name(cohort_table)
  validate_baseline_covariate_settings(covariate_settings)

  on.exit(
    drop_feature_extraction_cohort_table(
      connection = connection,
      table_name = cohort_table
    ),
    add = TRUE
  )
  create_feature_extraction_cohort_table(
    connection = connection,
    cohort_tables = cohort_tables,
    table_name = cohort_table
  )
  extract_baseline_covariates(
    connection_details = connection_details,
    cdm_database_schema = cdm_database_schema,
    cohort_table = cohort_table,
    covariate_settings = covariate_settings
  )
}

#' Collect one non-person-level covariate reference table
#'
#' @param covariate_data FeatureExtraction covariate data.
#' @param table_name Reference table name.
#'
#' @return A reference data frame.
collect_covariate_reference <- function(covariate_data, table_name) {
  reference_table <- covariate_data[[table_name]]
  if (is.null(reference_table)) {
    cli::cli_abort(
      "Covariate data is missing its {.field {table_name}} reference.",
      class = "covariate_data_error"
    )
  }
  as.data.frame(
    dplyr::collect(reference_table),
    stringsAsFactors = FALSE
  )
}

#' Validate extracted baseline covariate data
#'
#' @param covariate_data FeatureExtraction covariate data.
#'
#' @return `TRUE`, invisibly.
validate_covariate_data <- function(covariate_data) {
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    cli::cli_abort(
      "{.arg covariate_data} must be FeatureExtraction covariate data.",
      class = "covariate_data_error"
    )
  }
  covariate_ref <- collect_covariate_reference(covariate_data, "covariateRef")
  analysis_ref <- collect_covariate_reference(covariate_data, "analysisRef")
  if (nrow(covariate_ref) == 0L || nrow(analysis_ref) == 0L) {
    cli::cli_abort(
      "Covariate and analysis references must both be non-empty.",
      class = "covariate_data_error"
    )
  }

  metadata <- attr(covariate_data, "metaData")
  cohort_ids <- as.integer(unlist(metadata$cohortIds, use.names = FALSE))
  cohort_counts <- metadata$cohortSubjectCounts
  if (!all(c(1L, 2L) %in% cohort_ids) ||
        is.null(cohort_counts) ||
        !all(c("1", "2") %in% names(cohort_counts)) ||
        any(cohort_counts[c("1", "2")] <= 0)) {
    cli::cli_abort(
      "Covariate data must represent non-empty cohort IDs 1 and 2.",
      class = "covariate_data_error"
    )
  }

  required_covariate_columns <- c("covariateName", "analysisId")
  required_analysis_columns <- c("analysisName", "analysisId", "isBinary")
  if (!all(required_covariate_columns %in% names(covariate_ref)) ||
        !all(required_analysis_columns %in% names(analysis_ref))) {
    cli::cli_abort(
      "Covariate reference data is missing required metadata columns.",
      class = "covariate_data_error"
    )
  }

  feature_names <- c(covariate_ref$covariateName, analysis_ref$analysisName)
  normalized_names <- tolower(gsub("[^a-z0-9]+", " ", feature_names))
  prohibited_patterns <- c(
    "\\bpost index\\b",
    "\\bafter index\\b",
    "\\brisk window\\b",
    "\\boutcome after index\\b",
    "\\bfuture follow up\\b",
    "\\bfuture observation\\b",
    "\\bpost observation\\b",
    "\\btime in cohort\\b",
    "\\bcohort end\\b"
  )
  has_prohibited_name <- vapply(normalized_names, function(feature_name) {
    any(vapply(prohibited_patterns, grepl, logical(1L), x = feature_name))
  }, logical(1L))
  if (any(has_prohibited_name)) {
    cli::cli_abort(
      "Covariate names must not encode post-index or future information.",
      class = "covariate_data_error"
    )
  }

  invisible(TRUE)
}

#' Convert FeatureExtraction binary flags to logical values
#'
#' @param values FeatureExtraction `isBinary` values.
#'
#' @return A logical vector.
as_covariate_binary_flag <- function(values) {
  normalized <- tolower(trimws(as.character(values)))
  true_values <- c("1", "true", "t", "yes", "y")
  false_values <- c("0", "false", "f", "no", "n")
  if (any(!normalized %in% c(true_values, false_values))) {
    cli::cli_abort(
      "Analysis references contain unrecognized binary flags.",
      class = "covariate_data_error"
    )
  }
  normalized %in% true_values
}

#' Summarize extracted baseline covariates
#'
#' @param covariate_data Valid FeatureExtraction covariate data.
#'
#' @return Exactly one aggregate metadata row.
summarize_covariates <- function(covariate_data) {
  validate_covariate_data(covariate_data)
  covariate_ref <- collect_covariate_reference(covariate_data, "covariateRef")
  analysis_ref <- collect_covariate_reference(covariate_data, "analysisRef")
  metadata <- attr(covariate_data, "metaData")
  cohort_counts <- metadata$cohortSubjectCounts

  analysis_match <- match(covariate_ref$analysisId, analysis_ref$analysisId)
  if (any(is.na(analysis_match))) {
    cli::cli_abort(
      "Every covariate reference must map to an analysis reference.",
      class = "covariate_data_error"
    )
  }
  binary_analysis <- as_covariate_binary_flag(analysis_ref$isBinary)
  binary_covariates <- binary_analysis[analysis_match]

  data.frame(
    target_subject_count = as.integer(cohort_counts[["1"]]),
    comparator_subject_count = as.integer(cohort_counts[["2"]]),
    covariate_count = as.integer(nrow(covariate_ref)),
    analysis_count = as.integer(nrow(analysis_ref)),
    binary_covariate_count = as.integer(sum(binary_covariates)),
    continuous_covariate_count = as.integer(sum(!binary_covariates)),
    check.names = FALSE
  )
}

#' Validate an exact project output path
#'
#' @param path Candidate output path.
#' @param expected_path Required project output path.
#' @param allowed_directory Directory that must contain the output.
#'
#' @return The canonical absolute output path.
validate_covariate_output_path <- function(
    path,
    expected_path,
    allowed_directory) {
  valid_path <- checkmate::test_string(path, min.chars = 1L) &&
    nzchar(trimws(path))
  if (!valid_path) {
    cli::cli_abort(
      "{.arg path} must be one non-empty character value.",
      class = "covariate_output_error"
    )
  }

  path_components <- unlist(fs::path_split(path), use.names = FALSE)
  contains_parent_traversal <- any(path_components == "..")
  contains_symbolic_link <-
    path_contains_symbolic_link(allowed_directory) ||
    path_contains_symbolic_link(path)
  if (contains_parent_traversal || contains_symbolic_link) {
    cli::cli_abort(
      "{.arg path} must not contain parent traversal or symbolic links.",
      class = "covariate_output_error"
    )
  }

  canonical_directory <- canonicalize_path(allowed_directory)
  canonical_expected_path <- canonicalize_path(expected_path)
  canonical_path <- canonicalize_path(path)
  is_below_allowed_directory <- fs::path_has_parent(
    canonical_path,
    canonical_directory
  )
  if (!is_below_allowed_directory ||
    !identical(
      as.character(canonical_path),
      as.character(canonical_expected_path)
    )) {
    cli::cli_abort(
      "{.arg path} must be the prescribed project output path.",
      class = "covariate_output_error"
    )
  }

  as.character(canonical_path)
}

#' Write aggregate baseline covariate metadata
#'
#' @param summary Exactly one aggregate covariate summary row.
#' @param path Destination CSV path.
#'
#' @return The output path.
write_covariate_summary <- function(
    summary,
    path = here::here("results", "tables", "covariate_summary.csv")) {
  expected_columns <- c(
    "target_subject_count",
    "comparator_subject_count",
    "covariate_count",
    "analysis_count",
    "binary_covariate_count",
    "continuous_covariate_count"
  )
  if (!is.data.frame(summary) ||
        nrow(summary) != 1L ||
        !identical(names(summary), expected_columns)) {
    cli::cli_abort(
      "{.arg summary} must be exactly one row of aggregate covariate metadata.",
      class = "covariate_output_error"
    )
  }
  valid_counts <- vapply(summary, function(value) {
    checkmate::test_integerish(
      value,
      lower = 0,
      len = 1L,
      any.missing = FALSE
    )
  }, logical(1L))
  if (!all(valid_counts)) {
    cli::cli_abort(
      "All covariate summary values must be non-negative whole numbers.",
      class = "covariate_output_error"
    )
  }
  expected_path <- here::here(
    "results",
    "tables",
    "covariate_summary.csv"
  )
  destination <- validate_covariate_output_path(
    path = path,
    expected_path = expected_path,
    allowed_directory = here::here("results", "tables")
  )
  fs::dir_create(dirname(destination))
  readr::write_csv(summary, file = destination)
  destination
}

#' Save person-level baseline covariates to the ignored local directory
#'
#' @param covariate_data FeatureExtraction covariate data.
#' @param path Required destination path,
#'   `data/derived/baseline_covariates.rds`.
#'
#' @return The validated output path.
save_local_covariate_data <- function(
    covariate_data,
    path = here::here("data", "derived", "baseline_covariates.rds")) {
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    cli::cli_abort(
      "{.arg covariate_data} must be FeatureExtraction covariate data.",
      class = "covariate_output_error"
    )
  }
  allowed_directory_path <- here::here("data", "derived")
  expected_path <- here::here(
    "data",
    "derived",
    "baseline_covariates.rds"
  )
  destination <- validate_covariate_output_path(
    path = path,
    expected_path = expected_path,
    allowed_directory = allowed_directory_path
  )
  fs::dir_create(dirname(destination))
  FeatureExtraction::saveCovariateData(
    covariateData = covariate_data,
    file = destination
  )
  destination
}
