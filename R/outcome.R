#' Validate a propensity-score matched population
#'
#' @param matched_population A data frame containing the matched population.
#'
#' @return The input data frame, invisibly.
validate_matched_population <- function(matched_population) {
  if (!is.data.frame(matched_population)) {
    stop("`matched_population` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(
    "rowId",
    "treatment",
    "propensityScore",
    "preferenceScore",
    "matchId"
  )

  if (!identical(names(matched_population), required_columns)) {
    stop(
      paste0(
        "`matched_population` must contain exactly these columns in order: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(matched_population) == 0L) {
    stop(
      "`matched_population` must contain at least one row.",
      call. = FALSE
    )
  }

  if (anyNA(matched_population$rowId)) {
    stop("`rowId` must not contain missing values.", call. = FALSE)
  }

  if (anyDuplicated(matched_population$rowId) > 0L) {
    stop("`rowId` must be unique.", call. = FALSE)
  }

  treatment <- matched_population$treatment

  if (!is.numeric(treatment) ||
        anyNA(treatment) ||
        any(!treatment %in% c(0, 1))) {
    stop(
      "`treatment` must contain only numeric 0 and 1 values.",
      call. = FALSE
    )
  }

  score_columns <- c("propensityScore", "preferenceScore")

  for (column_name in score_columns) {
    values <- matched_population[[column_name]]

    if (!is.numeric(values) ||
          anyNA(values) ||
          any(!is.finite(values)) ||
          any(values < 0 | values > 1)) {
      stop(
        paste0(
          "`",
          column_name,
          "` must contain finite numeric values between 0 and 1."
        ),
        call. = FALSE
      )
    }
  }

  match_id <- matched_population$matchId

  if (!is.atomic(match_id) || anyNA(match_id)) {
    stop(
      "`matchId` must contain non-missing atomic values.",
      call. = FALSE
    )
  }

  match_id_text <- as.character(match_id)

  if (any(!nzchar(match_id_text))) {
    stop("`matchId` values must not be empty.", call. = FALSE)
  }

  pair_indices <- split(
    seq_len(nrow(matched_population)),
    match_id_text,
    drop = TRUE
  )

  valid_pairs <- vapply(
    pair_indices,
    function(indices) {
      length(indices) == 2L &&
        identical(
          sort(as.integer(treatment[indices])),
          c(0L, 1L)
        )
    },
    logical(1)
  )

  if (length(valid_pairs) == 0L || any(!valid_pairs)) {
    stop(
      "Each `matchId` must contain exactly one target and one comparator.",
      call. = FALSE
    )
  }

  invisible(matched_population)
}

#' Load and validate a propensity-score matched population
#'
#' @param path Path to the local matched-population RDS artifact.
#'
#' @return A validated matched-population data frame.
load_matched_population <- function(path) {
  if (!is.character(path) ||
        length(path) != 1L ||
        is.na(path) ||
        !nzchar(path)) {
    stop(
      "`path` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (!file.exists(path)) {
    stop(
      "Matched-population artifact does not exist.",
      call. = FALSE
    )
  }

  matched_population <- readRDS(path)
  validate_matched_population(matched_population)

  matched_population
}

#' Validate treatment-cohort linkage data
#'
#' @param treatment_population A data frame linking study row identifiers to
#'   treatment cohort subjects and index dates.
#'
#' @return The input data frame, invisibly.
validate_treatment_population <- function(treatment_population) {
  if (!is.data.frame(treatment_population)) {
    stop("`treatment_population` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(
    "rowId",
    "subjectId",
    "cohortStartDate",
    "cohortEndDate",
    "treatment"
  )

  if (!identical(names(treatment_population), required_columns)) {
    stop(
      paste0(
        "`treatment_population` must contain exactly these columns in order: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(treatment_population) == 0L) {
    stop("`treatment_population` must not be empty.", call. = FALSE)
  }

  if (anyNA(treatment_population$rowId)) {
    stop("Treatment `rowId` values must not be missing.", call. = FALSE)
  }

  if (anyDuplicated(treatment_population$rowId) > 0L) {
    stop("Treatment `rowId` values must be unique.", call. = FALSE)
  }

  if (anyNA(treatment_population$subjectId)) {
    stop("Treatment `subjectId` values must not be missing.", call. = FALSE)
  }

  date_columns <- c("cohortStartDate", "cohortEndDate")

  for (column_name in date_columns) {
    values <- treatment_population[[column_name]]

    if (!inherits(values, "Date")) {
      stop(
        paste0("Treatment `", column_name, "` must have class Date."),
        call. = FALSE
      )
    }

    if (anyNA(values)) {
      stop(
        paste0("Treatment `", column_name, "` must not be missing."),
        call. = FALSE
      )
    }
  }

  invalid_end_date <- treatment_population$cohortEndDate <
    treatment_population$cohortStartDate

  if (any(invalid_end_date)) {
    stop(
      "Treatment cohort end dates must not precede index dates.",
      call. = FALSE
    )
  }

  treatment <- treatment_population$treatment

  if (!is.numeric(treatment)) {
    stop("Treatment assignments must be numeric.", call. = FALSE)
  }

  if (anyNA(treatment) || any(!treatment %in% c(0, 1))) {
    stop(
      "Treatment assignments must contain only 0 and 1.",
      call. = FALSE
    )
  }

  invisible(treatment_population)
}

#' Validate an outcome cohort
#'
#' @param outcome_cohort A data frame containing outcome cohort episodes.
#'
#' @return The input data frame, invisibly.
validate_outcome_cohort <- function(outcome_cohort) {
  if (!is.data.frame(outcome_cohort)) {
    stop("`outcome_cohort` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(
    "subjectId",
    "cohortStartDate",
    "cohortEndDate"
  )

  if (!identical(names(outcome_cohort), required_columns)) {
    stop(
      paste0(
        "`outcome_cohort` must contain exactly these columns in order: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (anyNA(outcome_cohort$subjectId)) {
    stop("Outcome `subjectId` values must not be missing.", call. = FALSE)
  }

  date_columns <- c("cohortStartDate", "cohortEndDate")

  for (column_name in date_columns) {
    values <- outcome_cohort[[column_name]]

    if (!inherits(values, "Date")) {
      stop(
        paste0("Outcome `", column_name, "` must have class Date."),
        call. = FALSE
      )
    }

    if (anyNA(values)) {
      stop(
        paste0("Outcome `", column_name, "` must not be missing."),
        call. = FALSE
      )
    }
  }

  invalid_end_date <- outcome_cohort$cohortEndDate <
    outcome_cohort$cohortStartDate

  if (any(invalid_end_date)) {
    stop(
      "Outcome cohort end dates must not precede start dates.",
      call. = FALSE
    )
  }

  invisible(outcome_cohort)
}

#' Validate a fixed time-at-risk window
#'
#' @param risk_window_start_days First included day after index.
#' @param risk_window_end_days Last included day after index.
#'
#' @return The validated integer bounds.
validate_risk_window <- function(
    risk_window_start_days,
    risk_window_end_days) {
  values <- c(risk_window_start_days, risk_window_end_days)

  if (!is.numeric(values) || length(values) != 2L) {
    stop("Risk-window bounds must be numeric scalars.", call. = FALSE)
  }

  if (anyNA(values) || any(!is.finite(values))) {
    stop("Risk-window bounds must be finite and non-missing.", call. = FALSE)
  }

  if (any(values != as.integer(values))) {
    stop("Risk-window bounds must be whole numbers.", call. = FALSE)
  }

  values <- as.integer(values)

  if (values[[1]] < 0L) {
    stop("Risk-window start must not be negative.", call. = FALSE)
  }

  if (values[[2]] < values[[1]]) {
    stop("Risk-window end must not precede its start.", call. = FALSE)
  }

  values
}

#' Build the matched binary-outcome analysis population
#'
#' Outcome occurrence is defined by an outcome cohort start date falling
#' within the inclusive fixed risk window after the treatment index date.
#' Direct subject identifiers and dates are not returned.
#'
#' @param matched_population Validated propensity-score matched population.
#' @param treatment_population Row-to-subject treatment cohort linkage.
#' @param outcome_cohort Outcome cohort episodes.
#' @param risk_window_start_days First included day after index.
#' @param risk_window_end_days Last included day after index.
#'
#' @return A data frame containing rowId, treatment, matchId, and outcome.
build_matched_outcome_population <- function(
    matched_population,
    treatment_population,
    outcome_cohort,
    risk_window_start_days,
    risk_window_end_days) {
  validate_matched_population(matched_population)
  validate_treatment_population(treatment_population)
  validate_outcome_cohort(outcome_cohort)

  risk_window <- validate_risk_window(
    risk_window_start_days,
    risk_window_end_days
  )

  linkage_index <- match(
    matched_population$rowId,
    treatment_population$rowId
  )

  if (anyNA(linkage_index)) {
    stop(
      "Every matched `rowId` must exist in `treatment_population`.",
      call. = FALSE
    )
  }

  linkage <- treatment_population[linkage_index, , drop = FALSE]

  if (!identical(
    as.numeric(linkage$treatment),
    as.numeric(matched_population$treatment)
  )) {
    stop(
      "Treatment assignments disagree between matched and cohort data.",
      call. = FALSE
    )
  }

  risk_start <- linkage$cohortStartDate + risk_window[[1]]
  risk_end <- linkage$cohortStartDate + risk_window[[2]]

  outcome_dates_by_subject <- split(
    outcome_cohort$cohortStartDate,
    as.character(outcome_cohort$subjectId),
    drop = TRUE
  )

  outcome <- vapply(
    seq_len(nrow(linkage)),
    function(index) {
      subject_key <- as.character(linkage$subjectId[[index]])
      subject_dates <- outcome_dates_by_subject[[subject_key]]

      if (is.null(subject_dates)) {
        return(FALSE)
      }

      any(
        subject_dates >= risk_start[[index]] &
          subject_dates <= risk_end[[index]]
      )
    },
    logical(1)
  )

  analysis_population <- data.frame(
    rowId = matched_population$rowId,
    treatment = matched_population$treatment,
    matchId = matched_population$matchId,
    outcome = as.integer(outcome)
  )

  validate_matched_population(
    data.frame(
      rowId = matched_population$rowId,
      treatment = matched_population$treatment,
      propensityScore = matched_population$propensityScore,
      preferenceScore = matched_population$preferenceScore,
      matchId = matched_population$matchId
    )
  )

  analysis_population
}

#' Build treatment linkage from feature-table rows
#'
#' The source rows are generated by
#' `create_feature_extraction_cohort_table()`. Subject identifiers and dates
#' remain internal and are excluded from aggregate outcome exports.
#'
#' @param feature_rows Materialized feature-extraction cohort-table rows.
#' @param matched_population Validated propensity-score matched population.
#'
#' @return Internal row-to-subject and index-date treatment linkage.
build_treatment_from_feature_rows <- function(
    feature_rows,
    matched_population) {
  if (!is.data.frame(feature_rows)) {
    stop("`feature_rows` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(
    "row_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date",
    "treatment"
  )

  missing_columns <- setdiff(required_columns, names(feature_rows))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "`feature_rows` is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  validate_matched_population(matched_population)

  if (nrow(feature_rows) == 0L) {
    stop("`feature_rows` must not be empty.", call. = FALSE)
  }

  if (anyNA(feature_rows$row_id)) {
    stop(
      "Feature-table `row_id` values must not be missing.",
      call. = FALSE
    )
  }

  if (anyDuplicated(feature_rows$row_id) > 0L) {
    stop(
      "Feature-table `row_id` values must be unique.",
      call. = FALSE
    )
  }

  if (anyNA(feature_rows$subject_id)) {
    stop(
      "Feature-table `subject_id` values must not be missing.",
      call. = FALSE
    )
  }

  date_columns <- c(
    "cohort_start_date",
    "cohort_end_date"
  )

  normalized_dates <- lapply(
    date_columns,
    function(column_name) {
      values <- feature_rows[[column_name]]

      if (inherits(values, "Date")) {
        result <- values
      } else if (is.character(values)) {
        result <- as.Date(values)
      } else {
        stop(
          paste0(
            "Feature-table `",
            column_name,
            "` must have class Date or character."
          ),
          call. = FALSE
        )
      }

      if (anyNA(result)) {
        stop(
          paste0(
            "Feature-table `",
            column_name,
            "` contains invalid or missing dates."
          ),
          call. = FALSE
        )
      }

      result
    }
  )

  cohort_start_date <- normalized_dates[[1]]
  cohort_end_date <- normalized_dates[[2]]

  if (any(cohort_end_date < cohort_start_date)) {
    stop(
      "Feature-table cohort end dates must not precede start dates.",
      call. = FALSE
    )
  }

  source_treatment <- feature_rows$treatment

  if (!is.numeric(source_treatment) ||
        anyNA(source_treatment) ||
        any(!source_treatment %in% c(0, 1))) {
    stop(
      "Feature-table `treatment` must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  linkage_index <- match(
    matched_population$rowId,
    feature_rows$row_id
  )

  if (anyNA(linkage_index)) {
    stop(
      "Every matched `rowId` must exist in the feature table.",
      call. = FALSE
    )
  }

  linked_treatment <- source_treatment[linkage_index]

  if (!identical(
    as.numeric(linked_treatment),
    as.numeric(matched_population$treatment)
  )) {
    stop(
      "Treatment assignments disagree between matched and feature-table data.",
      call. = FALSE
    )
  }

  treatment_population <- data.frame(
    rowId = matched_population$rowId,
    subjectId = feature_rows$subject_id[linkage_index],
    cohortStartDate = cohort_start_date[linkage_index],
    cohortEndDate = cohort_end_date[linkage_index],
    treatment = matched_population$treatment
  )

  validate_treatment_population(treatment_population)

  treatment_population
}

#' Load matched treatment linkage from cohort tables
#'
#' A feature-extraction cohort table is created temporarily, queried for the
#' five required linkage columns, and removed before returning. Subject
#' identifiers and dates remain internal to the outcome-analysis process.
#'
#' @param connection Open DatabaseConnector connection.
#' @param cohort_tables Named target and comparator cohort-table definitions.
#' @param matched_population Validated propensity-score matched population.
#' @param table_name Valid temporary feature-table name.
#' @param create_table Optional create callback used by tests.
#' @param drop_table Optional drop callback used by tests.
#' @param query_rows Optional query callback used by tests.
#'
#' @return Internal matched treatment linkage.
load_treatment_from_cohort_tables <- function(
    connection,
    cohort_tables,
    matched_population,
    table_name,
    create_table = NULL,
    drop_table = NULL,
    query_rows = NULL) {
  validate_matched_population(matched_population)

  if (!is.character(table_name) ||
        length(table_name) != 1L ||
        is.na(table_name) ||
        !nzchar(table_name)) {
    stop(
      "`table_name` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (is.null(create_table)) {
    create_table <- get(
      "create_feature_extraction_cohort_table",
      mode = "function",
      inherits = TRUE
    )
  }

  if (is.null(drop_table)) {
    drop_table <- get(
      "drop_feature_extraction_cohort_table",
      mode = "function",
      inherits = TRUE
    )
  }

  if (is.null(query_rows)) {
    query_rows <- function(connection, table_name) {
      sql <- paste(
        "SELECT",
        "row_id,",
        "subject_id,",
        "cohort_start_date,",
        "cohort_end_date,",
        "cohort_definition_id",
        "FROM",
        table_name
      )

      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = FALSE
      )
    }
  }

  callbacks <- list(
    create_table = create_table,
    drop_table = drop_table,
    query_rows = query_rows
  )

  invalid_callbacks <- names(callbacks)[
    !vapply(callbacks, is.function, logical(1))
  ]

  if (length(invalid_callbacks) > 0L) {
    stop(
      paste0(
        "Database-loader callbacks must be functions: ",
        paste(invalid_callbacks, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  create_table(
    connection = connection,
    cohort_tables = cohort_tables,
    table_name = table_name
  )

  on.exit(
    drop_table(
      connection = connection,
      table_name = table_name
    ),
    add = TRUE
  )

  feature_rows <- query_rows(
    connection = connection,
    table_name = table_name
  )

  feature_rows <- normalize_feature_treatment_rows(
    feature_rows
  )

  build_treatment_from_feature_rows(
    feature_rows = feature_rows,
    matched_population = matched_population
  )
}

#' Validate an outcome cohort-table name
#'
#' @param table_name Physical outcome cohort-table name.
#'
#' @return The validated table name, invisibly.
validate_outcome_table_name <- function(table_name) {
  if (!is.character(table_name) ||
        length(table_name) != 1L ||
        is.na(table_name) ||
        !nzchar(table_name)) {
    stop(
      "`table_name` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (!grepl(
    "^[A-Za-z][A-Za-z0-9_]*$",
    table_name
  )) {
    stop(
      "`table_name` contains unsupported characters.",
      call. = FALSE
    )
  }

  invisible(table_name)
}

#' Normalize outcome cohort-table rows
#'
#' @param outcome_rows Materialized rows from the outcome cohort table.
#'
#' @return An internal outcome-cohort data frame.
normalize_outcome_cohort_rows <- function(outcome_rows) {
  if (!is.data.frame(outcome_rows)) {
    stop("`outcome_rows` must be a data frame.", call. = FALSE)
  }

  required_columns <- c(
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  missing_columns <- setdiff(
    required_columns,
    names(outcome_rows)
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "`outcome_rows` is missing required columns: ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (anyNA(outcome_rows$subject_id)) {
    stop(
      "Outcome-table `subject_id` values must not be missing.",
      call. = FALSE
    )
  }

  date_columns <- c(
    "cohort_start_date",
    "cohort_end_date"
  )

  normalized_dates <- lapply(
    date_columns,
    function(column_name) {
      values <- outcome_rows[[column_name]]

      if (inherits(values, "Date")) {
        result <- values
      } else if (is.character(values)) {
        result <- tryCatch(
          as.Date(values),
          error = function(condition) {
            rep(as.Date(NA), length(values))
          }
        )
      } else {
        stop(
          paste0(
            "Outcome-table `",
            column_name,
            "` must have class Date or character."
          ),
          call. = FALSE
        )
      }

      if (anyNA(result)) {
        stop(
          paste0(
            "Outcome-table `",
            column_name,
            "` contains invalid or missing dates."
          ),
          call. = FALSE
        )
      }

      result
    }
  )

  outcome_cohort <- data.frame(
    subjectId = outcome_rows$subject_id,
    cohortStartDate = normalized_dates[[1]],
    cohortEndDate = normalized_dates[[2]]
  )

  validate_outcome_cohort(outcome_cohort)

  outcome_cohort
}

#' Load outcome episodes from an outcome cohort table
#'
#' @param connection Open DatabaseConnector connection.
#' @param outcome_table Physical outcome cohort-table name.
#' @param query_rows Optional query callback used by tests.
#'
#' @return An internal outcome-cohort data frame.
load_outcome_from_cohort_table <- function(
    connection,
    outcome_table,
    query_rows = NULL) {
  validate_outcome_table_name(outcome_table)

  if (is.null(query_rows)) {
    query_rows <- function(connection, outcome_table) {
      sql <- paste(
        "SELECT",
        "subject_id,",
        "cohort_start_date,",
        "cohort_end_date",
        "FROM",
        outcome_table
      )

      DatabaseConnector::querySql(
        connection = connection,
        sql = sql,
        snakeCaseToCamelCase = FALSE
      )
    }
  }

  if (!is.function(query_rows)) {
    stop(
      "`query_rows` must be a function.",
      call. = FALSE
    )
  }

  outcome_rows <- query_rows(
    connection = connection,
    outcome_table = outcome_table
  )

  normalize_outcome_cohort_rows(outcome_rows)
}

#' Validate cohort tables required by outcome analysis
#'
#' @param cohort_tables Named target, comparator, and outcome table names.
#'
#' @return The validated table list, invisibly.
validate_outcome_cohort_tables <- function(cohort_tables) {
  if (!is.list(cohort_tables) || is.null(names(cohort_tables))) {
    stop(
      "`cohort_tables` must be a named list.",
      call. = FALSE
    )
  }

  required_names <- c(
    "target",
    "comparator",
    "outcome"
  )

  missing_names <- setdiff(
    required_names,
    names(cohort_tables)
  )

  if (length(missing_names) > 0L) {
    stop(
      paste0(
        "`cohort_tables` is missing required entries: ",
        paste(missing_names, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  for (table_role in required_names) {
    tryCatch(
      validate_outcome_table_name(
        cohort_tables[[table_role]]
      ),
      error = function(condition) {
        stop(
          paste0(
            "Invalid ",
            table_role,
            " cohort table: ",
            conditionMessage(condition)
          ),
          call. = FALSE
        )
      }
    )
  }

  invisible(cohort_tables)
}

#' Build matched outcomes from study cohort tables
#'
#' @param connection Open DatabaseConnector connection.
#' @param cohort_tables Named target, comparator, and outcome tables.
#' @param matched_population Propensity-score matched population.
#' @param feature_table_name Temporary feature-table name.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#' @param create_table Optional treatment-table create callback.
#' @param drop_table Optional treatment-table drop callback.
#' @param treatment_query Optional treatment-table query callback.
#' @param outcome_query Optional outcome-table query callback.
#'
#' @return Matched analysis rows without subject identifiers or dates.
build_matched_outcome_from_tables <- function(
    connection,
    cohort_tables,
    matched_population,
    feature_table_name,
    risk_window_start_days,
    risk_window_end_days,
    create_table = NULL,
    drop_table = NULL,
    treatment_query = NULL,
    outcome_query = NULL) {
  validate_outcome_cohort_tables(cohort_tables)

  treatment_population <- load_treatment_from_cohort_tables(
    connection = connection,
    cohort_tables = cohort_tables,
    matched_population = matched_population,
    table_name = feature_table_name,
    create_table = create_table,
    drop_table = drop_table,
    query_rows = treatment_query
  )

  outcome_cohort <- load_outcome_from_cohort_table(
    connection = connection,
    outcome_table = cohort_tables$outcome,
    query_rows = outcome_query
  )

  build_matched_outcome_population(
    matched_population = matched_population,
    treatment_population = treatment_population,
    outcome_cohort = outcome_cohort,
    risk_window_start_days = risk_window_start_days,
    risk_window_end_days = risk_window_end_days
  )
}

#' Summarize matched binary outcomes
#'
#' @param analysis_population Matched row-level outcome data.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#'
#' @return Aggregate subject and event counts by treatment group.
summarize_matched_outcomes <- function(
    analysis_population,
    risk_window_start_days,
    risk_window_end_days) {
  required_columns <- c(
    "rowId",
    "treatment",
    "matchId",
    "outcome"
  )

  if (!is.data.frame(analysis_population)) {
    stop(
      "`analysis_population` must be a data frame.",
      call. = FALSE
    )
  }

  if (!identical(
    names(analysis_population),
    required_columns
  )) {
    stop(
      paste0(
        "`analysis_population` must contain exactly: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(analysis_population) == 0L) {
    stop(
      "`analysis_population` must not be empty.",
      call. = FALSE
    )
  }

  if (anyNA(analysis_population$rowId) ||
        anyDuplicated(analysis_population$rowId) > 0L) {
    stop(
      "Analysis `rowId` values must be complete and unique.",
      call. = FALSE
    )
  }

  treatment <- analysis_population$treatment
  outcome <- analysis_population$outcome
  match_id <- analysis_population$matchId

  if (!is.numeric(treatment) ||
        anyNA(treatment) ||
        any(!treatment %in% c(0, 1))) {
    stop(
      "Analysis `treatment` must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.numeric(outcome) ||
        anyNA(outcome) ||
        any(!outcome %in% c(0, 1))) {
    stop(
      "Analysis `outcome` must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (anyNA(match_id)) {
    stop(
      "Analysis `matchId` values must not be missing.",
      call. = FALSE
    )
  }

  matched_sets <- split(
    seq_len(nrow(analysis_population)),
    as.character(match_id),
    drop = TRUE
  )

  valid_sets <- vapply(
    matched_sets,
    function(indices) {
      length(indices) == 2L &&
        identical(
          sort(as.integer(treatment[indices])),
          c(0L, 1L)
        )
    },
    logical(1)
  )

  if (length(valid_sets) == 0L || any(!valid_sets)) {
    stop(
      "Analysis data must preserve one target and one comparator per match.",
      call. = FALSE
    )
  }

  risk_window <- validate_risk_window(
    risk_window_start_days,
    risk_window_end_days
  )

  treatment_values <- c(1, 0)

  subject_counts <- vapply(
    treatment_values,
    function(value) {
      as.integer(sum(treatment == value))
    },
    integer(1)
  )

  event_counts <- vapply(
    treatment_values,
    function(value) {
      as.integer(sum(outcome[treatment == value]))
    },
    integer(1)
  )

  data.frame(
    group = c("target", "comparator"),
    treatment = treatment_values,
    subjectCount = subject_counts,
    eventCount = event_counts,
    riskWindowStartDays = rep(risk_window[[1]], 2L),
    riskWindowEndDays = rep(risk_window[[2]], 2L)
  )
}

#' Compute a matched-set cluster-robust covariance
#'
#' @param design Numeric outcome-model design matrix.
#' @param score_residual Observation-level score residuals.
#' @param weights Iteratively reweighted least-squares weights.
#' @param cluster Matched-set identifiers.
#'
#' @return A cluster-robust covariance matrix with CR1 correction.
compute_cluster_robust_vcov <- function(
    design,
    score_residual,
    weights,
    cluster) {
  if (!is.matrix(design)) {
    stop(
      "`design` must be a matrix.",
      call. = FALSE
    )
  }

  if (!is.numeric(design)) {
    stop(
      "`design` must be numeric.",
      call. = FALSE
    )
  }

  if (nrow(design) == 0L || ncol(design) == 0L) {
    stop(
      "`design` must not be empty.",
      call. = FALSE
    )
  }

  if (anyNA(design) || any(!is.finite(design))) {
    stop(
      "`design` must contain only finite values.",
      call. = FALSE
    )
  }

  observation_count <- nrow(design)
  parameter_count <- ncol(design)

  numeric_inputs <- list(
    score_residual = score_residual,
    weights = weights
  )

  for (input_name in names(numeric_inputs)) {
    values <- numeric_inputs[[input_name]]

    if (!is.numeric(values)) {
      stop(
        paste0("`", input_name, "` must be numeric."),
        call. = FALSE
      )
    }

    if (length(values) != observation_count) {
      stop(
        paste0(
          "`",
          input_name,
          "` must contain one value per design row."
        ),
        call. = FALSE
      )
    }

    if (anyNA(values) || any(!is.finite(values))) {
      stop(
        paste0(
          "`",
          input_name,
          "` must contain only finite values."
        ),
        call. = FALSE
      )
    }
  }

  if (any(weights <= 0)) {
    stop(
      "`weights` must be strictly positive.",
      call. = FALSE
    )
  }

  if (length(cluster) != observation_count) {
    stop(
      "`cluster` must contain one value per design row.",
      call. = FALSE
    )
  }

  if (anyNA(cluster)) {
    stop(
      "`cluster` values must not be missing.",
      call. = FALSE
    )
  }

  cluster_group <- as.character(cluster)
  cluster_count <- length(unique(cluster_group))

  if (cluster_count < 2L) {
    stop(
      "At least two matched-set clusters are required.",
      call. = FALSE
    )
  }

  if (observation_count <= parameter_count) {
    stop(
      "The model requires more observations than parameters.",
      call. = FALSE
    )
  }

  information <- crossprod(
    design,
    design * weights
  )

  bread <- tryCatch(
    solve(information),
    error = function(condition) {
      stop(
        "The outcome-model information matrix is singular.",
        call. = FALSE
      )
    }
  )

  score_matrix <- design * score_residual

  cluster_scores <- rowsum(
    score_matrix,
    group = cluster_group,
    reorder = FALSE
  )

  meat <- crossprod(cluster_scores)

  correction <- (
    cluster_count / (cluster_count - 1)
  ) * (
    (observation_count - 1) /
      (observation_count - parameter_count)
  )

  covariance <- correction *
    bread %*%
      meat %*%
      bread

  covariance <- (
    covariance + t(covariance)
  ) / 2

  if (anyNA(covariance)) {
    stop(
      "The cluster-robust covariance contains missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(covariance))) {
    stop(
      "The cluster-robust covariance contains non-finite values.",
      call. = FALSE
    )
  }

  covariance
}

#' Fit the matched binary outcome model
#'
#' @param analysis_population Matched row-level binary-outcome data.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#' @param confidence_level Two-sided confidence level.
#'
#' @return One aggregate adjusted observational association result.
fit_matched_outcome_model <- function(
    analysis_population,
    risk_window_start_days,
    risk_window_end_days,
    confidence_level = 0.95) {
  aggregate_counts <- summarize_matched_outcomes(
    analysis_population = analysis_population,
    risk_window_start_days = risk_window_start_days,
    risk_window_end_days = risk_window_end_days
  )

  if (!is.numeric(confidence_level)) {
    stop(
      "`confidence_level` must be numeric.",
      call. = FALSE
    )
  }

  if (length(confidence_level) != 1L) {
    stop(
      "`confidence_level` must contain one value.",
      call. = FALSE
    )
  }

  if (is.na(confidence_level)) {
    stop(
      "`confidence_level` must not be missing.",
      call. = FALSE
    )
  }

  if (!is.finite(confidence_level)) {
    stop(
      "`confidence_level` must be finite.",
      call. = FALSE
    )
  }

  if (confidence_level <= 0 || confidence_level >= 1) {
    stop(
      "`confidence_level` must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  treatment <- as.numeric(
    analysis_population$treatment
  )

  outcome <- as.numeric(
    analysis_population$outcome
  )

  match_id <- analysis_population$matchId
  matched_set_count <- length(unique(match_id))

  if (matched_set_count < 2L) {
    stop(
      "At least two matched sets are required for outcome estimation.",
      call. = FALSE
    )
  }

  outcome_table <- table(
    factor(treatment, levels = c(0, 1)),
    factor(outcome, levels = c(0, 1))
  )

  if (any(outcome_table == 0L)) {
    stop(
      paste(
        "The treatment-by-outcome table contains a zero cell;",
        "the logistic odds-ratio estimate is separated."
      ),
      call. = FALSE
    )
  }

  design <- cbind(
    intercept = 1,
    treatment = treatment
  )

  model_warnings <- character()

  fit <- withCallingHandlers(
    stats::glm.fit(
      x = design,
      y = outcome,
      family = stats::binomial()
    ),
    warning = function(condition) {
      model_warnings <<- c(
        model_warnings,
        conditionMessage(condition)
      )

      invokeRestart("muffleWarning")
    }
  )

  if (length(model_warnings) > 0L) {
    stop(
      paste(
        "The matched logistic model emitted a fitting warning:",
        paste(model_warnings, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  if (!isTRUE(fit$converged)) {
    stop(
      "The matched logistic model did not converge.",
      call. = FALSE
    )
  }

  coefficients <- fit$coefficients

  if (length(coefficients) != 2L) {
    stop(
      "The matched logistic model returned unexpected coefficients.",
      call. = FALSE
    )
  }

  if (anyNA(coefficients)) {
    stop(
      "The matched logistic model coefficients contain missing values.",
      call. = FALSE
    )
  }

  if (any(!is.finite(coefficients))) {
    stop(
      "The matched logistic model coefficients are not finite.",
      call. = FALSE
    )
  }

  if (fit$rank != ncol(design)) {
    stop(
      "The matched logistic model matrix is rank deficient.",
      call. = FALSE
    )
  }

  score_residual <- outcome - fit$fitted.values

  covariance <- compute_cluster_robust_vcov(
    design = design,
    score_residual = score_residual,
    weights = fit$weights,
    cluster = match_id
  )

  standard_error <- sqrt(
    covariance[2L, 2L]
  )

  if (!is.finite(standard_error)) {
    stop(
      "The treatment standard error is not finite.",
      call. = FALSE
    )
  }

  if (standard_error <= 0) {
    stop(
      "The treatment standard error must be positive.",
      call. = FALSE
    )
  }

  log_odds_ratio <- unname(
    coefficients[[2L]]
  )

  critical_value <- stats::qnorm(
    1 - (1 - confidence_level) / 2
  )

  confidence_limits <- log_odds_ratio +
    c(-1, 1) *
      critical_value *
      standard_error

  target_row <- aggregate_counts$group == "target"

  comparator_row <-
    aggregate_counts$group == "comparator"

  data.frame(
    effectMeasure = "odds ratio",
    estimate = exp(log_odds_ratio),
    ciLower = exp(confidence_limits[[1L]]),
    ciUpper = exp(confidence_limits[[2L]]),
    confidenceLevel = confidence_level,
    logOddsRatio = log_odds_ratio,
    standardError = standard_error,
    subjectCount = as.integer(
      sum(aggregate_counts$subjectCount)
    ),
    eventCount = as.integer(
      sum(aggregate_counts$eventCount)
    ),
    targetSubjectCount = as.integer(
      aggregate_counts$subjectCount[target_row]
    ),
    targetEventCount = as.integer(
      aggregate_counts$eventCount[target_row]
    ),
    comparatorSubjectCount = as.integer(
      aggregate_counts$subjectCount[comparator_row]
    ),
    comparatorEventCount = as.integer(
      aggregate_counts$eventCount[comparator_row]
    ),
    matchedSetCount = as.integer(matched_set_count),
    varianceEstimator =
      "matched-set cluster-robust CR1",
    modelConverged = TRUE,
    zeroCellDetected = FALSE,
    interpretation = paste(
      "Adjusted observational association",
      "under the stated design assumptions."
    )
  )
}

#' Normalize feature-table treatment rows
#'
#' @param feature_rows Feature-table rows returned from the database.
#'
#' @return Feature rows with a validated binary treatment column.
normalize_feature_treatment_rows <- function(feature_rows) {
  treatment_schema <- c(
    "row_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date",
    "treatment"
  )

  cohort_definition_schema <- c(
    "row_id",
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  if (!is.data.frame(feature_rows)) {
    stop(
      "`feature_rows` must be a data frame.",
      call. = FALSE
    )
  }

  if (identical(
    names(feature_rows),
    treatment_schema
  )) {
    return(feature_rows)
  }

  if (!identical(
    sort(names(feature_rows)),
    sort(cohort_definition_schema)
  )) {
    stop(
      paste(
        "`feature_rows` must use either the treatment schema",
        "or the cohort-definition schema."
      ),
      call. = FALSE
    )
  }

  cohort_id <- feature_rows$cohort_definition_id

  if (!is.numeric(cohort_id)) {
    stop(
      "`cohort_definition_id` must be numeric.",
      call. = FALSE
    )
  }

  if (anyNA(cohort_id) ||
        any(!is.finite(cohort_id))) {
    stop(
      "`cohort_definition_id` must contain finite values.",
      call. = FALSE
    )
  }

  if (any(cohort_id != floor(cohort_id))) {
    stop(
      "`cohort_definition_id` values must be whole numbers.",
      call. = FALSE
    )
  }

  if (any(!cohort_id %in% c(1, 2))) {
    stop(
      paste(
        "Only cohort_definition_id 1 for target",
        "and 2 for comparator are supported."
      ),
      call. = FALSE
    )
  }

  result <- feature_rows[
    c(
      "row_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    )
  ]

  result$treatment <- as.numeric(
    cohort_id == 1
  )

  result <- result[treatment_schema]
  row.names(result) <- NULL

  result
}
