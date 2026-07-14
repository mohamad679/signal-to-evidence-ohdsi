#' Validate and normalize candidate concepts
#'
#' @param data A data frame containing candidate concept identifiers and names.
#' @param concept_type A non-empty label used to identify the candidate type in errors.
#'
#' @return A data frame containing normalized `concept_id` and `concept_name` columns.
validate_candidate_concepts <- function(data, concept_type) {
  valid_concept_type <- checkmate::test_string(concept_type, min.chars = 1L) &&
    nzchar(trimws(concept_type))
  if (!valid_concept_type) {
    cli::cli_abort(
      "{.arg concept_type} must be one non-empty character value.",
      class = "feasibility_argument_error"
    )
  }
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.field {concept_type} candidates} must be a data frame.",
      class = "feasibility_candidate_error"
    )
  }
  if (anyDuplicated(names(data)) > 0L) {
    cli::cli_abort(
      "{.field {concept_type} candidates} must have unique column names.",
      class = "feasibility_candidate_error"
    )
  }

  required_columns <- c("concept_id", "concept_name")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      paste0(
        concept_type,
        " candidates are missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      class = "feasibility_candidate_error"
    )
  }
  if (!is.numeric(data$concept_id) ||
        !checkmate::test_integerish(data$concept_id, any.missing = FALSE)) {
    cli::cli_abort(
      "{.field {concept_type} candidate concept_id} must contain whole numbers.",
      class = "feasibility_candidate_error"
    )
  }
  if (any(data$concept_id == 0)) {
    cli::cli_abort(
      "{.field {concept_type} candidate concept_id} must not contain ID 0.",
      class = "feasibility_candidate_error"
    )
  }
  if (any(data$concept_id < 0)) {
    cli::cli_abort(
      "{.field {concept_type} candidate concept_id} must be positive.",
      class = "feasibility_candidate_error"
    )
  }
  if (anyDuplicated(data$concept_id) > 0L) {
    cli::cli_abort(
      "{.field {concept_type} candidate concept_id} must be unique.",
      class = "feasibility_candidate_error"
    )
  }

  concept_names <- if (is.factor(data$concept_name)) {
    as.character(data$concept_name)
  } else {
    data$concept_name
  }
  valid_names <- is.character(concept_names) &&
    !anyNA(concept_names) &&
    all(nzchar(trimws(concept_names)))
  if (!valid_names) {
    cli::cli_abort(
      "{.field {concept_type} candidate concept_name} must contain non-empty names.",
      class = "feasibility_candidate_error"
    )
  }

  data.frame(
    concept_id = as.numeric(data$concept_id),
    concept_name = trimws(concept_names),
    stringsAsFactors = FALSE
  )
}

#' Validate a database schema for use as a SqlRender identifier
#'
#' @param database_schema Schema containing the OMOP CDM tables.
#'
#' @return The validated schema name.
validate_feasibility_schema <- function(database_schema) {
  valid_schema <- checkmate::test_string(database_schema, min.chars = 1L) &&
    grepl(
      "^[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)*$",
      database_schema
    )
  if (!valid_schema) {
    cli::cli_abort(
      "{.arg database_schema} must be a valid schema identifier.",
      class = "feasibility_argument_error"
    )
  }
  database_schema
}

#' Validate one whole-number feasibility parameter
#'
#' @param value Value to validate.
#' @param argument Argument name used in errors.
#' @param lower Smallest permitted value.
#'
#' @return The validated value as a numeric whole number.
validate_feasibility_integer <- function(value, argument, lower = 0L) {
  valid_value <- checkmate::test_integerish(
    value,
    lower = lower,
    len = 1L,
    any.missing = FALSE
  )
  if (!valid_value) {
    cli::cli_abort(
      "{.arg {argument}} must be one whole number greater than or equal to {lower}.",
      class = "feasibility_argument_error"
    )
  }
  as.numeric(value)
}

#' Render, translate, and execute an aggregate feasibility query
#'
#' @param connection An open DatabaseConnector connection.
#' @param sql Parameterized SQL text.
#' @param parameters Named SqlRender parameter values.
#'
#' @return A data frame containing the query result.
run_feasibility_query <- function(connection, sql, parameters) {
  if (!checkmate::test_list(parameters, names = "unique")) {
    cli::cli_abort(
      "{.arg parameters} must be a named list with unique names.",
      class = "feasibility_argument_error"
    )
  }
  rendered_sql <- do.call(
    SqlRender::render,
    c(list(sql = sql), parameters)
  )
  translated_sql <- SqlRender::translate(
    sql = rendered_sql,
    targetDialect = DatabaseConnector::dbms(connection)
  )
  result <- DatabaseConnector::querySql(
    connection = connection,
    sql = translated_sql,
    snakeCaseToCamelCase = FALSE
  )
  as.data.frame(result, stringsAsFactors = FALSE)
}

#' Get frequent drug concepts for feasibility evaluation
#'
#' @param connection An open DatabaseConnector connection.
#' @param database_schema Schema containing the OMOP CDM tables.
#' @param maximum_candidates Maximum number of candidates to return.
#'
#' @return An aggregate data frame of drug candidates and exposure counts.
get_candidate_drugs <- function(connection,
                                database_schema = "main",
                                maximum_candidates = 12L) {
  database_schema <- validate_feasibility_schema(database_schema)
  maximum_candidates <- validate_feasibility_integer(
    maximum_candidates,
    "maximum_candidates",
    lower = 1L
  )
  sql <- paste(
    "-- Aggregate candidate drug counts; no person-level rows are returned.",
    "SELECT TOP @maximum_candidates",
    "  drug_exposure.drug_concept_id AS concept_id,",
    "  concept.concept_name AS concept_name,",
    "  COUNT(*) AS exposure_count,",
    "  COUNT(DISTINCT drug_exposure.person_id) AS exposed_person_count",
    "FROM @database_schema.drug_exposure drug_exposure",
    "INNER JOIN @database_schema.concept concept",
    "  ON drug_exposure.drug_concept_id = concept.concept_id",
    "WHERE drug_exposure.drug_concept_id <> 0",
    "  AND concept.concept_name IS NOT NULL",
    "  AND TRIM(concept.concept_name) <> ''",
    "GROUP BY drug_exposure.drug_concept_id, concept.concept_name",
    "ORDER BY exposed_person_count DESC, exposure_count DESC, concept_id ASC;",
    sep = "\n"
  )
  result <- run_feasibility_query(
    connection = connection,
    sql = sql,
    parameters = list(
      maximum_candidates = maximum_candidates,
      database_schema = database_schema
    )
  )
  candidates <- validate_candidate_concepts(result, "drug")
  data.frame(
    candidates,
    exposure_count = as.numeric(result$exposure_count),
    exposed_person_count = as.numeric(result$exposed_person_count),
    stringsAsFactors = FALSE
  )
}

#' Get frequent outcome concepts for feasibility evaluation
#'
#' @param connection An open DatabaseConnector connection.
#' @param database_schema Schema containing the OMOP CDM tables.
#' @param maximum_candidates Maximum number of candidates to return.
#'
#' @return An aggregate data frame of outcome candidates and occurrence counts.
get_candidate_outcomes <- function(connection,
                                   database_schema = "main",
                                   maximum_candidates = 12L) {
  database_schema <- validate_feasibility_schema(database_schema)
  maximum_candidates <- validate_feasibility_integer(
    maximum_candidates,
    "maximum_candidates",
    lower = 1L
  )
  sql <- paste(
    "-- Aggregate candidate outcome counts; no person-level rows are returned.",
    "SELECT TOP @maximum_candidates",
    "  condition_occurrence.condition_concept_id AS concept_id,",
    "  concept.concept_name AS concept_name,",
    "  COUNT(*) AS occurrence_count,",
    "  COUNT(DISTINCT condition_occurrence.person_id) AS affected_person_count",
    "FROM @database_schema.condition_occurrence condition_occurrence",
    "INNER JOIN @database_schema.concept concept",
    "  ON condition_occurrence.condition_concept_id = concept.concept_id",
    "WHERE condition_occurrence.condition_concept_id <> 0",
    "  AND concept.concept_name IS NOT NULL",
    "  AND TRIM(concept.concept_name) <> ''",
    "GROUP BY condition_occurrence.condition_concept_id, concept.concept_name",
    paste0(
      "ORDER BY affected_person_count DESC, occurrence_count DESC, ",
      "concept_id ASC;"
    ),
    sep = "\n"
  )
  result <- run_feasibility_query(
    connection = connection,
    sql = sql,
    parameters = list(
      maximum_candidates = maximum_candidates,
      database_schema = database_schema
    )
  )
  candidates <- validate_candidate_concepts(result, "outcome")
  data.frame(
    candidates,
    occurrence_count = as.numeric(result$occurrence_count),
    affected_person_count = as.numeric(result$affected_person_count),
    stringsAsFactors = FALSE
  )
}

#' Evaluate one aggregate drug-pair and outcome feasibility combination
#'
#' The query applies the configured new-user, prior-observation, prior-outcome,
#' and post-index risk-window restrictions before returning one aggregate row.
#'
#' @param connection An open DatabaseConnector connection.
#' @param target_concept_id Target drug concept identifier.
#' @param comparator_concept_id Comparator drug concept identifier.
#' @param outcome_concept_id Outcome condition concept identifier.
#' @param database_schema Schema containing the OMOP CDM tables.
#' @param washout_days Pre-index treatment washout in days.
#' @param minimum_prior_observation_days Required prior observation in days.
#' @param risk_window_start_days First post-index outcome day.
#' @param risk_window_end_days Last post-index outcome day.
#'
#' @return Exactly one row containing aggregate cohort and outcome counts.
evaluate_feasibility_combination <- function(
    connection,
    target_concept_id,
    comparator_concept_id,
    outcome_concept_id,
    database_schema = "main",
    washout_days = 180L,
    minimum_prior_observation_days = 180L,
    risk_window_start_days = 1L,
    risk_window_end_days = 30L) {
  database_schema <- validate_feasibility_schema(database_schema)
  target_concept_id <- validate_feasibility_integer(
    target_concept_id,
    "target_concept_id",
    lower = 1L
  )
  comparator_concept_id <- validate_feasibility_integer(
    comparator_concept_id,
    "comparator_concept_id",
    lower = 1L
  )
  outcome_concept_id <- validate_feasibility_integer(
    outcome_concept_id,
    "outcome_concept_id",
    lower = 1L
  )
  if (target_concept_id == comparator_concept_id) {
    cli::cli_abort(
      "Target and comparator concept identifiers must be distinct.",
      class = "feasibility_argument_error"
    )
  }
  washout_days <- validate_feasibility_integer(washout_days, "washout_days")
  minimum_prior_observation_days <- validate_feasibility_integer(
    minimum_prior_observation_days,
    "minimum_prior_observation_days"
  )
  risk_window_start_days <- validate_feasibility_integer(
    risk_window_start_days,
    "risk_window_start_days",
    lower = 1L
  )
  risk_window_end_days <- validate_feasibility_integer(
    risk_window_end_days,
    "risk_window_end_days",
    lower = 1L
  )
  if (risk_window_end_days < risk_window_start_days) {
    cli::cli_abort(
      "{.arg risk_window_end_days} must not precede risk_window_start_days.",
      class = "feasibility_argument_error"
    )
  }

  sql <- paste(
    "-- Index: first qualifying target or comparator exposure per person.",
    "-- Washout: excludes either study drug before index.",
    "-- Risk window: counts persons with an observed post-index outcome.",
    "WITH study_exposures AS (",
    "  SELECT DISTINCT",
    "    person_id,",
    "    drug_concept_id AS arm_concept_id,",
    "    drug_exposure_start_date AS index_date",
    "  FROM @database_schema.drug_exposure",
    "  WHERE drug_concept_id IN (@target_concept_id, @comparator_concept_id)",
    "),",
    "study_exposure_dates AS (",
    "  SELECT DISTINCT person_id, index_date",
    "  FROM study_exposures",
    "),",
    "exposure_history AS (",
    "  SELECT",
    "    person_id,",
    "    index_date,",
    "    LAG(index_date) OVER (",
    "      PARTITION BY person_id ORDER BY index_date ASC",
    "    ) AS previous_exposure_date",
    "  FROM study_exposure_dates",
    "),",
    "first_outcomes AS (",
    "  SELECT person_id, MIN(condition_start_date) AS first_outcome_date",
    "  FROM @database_schema.condition_occurrence",
    "  WHERE condition_concept_id = @outcome_concept_id",
    "  GROUP BY person_id",
    "),",
    "eligible_exposures AS (",
    "  SELECT",
    "    exposure.person_id,",
    "    exposure.arm_concept_id,",
    "    exposure.index_date,",
    paste0(
      "    MAX(DATEDIFF(day, observation.observation_period_start_date, ",
      "exposure.index_date)) AS prior_observation_days"
    ),
    "  FROM study_exposures exposure",
    "  INNER JOIN exposure_history history",
    "    ON exposure.person_id = history.person_id",
    "    AND exposure.index_date = history.index_date",
    "  INNER JOIN @database_schema.observation_period observation",
    "    ON exposure.person_id = observation.person_id",
    "    AND exposure.index_date >= observation.observation_period_start_date",
    "    AND exposure.index_date <= observation.observation_period_end_date",
    "  LEFT JOIN first_outcomes first_outcome",
    "    ON exposure.person_id = first_outcome.person_id",
    "  WHERE DATEDIFF(day, observation.observation_period_start_date,",
    "      exposure.index_date) >= @minimum_prior_observation_days",
    "    AND (",
    "      history.previous_exposure_date IS NULL",
    paste0(
      "      OR history.previous_exposure_date < ",
      "DATEADD(day, -@washout_days, exposure.index_date)"
    ),
    "    )",
    "    AND (",
    "      first_outcome.first_outcome_date IS NULL",
    "      OR first_outcome.first_outcome_date > exposure.index_date",
    "    )",
    "  GROUP BY exposure.person_id, exposure.arm_concept_id, exposure.index_date",
    "),",
    "first_qualifying_indexes AS (",
    "  SELECT",
    "    person_id,",
    "    arm_concept_id,",
    "    index_date,",
    "    prior_observation_days,",
    "    ROW_NUMBER() OVER (",
    "      PARTITION BY person_id",
    "      ORDER BY index_date ASC, arm_concept_id ASC",
    "    ) AS entry_number",
    "  FROM eligible_exposures",
    "),",
    "cohort_entries AS (",
    "  SELECT person_id, arm_concept_id, index_date, prior_observation_days",
    "  FROM first_qualifying_indexes",
    "  WHERE entry_number = 1",
    "),",
    "observed_outcomes AS (",
    "  SELECT DISTINCT outcome.person_id, outcome.condition_start_date",
    "  FROM @database_schema.condition_occurrence outcome",
    "  INNER JOIN @database_schema.observation_period observation",
    "    ON outcome.person_id = observation.person_id",
    "    AND outcome.condition_start_date >= observation.observation_period_start_date",
    "    AND outcome.condition_start_date <= observation.observation_period_end_date",
    "  WHERE outcome.condition_concept_id = @outcome_concept_id",
    "),",
    "cohort_outcomes AS (",
    "  SELECT",
    "    cohort.person_id,",
    "    cohort.arm_concept_id,",
    "    cohort.index_date,",
    "    cohort.prior_observation_days,",
    "    MAX(CASE WHEN outcome.person_id IS NULL THEN 0 ELSE 1 END) AS has_outcome",
    "  FROM cohort_entries cohort",
    "  LEFT JOIN observed_outcomes outcome",
    "    ON cohort.person_id = outcome.person_id",
    paste0(
      "    AND outcome.condition_start_date >= ",
      "DATEADD(day, @risk_window_start_days, cohort.index_date)"
    ),
    paste0(
      "    AND outcome.condition_start_date <= ",
      "DATEADD(day, @risk_window_end_days, cohort.index_date)"
    ),
    "  GROUP BY cohort.person_id, cohort.arm_concept_id, cohort.index_date,",
    "    cohort.prior_observation_days",
    "),",
    "aggregate_counts AS (",
    "  SELECT",
    paste0(
      "    SUM(CASE WHEN arm_concept_id = @target_concept_id ",
      "THEN 1 ELSE 0 END) AS target_subject_count,"
    ),
    paste0(
      "    SUM(CASE WHEN arm_concept_id = @comparator_concept_id ",
      "THEN 1 ELSE 0 END) AS comparator_subject_count,"
    ),
    paste0(
      "    SUM(CASE WHEN arm_concept_id = @target_concept_id ",
      "THEN has_outcome ELSE 0 END) AS target_outcome_count,"
    ),
    paste0(
      "    SUM(CASE WHEN arm_concept_id = @comparator_concept_id ",
      "THEN has_outcome ELSE 0 END) AS comparator_outcome_count"
    ),
    "  FROM cohort_outcomes",
    "),",
    "ranked_prior_observation AS (",
    "  SELECT",
    "    prior_observation_days,",
    "    ROW_NUMBER() OVER (ORDER BY prior_observation_days ASC) AS prior_rank,",
    "    COUNT(*) OVER () AS entry_count",
    "  FROM cohort_entries",
    ")",
    "SELECT",
    "  @target_concept_id AS target_concept_id,",
    "  @comparator_concept_id AS comparator_concept_id,",
    "  @outcome_concept_id AS outcome_concept_id,",
    "  COALESCE(target_subject_count, 0) AS target_subject_count,",
    "  COALESCE(comparator_subject_count, 0) AS comparator_subject_count,",
    "  COALESCE(target_outcome_count, 0) AS target_outcome_count,",
    "  COALESCE(comparator_outcome_count, 0) AS comparator_outcome_count,",
    "  (SELECT AVG(prior_observation_days * 1.0)",
    "   FROM ranked_prior_observation",
    "   WHERE prior_rank IN (",
    "     CAST((entry_count + 1) / 2 AS INTEGER),",
    "     CAST((entry_count + 2) / 2 AS INTEGER)",
    "   )) AS median_prior_observation_days",
    "FROM aggregate_counts;",
    sep = "\n"
  )
  parameters <- list(
    database_schema = database_schema,
    target_concept_id = target_concept_id,
    comparator_concept_id = comparator_concept_id,
    outcome_concept_id = outcome_concept_id,
    washout_days = washout_days,
    minimum_prior_observation_days = minimum_prior_observation_days,
    risk_window_start_days = risk_window_start_days,
    risk_window_end_days = risk_window_end_days
  )
  result <- run_feasibility_query(connection, sql, parameters)
  required_columns <- c(
    "target_concept_id",
    "comparator_concept_id",
    "outcome_concept_id",
    "target_subject_count",
    "comparator_subject_count",
    "target_outcome_count",
    "comparator_outcome_count",
    "median_prior_observation_days"
  )
  if (nrow(result) != 1L || !identical(names(result), required_columns)) {
    cli::cli_abort(
      "Feasibility evaluation did not return the required single aggregate row.",
      class = "feasibility_result_error"
    )
  }
  result
}

#' Create an empty feasibility matrix with stable column types
#'
#' @return An empty aggregate feasibility matrix.
empty_feasibility_matrix <- function() {
  data.frame(
    target_concept_id = numeric(),
    target_concept_name = character(),
    comparator_concept_id = numeric(),
    comparator_concept_name = character(),
    outcome_concept_id = numeric(),
    outcome_concept_name = character(),
    target_subject_count = numeric(),
    comparator_subject_count = numeric(),
    target_outcome_count = numeric(),
    comparator_outcome_count = numeric(),
    median_prior_observation_days = numeric(),
    total_outcome_count = numeric(),
    feasible = logical(),
    feasibility_reason = character(),
    stringsAsFactors = FALSE
  )
}

#' Build the aggregate feasibility matrix
#'
#' @param connection An open DatabaseConnector connection.
#' @param drug_candidates Candidate drugs with concept identifiers and names.
#' @param outcome_candidates Candidate outcomes with concept identifiers and names.
#' @param config Validated study configuration.
#'
#' @return A deterministically ordered aggregate feasibility matrix.
build_feasibility_matrix <- function(connection,
                                     drug_candidates,
                                     outcome_candidates,
                                     config) {
  drugs <- validate_candidate_concepts(drug_candidates, "drug")
  outcomes <- validate_candidate_concepts(outcome_candidates, "outcome")
  if (!is.list(config) || is.null(config$design) || is.null(config$feasibility)) {
    cli::cli_abort(
      "{.arg config} must contain design and feasibility sections.",
      class = "feasibility_argument_error"
    )
  }

  design_fields <- c(
    "washout_days",
    "minimum_prior_observation_days",
    "risk_window_start_days",
    "risk_window_end_days"
  )
  threshold_fields <- c(
    "minimum_subjects_per_arm",
    "minimum_total_outcomes",
    "minimum_outcomes_per_arm"
  )
  if (!all(design_fields %in% names(config$design)) ||
        !all(threshold_fields %in% names(config$feasibility))) {
    cli::cli_abort(
      "{.arg config} is missing required design or feasibility values.",
      class = "feasibility_argument_error"
    )
  }
  design <- lapply(design_fields, function(field) {
    lower <- if (field %in% c("risk_window_start_days", "risk_window_end_days")) {
      1L
    } else {
      0L
    }
    validate_feasibility_integer(config$design[[field]], field, lower)
  })
  names(design) <- design_fields
  if (design$risk_window_end_days < design$risk_window_start_days) {
    cli::cli_abort(
      "Configured risk-window end must not precede its start.",
      class = "feasibility_argument_error"
    )
  }
  thresholds <- lapply(threshold_fields, function(field) {
    validate_feasibility_integer(config$feasibility[[field]], field)
  })
  names(thresholds) <- threshold_fields
  database_schema <- "main"
  if (!is.null(config$project) && !is.null(config$project$database_schema)) {
    database_schema <- validate_feasibility_schema(config$project$database_schema)
  }

  if (nrow(drugs) < 2L || nrow(outcomes) == 0L) {
    return(empty_feasibility_matrix())
  }
  drugs <- drugs[order(drugs$concept_id), , drop = FALSE]
  outcomes <- outcomes[order(outcomes$concept_id), , drop = FALSE]
  drug_pairs <- utils::combn(drugs$concept_id, 2L)
  combinations <- vector("list", ncol(drug_pairs) * nrow(outcomes))
  result_index <- 1L

  for (pair_index in seq_len(ncol(drug_pairs))) {
    for (outcome_index in seq_len(nrow(outcomes))) {
      target_id <- drug_pairs[1L, pair_index]
      comparator_id <- drug_pairs[2L, pair_index]
      outcome_id <- outcomes$concept_id[[outcome_index]]
      aggregate_result <- evaluate_feasibility_combination(
        connection = connection,
        target_concept_id = target_id,
        comparator_concept_id = comparator_id,
        outcome_concept_id = outcome_id,
        database_schema = database_schema,
        washout_days = design$washout_days,
        minimum_prior_observation_days = design$minimum_prior_observation_days,
        risk_window_start_days = design$risk_window_start_days,
        risk_window_end_days = design$risk_window_end_days
      )
      combinations[[result_index]] <- data.frame(
        target_concept_id = target_id,
        target_concept_name = drugs$concept_name[drugs$concept_id == target_id],
        comparator_concept_id = comparator_id,
        comparator_concept_name = drugs$concept_name[drugs$concept_id == comparator_id],
        outcome_concept_id = outcome_id,
        outcome_concept_name = outcomes$concept_name[[outcome_index]],
        target_subject_count = as.numeric(aggregate_result$target_subject_count),
        comparator_subject_count = as.numeric(
          aggregate_result$comparator_subject_count
        ),
        target_outcome_count = as.numeric(aggregate_result$target_outcome_count),
        comparator_outcome_count = as.numeric(
          aggregate_result$comparator_outcome_count
        ),
        median_prior_observation_days = as.numeric(
          aggregate_result$median_prior_observation_days
        ),
        stringsAsFactors = FALSE
      )
      result_index <- result_index + 1L
    }
  }

  matrix <- do.call(rbind, combinations)
  matrix$total_outcome_count <- matrix$target_outcome_count +
    matrix$comparator_outcome_count
  subject_threshold_failed <- matrix$target_subject_count <
    thresholds$minimum_subjects_per_arm |
    matrix$comparator_subject_count < thresholds$minimum_subjects_per_arm
  total_outcome_threshold_failed <- matrix$total_outcome_count <
    thresholds$minimum_total_outcomes
  arm_outcome_threshold_failed <- matrix$target_outcome_count <
    thresholds$minimum_outcomes_per_arm |
    matrix$comparator_outcome_count < thresholds$minimum_outcomes_per_arm
  matrix$feasible <- !subject_threshold_failed &
    !total_outcome_threshold_failed &
    !arm_outcome_threshold_failed
  matrix$feasibility_reason <- vapply(seq_len(nrow(matrix)), function(row_index) {
    failed_thresholds <- character()
    if (subject_threshold_failed[[row_index]]) {
      failed_thresholds <- c(failed_thresholds, "minimum_subjects_per_arm")
    }
    if (total_outcome_threshold_failed[[row_index]]) {
      failed_thresholds <- c(failed_thresholds, "minimum_total_outcomes")
    }
    if (arm_outcome_threshold_failed[[row_index]]) {
      failed_thresholds <- c(failed_thresholds, "minimum_outcomes_per_arm")
    }
    if (length(failed_thresholds) == 0L) {
      return("All engineering thresholds passed")
    }
    paste0("Failed engineering threshold(s): ", paste(failed_thresholds, collapse = ", "))
  }, character(1L))

  minimum_arm_size <- pmin(
    matrix$target_subject_count,
    matrix$comparator_subject_count
  )
  ordered_rows <- order(
    -as.integer(matrix$feasible),
    -minimum_arm_size,
    -matrix$total_outcome_count,
    matrix$target_concept_id,
    matrix$comparator_concept_id,
    matrix$outcome_concept_id
  )
  matrix <- matrix[
    ordered_rows,
    names(empty_feasibility_matrix()),
    drop = FALSE
  ]
  rownames(matrix) <- NULL
  matrix
}

#' Write the aggregate feasibility matrix
#'
#' @param feasibility_matrix Aggregate feasibility matrix to export.
#' @param path Destination CSV path.
#'
#' @return The output path.
write_feasibility_output <- function(
    feasibility_matrix,
    path = here::here("results", "tables", "feasibility_matrix.csv")) {
  if (!is.data.frame(feasibility_matrix)) {
    cli::cli_abort(
      "{.arg feasibility_matrix} must be a data frame.",
      class = "feasibility_output_error"
    )
  }
  valid_path <- checkmate::test_string(path, min.chars = 1L) && nzchar(trimws(path))
  if (!valid_path) {
    cli::cli_abort(
      "{.arg path} must be one non-empty character value.",
      class = "feasibility_output_error"
    )
  }
  column_names <- tolower(names(feasibility_matrix))
  allowed_concept_ids <- c(
    "target_concept_id",
    "comparator_concept_id",
    "outcome_concept_id"
  )
  identifier_columns <- column_names[
    (column_names == "id" | grepl("_id$", column_names)) &
      !column_names %in% allowed_concept_ids
  ]
  date_columns <- column_names[grepl("date", column_names)]
  date_type_columns <- column_names[vapply(feasibility_matrix, function(column) {
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
        "Feasibility output contains prohibited identifier or date column(s): ",
        paste(prohibited_columns, collapse = ", "),
        "."
      ),
      class = "feasibility_output_error"
    )
  }

  fs::dir_create(dirname(path))
  readr::write_csv(feasibility_matrix, path)
  path
}
