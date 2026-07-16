#' Retrieve a required study function
#'
#' @param name Function name.
#'
#' @return The requested function.
get_sensitivity_function <- function(name) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name)
  ) {
    stop(
      "`name` must be one non-empty character value.",
      call. = FALSE
    )
  }

  get(
    name,
    mode = "function",
    inherits = TRUE
  )
}

#' Validate the frozen sensitivity configuration
#'
#' @param config Study configuration.
#'
#' @return `TRUE`, invisibly.
validate_sensitivity_config <- function(config) {
  validate_primary <- get_sensitivity_function(
    "validate_propensity_score_config"
  )

  validate_primary(config)

  if (
    is.null(config$sensitivity) ||
      !is.list(config$sensitivity)
  ) {
    stop(
      "`config$sensitivity` must be a list.",
      call. = FALSE
    )
  }

  expected_names <- c(
    "risk_windows",
    "adjustment_methods",
    "washout_days"
  )

  if (!identical(names(config$sensitivity), expected_names)) {
    stop(
      paste0(
        "`config$sensitivity` must contain exactly: ",
        paste(expected_names, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  valid_windows <-
    is.list(config$sensitivity$risk_windows) &&
    identical(
      lapply(config$sensitivity$risk_windows, as.integer),
      list(
        c(1L, 14L),
        c(1L, 30L),
        c(1L, 60L)
      )
    )

  if (!valid_windows) {
    stop(
      "Sensitivity risk windows must be 1-14, 1-30, and 1-60.",
      call. = FALSE
    )
  }

  if (!identical(
    config$sensitivity$adjustment_methods,
    c("matching", "weighting")
  )) {
    stop(
      "Adjustment methods must be matching and weighting.",
      call. = FALSE
    )
  }

  if (!identical(
    as.integer(config$sensitivity$washout_days),
    c(180L, 365L)
  )) {
    stop(
      "Washout periods must be 180 and 365 days.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Create frozen one-factor-at-a-time scenarios
#'
#' @param config Study configuration.
#'
#' @return Exactly five analysis scenarios.
create_sensitivity_scenarios <- function(config) {
  validate_sensitivity_config(config)

  data.frame(
    scenarioOrder = seq_len(5L),
    scenarioId = c(
      "primary",
      "risk_1_14",
      "risk_1_60",
      "weighting_att",
      "washout_365"
    ),
    changedParameter = c(
      "none",
      "risk_window",
      "risk_window",
      "adjustment_method",
      "washout_days"
    ),
    riskWindowStartDays = rep(1L, 5L),
    riskWindowEndDays = c(30L, 14L, 60L, 30L, 30L),
    adjustmentMethod = c(
      "matching",
      "matching",
      "matching",
      "weighting",
      "matching"
    ),
    washoutDays = c(180L, 180L, 180L, 180L, 365L),
    trimPreferenceScore = rep(TRUE, 5L),
    trimFraction = rep(0.05, 5L),
    estimand = rep("ATT", 5L),
    isPrimary = c(TRUE, rep(FALSE, 4L)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Calculate Kish effective sample size
#'
#' @param weights Strictly positive weights.
#'
#' @return Effective sample size.
calculate_effective_sample_size <- function(weights) {
  if (
    !is.numeric(weights) ||
      length(weights) == 0L ||
      anyNA(weights) ||
      any(!is.finite(weights)) ||
      any(weights <= 0)
  ) {
    stop(
      "`weights` must contain finite strictly positive values.",
      call. = FALSE
    )
  }

  sum(weights)^2 / sum(weights^2)
}

#' Calculate ATT propensity-score weights
#'
#' @param ps_population Trimmed propensity-score population.
#'
#' @return Population with `analysisWeight`.
calculate_att_weights <- function(ps_population) {
  validate_population <- get_sensitivity_function(
    "validate_ps_population"
  )

  expected_columns <- c(
    "rowId",
    "treatment",
    "propensityScore",
    "preferenceScore"
  )

  validate_population(
    ps_population,
    required = expected_columns,
    allowed = expected_columns
  )

  if (
    !all(c(0, 1) %in% ps_population$treatment) ||
      any(
        ps_population$propensityScore <= 0 |
          ps_population$propensityScore >= 1
      )
  ) {
    stop(
      "ATT weighting requires both groups and propensity scores in (0, 1).",
      call. = FALSE
    )
  }

  analysis_weight <- ifelse(
    ps_population$treatment == 1,
    1,
    ps_population$propensityScore / (1 - ps_population$propensityScore)
  )

  if (
    anyNA(analysis_weight) ||
      any(!is.finite(analysis_weight)) ||
      any(analysis_weight <= 0)
  ) {
    stop(
      "Calculated ATT weights must be finite and positive.",
      call. = FALSE
    )
  }

  result <- ps_population
  result$analysisWeight <- as.numeric(analysis_weight)
  row.names(result) <- NULL
  result
}

#' Compute one weighted sparse standardized mean difference
#'
#' @param values Sparse covariate values.
#' @param row_ids Sparse row identifiers.
#' @param population Weighted treatment population.
#' @param missing_means_zero FeatureExtraction missingness flag.
#'
#' @return Weighted standardized mean difference.
compute_weighted_sparse_smd <- function(
    values,
    row_ids,
    population,
    missing_means_zero = NULL) {
  required_columns <- c(
    "rowId",
    "treatment",
    "analysisWeight"
  )

  valid_population <-
    is.data.frame(population) &&
    all(required_columns %in% names(population)) &&
    nrow(population) > 0L &&
    is.numeric(population$rowId) &&
    !anyNA(population$rowId) &&
    anyDuplicated(population$rowId) == 0L &&
    is.numeric(population$treatment) &&
    !anyNA(population$treatment) &&
    all(population$treatment %in% c(0, 1)) &&
    all(c(0, 1) %in% population$treatment) &&
    is.numeric(population$analysisWeight) &&
    !anyNA(population$analysisWeight) &&
    all(is.finite(population$analysisWeight)) &&
    all(population$analysisWeight > 0)

  if (!valid_population) {
    stop(
      "Weighted balance population is invalid.",
      call. = FALSE
    )
  }

  valid_values <-
    is.numeric(values) &&
    is.numeric(row_ids) &&
    length(values) == length(row_ids) &&
    !anyNA(row_ids) &&
    all(is.finite(values))

  if (!valid_values) {
    stop(
      "Sparse covariate values are invalid.",
      call. = FALSE
    )
  }

  invisible(missing_means_zero)

  positions <- match(row_ids, population$rowId)
  retained <- !is.na(positions)
  retained_rows <- row_ids[retained]

  if (anyDuplicated(retained_rows) > 0L) {
    stop(
      "A covariate has duplicate values for one row ID.",
      call. = FALSE
    )
  }

  retained_values <- as.numeric(values[retained])
  retained_positions <- positions[retained]

  group_moments <- function(group_value) {
    members <- population$treatment == group_value
    total_weight <- sum(population$analysisWeight[members])
    retained_group <-
      population$treatment[retained_positions] == group_value
    group_positions <- retained_positions[retained_group]
    group_values <- retained_values[retained_group]
    group_weights <- population$analysisWeight[group_positions]
    weighted_sum <- sum(group_weights * group_values)
    weighted_square_sum <- sum(group_weights * group_values^2)
    mean_value <- weighted_sum / total_weight
    variance_value <-
      weighted_square_sum / total_weight - mean_value^2

    c(
      mean = mean_value,
      standardDeviation = sqrt(max(variance_value, 0))
    )
  }

  target <- group_moments(1)
  comparator <- group_moments(0)
  difference <- target[["mean"]] - comparator[["mean"]]
  denominator <- sqrt(
    (
      target[["standardDeviation"]]^2 +
        comparator[["standardDeviation"]]^2
    ) / 2
  )
  tolerance <- sqrt(.Machine$double.eps)

  if (denominator <= tolerance) {
    if (abs(difference) <= tolerance) {
      return(0)
    }

    return(NA_real_)
  }

  result <- difference / denominator

  if (!is.finite(result)) {
    return(NA_real_)
  }

  result
}

#' Compute balance before and after ATT weighting
#'
#' @param covariate_data FeatureExtraction CovariateData.
#' @param population_before Population before adjustment.
#' @param weighted_population ATT-weighted population.
#' @param threshold Absolute SMD threshold.
#'
#' @return One aggregate row per covariate.
compute_weighted_covariate_balance <- function(
    covariate_data,
    population_before,
    weighted_population,
    threshold = 0.1) {
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    stop(
      "`covariate_data` must be FeatureExtraction CovariateData.",
      call. = FALSE
    )
  }

  validate_population <- get_sensitivity_function(
    "validate_ps_population"
  )

  validate_population(
    population_before,
    required = c("rowId", "treatment")
  )

  validate_population(
    weighted_population,
    required = c("rowId", "treatment")
  )

  if (
    !"analysisWeight" %in% names(weighted_population) ||
      !is.numeric(weighted_population$analysisWeight) ||
      anyNA(weighted_population$analysisWeight) ||
      any(!is.finite(weighted_population$analysisWeight)) ||
      any(weighted_population$analysisWeight <= 0)
  ) {
    stop(
      "The weighted population contains invalid weights.",
      call. = FALSE
    )
  }

  positions <- match(
    weighted_population$rowId,
    population_before$rowId
  )

  if (
    anyNA(positions) ||
      any(
        weighted_population$treatment !=
          population_before$treatment[positions]
      )
  ) {
    stop(
      "The weighted population must be a treatment-consistent subset.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      !is.finite(threshold) ||
      threshold < 0
  ) {
    stop(
      "`threshold` must be one finite non-negative value.",
      call. = FALSE
    )
  }

  covariates <- covariate_data$covariates |>
    dplyr::select(
      "rowId",
      "covariateId",
      "covariateValue"
    ) |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)

  covariate_ref <- covariate_data$covariateRef |>
    dplyr::select(
      "covariateId",
      "covariateName",
      "analysisId"
    ) |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)

  analysis_ref <- covariate_data$analysisRef |>
    dplyr::select(
      "analysisId",
      "isBinary",
      "missingMeansZero"
    ) |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)

  if (
    nrow(covariates) == 0L ||
      nrow(covariate_ref) == 0L ||
      nrow(analysis_ref) == 0L ||
      anyDuplicated(covariate_ref$covariateId) > 0L ||
      anyDuplicated(analysis_ref$analysisId) > 0L
  ) {
    stop(
      "Covariate references must be non-empty and unique.",
      call. = FALSE
    )
  }

  analysis_position <- match(
    covariate_ref$analysisId,
    analysis_ref$analysisId
  )

  if (anyNA(analysis_position)) {
    stop(
      "Every covariate must map to an analysis reference.",
      call. = FALSE
    )
  }

  parse_flag <- get_sensitivity_function("as_ps_flag")
  compute_unweighted_smd <- get_sensitivity_function(
    "compute_sparse_smd"
  )
  matched_analysis_ref <- analysis_ref[
    analysis_position,
    ,
    drop = FALSE
  ]
  is_binary <- parse_flag(matched_analysis_ref$isBinary)
  missing_means_zero <- parse_flag(
    matched_analysis_ref$missingMeansZero,
    allow_missing = TRUE
  )
  split_rows <- split(
    seq_len(nrow(covariates)),
    covariates$covariateId
  )

  calculate_unweighted <- function(covariate_id) {
    rows <- split_rows[[as.character(covariate_id)]]

    if (is.null(rows)) {
      rows <- integer()
    }

    reference_row <- match(
      covariate_id,
      covariate_ref$covariateId
    )

    compute_unweighted_smd(
      values = covariates$covariateValue[rows],
      row_ids = covariates$rowId[rows],
      population = population_before,
      missing_means_zero = missing_means_zero[reference_row]
    )
  }

  calculate_weighted <- function(covariate_id) {
    rows <- split_rows[[as.character(covariate_id)]]

    if (is.null(rows)) {
      rows <- integer()
    }

    reference_row <- match(
      covariate_id,
      covariate_ref$covariateId
    )

    compute_weighted_sparse_smd(
      values = covariates$covariateValue[rows],
      row_ids = covariates$rowId[rows],
      population = weighted_population,
      missing_means_zero = missing_means_zero[reference_row]
    )
  }

  before_smd <- vapply(
    covariate_ref$covariateId,
    calculate_unweighted,
    numeric(1L)
  )
  after_smd <- vapply(
    covariate_ref$covariateId,
    calculate_weighted,
    numeric(1L)
  )

  data.frame(
    covariateId = covariate_ref$covariateId,
    covariateName = as.character(covariate_ref$covariateName),
    analysisId = covariate_ref$analysisId,
    isBinary = is_binary,
    beforeSmd = before_smd,
    afterSmd = after_smd,
    balanced = !is.na(after_smd) & abs(after_smd) < threshold,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Validate weighted binary-outcome rows
#'
#' @param analysis_population Weighted outcome rows.
#'
#' @return The population, invisibly.
validate_weighted_outcome_population <- function(
    analysis_population) {
  expected_columns <- c(
    "rowId",
    "treatment",
    "analysisWeight",
    "outcome"
  )

  if (
    !is.data.frame(analysis_population) ||
      !identical(names(analysis_population), expected_columns) ||
      nrow(analysis_population) == 0L
  ) {
    stop(
      paste0(
        "`analysis_population` must contain exactly: ",
        paste(expected_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (
    !is.numeric(analysis_population$rowId) ||
      anyNA(analysis_population$rowId) ||
      anyDuplicated(analysis_population$rowId) > 0L
  ) {
    stop(
      "Weighted analysis row IDs must be complete and unique.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(analysis_population$treatment) ||
      anyNA(analysis_population$treatment) ||
      any(!analysis_population$treatment %in% c(0, 1)) ||
      !all(c(0, 1) %in% analysis_population$treatment)
  ) {
    stop(
      "Weighted treatment must contain both numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(analysis_population$outcome) ||
      anyNA(analysis_population$outcome) ||
      any(!analysis_population$outcome %in% c(0, 1))
  ) {
    stop(
      "Weighted outcome must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(analysis_population$analysisWeight) ||
      anyNA(analysis_population$analysisWeight) ||
      any(!is.finite(analysis_population$analysisWeight)) ||
      any(analysis_population$analysisWeight <= 0)
  ) {
    stop(
      "Analysis weights must be finite and positive.",
      call. = FALSE
    )
  }

  invisible(analysis_population)
}

#' Build weighted binary-outcome rows
#'
#' @param weighted_population ATT-weighted PS population.
#' @param treatment_population Internal treatment linkage.
#' @param outcome_cohort Internal outcome cohort.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#'
#' @return Weighted rows without person identifiers or dates.
build_weighted_outcome_population <- function(
    weighted_population,
    treatment_population,
    outcome_cohort,
    risk_window_start_days,
    risk_window_end_days) {
  validate_population <- get_sensitivity_function(
    "validate_ps_population"
  )
  weighted_columns <- c(
    "rowId",
    "treatment",
    "propensityScore",
    "preferenceScore",
    "analysisWeight"
  )

  validate_population(
    weighted_population,
    required = weighted_columns,
    allowed = weighted_columns
  )

  if (
    anyNA(weighted_population$analysisWeight) ||
      any(!is.finite(weighted_population$analysisWeight)) ||
      any(weighted_population$analysisWeight <= 0)
  ) {
    stop(
      "Weighted population contains invalid weights.",
      call. = FALSE
    )
  }

  validate_treatment <- get_sensitivity_function(
    "validate_treatment_population"
  )
  validate_outcome <- get_sensitivity_function(
    "validate_outcome_cohort"
  )
  validate_window <- get_sensitivity_function(
    "validate_risk_window"
  )

  validate_treatment(treatment_population)
  validate_outcome(outcome_cohort)
  risk_window <- validate_window(
    risk_window_start_days,
    risk_window_end_days
  )
  linkage_position <- match(
    weighted_population$rowId,
    treatment_population$rowId
  )

  if (anyNA(linkage_position)) {
    stop(
      "Every weighted row must have treatment linkage.",
      call. = FALSE
    )
  }

  linkage <- treatment_population[
    linkage_position,
    ,
    drop = FALSE
  ]

  if (!identical(
    as.numeric(linkage$treatment),
    as.numeric(weighted_population$treatment)
  )) {
    stop(
      "Treatment assignments disagree in weighted linkage.",
      call. = FALSE
    )
  }

  risk_start <- linkage$cohortStartDate + risk_window[[1L]]
  risk_end <- linkage$cohortStartDate + risk_window[[2L]]
  outcome_dates <- split(
    outcome_cohort$cohortStartDate,
    as.character(outcome_cohort$subjectId),
    drop = TRUE
  )
  outcome <- vapply(
    seq_len(nrow(linkage)),
    function(index) {
      subject_dates <- outcome_dates[[
        as.character(linkage$subjectId[index])
      ]]

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
  result <- data.frame(
    rowId = weighted_population$rowId,
    treatment = weighted_population$treatment,
    analysisWeight = weighted_population$analysisWeight,
    outcome = as.integer(outcome),
    check.names = FALSE
  )

  validate_weighted_outcome_population(result)
  result
}

#' Fit an ATT-weighted logistic outcome model
#'
#' @param analysis_population Weighted binary-outcome rows.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#' @param confidence_level Two-sided confidence level.
#'
#' @return One aggregate observational association result.
fit_weighted_outcome_model <- function(
    analysis_population,
    risk_window_start_days,
    risk_window_end_days,
    confidence_level = 0.95) {
  validate_weighted_outcome_population(analysis_population)
  validate_window <- get_sensitivity_function("validate_risk_window")
  risk_window <- validate_window(
    risk_window_start_days,
    risk_window_end_days
  )

  if (
    !is.numeric(confidence_level) ||
      length(confidence_level) != 1L ||
      is.na(confidence_level) ||
      !is.finite(confidence_level) ||
      confidence_level <= 0 ||
      confidence_level >= 1
  ) {
    stop(
      "`confidence_level` must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  treatment <- as.numeric(analysis_population$treatment)
  outcome <- as.numeric(analysis_population$outcome)
  analysis_weight <- as.numeric(analysis_population$analysisWeight)
  outcome_table <- table(
    factor(treatment, levels = c(0, 1)),
    factor(outcome, levels = c(0, 1))
  )

  if (any(outcome_table == 0L)) {
    stop(
      paste(
        "The weighted treatment-by-outcome table contains a zero cell;",
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
      weights = analysis_weight,
      family = stats::quasibinomial()
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
        "The weighted model emitted a fitting warning:",
        paste(model_warnings, collapse = "; ")
      ),
      call. = FALSE
    )
  }

  valid_fit <-
    isTRUE(fit$converged) &&
    fit$rank == ncol(design) &&
    length(fit$coefficients) == 2L &&
    !anyNA(fit$coefficients) &&
    all(is.finite(fit$coefficients))

  if (!valid_fit) {
    stop(
      "The weighted logistic model did not produce a valid fit.",
      call. = FALSE
    )
  }

  score_residual <-
    analysis_weight * (outcome - fit$fitted.values)
  robust_covariance <- get_sensitivity_function(
    "compute_cluster_robust_vcov"
  )
  covariance <- robust_covariance(
    design = design,
    score_residual = score_residual,
    weights = fit$weights,
    cluster = analysis_population$rowId
  )
  standard_error <- sqrt(covariance[2L, 2L])

  if (!is.finite(standard_error) || standard_error <= 0) {
    stop(
      "The weighted standard error must be finite and positive.",
      call. = FALSE
    )
  }

  log_odds_ratio <- unname(fit$coefficients[[2L]])
  critical_value <- stats::qnorm(
    1 - (1 - confidence_level) / 2
  )
  confidence_limits <-
    log_odds_ratio +
    c(-1, 1) * critical_value * standard_error
  target <- treatment == 1
  comparator <- treatment == 0
  target_ess <- calculate_effective_sample_size(
    analysis_weight[target]
  )
  comparator_ess <- calculate_effective_sample_size(
    analysis_weight[comparator]
  )

  data.frame(
    effectMeasure = "odds ratio",
    estimate = exp(log_odds_ratio),
    ciLower = exp(confidence_limits[[1L]]),
    ciUpper = exp(confidence_limits[[2L]]),
    confidenceLevel = confidence_level,
    logOddsRatio = log_odds_ratio,
    standardError = standard_error,
    subjectCount = as.integer(length(outcome)),
    eventCount = as.integer(sum(outcome)),
    targetSubjectCount = as.integer(sum(target)),
    targetEventCount = as.integer(sum(outcome[target])),
    comparatorSubjectCount = as.integer(sum(comparator)),
    comparatorEventCount = as.integer(sum(outcome[comparator])),
    targetWeightTotal = sum(analysis_weight[target]),
    comparatorWeightTotal = sum(analysis_weight[comparator]),
    targetEffectiveSampleSize = target_ess,
    comparatorEffectiveSampleSize = comparator_ess,
    effectiveSampleSize = target_ess + comparator_ess,
    minimumWeight = min(analysis_weight),
    maximumWeight = max(analysis_weight),
    riskWindowStartDays = risk_window[[1L]],
    riskWindowEndDays = risk_window[[2L]],
    varianceEstimator = "individual-level robust CR1",
    modelConverged = TRUE,
    zeroCellDetected = FALSE,
    interpretation = paste(
      "Adjusted observational association",
      "under the stated design assumptions."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
