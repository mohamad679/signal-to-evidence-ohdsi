#' Return the aggregate reporting artifact contract
#'
#' @return A named character vector of required aggregate CSV paths.
reporting_table_paths <- function() {
  c(
    candidate_drug_review =
      "results/tables/candidate_drug_review.csv",
    candidate_outcome_review =
      "results/tables/candidate_outcome_review.csv",
    cohort_counts =
      "results/tables/cohort_counts.csv",
    covariate_balance =
      "results/tables/covariate_balance.csv",
    covariate_summary =
      "results/tables/covariate_summary.csv",
    feasibility_matrix =
      "results/tables/feasibility_matrix.csv",
    outcome_analysis_summary =
      "results/tables/outcome_analysis_summary.csv",
    population_summary =
      "results/tables/population_summary.csv",
    propensity_score_summary =
      "results/tables/propensity_score_summary.csv",
    sensitivity_analysis_summary =
      "results/tables/sensitivity_analysis_summary.csv",
    subgroup_analysis_summary =
      "results/tables/subgroup_analysis_summary.csv",
    table_counts =
      "results/tables/table_counts.csv",
    targeted_feasibility_review =
      "results/tables/targeted_feasibility_review.csv",
    top_conditions =
      "results/tables/top_conditions.csv",
    top_drugs =
      "results/tables/top_drugs.csv"
  )
}

#' Validate a reporting project root
#'
#' @param project_root Repository root.
#'
#' @return Normalized project root.
validate_reporting_project_root <- function(project_root) {
  valid <- is.character(project_root) &&
    length(project_root) == 1L &&
    !is.na(project_root) &&
    nzchar(project_root) &&
    dir.exists(project_root)

  if (!valid) {
    stop(
      "`project_root` must identify an existing directory.",
      call. = FALSE
    )
  }

  normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
}

#' Normalize reporting column names for privacy checks
#'
#' @param column_names Column names.
#'
#' @return Lowercase alphanumeric names.
normalize_reporting_column_names <- function(column_names) {
  if (!is.character(column_names) || anyNA(column_names)) {
    stop(
      "`column_names` must be a character vector without missing values.",
      call. = FALSE
    )
  }

  gsub("[^a-z0-9]", "", tolower(column_names))
}

#' Validate that a reporting table is aggregate and disclosure-safe
#'
#' @param table Aggregate table.
#' @param artifact_name Artifact label.
#'
#' @return `TRUE`, invisibly.
validate_disclosure_safe_reporting_table <- function(
    table,
    artifact_name = "reporting table") {
  if (!is.data.frame(table)) {
    stop(
      sprintf("`%s` must be a data frame.", artifact_name),
      call. = FALSE
    )
  }

  normalized <- normalize_reporting_column_names(names(table))
  prohibited <- c(
    "personid",
    "rowid",
    "subjectid",
    "indexdate",
    "cohortstartdate",
    "cohortenddate",
    "eventdate",
    "drugexposurestartdate"
  )
  detected <- intersect(normalized, prohibited)

  if (length(detected) > 0L) {
    stop(
      sprintf(
        "`%s` contains prohibited person-level columns: %s.",
        artifact_name,
        paste(detected, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Require columns in an aggregate reporting table
#'
#' @param table Aggregate table.
#' @param required Required column names.
#' @param artifact_name Artifact label.
#'
#' @return `TRUE`, invisibly.
require_reporting_columns <- function(
    table,
    required,
    artifact_name = "reporting table") {
  if (!is.data.frame(table)) {
    stop(
      sprintf("`%s` must be a data frame.", artifact_name),
      call. = FALSE
    )
  }

  if (!is.character(required) || length(required) == 0L || anyNA(required)) {
    stop(
      "`required` must be a non-empty character vector.",
      call. = FALSE
    )
  }

  missing_columns <- setdiff(required, names(table))

  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "`%s` is missing required columns: %s.",
        artifact_name,
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Read one aggregate reporting CSV
#'
#' @param project_root Repository root.
#' @param relative_path Repository-relative CSV path.
#'
#' @return Parsed data frame.
read_reporting_csv <- function(project_root, relative_path) {
  root <- validate_reporting_project_root(project_root)

  valid_path <- is.character(relative_path) &&
    length(relative_path) == 1L &&
    !is.na(relative_path) &&
    nzchar(relative_path) &&
    !grepl("(^|/)\\.\\.(/|$)", relative_path)

  if (!valid_path) {
    stop(
      "`relative_path` must be one safe repository-relative path.",
      call. = FALSE
    )
  }

  path <- file.path(root, relative_path)

  if (!file.exists(path)) {
    stop(
      sprintf("Required reporting artifact is missing: %s.", relative_path),
      call. = FALSE
    )
  }

  result <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (nrow(result) == 0L || ncol(result) == 0L) {
    stop(
      sprintf("Reporting artifact is empty: %s.", relative_path),
      call. = FALSE
    )
  }

  validate_disclosure_safe_reporting_table(
    result,
    artifact_name = relative_path
  )

  result
}

#' Validate a unique key in an aggregate reporting table
#'
#' @param table Aggregate table.
#' @param key Key columns.
#' @param artifact_name Artifact label.
#'
#' @return `TRUE`, invisibly.
validate_reporting_unique_key <- function(
    table,
    key,
    artifact_name = "reporting table") {
  require_reporting_columns(table, key, artifact_name)

  key_frame <- table[, key, drop = FALSE]

  if (anyNA(key_frame)) {
    stop(
      sprintf("`%s` has missing values in its key.", artifact_name),
      call. = FALSE
    )
  }

  duplicated_key <- duplicated(key_frame) | duplicated(
    key_frame,
    fromLast = TRUE
  )

  if (any(duplicated_key)) {
    stop(
      sprintf("`%s` has duplicated key rows.", artifact_name),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate all aggregate inputs required by the Quarto reports
#'
#' @param project_root Repository root.
#'
#' @return Named list of validated aggregate tables.
validate_reporting_inputs <- function(project_root) {
  root <- validate_reporting_project_root(project_root)
  paths <- reporting_table_paths()
  tables <- lapply(paths, function(path) {
    read_reporting_csv(root, path)
  })

  names(tables) <- names(paths)

  outcome <- tables$outcome_analysis_summary
  require_reporting_columns(
    outcome,
    c(
      "riskWindowStartDays",
      "riskWindowEndDays",
      "effectMeasure",
      "estimate",
      "ciLower",
      "ciUpper",
      "confidenceLevel",
      "subjectCount",
      "eventCount",
      "varianceEstimator",
      "modelConverged",
      "interpretation"
    ),
    "outcome_analysis_summary"
  )

  if (
    nrow(outcome) != 1L ||
      outcome$riskWindowStartDays[[1L]] != 1 ||
      outcome$riskWindowEndDays[[1L]] != 30 ||
      outcome$effectMeasure[[1L]] != "odds ratio" ||
      !isTRUE(outcome$modelConverged[[1L]]) ||
      !is.finite(outcome$estimate[[1L]]) ||
      !is.finite(outcome$ciLower[[1L]]) ||
      !is.finite(outcome$ciUpper[[1L]]) ||
      outcome$ciLower[[1L]] > outcome$estimate[[1L]] ||
      outcome$ciUpper[[1L]] < outcome$estimate[[1L]] ||
      outcome$interpretation[[1L]] !=
        "Adjusted observational association under the stated design assumptions."
  ) {
    stop(
      "Primary outcome summary violates the frozen reporting contract.",
      call. = FALSE
    )
  }

  subgroup <- tables$subgroup_analysis_summary
  require_reporting_columns(
    subgroup,
    c(
      "subgroupType",
      "subgroupLevel",
      "estimabilityStatus",
      "estimabilityReason",
      "effectMeasure",
      "estimate",
      "ciLower",
      "ciUpper",
      "subjectCount",
      "eventCount",
      "balanceStatus",
      "interpretation"
    ),
    "subgroup_analysis_summary"
  )
  validate_reporting_unique_key(
    subgroup,
    c("subgroupType", "subgroupLevel"),
    "subgroup_analysis_summary"
  )

  expected_subgroups <- data.frame(
    subgroupType = c("sex", "sex", "age", "age"),
    subgroupLevel = c("FEMALE", "MALE", "<65", ">=65"),
    stringsAsFactors = FALSE
  )
  observed_subgroups <- subgroup[, c("subgroupType", "subgroupLevel")]
  subgroup_matches <- merge(
    expected_subgroups,
    observed_subgroups,
    by = c("subgroupType", "subgroupLevel")
  )

  if (
    nrow(subgroup) != 4L ||
      nrow(subgroup_matches) != 4L ||
      !all(
        subgroup$estimabilityStatus %in%
          c("ESTIMABLE", "NOT_ESTIMABLE")
      ) ||
      !any(subgroup$estimabilityStatus == "NOT_ESTIMABLE")
  ) {
    stop(
      "Subgroup summary violates the frozen reporting contract.",
      call. = FALSE
    )
  }

  sensitivity <- tables$sensitivity_analysis_summary
  require_reporting_columns(
    sensitivity,
    c(
      "scenarioOrder",
      "scenarioId",
      "changedParameter",
      "isPrimary",
      "adjustmentMethod",
      "estimand",
      "washoutDays",
      "riskWindowStartDays",
      "riskWindowEndDays",
      "effectMeasure",
      "estimate",
      "ciLower",
      "ciUpper",
      "modelConverged",
      "interpretation"
    ),
    "sensitivity_analysis_summary"
  )
  validate_reporting_unique_key(
    sensitivity,
    "scenarioId",
    "sensitivity_analysis_summary"
  )

  expected_scenarios <- c(
    "primary",
    "risk_1_14",
    "risk_1_60",
    "weighting_att",
    "washout_365"
  )

  if (
    nrow(sensitivity) != 5L ||
      !identical(sensitivity$scenarioId, expected_scenarios) ||
      sum(sensitivity$isPrimary) != 1L ||
      !all(sensitivity$modelConverged) ||
      !all(sensitivity$estimand == "ATT")
  ) {
    stop(
      "Sensitivity summary violates the frozen reporting contract.",
      call. = FALSE
    )
  }

  propensity <- tables$propensity_score_summary
  require_reporting_columns(
    propensity,
    c(
      "target_before",
      "comparator_before",
      "matched_target_count",
      "matched_comparator_count",
      "matched_pair_count",
      "covariate_count",
      "unbalanced_before_count",
      "unbalanced_after_count",
      "maximum_absolute_smd_before",
      "maximum_absolute_smd_after",
      "balance_threshold"
    ),
    "propensity_score_summary"
  )

  if (
    nrow(propensity) != 1L ||
      propensity$balance_threshold[[1L]] != 0.1 ||
      propensity$matched_target_count[[1L]] !=
        propensity$matched_comparator_count[[1L]] ||
      propensity$matched_pair_count[[1L]] !=
        propensity$matched_target_count[[1L]]
  ) {
    stop(
      "Propensity-score summary violates the reporting contract.",
      call. = FALSE
    )
  }

  balance <- tables$covariate_balance
  require_reporting_columns(
    balance,
    c(
      "covariateId",
      "covariateName",
      "analysisId",
      "beforeSmd",
      "afterSmd",
      "balanced"
    ),
    "covariate_balance"
  )
  validate_reporting_unique_key(
    balance,
    "covariateId",
    "covariate_balance"
  )

  if (
    any(!is.finite(balance$beforeSmd)) ||
      any(!is.finite(balance$afterSmd))
  ) {
    stop(
      "Covariate balance contains non-finite SMD values.",
      call. = FALSE
    )
  }

  class(tables) <- c("validated_reporting_inputs", class(tables))
  tables
}

#' Format an estimate and confidence interval
#'
#' @param estimate Point estimate.
#' @param lower Lower confidence limit.
#' @param upper Upper confidence limit.
#' @param digits Decimal places.
#'
#' @return Formatted character value.
format_estimate_ci <- function(estimate, lower, upper, digits = 2L) {
  values <- c(estimate, lower, upper)

  valid_digits <- is.numeric(digits) &&
    length(digits) == 1L &&
    !is.na(digits) &&
    is.finite(digits) &&
    digits >= 0 &&
    digits == as.integer(digits)

  if (
    !is.numeric(values) ||
      length(values) != 3L ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      lower > estimate ||
      upper < estimate ||
      !valid_digits
  ) {
    stop(
      "Estimate, interval, and digits are invalid.",
      call. = FALSE
    )
  }

  template <- paste0("%.", as.integer(digits), "f (%.", as.integer(digits),
    "f to %.", as.integer(digits), "f)"
  )

  sprintf(template, estimate, lower, upper)
}

#' Prepare effect-estimate data for report plots
#'
#' @param reporting_inputs Validated reporting inputs.
#'
#' @return Plot-ready aggregate data frame.
prepare_effect_estimate_plot_data <- function(reporting_inputs) {
  if (!inherits(reporting_inputs, "validated_reporting_inputs")) {
    stop(
      "`reporting_inputs` must be validated reporting inputs.",
      call. = FALSE
    )
  }

  primary <- reporting_inputs$outcome_analysis_summary
  subgroup <- reporting_inputs$subgroup_analysis_summary
  sensitivity <- reporting_inputs$sensitivity_analysis_summary

  primary_rows <- data.frame(
    section = "Primary",
    label = "Primary analysis",
    estimate = primary$estimate,
    ciLower = primary$ciLower,
    ciUpper = primary$ciUpper,
    estimabilityStatus = "ESTIMABLE",
    stringsAsFactors = FALSE
  )

  subgroup_rows <- data.frame(
    section = "Subgroup",
    label = paste(
      subgroup$subgroupType,
      subgroup$subgroupLevel,
      sep = ": "
    ),
    estimate = subgroup$estimate,
    ciLower = subgroup$ciLower,
    ciUpper = subgroup$ciUpper,
    estimabilityStatus = subgroup$estimabilityStatus,
    stringsAsFactors = FALSE
  )

  sensitivity_rows <- data.frame(
    section = "Sensitivity",
    label = sensitivity$scenarioId,
    estimate = sensitivity$estimate,
    ciLower = sensitivity$ciLower,
    ciUpper = sensitivity$ciUpper,
    estimabilityStatus = ifelse(
      sensitivity$modelConverged,
      "ESTIMABLE",
      "NOT_ESTIMABLE"
    ),
    stringsAsFactors = FALSE
  )

  result <- rbind(
    primary_rows,
    subgroup_rows,
    sensitivity_rows
  )
  row.names(result) <- NULL
  result
}

#' Prepare covariate-balance data for report plots
#'
#' @param reporting_inputs Validated reporting inputs.
#'
#' @return Long-form aggregate balance data.
prepare_balance_plot_data <- function(reporting_inputs) {
  if (!inherits(reporting_inputs, "validated_reporting_inputs")) {
    stop(
      "`reporting_inputs` must be validated reporting inputs.",
      call. = FALSE
    )
  }

  balance <- reporting_inputs$covariate_balance

  result <- rbind(
    data.frame(
      covariateId = balance$covariateId,
      covariateName = balance$covariateName,
      stage = "Before adjustment",
      absoluteSmd = abs(balance$beforeSmd),
      stringsAsFactors = FALSE
    ),
    data.frame(
      covariateId = balance$covariateId,
      covariateName = balance$covariateName,
      stage = "After adjustment",
      absoluteSmd = abs(balance$afterSmd),
      stringsAsFactors = FALSE
    )
  )

  result <- result[
    order(result$absoluteSmd, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  row.names(result) <- NULL
  result
}
