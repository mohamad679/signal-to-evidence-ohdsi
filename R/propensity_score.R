#' Abort with a focused propensity-score condition
#'
#' @param message Error message.
#' @param class One of the propensity-score condition classes.
#'
#' @return This function does not return.
utils::globalVariables(".data")

abort_propensity_score <- function(message, class) {
  cli::cli_abort(message, class = c(class, "propensity_score_error"))
}

#' Normalize a column name for privacy checks
#'
#' @param names Column names.
#'
#' @return Lowercase alphanumeric column names.
normalize_ps_names <- function(names) {
  tolower(gsub("[^a-z0-9]+", "", names))
}

#' Find prohibited person-level columns
#'
#' `rowId` and `matchId` are permitted study-local identifiers. Direct person,
#' subject, outcome, and date fields are not permitted in propensity-score
#' artifacts.
#'
#' @param data A data frame.
#'
#' @return Prohibited column names.
find_prohibited_ps_columns <- function(data) {
  normalized <- normalize_ps_names(names(data))
  direct_identifier <- (grepl("id$", normalized) |
                          grepl("identifier", normalized)) &
    !normalized %in% c("rowid", "matchid", "covariateid", "analysisid")
  prohibited <- direct_identifier | grepl(
    "person|subject|outcome|date|cohortstart|cohortend",
    normalized
  )
  names(data)[prohibited]
}

#' Validate a propensity-score population
#'
#' @param population Population data frame.
#' @param required Required columns.
#' @param allowed Optional exact set of allowed columns.
#' @param allow_empty Whether an empty population is permitted.
#'
#' @return The population, invisibly.
validate_ps_population <- function(
    population,
    required,
    allowed = NULL,
    allow_empty = FALSE) {
  if (!is.data.frame(population)) {
    abort_propensity_score(
      "{.arg population} must be a data frame.",
      "propensity_score_argument_error"
    )
  }
  if (!allow_empty && nrow(population) == 0L) {
    abort_propensity_score(
      "{.arg population} must contain at least one row.",
      "propensity_score_data_error"
    )
  }
  if (!all(required %in% names(population))) {
    missing_columns <- setdiff(required, names(population))
    abort_propensity_score(
      paste0(
        "{.arg population} is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      "propensity_score_argument_error"
    )
  }
  if (!is.null(allowed) && !identical(names(population), allowed)) {
    abort_propensity_score(
      paste0(
        "{.arg population} must contain exactly these columns in order: ",
        paste(allowed, collapse = ", "),
        "."
      ),
      "propensity_score_privacy_error"
    )
  }

  prohibited <- find_prohibited_ps_columns(population)
  if (length(prohibited) > 0L) {
    abort_propensity_score(
      paste0(
        "Person-level outcome, subject, person, and date columns are prohibited: ",
        paste(prohibited, collapse = ", "),
        "."
      ),
      "propensity_score_privacy_error"
    )
  }

  row_id <- population$rowId
  valid_row_id <- is.numeric(row_id) &&
    length(row_id) == nrow(population) &&
    all(is.finite(row_id)) &&
    all(row_id == round(row_id)) &&
    all(row_id > 0) &&
    !anyDuplicated(row_id)
  if (!valid_row_id) {
    abort_propensity_score(
      "{.field rowId} must contain unique positive whole numbers.",
      "propensity_score_data_error"
    )
  }

  treatment <- population$treatment
  valid_treatment <- is.numeric(treatment) &&
    length(treatment) == nrow(population) &&
    !anyNA(treatment) &&
    all(treatment %in% c(0, 1))
  if (!valid_treatment) {
    abort_propensity_score(
      "{.field treatment} must contain only 1 for target and 0 for comparator.",
      "propensity_score_data_error"
    )
  }

  for (score_name in intersect(
    c("propensityScore", "preferenceScore"),
    names(population)
  )) {
    score <- population[[score_name]]
    if (!is.numeric(score) || anyNA(score) || any(!is.finite(score))) {
      abort_propensity_score(
        "{.field {score_name}} must contain finite numeric values.",
        "propensity_score_data_error"
      )
    }
  }

  invisible(population)
}

#' Interpret FeatureExtraction logical flags
#'
#' @param values FeatureExtraction flag values.
#' @param allow_missing Whether missing flags are permitted.
#'
#' @return A logical vector, with missing flags represented as `FALSE` when
#'   permitted.
as_ps_flag <- function(values, allow_missing = FALSE) {
  normalized <- tolower(trimws(as.character(values)))
  missing <- is.na(values) | normalized == "" | normalized == "na"
  true_values <- c("1", "true", "t", "yes", "y")
  false_values <- c("0", "false", "f", "no", "n")
  invalid <- !missing & !normalized %in% c(true_values, false_values)
  if (any(invalid) || (!allow_missing && any(missing))) {
    abort_propensity_score(
      "FeatureExtraction reference data contains invalid logical flags.",
      "propensity_score_data_error"
    )
  }
  result <- normalized %in% true_values
  result[missing] <- FALSE
  result
}

#' Validate the propensity-score study configuration
#'
#' Only the frozen primary specification is supported: propensity-score
#' matching for the ATT, symmetric 5% preference-score trimming, 1:1 matching,
#' a standard-deviation caliper, and an absolute SMD threshold of 0.1.
#'
#' @param config Study configuration.
#'
#' @return `TRUE`, invisibly.
validate_propensity_score_config <- function(config) {
  if (!checkmate::test_list(config, names = "unique") ||
        is.null(config$project) ||
        is.null(config$propensity_score) ||
        is.null(config$balance)) {
    abort_propensity_score(
      paste0(
        "{.arg config} must contain project, propensity_score, and balance ",
        "sections."
      ),
      "propensity_score_argument_error"
    )
  }

  ps <- config$propensity_score
  required_ps <- c(
    "method",
    "estimand",
    "trim_preference_score",
    "trim_fraction",
    "matching_ratio",
    "caliper_scale"
  )
  if (!all(required_ps %in% names(ps))) {
    abort_propensity_score(
      "{.field propensity_score} is missing a required primary setting.",
      "propensity_score_argument_error"
    )
  }

  supported <- identical(ps$method, "matching") &&
    identical(ps$estimand, "ATT") &&
    isTRUE(ps$trim_preference_score) &&
    checkmate::test_number(ps$trim_fraction, finite = TRUE) &&
    identical(as.numeric(ps$trim_fraction), 0.05) &&
    checkmate::test_integerish(
      ps$matching_ratio,
      lower = 1,
      upper = 1,
      len = 1L,
      any.missing = FALSE
    ) &&
    identical(ps$caliper_scale, "standard_deviation") &&
    checkmate::test_number(
      config$balance$absolute_smd_threshold,
      finite = TRUE
    ) &&
    identical(as.numeric(config$balance$absolute_smd_threshold), 0.1) &&
    checkmate::test_integerish(
      config$project$random_seed,
      lower = 1,
      len = 1L,
      any.missing = FALSE
    )

  if (!supported) {
    abort_propensity_score(
      paste0(
        "Only matching, ATT, 0.05 preference-score trimming, a 1:1 ratio, ",
        "the standard_deviation caliper scale, and a 0.1 balance threshold ",
        "are supported."
      ),
      "propensity_score_argument_error"
    )
  }

  invisible(TRUE)
}

#' Validate a project-local propensity-score path
#'
#' @param path Candidate path.
#' @param allowed_directory Required parent directory.
#' @param must_exist Whether the candidate must be an existing file.
#'
#' @return Canonical absolute path.
validate_local_ps_path <- function(path, allowed_directory, must_exist = FALSE) {
  valid_path <- checkmate::test_string(path, min.chars = 1L) &&
    nzchar(trimws(path))
  if (!valid_path) {
    abort_propensity_score(
      "{.arg path} must be one non-empty character value.",
      "propensity_score_argument_error"
    )
  }

  components <- unlist(fs::path_split(path), use.names = FALSE)
  has_unsafe_component <- any(components == "..")
  has_symbolic_link <- path_contains_symbolic_link(allowed_directory) || # nolint: object_usage_linter, line_length_linter.
    path_contains_symbolic_link(path)
  if (has_unsafe_component || has_symbolic_link) {
    abort_propensity_score(
      "{.arg path} must not contain parent traversal or symbolic links.",
      "propensity_score_privacy_error"
    )
  }

  allowed <- canonicalize_path(allowed_directory) # nolint: object_usage_linter, line_length_linter.
  candidate <- canonicalize_path(path) # nolint: object_usage_linter, line_length_linter.
  is_directory <- identical(as.character(candidate), as.character(allowed))
  is_allowed <- fs::path_has_parent(candidate, allowed)
  if (is_directory || !is_allowed) {
    abort_propensity_score(
      "{.arg path} must remain inside {.path data/derived}.",
      "propensity_score_privacy_error"
    )
  }
  if (must_exist && !isTRUE(fs::is_file(candidate))) {
    abort_propensity_score(
      "Baseline covariate archive is unavailable: {.path {candidate}}.",
      "propensity_score_data_error"
    )
  }
  as.character(candidate)
}

#' Load the saved baseline FeatureExtraction archive
#'
#' @param path Path below `data/derived`.
#'
#' @return A FeatureExtraction CovariateData object. The caller is
#'   responsible for closing it with `Andromeda::close()`.
load_baseline_covariates <- function(
    path = here::here("data", "derived", "baseline_covariates.rds")) {
  source <- validate_local_ps_path(
    path = path,
    allowed_directory = here::here("data", "derived"),
    must_exist = TRUE
  )
  covariate_data <- FeatureExtraction::loadCovariateData(file = source)
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    if (Andromeda::isAndromeda(covariate_data) &&
          Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    }
    abort_propensity_score(
      "The baseline archive did not contain FeatureExtraction CovariateData.",
      "propensity_score_data_error"
    )
  }
  covariate_data
}

#' Create the deterministic treatment population
#'
#' Reuses the baseline-extraction row-ID construction and verifies complete,
#' one-to-one agreement with the saved sparse covariate archive.
#'
#' @param connection Open DatabaseConnector connection.
#' @param cohort_tables Named target, comparator, and outcome cohort tables.
#' @param covariate_data Loaded FeatureExtraction CovariateData.
#' @param table_name Persistent working table name.
#'
#' @return A data frame containing only `rowId` and `treatment`.
create_propensity_score_population <- function(
    connection,
    cohort_tables,
    covariate_data,
    table_name = "study_ps_cohort") {
  validate_cohort_connection(connection) # nolint: object_usage_linter, line_length_linter.
  table_name <- validate_covariate_table_name(table_name) # nolint: object_usage_linter, line_length_linter.
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    abort_propensity_score(
      "{.arg covariate_data} must be FeatureExtraction CovariateData.",
      "propensity_score_argument_error"
    )
  }

  create_feature_extraction_cohort_table( # nolint: object_usage_linter, line_length_linter.
    connection = connection,
    cohort_tables = cohort_tables,
    table_name = table_name
  )
  cohort_rows <- DatabaseConnector::querySql(
    connection = connection,
    sql = paste(
      "SELECT row_id, cohort_definition_id, subject_id",
      paste0("FROM main.", table_name),
      "ORDER BY row_id"
    ),
    snakeCaseToCamelCase = FALSE
  )
  cohort_rows <- as.data.frame(cohort_rows, stringsAsFactors = FALSE)
  expected_columns <- c("row_id", "cohort_definition_id", "subject_id")
  if (!identical(names(cohort_rows), expected_columns) ||
        nrow(cohort_rows) != 2630L ||
        anyDuplicated(cohort_rows$row_id) ||
        anyDuplicated(cohort_rows$subject_id)) {
    abort_propensity_score(
      "The PS working table must contain one row per person and 2,630 rows.",
      "propensity_score_data_error"
    )
  }

  target_count <- sum(cohort_rows$cohort_definition_id == 1)
  comparator_count <- sum(cohort_rows$cohort_definition_id == 2)
  valid_cohort_ids <- all(cohort_rows$cohort_definition_id %in% c(1, 2))
  if (!valid_cohort_ids || target_count != 1800L || comparator_count != 830L) {
    abort_propensity_score(
      "The PS population must contain exactly 1,800 target and 830 comparator rows.",
      "propensity_score_data_error"
    )
  }

  covariate_row_ids <- covariate_data$covariates |>
    dplyr::distinct(.data[["rowId"]]) |> # nolint: object_usage_linter, line_length_linter.
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)
  if (!identical(names(covariate_row_ids), "rowId") ||
        anyDuplicated(covariate_row_ids$rowId)) {
    abort_propensity_score(
      "The covariate archive contains invalid row identifiers.",
      "propensity_score_data_error"
    )
  }

  cohort_row_ids <- as.numeric(cohort_rows$row_id)
  archive_row_ids <- as.numeric(covariate_row_ids$rowId)
  if (!setequal(cohort_row_ids, archive_row_ids)) {
    abort_propensity_score(
      paste0(
        "Every cohort row must occur in the covariate archive, and every ",
        "covariate row must have exactly one treatment assignment."
      ),
      "propensity_score_data_error"
    )
  }

  population <- data.frame(
    rowId = cohort_row_ids,
    treatment = as.integer(cohort_rows$cohort_definition_id == 1),
    check.names = FALSE
  )
  validate_ps_population(
    population,
    required = c("rowId", "treatment"),
    allowed = c("rowId", "treatment")
  )
  population
}

#' Create supported Cyclops model data from baseline covariates
#'
#' CohortMethodData cannot be constructed from a saved CovariateData archive by
#' an exported constructor in CohortMethod 6.0.3. This function therefore uses
#' Cyclops' exported sparse data converter without modifying package objects.
#'
#' @param covariate_data Loaded FeatureExtraction CovariateData.
#' @param population Treatment population containing only row ID and treatment.
#'
#' @return A validated `propensity_score_model_data` object.
create_propensity_score_model_data <- function(covariate_data, population) {
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    abort_propensity_score(
      "{.arg covariate_data} must be FeatureExtraction CovariateData.",
      "propensity_score_argument_error"
    )
  }
  validate_covariate_data(covariate_data) # nolint: object_usage_linter, line_length_linter.
  validate_ps_population(
    population,
    required = c("rowId", "treatment"),
    allowed = c("rowId", "treatment")
  )

  covariates <- covariate_data$covariates |>
    dplyr::select("rowId", "covariateId", "covariateValue") |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)
  expected_columns <- c("rowId", "covariateId", "covariateValue")
  valid_values <- identical(names(covariates), expected_columns) &&
    nrow(covariates) > 0L &&
    all(vapply(covariates, is.numeric, logical(1L))) &&
    all(is.finite(covariates$rowId)) &&
    all(is.finite(covariates$covariateId)) &&
    all(is.finite(covariates$covariateValue)) &&
    all(covariates$rowId == round(covariates$rowId)) &&
    all(covariates$covariateId == round(covariates$covariateId))
  duplicate_key <- anyDuplicated(paste(
    covariates$rowId,
    covariates$covariateId,
    sep = ":"
  ))
  if (!valid_values || duplicate_key) {
    abort_propensity_score(
      "Sparse baseline covariates have an invalid structure or duplicate key.",
      "propensity_score_data_error"
    )
  }
  if (!setequal(unique(covariates$rowId), population$rowId)) {
    abort_propensity_score(
      "Model rows and treatment assignments must agree one-to-one.",
      "propensity_score_data_error"
    )
  }

  covariates <- covariates[order(covariates$covariateId, covariates$rowId), ]
  treatment_data <- data.frame(
    rowId = population$rowId,
    y = population$treatment,
    check.names = FALSE
  )
  treatment_data <- treatment_data[order(treatment_data$rowId), ]
  cyclops_data <- tryCatch(
    Cyclops::convertToCyclopsData(
      treatment_data,
      covariates,
      modelType = "lr",
      addIntercept = TRUE,
      checkRowIds = TRUE,
      normalize = "stdev",
      quiet = FALSE
    ),
    error = function(error) {
      abort_propensity_score(
        paste0("Cyclops model-data construction failed: ", conditionMessage(error)),
        "propensity_score_data_error"
      )
    }
  )
  if (!inherits(cyclops_data, "cyclopsData")) {
    abort_propensity_score(
      "Cyclops did not return a supported cyclopsData object.",
      "propensity_score_data_error"
    )
  }

  structure(
    list(
      cyclops_data = cyclops_data,
      row_ids = as.numeric(treatment_data$rowId)
    ),
    class = "propensity_score_model_data"
  )
}

#' Estimate propensity scores from baseline covariates
#'
#' Fits treatment assignment with Cyclops regularized logistic regression. No
#' outcomes, post-index fields, person IDs, or dates are accepted.
#'
#' @param model_data Validated model data from
#'   `create_propensity_score_model_data()`.
#' @param population Treatment population.
#' @param config Study configuration.
#'
#' @return One row per population row with row ID, treatment, and propensity
#'   score.
estimate_propensity_scores <- function(model_data, population, config) {
  validate_propensity_score_config(config)
  validate_ps_population(
    population,
    required = c("rowId", "treatment"),
    allowed = c("rowId", "treatment")
  )
  valid_model_data <- inherits(model_data, "propensity_score_model_data") &&
    identical(names(model_data), c("cyclops_data", "row_ids")) &&
    inherits(model_data$cyclops_data, "cyclopsData") &&
    is.numeric(model_data$row_ids) &&
    !anyDuplicated(model_data$row_ids) &&
    setequal(model_data$row_ids, population$rowId)
  if (!valid_model_data) {
    abort_propensity_score(
      "{.arg model_data} must be validated propensity-score model data.",
      "propensity_score_argument_error"
    )
  }

  seed <- as.integer(config$project$random_seed)
  set.seed(seed)
  prior <- Cyclops::createPrior(
    priorType = "laplace",
    exclude = c(0),
    useCrossValidation = TRUE
  )
  control <- Cyclops::createControl(
    noiseLevel = "silent",
    cvType = "auto",
    seed = seed,
    resetCoefficients = TRUE,
    tolerance = 2e-7,
    cvRepetitions = 10,
    startingVariance = 0.01
  )
  fit <- tryCatch(
    Cyclops::fitCyclopsModel(
      cyclopsData = model_data$cyclops_data,
      prior = prior,
      control = control
    ),
    error = function(error) {
      abort_propensity_score(
        paste0("Cyclops propensity-score estimation failed: ", conditionMessage(error)),
        "propensity_score_data_error"
      )
    }
  )
  if (!inherits(fit, "cyclopsFit") ||
        is.null(fit$return_flag) ||
        !identical(fit$return_flag, "SUCCESS")) {
    abort_propensity_score(
      "Cyclops did not report successful propensity-score convergence.",
      "propensity_score_data_error"
    )
  }

  prediction <- stats::predict(fit)
  scores <- as.numeric(prediction)
  score_names <- names(prediction)
  if (length(scores) != nrow(population) || is.null(score_names)) {
    abort_propensity_score(
      "Cyclops predictions did not map to every treatment row.",
      "propensity_score_data_error"
    )
  }
  numeric_names <- grepl("^[0-9]+(\\.0+)?$", score_names)
  if (!all(numeric_names)) {
    abort_propensity_score(
      "Cyclops prediction row identifiers must be numeric.",
      "propensity_score_data_error"
    )
  }
  score_rows <- as.numeric(score_names)
  if (!setequal(score_rows, population$rowId)) {
    abort_propensity_score(
      "Cyclops prediction row identifiers do not match the population.",
      "propensity_score_data_error"
    )
  }
  mapped_scores <- scores[match(population$rowId, score_rows)]
  if (anyNA(mapped_scores) ||
        any(!is.finite(mapped_scores)) ||
        any(mapped_scores <= 0 | mapped_scores >= 1)) {
    abort_propensity_score(
      "Propensity scores must be finite and strictly between zero and one.",
      "propensity_score_data_error"
    )
  }

  data.frame(
    rowId = population$rowId,
    treatment = population$treatment,
    propensityScore = mapped_scores,
    check.names = FALSE
  )
}

#' Calculate odds-based preference scores
#'
#' @param ps_population Population with propensity scores.
#'
#' @return The population with `preferenceScore` appended.
calculate_preference_scores <- function(ps_population) {
  validate_ps_population(
    ps_population,
    required = c("rowId", "treatment", "propensityScore"),
    allowed = c("rowId", "treatment", "propensityScore")
  )
  if (any(ps_population$propensityScore <= 0 |
            ps_population$propensityScore >= 1)) {
    abort_propensity_score(
      "Propensity scores must be strictly between zero and one.",
      "propensity_score_data_error"
    )
  }
  prevalence <- mean(ps_population$treatment)
  if (!is.finite(prevalence) || prevalence <= 0 || prevalence >= 1) {
    abort_propensity_score(
      "Preference scores require non-empty target and comparator groups.",
      "propensity_score_data_error"
    )
  }

  preference_score <- stats::plogis(
    stats::qlogis(ps_population$propensityScore) - stats::qlogis(prevalence)
  )
  if (anyNA(preference_score) ||
        any(!is.finite(preference_score)) ||
        any(preference_score < 0 | preference_score > 1)) {
    abort_propensity_score(
      "Preference scores must be finite and between zero and one.",
      "propensity_score_data_error"
    )
  }
  ps_population$preferenceScore <- preference_score
  ps_population
}

#' Trim a population symmetrically by preference score
#'
#' @param ps_population Population with preference scores.
#' @param trim_fraction Inclusive lower trimming boundary.
#'
#' @return Retained population rows.
trim_propensity_score_population <- function(
    ps_population,
    trim_fraction = 0.05) {
  validate_ps_population(
    ps_population,
    required = c(
      "rowId",
      "treatment",
      "propensityScore",
      "preferenceScore"
    ),
    allowed = c(
      "rowId",
      "treatment",
      "propensityScore",
      "preferenceScore"
    )
  )
  if (!checkmate::test_number(
    trim_fraction,
    lower = 0,
    upper = 0.5,
    finite = TRUE
  ) || trim_fraction >= 0.5) {
    abort_propensity_score(
      "{.arg trim_fraction} must be finite and in [0, 0.5).",
      "propensity_score_argument_error"
    )
  }
  if (any(ps_population$preferenceScore < 0 |
            ps_population$preferenceScore > 1)) {
    abort_propensity_score(
      "Preference scores must be between zero and one.",
      "propensity_score_data_error"
    )
  }

  retained <- ps_population[
    ps_population$preferenceScore >= trim_fraction &
      ps_population$preferenceScore <= 1 - trim_fraction,
    ,
    drop = FALSE
  ]
  if (nrow(retained) == 0L || length(unique(retained$treatment)) != 2L) {
    abort_propensity_score(
      "Preference-score trimming must retain both treatment groups.",
      "propensity_score_data_error"
    )
  }
  rownames(retained) <- NULL
  retained
}

#' Match target subjects to comparator subjects on propensity score
#'
#' Uses CohortMethod 6.0.3's exported population-only matching API. Its
#' inspected default caliper is 0.2 standard deviations of the logit propensity
#' score. Input ordering supplies deterministic row-ID tie breaking.
#'
#' @param ps_population Trimmed population with preference scores.
#' @param config Study configuration.
#'
#' @return A 1:1 matched ATT population.
match_propensity_score_population <- function(ps_population, config) {
  validate_propensity_score_config(config)
  expected_columns <- c(
    "rowId",
    "treatment",
    "propensityScore",
    "preferenceScore"
  )
  validate_ps_population(
    ps_population,
    required = expected_columns,
    allowed = expected_columns
  )
  if (any(ps_population$propensityScore <= 0 |
            ps_population$propensityScore >= 1) ||
        length(unique(ps_population$treatment)) != 2L) {
    abort_propensity_score(
      "Matching requires valid scores and both treatment groups.",
      "propensity_score_data_error"
    )
  }

  ordered <- ps_population[
    order(ps_population$propensityScore, ps_population$rowId),
    ,
    drop = FALSE
  ]
  installed_defaults <- CohortMethod::createMatchOnPsArgs()
  default_caliper <- installed_defaults$caliper
  default_caliper_scale <- installed_defaults$caliperScale
  supported_defaults <- checkmate::test_number(
    default_caliper,
    lower = 0,
    finite = TRUE
  ) && identical(default_caliper_scale, "standardized logit")
  if (!supported_defaults) {
    abort_propensity_score(
      "The installed CohortMethod matching default is not supported.",
      "propensity_score_argument_error"
    )
  }
  match_args <- CohortMethod::createMatchOnPsArgs(
    caliper = default_caliper,
    caliperScale = default_caliper_scale,
    maxRatio = as.integer(config$propensity_score$matching_ratio),
    allowReverseMatch = FALSE,
    matchColumns = c(),
    matchCovariateIds = c()
  )
  matched <- tryCatch(
    CohortMethod::matchOnPs(
      population = ordered,
      matchOnPsArgs = match_args,
      cohortMethodData = NULL
    ),
    error = function(error) {
      abort_propensity_score(
        paste0("CohortMethod propensity-score matching failed: ", conditionMessage(error)),
        "propensity_score_data_error"
      )
    }
  )
  matched <- as.data.frame(matched, stringsAsFactors = FALSE)
  if (!"stratumId" %in% names(matched) || nrow(matched) == 0L) {
    abort_propensity_score(
      "Propensity-score matching did not produce matched pairs.",
      "propensity_score_data_error"
    )
  }

  pair_table <- table(matched$stratumId, matched$treatment)
  valid_pairs <- nrow(pair_table) > 0L &&
    ncol(pair_table) == 2L &&
    all(c("0", "1") %in% colnames(pair_table)) &&
    all(pair_table[, c("0", "1"), drop = FALSE] == 1L) &&
    !anyDuplicated(matched$rowId)
  if (!valid_pairs) {
    abort_propensity_score(
      "Every match must contain one target and one unreused comparator.",
      "propensity_score_data_error"
    )
  }

  target_rows <- matched[matched$treatment == 1, c("stratumId", "rowId")]
  target_rows <- target_rows[order(target_rows$rowId), , drop = FALSE]
  stratum_order <- target_rows$stratumId
  matched$matchId <- match(matched$stratumId, stratum_order)
  matched <- matched[order(matched$matchId, -matched$treatment), , drop = FALSE]
  result <- matched[, c(expected_columns, "matchId"), drop = FALSE]
  rownames(result) <- NULL
  validate_ps_population(
    result,
    required = c(expected_columns, "matchId"),
    allowed = c(expected_columns, "matchId")
  )
  result
}

#' Compute one standardized mean difference
#'
#' @param values Sparse observed covariate values.
#' @param row_ids Corresponding row IDs.
#' @param population Population containing row ID and treatment.
#' @param missing_means_zero Whether absent sparse values represent zero.
#'
#' @return One standardized mean difference.
compute_sparse_smd <- function(
  values,
  row_ids,
  population,
  missing_means_zero = NULL
) {
  required_columns <- c("rowId", "treatment")

  valid_population <-
    is.data.frame(population) &&
    all(required_columns %in% names(population)) &&
    nrow(population) > 0L &&
    !anyNA(population$rowId) &&
    !anyNA(population$treatment) &&
    anyDuplicated(population$rowId) == 0L &&
    all(population$treatment %in% c(0, 1)) &&
    all(c(0, 1) %in% population$treatment)

  if (!valid_population) {
    abort_propensity_score(
      "Population for balance calculation is invalid.",
      "propensity_score_data_error"
    )
  }

  valid_values <-
    is.numeric(values) &&
    is.numeric(row_ids) &&
    length(values) == length(row_ids) &&
    !anyNA(row_ids) &&
    all(is.finite(values))

  if (!valid_values) {
    abort_propensity_score(
      "Sparse covariate values are invalid.",
      "propensity_score_data_error"
    )
  }

  # CohortMethod uses the complete treatment-group size as
  # denominator. Absent sparse rows therefore contribute zero.
  invisible(missing_means_zero)

  positions <- match(row_ids, population$rowId)
  retained <- !is.na(positions)

  retained_row_ids <- row_ids[retained]
  retained_values <- as.numeric(values[retained])
  retained_treatment <- population$treatment[
    positions[retained]
  ]

  if (anyDuplicated(retained_row_ids) > 0L) {
    abort_propensity_score(
      "A covariate has duplicate values for one row ID.",
      "propensity_score_data_error"
    )
  }

  group_moments <- function(group_value) {
    group_size <- sum(
      population$treatment == group_value
    )

    group_values <- retained_values[
      retained_treatment == group_value
    ]

    sum_value <- sum(group_values)
    sum_squared <- sum(group_values^2)

    mean_value <- sum_value / group_size

    variance_value <- (
      sum_squared - sum_value^2 / group_size
    ) / group_size

    c(
      mean = mean_value,
      standard_deviation = sqrt(
        max(variance_value, 0)
      )
    )
  }

  target <- group_moments(1)
  comparator <- group_moments(0)

  difference <-
    target[["mean"]] - comparator[["mean"]]

  denominator <- sqrt(
    (
      target[["standard_deviation"]]^2 +
        comparator[["standard_deviation"]]^2
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
#' Compute aggregate covariate balance before and after matching
#'
#' Direct calculation is used because the saved FeatureExtraction archive does
#' not satisfy CohortMethod's required CohortMethodData object contract.
#'
#' @param covariate_data Loaded FeatureExtraction CovariateData.
#' @param population_before Population before adjustment.
#' @param population_after Matched population.
#' @param threshold Absolute SMD threshold.
#'
#' @return One aggregate row per covariate.
compute_propensity_score_balance <- function(
    covariate_data,
    population_before,
    population_after,
    threshold = 0.1) {
  if (!FeatureExtraction::isCovariateData(covariate_data)) {
    abort_propensity_score(
      "{.arg covariate_data} must be FeatureExtraction CovariateData.",
      "propensity_score_argument_error"
    )
  }
  required_population <- c("rowId", "treatment")
  validate_ps_population(population_before, required = required_population)
  validate_ps_population(population_after, required = required_population)
  if (!all(population_after$rowId %in% population_before$rowId) ||
    any(population_after$treatment != population_before$treatment[
      match(population_after$rowId, population_before$rowId)
    ])) {
    abort_propensity_score(
      "The adjusted population must be a treatment-consistent subset.",
      "propensity_score_data_error"
    )
  }
  if (!checkmate::test_number(threshold, lower = 0, finite = TRUE)) {
    abort_propensity_score(
      "{.arg threshold} must be one finite non-negative number.",
      "propensity_score_argument_error"
    )
  }

  covariates <- covariate_data$covariates |>
    dplyr::select("rowId", "covariateId", "covariateValue") |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)
  covariate_ref <- covariate_data$covariateRef |>
    dplyr::select("covariateId", "covariateName", "analysisId") |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)
  analysis_ref <- covariate_data$analysisRef |>
    dplyr::select("analysisId", "isBinary", "missingMeansZero") |>
    dplyr::collect() |>
    as.data.frame(stringsAsFactors = FALSE)
  if (nrow(covariates) == 0L ||
        nrow(covariate_ref) == 0L ||
        nrow(analysis_ref) == 0L ||
        anyDuplicated(covariate_ref$covariateId) ||
        anyDuplicated(analysis_ref$analysisId)) {
    abort_propensity_score(
      "Covariate and analysis references must be non-empty and unique.",
      "propensity_score_data_error"
    )
  }
  analysis_match <- match(covariate_ref$analysisId, analysis_ref$analysisId)
  if (anyNA(analysis_match)) {
    abort_propensity_score(
      "Every covariate must map to exactly one analysis reference.",
      "propensity_score_data_error"
    )
  }
  matched_analysis_ref <- analysis_ref[analysis_match, , drop = FALSE]
  is_binary <- as_ps_flag(matched_analysis_ref$isBinary)
  missing_means_zero <- as_ps_flag(
    matched_analysis_ref$missingMeansZero,
    allow_missing = TRUE
  )

  split_rows <- split(seq_len(nrow(covariates)), covariates$covariateId)
  calculate_for_population <- function(population) {
    vapply(covariate_ref$covariateId, function(covariate_id) {
      rows <- split_rows[[as.character(covariate_id)]]
      if (is.null(rows)) {
        rows <- integer()
      }
      ref_row <- match(covariate_id, covariate_ref$covariateId)
      compute_sparse_smd(
        values = covariates$covariateValue[rows],
        row_ids = covariates$rowId[rows],
        population = population,
        missing_means_zero = missing_means_zero[[ref_row]]
      )
    }, numeric(1L))
  }
  before_smd <- calculate_for_population(population_before)
  after_smd <- calculate_for_population(population_after)

  data.frame(
    covariateId = covariate_ref$covariateId,
    covariateName = as.character(covariate_ref$covariateName),
    analysisId = covariate_ref$analysisId,
    isBinary = is_binary,
    beforeSmd = before_smd,
    afterSmd = after_smd,
    balanced = !is.na(after_smd) & abs(after_smd) < threshold,
    check.names = FALSE
  )
}

#' Summarize propensity-score adjustment
#'
#' @param population_before Population before trimming.
#' @param population_trimmed Population after preference-score trimming.
#' @param population_matched Matched population.
#' @param balance Aggregate covariate balance.
#' @param threshold Absolute SMD threshold.
#'
#' @return Exactly one aggregate summary row.
summarize_propensity_score_adjustment <- function(
    population_before,
    population_trimmed,
    population_matched,
    balance,
    threshold) {
  validate_ps_population(
    population_before,
    required = c("rowId", "treatment", "propensityScore")
  )
  validate_ps_population(
    population_trimmed,
    required = c("rowId", "treatment", "propensityScore", "preferenceScore")
  )
  validate_ps_population(
    population_matched,
    required = c("rowId", "treatment", "matchId")
  )
  if (!all(population_trimmed$rowId %in% population_before$rowId) ||
        !all(population_matched$rowId %in% population_trimmed$rowId)) {
    abort_propensity_score(
      "Trimmed and matched populations must be nested subsets.",
      "propensity_score_data_error"
    )
  }
  if (!checkmate::test_number(threshold, lower = 0, finite = TRUE)) {
    abort_propensity_score(
      "{.arg threshold} must be one finite non-negative number.",
      "propensity_score_argument_error"
    )
  }
  expected_balance <- c(
    "covariateId",
    "covariateName",
    "analysisId",
    "isBinary",
    "beforeSmd",
    "afterSmd",
    "balanced"
  )
  if (!is.data.frame(balance) ||
        nrow(balance) == 0L ||
        !identical(names(balance), expected_balance)) {
    abort_propensity_score(
      "{.arg balance} must contain the required aggregate covariate columns.",
      "propensity_score_argument_error"
    )
  }

  pair_table <- table(population_matched$matchId, population_matched$treatment)
  if (ncol(pair_table) != 2L ||
        !all(c("0", "1") %in% colnames(pair_table)) ||
        any(pair_table[, c("0", "1"), drop = FALSE] != 1L)) {
    abort_propensity_score(
      "The matched population must contain one target and comparator per pair.",
      "propensity_score_data_error"
    )
  }
  before_abs <- abs(balance$beforeSmd)
  after_abs <- abs(balance$afterSmd)
  if (all(is.na(before_abs)) || all(is.na(after_abs))) {
    abort_propensity_score(
      "At least one covariate must have evaluable balance before and after.",
      "propensity_score_data_error"
    )
  }

  data.frame(
    target_before = as.integer(sum(population_before$treatment == 1)),
    comparator_before = as.integer(sum(population_before$treatment == 0)),
    target_after_trimming = as.integer(sum(population_trimmed$treatment == 1)),
    comparator_after_trimming = as.integer(sum(population_trimmed$treatment == 0)),
    matched_target_count = as.integer(sum(population_matched$treatment == 1)),
    matched_comparator_count = as.integer(sum(population_matched$treatment == 0)),
    matched_pair_count = as.integer(length(unique(population_matched$matchId))),
    covariate_count = as.integer(nrow(balance)),
    unbalanced_before_count = as.integer(sum(is.na(before_abs) |
                                               before_abs >= threshold)),
    unbalanced_after_count = as.integer(sum(is.na(after_abs) |
                                              after_abs >= threshold)),
    maximum_absolute_smd_before = max(before_abs, na.rm = TRUE),
    maximum_absolute_smd_after = max(after_abs, na.rm = TRUE),
    balance_threshold = as.numeric(threshold),
    check.names = FALSE
  )
}

#' Save the local matched propensity-score population
#'
#' @param population Matched population with no direct person IDs or dates.
#' @param path Destination below `data/derived`.
#'
#' @return Canonical output path.
save_local_ps_population <- function(
    population,
    path = here::here("data", "derived", "ps_matched_population.rds")) {
  expected_columns <- c(
    "rowId",
    "treatment",
    "propensityScore",
    "preferenceScore",
    "matchId"
  )
  validate_ps_population(
    population,
    required = expected_columns,
    allowed = expected_columns
  )
  destination <- validate_local_ps_path(
    path = path,
    allowed_directory = here::here("data", "derived")
  )
  fs::dir_create(dirname(destination))
  saveRDS(population, file = destination)
  destination
}

#' Validate an ordinary output path
#'
#' @param path Candidate output path.
#'
#' @return Absolute output path.
validate_ps_output_path <- function(path) {
  if (!checkmate::test_string(path, min.chars = 1L) || !nzchar(trimws(path))) {
    abort_propensity_score(
      "{.arg path} must be one non-empty character value.",
      "propensity_score_output_error"
    )
  }
  as.character(fs::path_abs(path))
}

#' Write the aggregate propensity-score summary
#'
#' @param summary Exactly one aggregate summary row.
#' @param path Destination CSV path.
#'
#' @return Output path.
write_propensity_score_summary <- function(
    summary,
    path = here::here(
      "results",
      "tables",
      "propensity_score_summary.csv"
    )) {
  expected_columns <- c(
    "target_before",
    "comparator_before",
    "target_after_trimming",
    "comparator_after_trimming",
    "matched_target_count",
    "matched_comparator_count",
    "matched_pair_count",
    "covariate_count",
    "unbalanced_before_count",
    "unbalanced_after_count",
    "maximum_absolute_smd_before",
    "maximum_absolute_smd_after",
    "balance_threshold"
  )
  if (!is.data.frame(summary) ||
        nrow(summary) != 1L ||
        !identical(names(summary), expected_columns) ||
        length(find_prohibited_ps_columns(summary)) > 0L) {
    abort_propensity_score(
      "{.arg summary} must be exactly one required aggregate row.",
      "propensity_score_output_error"
    )
  }
  count_columns <- expected_columns[seq_len(10L)]
  valid_counts <- vapply(summary[count_columns], function(value) {
    checkmate::test_integerish(
      value,
      lower = 0,
      len = 1L,
      any.missing = FALSE
    )
  }, logical(1L))
  valid_maxima <- all(vapply(
    summary[expected_columns[11:12]],
    function(value) {
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        value >= 0
    },
    logical(1L)
  ))
  valid_threshold <- checkmate::test_number(
    summary$balance_threshold,
    lower = 0,
    finite = TRUE
  )
  if (!all(valid_counts) || !valid_maxima || !valid_threshold) {
    abort_propensity_score(
      "Propensity-score summary values must be finite and non-negative.",
      "propensity_score_output_error"
    )
  }
  destination <- validate_ps_output_path(path)
  fs::dir_create(dirname(destination))
  readr::write_csv(summary, file = destination)
  destination
}

#' Write aggregate covariate balance
#'
#' @param balance Aggregate covariate-level balance.
#' @param path Destination CSV path.
#'
#' @return Output path.
write_covariate_balance <- function(
    balance,
    path = here::here(
      "results",
      "tables",
      "covariate_balance.csv"
    )) {
  expected_columns <- c(
    "covariateId",
    "covariateName",
    "analysisId",
    "isBinary",
    "beforeSmd",
    "afterSmd",
    "balanced"
  )
  prohibited <- if (is.data.frame(balance)) {
    normalized <- normalize_ps_names(names(balance))
    names(balance)[grepl(
      "rowid|person|subject|date|cohortstart|cohortend|covariatevalue",
      normalized
    )]
  } else {
    character()
  }
  if (!is.data.frame(balance) ||
        nrow(balance) == 0L ||
        !identical(names(balance), expected_columns) ||
        length(prohibited) > 0L) {
    abort_propensity_score(
      "{.arg balance} must contain only aggregate covariate-level columns.",
      "propensity_score_output_error"
    )
  }
  valid_balance <- is.numeric(balance$covariateId) &&
    !anyNA(balance$covariateId) &&
    !anyDuplicated(balance$covariateId) &&
    is.character(balance$covariateName) &&
    !anyNA(balance$covariateName) &&
    is.numeric(balance$analysisId) &&
    !anyNA(balance$analysisId) &&
    is.logical(balance$isBinary) &&
    !anyNA(balance$isBinary) &&
    is.numeric(balance$beforeSmd) &&
    is.numeric(balance$afterSmd) &&
    is.logical(balance$balanced) &&
    !anyNA(balance$balanced)
  if (!valid_balance) {
    abort_propensity_score(
      "Aggregate covariate balance contains invalid values.",
      "propensity_score_output_error"
    )
  }
  destination <- validate_ps_output_path(path)
  fs::dir_create(dirname(destination))
  readr::write_csv(balance, file = destination)
  destination
}

#' Plot aggregate propensity-score overlap
#'
#' @param population_before Population before matching.
#' @param population_after Population after matching.
#' @param path Destination PNG path.
#'
#' @return Output path.
plot_propensity_score_overlap <- function(
    population_before,
    population_after,
    path = here::here("figures", "propensity_score_overlap.png")) {
  validate_ps_population(
    population_before,
    required = c("rowId", "treatment", "propensityScore")
  )
  validate_ps_population(
    population_after,
    required = c("rowId", "treatment", "propensityScore")
  )
  plot_data <- rbind(
    data.frame(
      stage = "Before matching",
      treatment = population_before$treatment,
      propensityScore = population_before$propensityScore
    ),
    data.frame(
      stage = "After matching",
      treatment = population_after$treatment,
      propensityScore = population_after$propensityScore
    )
  )
  plot_data$treatment <- factor(
    plot_data$treatment,
    levels = c(0, 1),
    labels = c("Comparator", "Target")
  )
  plot_data$stage <- factor(
    plot_data$stage,
    levels = c("Before matching", "After matching")
  )

  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[["propensityScore"]], # nolint: object_usage_linter
      fill = .data[["treatment"]] # nolint: object_usage_linter
    )
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = 30,
      position = "identity",
      alpha = 0.4
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["stage"]]), ncol = 1) + # nolint: object_usage_linter
    ggplot2::labs(
      x = "Propensity score",
      y = "Density",
      fill = "Treatment group"
    ) +
    ggplot2::theme_minimal()
  destination <- validate_ps_output_path(path)
  fs::dir_create(dirname(destination))
  ggplot2::ggsave(
    filename = destination,
    plot = plot,
    width = 8,
    height = 6,
    dpi = 300
  )
  destination
}

#' Plot absolute covariate balance before and after matching
#'
#' @param balance Aggregate covariate-level balance.
#' @param path Destination PNG path.
#'
#' @return Output path.
plot_covariate_balance <- function(
    balance,
    path = here::here("figures", "covariate_balance.png")) {
  expected_columns <- c(
    "covariateId",
    "covariateName",
    "analysisId",
    "isBinary",
    "beforeSmd",
    "afterSmd",
    "balanced"
  )
  if (!is.data.frame(balance) ||
        nrow(balance) == 0L ||
        !identical(names(balance), expected_columns)) {
    abort_propensity_score(
      "{.arg balance} must contain aggregate covariate balance.",
      "propensity_score_argument_error"
    )
  }
  plot_data <- rbind(
    data.frame(
      covariateName = balance$covariateName,
      stage = "Before matching",
      absoluteSmd = abs(balance$beforeSmd)
    ),
    data.frame(
      covariateName = balance$covariateName,
      stage = "After matching",
      absoluteSmd = abs(balance$afterSmd)
    )
  )
  finite_data <- plot_data[is.finite(plot_data$absoluteSmd), , drop = FALSE]
  if (nrow(finite_data) == 0L) {
    abort_propensity_score(
      "Covariate balance must contain at least one finite SMD.",
      "propensity_score_data_error"
    )
  }
  covariate_order <- stats::aggregate(
    absoluteSmd ~ covariateName,
    data = finite_data,
    FUN = max
  )
  covariate_order <- covariate_order[order(covariate_order$absoluteSmd), ]
  finite_data$covariateName <- factor(
    finite_data$covariateName,
    levels = covariate_order$covariateName
  )

  plot <- ggplot2::ggplot(
    finite_data,
    ggplot2::aes(
      x = .data[["absoluteSmd"]], # nolint: object_usage_linter
      y = .data[["covariateName"]], # nolint: object_usage_linter
      color = .data[["stage"]] # nolint: object_usage_linter
    )
  ) +
    ggplot2::geom_point(alpha = 0.7, size = 1.5) +
    ggplot2::geom_vline(
      xintercept = 0.1,
      linetype = "dashed",
      color = "firebrick"
    ) +
    ggplot2::labs(
      x = "Absolute standardized mean difference",
      y = NULL,
      color = "Adjustment stage"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6))
  destination <- validate_ps_output_path(path)
  fs::dir_create(dirname(destination))
  plot_height <- min(16, max(6, nrow(covariate_order) * 0.04))
  ggplot2::ggsave(
    filename = destination,
    plot = plot,
    width = 9,
    height = plot_height,
    dpi = 300,
    limitsize = FALSE
  )
  destination
}
