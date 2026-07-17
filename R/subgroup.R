#' Validate the prespecified subgroup configuration
#'
#' @param config Validated study configuration.
#'
#' @return The configured age cutoff as an integer.
validate_subgroup_config <- function(config) {
  if (!is.list(config) ||
        is.null(config$subgroups) ||
        !is.list(config$subgroups)) {
    stop(
      "`config` must contain a subgroup configuration.",
      call. = FALSE
    )
  }

  subgroup_names <- names(config$subgroups)

  if (!setequal(
    subgroup_names,
    c("sex", "age_cutoff")
  )) {
    stop(
      "`config$subgroups` must contain only `sex` and `age_cutoff`.",
      call. = FALSE
    )
  }

  if (!isTRUE(config$subgroups$sex)) {
    stop(
      "The prespecified sex subgroup analysis must be enabled.",
      call. = FALSE
    )
  }

  age_cutoff <- config$subgroups$age_cutoff

  valid_age_cutoff <-
    is.numeric(age_cutoff) &&
    length(age_cutoff) == 1L &&
    !is.na(age_cutoff) &&
    is.finite(age_cutoff) &&
    age_cutoff == round(age_cutoff) &&
    age_cutoff > 0 &&
    age_cutoff <= .Machine$integer.max

  if (!valid_age_cutoff) {
    stop(
      "`config$subgroups$age_cutoff` must be one positive whole number.",
      call. = FALSE
    )
  }

  as.integer(age_cutoff)
}

#' Build exact age and sex subgroup assignments
#'
#' Uses FeatureExtraction analysis ID 1 for gender and analysis ID 2 for age.
#' Text matching is intentionally not used because unrelated covariates can
#' contain words such as sex or age.
#'
#' @param covariate_rows Sparse FeatureExtraction covariate rows.
#' @param covariate_ref FeatureExtraction covariate reference rows.
#' @param analysis_ref FeatureExtraction analysis reference rows.
#' @param age_cutoff Prespecified age cutoff.
#'
#' @return Internal row-level subgroup assignments.
build_subgroup_demographics <- function(
    covariate_rows,
    covariate_ref,
    analysis_ref,
    age_cutoff = 65L) {
  age_cutoff <- validate_subgroup_config(
    list(
      subgroups = list(
        sex = TRUE,
        age_cutoff = age_cutoff
      )
    )
  )

  required_covariate_columns <- c(
    "rowId",
    "covariateId",
    "covariateValue"
  )

  required_reference_columns <- c(
    "covariateId",
    "covariateName",
    "analysisId"
  )

  required_analysis_columns <- c(
    "analysisId",
    "analysisName"
  )

  if (!is.data.frame(covariate_rows) ||
    !all(
      required_covariate_columns %in%
        names(covariate_rows)
    )) {
    stop(
      "`covariate_rows` is missing required FeatureExtraction fields.",
      call. = FALSE
    )
  }

  if (!is.data.frame(covariate_ref) ||
    !all(
      required_reference_columns %in%
        names(covariate_ref)
    )) {
    stop(
      "`covariate_ref` is missing required reference fields.",
      call. = FALSE
    )
  }

  if (!is.data.frame(analysis_ref) ||
    !all(
      required_analysis_columns %in%
        names(analysis_ref)
    )) {
    stop(
      "`analysis_ref` is missing required analysis fields.",
      call. = FALSE
    )
  }

  if (nrow(covariate_rows) == 0L ||
        nrow(covariate_ref) == 0L ||
        nrow(analysis_ref) == 0L) {
    stop(
      "Demographic source tables must not be empty.",
      call. = FALSE
    )
  }

  if (anyDuplicated(
    paste(
      covariate_rows$rowId,
      covariate_rows$covariateId,
      sep = ":"
    )
  ) > 0L) {
    stop(
      "Sparse covariate rows must be unique by row and covariate.",
      call. = FALSE
    )
  }

  if (anyDuplicated(covariate_ref$covariateId) > 0L ||
        anyDuplicated(analysis_ref$analysisId) > 0L) {
    stop(
      "Demographic reference identifiers must be unique.",
      call. = FALSE
    )
  }

  numeric_covariate_values <-
    is.numeric(covariate_rows$rowId) &&
    is.numeric(covariate_rows$covariateId) &&
    is.numeric(covariate_rows$covariateValue) &&
    !anyNA(covariate_rows$rowId) &&
    !anyNA(covariate_rows$covariateId) &&
    !anyNA(covariate_rows$covariateValue) &&
    all(is.finite(covariate_rows$rowId)) &&
    all(is.finite(covariate_rows$covariateId)) &&
    all(is.finite(covariate_rows$covariateValue))

  if (!numeric_covariate_values) {
    stop(
      "Sparse demographic covariates must contain finite numeric values.",
      call. = FALSE
    )
  }

  gender_analysis <- analysis_ref[
    analysis_ref$analysisId == 1,
    required_analysis_columns,
    drop = FALSE
  ]

  age_analysis <- analysis_ref[
    analysis_ref$analysisId == 2,
    required_analysis_columns,
    drop = FALSE
  ]

  valid_gender_analysis <-
    nrow(gender_analysis) == 1L &&
    identical(
      as.character(gender_analysis$analysisName),
      "DemographicsGender"
    )

  valid_age_analysis <-
    nrow(age_analysis) == 1L &&
    identical(
      as.character(age_analysis$analysisName),
      "DemographicsAge"
    )

  if (!valid_gender_analysis || !valid_age_analysis) {
    stop(
      paste(
        "Expected analysis ID 1 to be DemographicsGender",
        "and analysis ID 2 to be DemographicsAge."
      ),
      call. = FALSE
    )
  }

  gender_ref <- covariate_ref[
    covariate_ref$analysisId == 1,
    required_reference_columns,
    drop = FALSE
  ]

  age_ref <- covariate_ref[
    covariate_ref$analysisId == 2,
    required_reference_columns,
    drop = FALSE
  ]

  gender_names <- as.character(
    gender_ref$covariateName
  )

  valid_gender_names <- grepl(
    "^gender[[:space:]]*=[[:space:]]*[^[:space:]].*$",
    gender_names,
    ignore.case = TRUE
  )

  valid_age_reference <-
    nrow(age_ref) == 1L &&
    identical(
      tolower(
        trimws(
          as.character(age_ref$covariateName)
        )
      ),
      "age in years"
    )

  if (nrow(gender_ref) < 2L ||
        any(!valid_gender_names) ||
        !valid_age_reference) {
    stop(
      "Gender or age covariate references have an unexpected structure.",
      call. = FALSE
    )
  }

  age_rows <- covariate_rows[
    covariate_rows$covariateId %in%
      age_ref$covariateId,
    required_covariate_columns,
    drop = FALSE
  ]

  gender_rows <- covariate_rows[
    covariate_rows$covariateId %in%
      gender_ref$covariateId &
      covariate_rows$covariateValue != 0,
    required_covariate_columns,
    drop = FALSE
  ]

  if (nrow(age_rows) == 0L ||
        anyDuplicated(age_rows$rowId) > 0L) {
    stop(
      "Age must occur exactly once for every demographic row.",
      call. = FALSE
    )
  }

  valid_age_values <-
    all(age_rows$covariateValue >= 0) &&
    all(age_rows$covariateValue <= 130) &&
    all(
      age_rows$covariateValue ==
        round(age_rows$covariateValue)
    )

  if (!valid_age_values) {
    stop(
      "Age values must be whole years between 0 and 130.",
      call. = FALSE
    )
  }

  if (nrow(gender_rows) == 0L ||
        anyDuplicated(gender_rows$rowId) > 0L ||
        any(gender_rows$covariateValue != 1)) {
    stop(
      "Gender must contain one active binary category per demographic row.",
      call. = FALSE
    )
  }

  if (!setequal(
    age_rows$rowId,
    gender_rows$rowId
  )) {
    stop(
      "Age and gender must map to the same row identifiers.",
      call. = FALSE
    )
  }

  gender_position <- match(
    gender_rows$covariateId,
    gender_ref$covariateId
  )

  if (anyNA(gender_position)) {
    stop(
      "Every gender value must map to a gender reference.",
      call. = FALSE
    )
  }

  sex <- sub(
    "^gender[[:space:]]*=[[:space:]]*",
    "",
    as.character(
      gender_ref$covariateName[
        gender_position
      ]
    ),
    ignore.case = TRUE
  )

  sex <- toupper(trimws(sex))

  if (any(!nzchar(sex))) {
    stop(
      "Gender category labels must not be empty.",
      call. = FALSE
    )
  }

  age <- as.integer(age_rows$covariateValue)

  result <- data.frame(
    rowId = as.numeric(age_rows$rowId),
    sex = sex[
      match(
        age_rows$rowId,
        gender_rows$rowId
      )
    ],
    age = age,
    ageGroup = ifelse(
      age < age_cutoff,
      "<65",
      ">=65"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  result <- result[
    order(result$rowId),
    ,
    drop = FALSE
  ]

  row.names(result) <- NULL

  result
}

#' Extract exact subgroup demographics from FeatureExtraction data
#'
#' @param covariate_data FeatureExtraction CovariateData.
#' @param age_cutoff Prespecified age cutoff.
#'
#' @return Internal row-level age and sex assignments.
extract_subgroup_demographics <- function(
    covariate_data,
    age_cutoff = 65L) {
  if (!FeatureExtraction::isCovariateData(
    covariate_data
  )) {
    stop(
      "`covariate_data` must be FeatureExtraction CovariateData.",
      call. = FALSE
    )
  }

  covariate_rows <- covariate_data$covariates |>
    dplyr::select(
      "rowId",
      "covariateId",
      "covariateValue"
    ) |>
    dplyr::collect() |>
    as.data.frame(
      stringsAsFactors = FALSE
    )

  covariate_ref <- collect_covariate_reference( # nolint: object_usage_linter.
    covariate_data,
    "covariateRef"
  )

  analysis_ref <- collect_covariate_reference( # nolint: object_usage_linter.
    covariate_data,
    "analysisRef"
  )

  build_subgroup_demographics(
    covariate_rows = covariate_rows,
    covariate_ref = covariate_ref,
    analysis_ref = analysis_ref,
    age_cutoff = age_cutoff
  )
}

#' Attach subgroup assignments to matched outcome rows
#'
#' @param analysis_population Matched binary-outcome population.
#' @param matched_population Matched propensity-score population.
#' @param demographics Exact row-level subgroup assignments.
#'
#' @return Internal subgroup analysis rows.
attach_subgroup_demographics <- function(
    analysis_population,
    matched_population,
    demographics) {
  summarize_matched_outcomes( # nolint: object_usage_linter.
    analysis_population = analysis_population,
    risk_window_start_days = 1L,
    risk_window_end_days = 1L
  )

  validate_matched_population( # nolint: object_usage_linter.
    matched_population
  )

  demographic_columns <- c(
    "rowId",
    "sex",
    "age",
    "ageGroup"
  )

  if (!is.data.frame(demographics) ||
    !identical(
      names(demographics),
      demographic_columns
    )) {
    stop(
      paste0(
        "`demographics` must contain exactly: ",
        paste(demographic_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(demographics) == 0L ||
        anyNA(demographics$rowId) ||
        anyDuplicated(demographics$rowId) > 0L) {
    stop(
      "Demographic row identifiers must be complete and unique.",
      call. = FALSE
    )
  }

  if (!is.character(demographics$sex) ||
        anyNA(demographics$sex) ||
        any(!nzchar(demographics$sex))) {
    stop(
      "Demographic sex categories must be non-empty strings.",
      call. = FALSE
    )
  }

  valid_age <-
    is.numeric(demographics$age) &&
    !anyNA(demographics$age) &&
    all(is.finite(demographics$age)) &&
    all(demographics$age >= 0) &&
    all(demographics$age <= 130) &&
    all(
      demographics$age ==
        round(demographics$age)
    )

  if (!valid_age) {
    stop(
      "Demographic ages must be whole years between 0 and 130.",
      call. = FALSE
    )
  }

  if (!is.character(demographics$ageGroup) ||
    anyNA(demographics$ageGroup) ||
    any(
      !demographics$ageGroup %in%
        c("<65", ">=65")
    )) {
    stop(
      "Age groups must contain only `<65` and `>=65`.",
      call. = FALSE
    )
  }

  analysis_position <- match(
    matched_population$rowId,
    analysis_population$rowId
  )

  demographic_position <- match(
    matched_population$rowId,
    demographics$rowId
  )

  if (anyNA(analysis_position) ||
        anyNA(demographic_position)) {
    stop(
      "Every matched row must have outcome and demographic data.",
      call. = FALSE
    )
  }

  linked_analysis <- analysis_population[
    analysis_position,
    ,
    drop = FALSE
  ]

  linked_demographics <- demographics[
    demographic_position,
    ,
    drop = FALSE
  ]

  treatment_agrees <- identical(
    as.numeric(linked_analysis$treatment),
    as.numeric(matched_population$treatment)
  )

  match_agrees <- identical(
    as.character(linked_analysis$matchId),
    as.character(matched_population$matchId)
  )

  if (!treatment_agrees || !match_agrees) {
    stop(
      "Outcome and matched-population assignments disagree.",
      call. = FALSE
    )
  }

  result <- data.frame(
    rowId = matched_population$rowId,
    treatment = matched_population$treatment,
    matchId = matched_population$matchId,
    outcome = linked_analysis$outcome,
    propensityScore =
      matched_population$propensityScore,
    sex = linked_demographics$sex,
    age = as.integer(
      linked_demographics$age
    ),
    ageGroup =
      linked_demographics$ageGroup,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  row.names(result) <- NULL

  result
}

#' Summarize membership in prespecified subgroups
#'
#' @param subgroup_population Internal matched subgroup analysis rows.
#'
#' @return Aggregate membership and matched-cluster diagnostics.
summarize_subgroup_membership <- function(
    subgroup_population) {
  required_columns <- c(
    "rowId",
    "treatment",
    "matchId",
    "outcome",
    "propensityScore",
    "sex",
    "age",
    "ageGroup"
  )

  if (!is.data.frame(subgroup_population) ||
    !identical(
      names(subgroup_population),
      required_columns
    )) {
    stop(
      paste0(
        "`subgroup_population` must contain exactly: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(subgroup_population) == 0L ||
    anyNA(subgroup_population$rowId) ||
    anyDuplicated(
      subgroup_population$rowId
    ) > 0L) {
    stop(
      "Subgroup population rows must be complete and unique.",
      call. = FALSE
    )
  }

  if (anyNA(subgroup_population$treatment) ||
    any(
      !subgroup_population$treatment %in%
        c(0, 1)
    )) {
    stop(
      "Subgroup treatment must contain only 0 and 1.",
      call. = FALSE
    )
  }

  make_summary <- function(
      subgroup_type,
      subgroup_level,
      selected) {
    selected_rows <- subgroup_population[
      selected,
      ,
      drop = FALSE
    ]

    represented_cluster_count <- length(
      unique(selected_rows$matchId)
    )

    if (nrow(selected_rows) == 0L) {
      complete_pair_count <- 0L
      singleton_cluster_count <- 0L
    } else {
      cluster_rows <- split(
        seq_len(nrow(selected_rows)),
        as.character(selected_rows$matchId),
        drop = TRUE
      )

      complete_pair <- vapply(
        cluster_rows,
        function(indices) {
          length(indices) == 2L &&
            identical(
              sort(
                as.integer(
                  selected_rows$treatment[
                    indices
                  ]
                )
              ),
              c(0L, 1L)
            )
        },
        logical(1)
      )

      complete_pair_count <- as.integer(
        sum(complete_pair)
      )

      singleton_cluster_count <- as.integer(
        sum(
          vapply(
            cluster_rows,
            length,
            integer(1)
          ) == 1L
        )
      )
    }

    data.frame(
      subgroupType = subgroup_type,
      subgroupLevel = subgroup_level,
      subjectCount = as.integer(
        nrow(selected_rows)
      ),
      targetSubjectCount = as.integer(
        sum(selected_rows$treatment == 1)
      ),
      comparatorSubjectCount = as.integer(
        sum(selected_rows$treatment == 0)
      ),
      representedClusterCount = as.integer(
        represented_cluster_count
      ),
      completePairCount = complete_pair_count,
      singletonClusterCount =
        singleton_cluster_count,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  sex_levels <- sort(
    unique(subgroup_population$sex)
  )

  sex_rows <- lapply(
    sex_levels,
    function(sex_level) {
      make_summary(
        subgroup_type = "sex",
        subgroup_level = sex_level,
        selected =
          subgroup_population$sex == sex_level
      )
    }
  )

  age_levels <- c("<65", ">=65")

  age_rows <- lapply(
    age_levels,
    function(age_level) {
      make_summary(
        subgroup_type = "age",
        subgroup_level = age_level,
        selected =
          subgroup_population$ageGroup ==
          age_level
      )
    }
  )

  result <- do.call(
    rbind,
    c(sex_rows, age_rows)
  )

  row.names(result) <- NULL

  result
}

#' Validate a matched subgroup analysis population
#'
#' @param subgroup_population Internal matched subgroup analysis rows.
#'
#' @return The input data frame, invisibly.
validate_subgroup_analysis_population <- function(
    subgroup_population) {
  required_columns <- c(
    "rowId",
    "treatment",
    "matchId",
    "outcome",
    "propensityScore",
    "sex",
    "age",
    "ageGroup"
  )

  if (!is.data.frame(subgroup_population) ||
    !identical(
      names(subgroup_population),
      required_columns
    )) {
    stop(
      paste0(
        "`subgroup_population` must contain exactly: ",
        paste(required_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(subgroup_population) == 0L) {
    stop(
      "`subgroup_population` must not be empty.",
      call. = FALSE
    )
  }

  if (anyNA(subgroup_population$rowId) ||
        anyDuplicated(subgroup_population$rowId) > 0L) {
    stop(
      "Subgroup row identifiers must be complete and unique.",
      call. = FALSE
    )
  }

  treatment <- subgroup_population$treatment
  outcome <- subgroup_population$outcome
  propensity_score <- subgroup_population$propensityScore

  if (!is.numeric(treatment) ||
        anyNA(treatment) ||
        any(!treatment %in% c(0, 1))) {
    stop(
      "Subgroup treatment must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.numeric(outcome) ||
        anyNA(outcome) ||
        any(!outcome %in% c(0, 1))) {
    stop(
      "Subgroup outcome must contain only numeric 0 and 1.",
      call. = FALSE
    )
  }

  if (!is.numeric(propensity_score) ||
        anyNA(propensity_score) ||
        any(!is.finite(propensity_score)) ||
        any(propensity_score < 0 | propensity_score > 1)) {
    stop(
      "Subgroup propensity scores must be finite values between 0 and 1.",
      call. = FALSE
    )
  }

  if (anyNA(subgroup_population$matchId)) {
    stop(
      "Subgroup match identifiers must not be missing.",
      call. = FALSE
    )
  }

  if (!is.character(subgroup_population$sex) ||
        anyNA(subgroup_population$sex) ||
        any(!nzchar(subgroup_population$sex))) {
    stop(
      "Subgroup sex categories must be non-empty strings.",
      call. = FALSE
    )
  }

  age <- subgroup_population$age

  valid_age <- is.numeric(age) &&
    !anyNA(age) &&
    all(is.finite(age)) &&
    all(age >= 0) &&
    all(age <= 130) &&
    all(age == round(age))

  if (!valid_age) {
    stop(
      "Subgroup ages must be whole years between 0 and 130.",
      call. = FALSE
    )
  }

  if (!is.character(subgroup_population$ageGroup) ||
    anyNA(subgroup_population$ageGroup) ||
    any(
      !subgroup_population$ageGroup %in%
        c("<65", ">=65")
    )) {
    stop(
      "Subgroup age groups must contain only `<65` and `>=65`.",
      call. = FALSE
    )
  }

  invisible(subgroup_population)
}

#' Calculate aggregate subgroup diagnostics
#'
#' @param subgroup_rows Selected subgroup rows.
#'
#' @return Aggregate sample, cluster, event, and overlap diagnostics.
calculate_subgroup_diagnostics <- function(
    subgroup_rows) {
  treatment <- subgroup_rows$treatment
  outcome <- subgroup_rows$outcome

  target_rows <- treatment == 1
  comparator_rows <- treatment == 0

  target_count <- as.integer(
    sum(target_rows)
  )

  comparator_count <- as.integer(
    sum(comparator_rows)
  )

  if (nrow(subgroup_rows) == 0L) {
    represented_cluster_count <- 0L
    complete_pair_count <- 0L
    singleton_cluster_count <- 0L
  } else {
    cluster_rows <- split(
      seq_len(nrow(subgroup_rows)),
      as.character(subgroup_rows$matchId),
      drop = TRUE
    )

    represented_cluster_count <- as.integer(
      length(cluster_rows)
    )

    complete_pairs <- vapply(
      cluster_rows,
      function(indices) {
        length(indices) == 2L &&
          identical(
            sort(
              as.integer(
                treatment[indices]
              )
            ),
            c(0L, 1L)
          )
      },
      logical(1)
    )

    cluster_sizes <- vapply(
      cluster_rows,
      length,
      integer(1)
    )

    complete_pair_count <- as.integer(
      sum(complete_pairs)
    )

    singleton_cluster_count <- as.integer(
      sum(cluster_sizes == 1L)
    )
  }

  if (target_count > 0L &&
        comparator_count > 0L) {
    target_range <- range(
      subgroup_rows$propensityScore[
        target_rows
      ]
    )

    comparator_range <- range(
      subgroup_rows$propensityScore[
        comparator_rows
      ]
    )

    overlap_lower <- max(
      target_range[[1L]],
      comparator_range[[1L]]
    )

    overlap_upper <- min(
      target_range[[2L]],
      comparator_range[[2L]]
    )

    overlap_available <- overlap_lower <=
      overlap_upper

    overlap_width <- if (
      overlap_available
    ) {
      overlap_upper - overlap_lower
    } else {
      0
    }
  } else {
    overlap_lower <- NA_real_
    overlap_upper <- NA_real_
    overlap_width <- NA_real_
    overlap_available <- FALSE
  }

  list(
    subject_count = as.integer(
      nrow(subgroup_rows)
    ),
    event_count = as.integer(
      sum(outcome)
    ),
    target_subject_count =
      target_count,
    target_event_count = as.integer(
      sum(outcome[target_rows])
    ),
    comparator_subject_count =
      comparator_count,
    comparator_event_count = as.integer(
      sum(outcome[comparator_rows])
    ),
    represented_cluster_count =
      represented_cluster_count,
    complete_pair_count =
      complete_pair_count,
    singleton_cluster_count =
      singleton_cluster_count,
    overlap_lower =
      overlap_lower,
    overlap_upper =
      overlap_upper,
    overlap_width =
      overlap_width,
    overlap_available =
      overlap_available
  )
}

#' Create one aggregate subgroup result
#'
#' @param subgroup_type Subgroup variable.
#' @param subgroup_level Subgroup category.
#' @param diagnostics Aggregate subgroup diagnostics.
#' @param status Estimability status.
#' @param reason Estimability explanation.
#' @param confidence_level Two-sided confidence level.
#' @param estimate Odds-ratio estimate.
#' @param ci_lower Lower confidence limit.
#' @param ci_upper Upper confidence limit.
#' @param log_odds_ratio Log odds ratio.
#' @param standard_error Cluster-robust standard error.
#' @param model_converged Whether the logistic model converged.
#' @param zero_cell_detected Whether a treatment-by-outcome cell was zero.
#'
#' @return One aggregate subgroup result row.
create_subgroup_result <- function(
    subgroup_type,
    subgroup_level,
    diagnostics,
    status,
    reason,
    confidence_level,
    estimate = NA_real_,
    ci_lower = NA_real_,
    ci_upper = NA_real_,
    log_odds_ratio = NA_real_,
    standard_error = NA_real_,
    model_converged = FALSE,
    zero_cell_detected = FALSE) {
  data.frame(
    subgroupType = subgroup_type,
    subgroupLevel = subgroup_level,
    estimabilityStatus = status,
    estimabilityReason = reason,
    effectMeasure = "odds ratio",
    estimate = estimate,
    ciLower = ci_lower,
    ciUpper = ci_upper,
    confidenceLevel = confidence_level,
    logOddsRatio = log_odds_ratio,
    standardError = standard_error,
    subjectCount =
      diagnostics$subject_count,
    eventCount =
      diagnostics$event_count,
    targetSubjectCount =
      diagnostics$target_subject_count,
    targetEventCount =
      diagnostics$target_event_count,
    comparatorSubjectCount =
      diagnostics$comparator_subject_count,
    comparatorEventCount =
      diagnostics$comparator_event_count,
    representedClusterCount =
      diagnostics$represented_cluster_count,
    completePairCount =
      diagnostics$complete_pair_count,
    singletonClusterCount =
      diagnostics$singleton_cluster_count,
    overlapLower =
      diagnostics$overlap_lower,
    overlapUpper =
      diagnostics$overlap_upper,
    overlapWidth =
      diagnostics$overlap_width,
    overlapAvailable =
      diagnostics$overlap_available,
    varianceEstimator =
      "matched-set cluster-robust CR1",
    modelConverged =
      model_converged,
    zeroCellDetected =
      zero_cell_detected,
    interpretation = paste(
      "Descriptive adjusted observational association",
      "under the stated design assumptions."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Fit one prespecified subgroup outcome model
#'
#' Original matched-set identifiers are retained for cluster-robust variance.
#' A subgroup may therefore contain singleton clusters when matched subjects
#' belong to different subgroup categories.
#'
#' @param subgroup_population Internal matched subgroup analysis rows.
#' @param subgroup_type Either `sex` or `age`.
#' @param subgroup_level Prespecified subgroup category.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#' @param confidence_level Two-sided confidence level.
#'
#' @return One aggregate estimable or non-estimable subgroup result.
fit_subgroup_outcome_model <- function(
    subgroup_population,
    subgroup_type,
    subgroup_level,
    risk_window_start_days,
    risk_window_end_days,
    confidence_level = 0.95) {
  validate_subgroup_analysis_population(
    subgroup_population
  )

  validate_risk_window( # nolint: object_usage_linter.
    risk_window_start_days,
    risk_window_end_days
  )

  valid_type <- is.character(subgroup_type) &&
    length(subgroup_type) == 1L &&
    !is.na(subgroup_type) &&
    subgroup_type %in% c("sex", "age")

  valid_level <- is.character(subgroup_level) &&
    length(subgroup_level) == 1L &&
    !is.na(subgroup_level) &&
    nzchar(subgroup_level)

  valid_confidence <- is.numeric(confidence_level) &&
    length(confidence_level) == 1L &&
    !is.na(confidence_level) &&
    is.finite(confidence_level) &&
    confidence_level > 0 &&
    confidence_level < 1

  if (!valid_type) {
    stop(
      "`subgroup_type` must be either `sex` or `age`.",
      call. = FALSE
    )
  }

  if (!valid_level) {
    stop(
      "`subgroup_level` must be one non-empty string.",
      call. = FALSE
    )
  }

  if (!valid_confidence) {
    stop(
      "`confidence_level` must be strictly between 0 and 1.",
      call. = FALSE
    )
  }

  selected <- if (
    subgroup_type == "sex"
  ) {
    subgroup_population$sex ==
      subgroup_level
  } else {
    subgroup_population$ageGroup ==
      subgroup_level
  }

  subgroup_rows <- subgroup_population[
    selected,
    ,
    drop = FALSE
  ]

  diagnostics <- calculate_subgroup_diagnostics(
    subgroup_rows
  )

  not_estimable <- function(
      reason,
      zero_cell = FALSE) {
    create_subgroup_result(
      subgroup_type = subgroup_type,
      subgroup_level = subgroup_level,
      diagnostics = diagnostics,
      status = "NOT_ESTIMABLE",
      reason = reason,
      confidence_level =
        confidence_level,
      zero_cell_detected =
        zero_cell
    )
  }

  if (diagnostics$subject_count == 0L) {
    return(
      not_estimable(
        "No subjects are available in this prespecified subgroup."
      )
    )
  }

  if (
    diagnostics$target_subject_count == 0L ||
      diagnostics$comparator_subject_count == 0L
  ) {
    return(
      not_estimable(
        "Both treatment groups are required for subgroup estimation."
      )
    )
  }

  if (
    diagnostics$represented_cluster_count <
      2L
  ) {
    return(
      not_estimable(
        "At least two matched-set clusters are required."
      )
    )
  }

  treatment <- as.numeric(
    subgroup_rows$treatment
  )

  outcome <- as.numeric(
    subgroup_rows$outcome
  )

  outcome_table <- table(
    factor(
      treatment,
      levels = c(0, 1)
    ),
    factor(
      outcome,
      levels = c(0, 1)
    )
  )

  zero_cell <- any(
    outcome_table == 0L
  )

  if (zero_cell) {
    return(
      not_estimable(
        paste(
          "The subgroup treatment-by-outcome table contains a zero cell;",
          "the logistic odds-ratio estimate is separated."
        ),
        zero_cell = TRUE
      )
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
    return(
      not_estimable(
        paste0(
          "The subgroup logistic model emitted a warning: ",
          paste(model_warnings, collapse = "; ")
        )
      )
    )
  }

  if (!isTRUE(fit$converged)) {
    return(
      not_estimable(
        "The subgroup logistic model did not converge."
      )
    )
  }

  coefficients <- fit$coefficients

  if (
    length(coefficients) != 2L ||
      anyNA(coefficients) ||
      any(!is.finite(coefficients)) ||
      fit$rank != ncol(design)
  ) {
    return(
      not_estimable(
        "The subgroup logistic model was rank deficient or invalid."
      )
    )
  }

  score_residual <- outcome -
    fit$fitted.values

  covariance_error <- NULL

  covariance <- tryCatch(
    compute_cluster_robust_vcov( # nolint: object_usage_linter.
      design = design,
      score_residual =
        score_residual,
      weights = fit$weights,
      cluster =
        subgroup_rows$matchId
    ),
    error = function(error) {
      covariance_error <<-
        conditionMessage(error)

      NULL
    }
  )

  if (is.null(covariance)) {
    return(
      not_estimable(
        paste0(
          "Cluster-robust variance estimation failed: ",
          covariance_error
        )
      )
    )
  }

  standard_error <- sqrt(
    covariance[2L, 2L]
  )

  if (
    !is.finite(standard_error) ||
      standard_error <= 0
  ) {
    return(
      not_estimable(
        "The subgroup treatment standard error is invalid."
      )
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

  create_subgroup_result(
    subgroup_type = subgroup_type,
    subgroup_level = subgroup_level,
    diagnostics = diagnostics,
    status = "ESTIMABLE",
    reason = "Model estimation completed successfully.",
    confidence_level =
      confidence_level,
    estimate = exp(log_odds_ratio),
    ci_lower = exp(
      confidence_limits[[1L]]
    ),
    ci_upper = exp(
      confidence_limits[[2L]]
    ),
    log_odds_ratio =
      log_odds_ratio,
    standard_error =
      standard_error,
    model_converged = TRUE,
    zero_cell_detected = FALSE
  )
}

#' Run all prespecified subgroup outcome models
#'
#' @param subgroup_population Internal matched subgroup analysis rows.
#' @param risk_window_start_days First included risk-window day.
#' @param risk_window_end_days Last included risk-window day.
#' @param confidence_level Two-sided confidence level.
#'
#' @return Aggregate subgroup outcome results.
run_prespecified_subgroup_models <- function(
    subgroup_population,
    risk_window_start_days,
    risk_window_end_days,
    confidence_level = 0.95) {
  validate_subgroup_analysis_population(
    subgroup_population
  )

  sex_levels <- sort(
    unique(
      subgroup_population$sex
    )
  )

  sex_results <- lapply(
    sex_levels,
    function(sex_level) {
      fit_subgroup_outcome_model(
        subgroup_population =
          subgroup_population,
        subgroup_type = "sex",
        subgroup_level =
          sex_level,
        risk_window_start_days =
          risk_window_start_days,
        risk_window_end_days =
          risk_window_end_days,
        confidence_level =
          confidence_level
      )
    }
  )

  age_levels <- c(
    "<65",
    ">=65"
  )

  age_results <- lapply(
    age_levels,
    function(age_level) {
      fit_subgroup_outcome_model(
        subgroup_population =
          subgroup_population,
        subgroup_type = "age",
        subgroup_level =
          age_level,
        risk_window_start_days =
          risk_window_start_days,
        risk_window_end_days =
          risk_window_end_days,
        confidence_level =
          confidence_level
      )
    }
  )

  result <- do.call(
    rbind,
    c(
      sex_results,
      age_results
    )
  )

  prohibited_fields <- c(
    "rowId",
    "matchId",
    "subjectId",
    "personId",
    "row_id",
    "match_id",
    "subject_id",
    "person_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  if (
    length(
      intersect(
        names(result),
        prohibited_fields
      )
    ) > 0L
  ) {
    stop(
      "Aggregate subgroup output contains person-level fields.",
      call. = FALSE
    )
  }

  row.names(result) <- NULL

  result
}

#' Summarize post-match covariate balance by subgroup
#'
#' Balance is evaluated separately within every observed sex category and the
#' two prespecified age categories. The selected matched rows are supplied as
#' both the before and after populations because this diagnostic describes
#' balance within the final matched subgroup rather than a new adjustment.
#'
#' @param covariate_data Loaded FeatureExtraction CovariateData.
#' @param subgroup_population Internal matched subgroup analysis rows.
#' @param threshold Absolute standardized-mean-difference threshold.
#' @param balance_function Optional balance-calculation function for testing.
#'
#' @return Aggregate subgroup balance diagnostics.
calculate_subgroup_balance_summary <- function(
    covariate_data,
    subgroup_population,
    threshold = 0.1,
    balance_function = NULL) {
  validate_subgroup_analysis_population(
    subgroup_population
  )

  valid_threshold <- is.numeric(threshold) &&
    length(threshold) == 1L &&
    !is.na(threshold) &&
    is.finite(threshold) &&
    threshold >= 0

  if (!valid_threshold) {
    stop(
      "`threshold` must be one finite non-negative number.",
      call. = FALSE
    )
  }

  if (is.null(balance_function)) {
    balance_function <-
      compute_propensity_score_balance # nolint: object_usage_linter.
  }

  if (!is.function(balance_function)) {
    stop(
      "`balance_function` must be a function.",
      call. = FALSE
    )
  }

  sex_levels <- sort(
    unique(
      subgroup_population$sex
    )
  )

  subgroup_plan <- rbind(
    data.frame(
      subgroupType = rep(
        "sex",
        length(sex_levels)
      ),
      subgroupLevel = sex_levels,
      stringsAsFactors = FALSE
    ),
    data.frame(
      subgroupType = c(
        "age",
        "age"
      ),
      subgroupLevel = c(
        "<65",
        ">=65"
      ),
      stringsAsFactors = FALSE
    )
  )

  summarize_level <- function(
      subgroup_type,
      subgroup_level) {
    selected <- if (
      subgroup_type == "sex"
    ) {
      subgroup_population$sex ==
        subgroup_level
    } else {
      subgroup_population$ageGroup ==
        subgroup_level
    }

    subgroup_rows <- subgroup_population[
      selected,
      ,
      drop = FALSE
    ]

    target_count <- as.integer(
      sum(
        subgroup_rows$treatment == 1
      )
    )

    comparator_count <- as.integer(
      sum(
        subgroup_rows$treatment == 0
      )
    )

    create_result <- function(
        status,
        reason,
        covariate_count = 0L,
        evaluable_count = 0L,
        unbalanced_count = 0L,
        maximum_absolute_smd = NA_real_) {
      data.frame(
        subgroupType =
          subgroup_type,
        subgroupLevel =
          subgroup_level,
        balanceStatus =
          status,
        balanceReason =
          reason,
        balanceSubjectCount = as.integer(
          nrow(subgroup_rows)
        ),
        balanceTargetCount =
          target_count,
        balanceComparatorCount =
          comparator_count,
        covariateCount = as.integer(
          covariate_count
        ),
        evaluableCovariateCount = as.integer(
          evaluable_count
        ),
        unbalancedCovariateCount = as.integer(
          unbalanced_count
        ),
        maximumAbsoluteSmd =
          maximum_absolute_smd,
        balanceThreshold = as.numeric(
          threshold
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    if (nrow(subgroup_rows) == 0L) {
      return(
        create_result(
          status = "NOT_EVALUABLE",
          reason = paste(
            "No subjects are available",
            "for subgroup balance assessment."
          )
        )
      )
    }

    if (
      target_count == 0L ||
        comparator_count == 0L
    ) {
      return(
        create_result(
          status = "NOT_EVALUABLE",
          reason = paste(
            "Both treatment groups are required",
            "for subgroup balance assessment."
          )
        )
      )
    }

    balance_population <- subgroup_rows[
      ,
      c(
        "rowId",
        "treatment"
      ),
      drop = FALSE
    ]

    balance <- balance_function(
      covariate_data = covariate_data,
      population_before =
        balance_population,
      population_after =
        balance_population,
      threshold = threshold
    )

    required_balance_columns <- c(
      "covariateId",
      "afterSmd",
      "balanced"
    )

    if (!is.data.frame(balance) ||
      nrow(balance) == 0L ||
      !all(
        required_balance_columns %in%
          names(balance)
      )) {
      stop(
        paste(
          "Subgroup balance calculation returned",
          "an invalid aggregate result."
        ),
        call. = FALSE
      )
    }

    after_absolute <- abs(
      balance$afterSmd
    )

    evaluable <- !is.na(
      after_absolute
    )

    unbalanced <- is.na(
      after_absolute
    ) |
      after_absolute >= threshold

    maximum_absolute_smd <- if (
      any(evaluable)
    ) {
      max(
        after_absolute[evaluable]
      )
    } else {
      NA_real_
    }

    create_result(
      status = "EVALUABLE",
      reason = paste(
        "Post-match subgroup balance",
        "calculated successfully."
      ),
      covariate_count =
        nrow(balance),
      evaluable_count =
        sum(evaluable),
      unbalanced_count =
        sum(unbalanced),
      maximum_absolute_smd =
        maximum_absolute_smd
    )
  }

  results <- lapply(
    seq_len(
      nrow(subgroup_plan)
    ),
    function(row_number) {
      summarize_level(
        subgroup_type =
          subgroup_plan$subgroupType[
            row_number
          ],
        subgroup_level =
          subgroup_plan$subgroupLevel[
            row_number
          ]
      )
    }
  )

  output <- do.call(
    rbind,
    results
  )

  prohibited_fields <- c(
    "rowId",
    "matchId",
    "subjectId",
    "personId",
    "row_id",
    "match_id",
    "subject_id",
    "person_id",
    "cohort_start_date",
    "cohort_end_date"
  )

  if (
    length(
      intersect(
        names(output),
        prohibited_fields
      )
    ) > 0L
  ) {
    stop(
      paste(
        "Aggregate subgroup balance output",
        "contains person-level fields."
      ),
      call. = FALSE
    )
  }

  row.names(output) <- NULL

  output
}
