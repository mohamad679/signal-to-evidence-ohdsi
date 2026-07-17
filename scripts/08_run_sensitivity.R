sensitivity_env <- new.env(
  parent = globalenv()
)

source_files <- c(
  "config.R",
  "database.R",
  "cohorts.R",
  "covariates.R",
  "propensity_score.R",
  "outcome.R",
  "sensitivity.R"
)

for (source_file in source_files) {
  sys.source(
    here::here(
      "R",
      source_file
    ),
    envir = sensitivity_env
  )
}

rm(
  source_file,
  source_files
)

build_sensitivity_population <- function(
    cohort_rows,
    covariate_row_ids) {
  expected_columns <- c(
    "row_id",
    "cohort_definition_id",
    "subject_id"
  )

  if (
    !is.data.frame(cohort_rows) ||
      !identical(
        names(cohort_rows),
        expected_columns
      ) ||
      nrow(cohort_rows) == 0L
  ) {
    stop(
      paste0(
        "`cohort_rows` must be non-empty and contain exactly: ",
        paste(
          expected_columns,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  numeric_columns <- c(
    "row_id",
    "cohort_definition_id",
    "subject_id"
  )

  valid_numeric <- vapply(
    cohort_rows[numeric_columns],
    function(values) {
      is.numeric(values) &&
        !anyNA(values) &&
        all(is.finite(values)) &&
        all(values == round(values))
    },
    logical(1L)
  )

  if (
    !all(valid_numeric) ||
      any(cohort_rows$row_id <= 0) ||
      anyDuplicated(cohort_rows$row_id) > 0L ||
      anyDuplicated(cohort_rows$subject_id) > 0L
  ) {
    stop(
      "Cohort rows must contain unique positive row and subject IDs.",
      call. = FALSE
    )
  }

  cohort_ids <- cohort_rows$cohort_definition_id

  if (
    any(!cohort_ids %in% c(1, 2)) ||
      !all(c(1, 2) %in% cohort_ids)
  ) {
    stop(
      "Sensitivity cohorts must contain non-empty cohort IDs 1 and 2.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(covariate_row_ids) ||
      length(covariate_row_ids) == 0L ||
      anyNA(covariate_row_ids) ||
      any(!is.finite(covariate_row_ids)) ||
      any(covariate_row_ids != round(covariate_row_ids)) ||
      anyDuplicated(covariate_row_ids) > 0L ||
      !setequal(
        cohort_rows$row_id,
        covariate_row_ids
      )
  ) {
    stop(
      "Cohort and covariate row IDs must agree one-to-one.",
      call. = FALSE
    )
  }

  population <- data.frame(
    rowId = as.numeric(
      cohort_rows$row_id
    ),
    treatment = as.integer(
      cohort_ids == 1
    ),
    check.names = FALSE
  )

  sensitivity_env$validate_ps_population(
    population,
    required = c(
      "rowId",
      "treatment"
    ),
    allowed = c(
      "rowId",
      "treatment"
    )
  )

  population
}

query_sensitivity_population <- function(
    connection,
    cohort_tables,
    covariate_data,
    table_name) {
  table_name <- sensitivity_env$validate_covariate_table_name(
    table_name
  )

  if (!FeatureExtraction::isCovariateData(
    covariate_data
  )) {
    stop(
      "`covariate_data` must be FeatureExtraction CovariateData.",
      call. = FALSE
    )
  }

  sensitivity_env$create_feature_extraction_cohort_table(
    connection = connection,
    cohort_tables = cohort_tables,
    table_name = table_name
  )

  on.exit(
    sensitivity_env$drop_feature_extraction_cohort_table(
      connection = connection,
      table_name = table_name
    ),
    add = TRUE
  )

  cohort_rows <- DatabaseConnector::querySql(
    connection = connection,
    sql = paste(
      "SELECT row_id, cohort_definition_id, subject_id",
      paste0(
        "FROM main.",
        table_name
      ),
      "ORDER BY row_id"
    ),
    snakeCaseToCamelCase = FALSE
  )

  cohort_rows <- as.data.frame(
    cohort_rows,
    stringsAsFactors = FALSE
  )

  covariate_rows <- covariate_data$covariates |>
    dplyr::distinct(
      .data[["rowId"]] # nolint: object_usage_linter.
    ) |>
    dplyr::collect() |>
    as.data.frame(
      stringsAsFactors = FALSE
    )

  if (!identical(
    names(covariate_rows),
    "rowId"
  )) {
    stop(
      "Covariate data did not expose the required row IDs.",
      call. = FALSE
    )
  }

  build_sensitivity_population(
    cohort_rows = cohort_rows,
    covariate_row_ids =
      covariate_rows$rowId
  )
}

build_sensitivity_linkage <- function(
    feature_rows,
    population) {
  sensitivity_env$validate_ps_population(
    population,
    required = c(
      "rowId",
      "treatment"
    )
  )

  normalized <- sensitivity_env$normalize_feature_treatment_rows(
    feature_rows
  )

  positions <- match(
    population$rowId,
    normalized$row_id
  )

  if (anyNA(positions)) {
    stop(
      "Every sensitivity row must occur in the feature table.",
      call. = FALSE
    )
  }

  linked <- normalized[
    positions,
    ,
    drop = FALSE
  ]

  if (!identical(
    as.numeric(linked$treatment),
    as.numeric(population$treatment)
  )) {
    stop(
      "Sensitivity treatment assignments disagree with cohort linkage.",
      call. = FALSE
    )
  }

  normalize_date <- function(values) {
    if (inherits(values, "Date")) {
      result <- values
    } else if (is.character(values)) {
      result <- as.Date(values)
    } else {
      result <- rep(
        as.Date(NA),
        length(values)
      )
    }

    if (anyNA(result)) {
      stop(
        "Sensitivity cohort linkage contains invalid dates.",
        call. = FALSE
      )
    }

    result
  }

  result <- data.frame(
    rowId = population$rowId,
    subjectId = linked$subject_id,
    cohortStartDate = normalize_date(
      linked$cohort_start_date
    ),
    cohortEndDate = normalize_date(
      linked$cohort_end_date
    ),
    treatment = population$treatment,
    check.names = FALSE
  )

  sensitivity_env$validate_treatment_population(
    result
  )

  result
}

query_sensitivity_linkage <- function(
    connection,
    cohort_tables,
    population,
    table_name) {
  table_name <- sensitivity_env$validate_covariate_table_name(
    table_name
  )

  sensitivity_env$create_feature_extraction_cohort_table(
    connection = connection,
    cohort_tables = cohort_tables,
    table_name = table_name
  )

  on.exit(
    sensitivity_env$drop_feature_extraction_cohort_table(
      connection = connection,
      table_name = table_name
    ),
    add = TRUE
  )

  feature_rows <- DatabaseConnector::querySql(
    connection = connection,
    sql = paste(
      "SELECT",
      "row_id,",
      "subject_id,",
      "cohort_start_date,",
      "cohort_end_date,",
      "cohort_definition_id",
      paste0(
        "FROM main.",
        table_name
      )
    ),
    snakeCaseToCamelCase = FALSE
  )

  feature_rows <- as.data.frame(
    feature_rows,
    stringsAsFactors = FALSE
  )

  build_sensitivity_linkage(
    feature_rows = feature_rows,
    population = population
  )
}

summarize_sensitivity_balance <- function(
    balance,
    threshold) {
  expected_columns <- c(
    "covariateId",
    "covariateName",
    "analysisId",
    "isBinary",
    "beforeSmd",
    "afterSmd",
    "balanced"
  )

  if (
    !is.data.frame(balance) ||
      nrow(balance) == 0L ||
      !identical(
        names(balance),
        expected_columns
      )
  ) {
    stop(
      "`balance` must contain the aggregate SMD contract.",
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

  absolute_smd <- abs(
    balance$afterSmd
  )

  finite_smd <- absolute_smd[
    is.finite(absolute_smd)
  ]

  maximum_smd <- if (length(finite_smd) == 0L) {
    NA_real_
  } else {
    max(finite_smd)
  }

  data.frame(
    residualImbalanceCount = as.integer(
      sum(
        !is.finite(absolute_smd) |
          absolute_smd >= threshold
      )
    ),
    maximumAbsoluteSmd = maximum_smd,
    balanceThreshold = threshold,
    check.names = FALSE
  )
}

format_sensitivity_result <- function(
    scenario,
    model_result,
    population_before,
    population_trimmed,
    population_adjusted,
    balance,
    threshold) {
  if (
    !is.data.frame(scenario) ||
      nrow(scenario) != 1L ||
      !is.data.frame(model_result) ||
      nrow(model_result) != 1L
  ) {
    stop(
      "Scenario and model result must each contain exactly one row.",
      call. = FALSE
    )
  }

  required_model <- c(
    "effectMeasure",
    "estimate",
    "ciLower",
    "ciUpper",
    "confidenceLevel",
    "logOddsRatio",
    "standardError",
    "subjectCount",
    "eventCount",
    "targetSubjectCount",
    "targetEventCount",
    "comparatorSubjectCount",
    "comparatorEventCount",
    "varianceEstimator",
    "modelConverged",
    "zeroCellDetected",
    "interpretation"
  )

  if (!all(required_model %in% names(model_result))) {
    stop(
      "The model result is missing required aggregate fields.",
      call. = FALSE
    )
  }

  for (population in list(
    population_before,
    population_trimmed,
    population_adjusted
  )) {
    sensitivity_env$validate_ps_population(
      population,
      required = c(
        "rowId",
        "treatment"
      )
    )
  }

  balance_summary <- summarize_sensitivity_balance(
    balance = balance,
    threshold = threshold
  )

  effective_size <- if (
    "effectiveSampleSize" %in%
      names(model_result)
  ) {
    model_result$effectiveSampleSize
  } else {
    nrow(population_adjusted)
  }

  data.frame(
    scenarioOrder = scenario$scenarioOrder,
    scenarioId = scenario$scenarioId,
    changedParameter = scenario$changedParameter,
    isPrimary = scenario$isPrimary,
    adjustmentMethod = scenario$adjustmentMethod,
    estimand = scenario$estimand,
    washoutDays = scenario$washoutDays,
    riskWindowStartDays = scenario$riskWindowStartDays,
    riskWindowEndDays = scenario$riskWindowEndDays,
    trimFraction = scenario$trimFraction,
    preAdjustmentCount = as.integer(
      nrow(population_before)
    ),
    postTrimCount = as.integer(
      nrow(population_trimmed)
    ),
    adjustedSubjectCount = as.integer(
      nrow(population_adjusted)
    ),
    adjustedTargetCount = as.integer(
      sum(population_adjusted$treatment == 1)
    ),
    adjustedComparatorCount = as.integer(
      sum(population_adjusted$treatment == 0)
    ),
    effectiveSampleSize = effective_size,
    residualImbalanceCount =
      balance_summary$residualImbalanceCount,
    maximumAbsoluteSmd =
      balance_summary$maximumAbsoluteSmd,
    balanceThreshold =
      balance_summary$balanceThreshold,
    effectMeasure = model_result$effectMeasure,
    estimate = model_result$estimate,
    ciLower = model_result$ciLower,
    ciUpper = model_result$ciUpper,
    confidenceLevel = model_result$confidenceLevel,
    logOddsRatio = model_result$logOddsRatio,
    standardError = model_result$standardError,
    subjectCount = model_result$subjectCount,
    eventCount = model_result$eventCount,
    targetSubjectCount =
      model_result$targetSubjectCount,
    targetEventCount =
      model_result$targetEventCount,
    comparatorSubjectCount =
      model_result$comparatorSubjectCount,
    comparatorEventCount =
      model_result$comparatorEventCount,
    varianceEstimator =
      model_result$varianceEstimator,
    modelConverged = model_result$modelConverged,
    zeroCellDetected = model_result$zeroCellDetected,
    interpretation = model_result$interpretation,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

validate_sensitivity_output <- function(output) {
  expected_scenarios <- c(
    "primary",
    "risk_1_14",
    "risk_1_60",
    "weighting_att",
    "washout_365"
  )

  if (
    !is.data.frame(output) ||
      nrow(output) != 5L ||
      !identical(
        output$scenarioId,
        expected_scenarios
      ) ||
      anyDuplicated(output$scenarioId) > 0L
  ) {
    stop(
      "Sensitivity output must contain the five ordered scenarios.",
      call. = FALSE
    )
  }

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

  if (length(intersect(
    names(output),
    prohibited_fields
  )) > 0L) {
    stop(
      "Sensitivity output contains person-level fields.",
      call. = FALSE
    )
  }

  effect_values <- c(
    output$estimate,
    output$ciLower,
    output$ciUpper,
    output$standardError
  )

  if (
    anyNA(effect_values) ||
      any(!is.finite(effect_values)) ||
      any(effect_values <= 0) ||
      any(output$ciLower > output$estimate) ||
      any(output$estimate > output$ciUpper) ||
      !all(output$modelConverged) ||
      any(output$zeroCellDetected)
  ) {
    stop(
      "Sensitivity effect estimates must be finite and estimable.",
      call. = FALSE
    )
  }

  invisible(output)
}

write_sensitivity_summary <- function(
    output,
    path) {
  validate_sensitivity_output(output)

  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
  ) {
    stop(
      "`path` must be one non-empty character value.",
      call. = FALSE
    )
  }

  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  utils::write.csv(
    output,
    file = path,
    row.names = FALSE,
    na = ""
  )

  invisible(path)
}

create_adjustment_objects <- function(
    connection,
    cohort_tables,
    covariate_data,
    config,
    tag) {
  table_name <- paste0(
    "study_sensitivity_ps_",
    tag,
    "_",
    Sys.getpid()
  )

  population <- query_sensitivity_population(
    connection = connection,
    cohort_tables = cohort_tables,
    covariate_data = covariate_data,
    table_name = table_name
  )

  model_data <- sensitivity_env$create_propensity_score_model_data(
    covariate_data = covariate_data,
    population = population
  )

  scores <- sensitivity_env$estimate_propensity_scores(
    model_data = model_data,
    population = population,
    config = config
  )

  scores <- sensitivity_env$calculate_preference_scores(
    scores
  )

  trimmed <- sensitivity_env$trim_propensity_score_population(
    ps_population = scores,
    trim_fraction =
      config$propensity_score$trim_fraction
  )

  matched <- sensitivity_env$match_propensity_score_population(
    ps_population = trimmed,
    config = config
  )

  weighted <- sensitivity_env$calculate_att_weights(
    trimmed
  )

  matching_balance <- sensitivity_env$compute_propensity_score_balance(
    covariate_data = covariate_data,
    population_before = population,
    population_after = matched,
    threshold =
      config$balance$absolute_smd_threshold
  )

  weighting_balance <- sensitivity_env$compute_weighted_covariate_balance(
    covariate_data = covariate_data,
    population_before = population,
    weighted_population = weighted,
    threshold =
      config$balance$absolute_smd_threshold
  )

  list(
    population = population,
    trimmed = trimmed,
    matched = matched,
    weighted = weighted,
    matching_balance = matching_balance,
    weighting_balance = weighting_balance
  )
}

fit_matched_sensitivity <- function(
    connection,
    cohort_tables,
    matched_population,
    scenario) {
  feature_table_name <- paste0(
    "study_sensitivity_outcome_",
    scenario$scenarioOrder,
    "_",
    Sys.getpid()
  )

  analysis_population <-
    sensitivity_env$build_matched_outcome_from_tables(
      connection = connection,
      cohort_tables = cohort_tables,
      matched_population = matched_population,
      feature_table_name = feature_table_name,
      risk_window_start_days =
      scenario$riskWindowStartDays,
      risk_window_end_days =
      scenario$riskWindowEndDays
    )

  sensitivity_env$fit_matched_outcome_model(
    analysis_population = analysis_population,
    risk_window_start_days =
      scenario$riskWindowStartDays,
    risk_window_end_days =
      scenario$riskWindowEndDays
  )
}

fit_weighted_sensitivity <- function(
    connection,
    cohort_tables,
    weighted_population,
    scenario) {
  table_name <- paste0(
    "study_sensitivity_weight_",
    Sys.getpid()
  )

  treatment_population <- query_sensitivity_linkage(
    connection = connection,
    cohort_tables = cohort_tables,
    population = weighted_population,
    table_name = table_name
  )

  outcome_cohort <- sensitivity_env$load_outcome_from_cohort_table(
    connection = connection,
    outcome_table = cohort_tables$outcome
  )

  analysis_population <-
    sensitivity_env$build_weighted_outcome_population(
      weighted_population = weighted_population,
      treatment_population = treatment_population,
      outcome_cohort = outcome_cohort,
      risk_window_start_days =
      scenario$riskWindowStartDays,
      risk_window_end_days =
      scenario$riskWindowEndDays
    )

  sensitivity_env$fit_weighted_outcome_model(
    analysis_population = analysis_population,
    risk_window_start_days =
      scenario$riskWindowStartDays,
    risk_window_end_days =
      scenario$riskWindowEndDays
  )
}

run_sensitivity_analysis <- function(
    baseline_path = here::here(
      "data",
      "derived",
      "baseline_covariates.rds"
    ),
    summary_path = here::here(
      "results",
      "tables",
      "sensitivity_analysis_summary.csv"
    )) {
  config <- sensitivity_env$read_study_config()

  sensitivity_env$validate_propensity_score_config(
    config
  )

  sensitivity_env$validate_sensitivity_config(
    config
  )

  scenarios <- sensitivity_env$create_sensitivity_scenarios(
    config
  )

  dataset_name <- if (
    is.null(config$database$dataset_name)
  ) {
    "GiBleed"
  } else {
    config$database$dataset_name
  }

  database_file <- sensitivity_env$get_eunomia_database_path(
    dataset_name
  )

  if (!file.exists(database_file)) {
    stop(
      "The project-local Eunomia database is unavailable.",
      call. = FALSE
    )
  }

  connection_details <-
    sensitivity_env$create_eunomia_connection_details(
      dataset_name = dataset_name,
      database_file = database_file
    )

  connection <- suppressMessages(
    DatabaseConnector::connect(
      connection_details
    )
  )

  on.exit(
    sensitivity_env$disconnect_safely(
      connection
    ),
    add = TRUE
  )

  sensitivity_env$validate_required_omop_tables(
    connection = connection,
    database_schema =
      config$project$database_schema
  )

  primary_cohorts <- sensitivity_env$create_study_cohorts(
    connection = connection,
    config = config
  )

  sensitivity_env$validate_outcome_cohort_tables(
    primary_cohorts
  )

  primary_covariates <-
    sensitivity_env$load_baseline_covariates(
      path = baseline_path
    )

  on.exit(
    if (
      !is.null(primary_covariates) &&
        Andromeda::isValidAndromeda(
          primary_covariates
        )
    ) {
      Andromeda::close(
        primary_covariates
      )
    },
    add = TRUE
  )

  primary_adjustment <- create_adjustment_objects(
    connection = connection,
    cohort_tables = primary_cohorts,
    covariate_data = primary_covariates,
    config = config,
    tag = "180"
  )

  threshold <- config$balance$absolute_smd_threshold
  results <- vector(
    "list",
    nrow(scenarios)
  )

  matched_ids <- c(
    "primary",
    "risk_1_14",
    "risk_1_60"
  )

  for (scenario_id in matched_ids) {
    position <- match(
      scenario_id,
      scenarios$scenarioId
    )

    scenario <- scenarios[
      position,
      ,
      drop = FALSE
    ]

    model_result <- fit_matched_sensitivity(
      connection = connection,
      cohort_tables = primary_cohorts,
      matched_population =
        primary_adjustment$matched,
      scenario = scenario
    )

    results[[position]] <- format_sensitivity_result(
      scenario = scenario,
      model_result = model_result,
      population_before =
        primary_adjustment$population,
      population_trimmed =
        primary_adjustment$trimmed,
      population_adjusted =
        primary_adjustment$matched,
      balance =
        primary_adjustment$matching_balance,
      threshold = threshold
    )
  }

  weighting_position <- match(
    "weighting_att",
    scenarios$scenarioId
  )

  weighting_scenario <- scenarios[
    weighting_position,
    ,
    drop = FALSE
  ]

  weighting_model <- fit_weighted_sensitivity(
    connection = connection,
    cohort_tables = primary_cohorts,
    weighted_population =
      primary_adjustment$weighted,
    scenario = weighting_scenario
  )

  results[[weighting_position]] <-
    format_sensitivity_result(
      scenario = weighting_scenario,
      model_result = weighting_model,
      population_before =
      primary_adjustment$population,
      population_trimmed =
      primary_adjustment$trimmed,
      population_adjusted =
      primary_adjustment$weighted,
      balance =
      primary_adjustment$weighting_balance,
      threshold = threshold
    )

  Andromeda::close(
    primary_covariates
  )

  primary_covariates <- NULL

  washout_config <- config
  washout_config$design$washout_days <- 365L

  washout_cohorts <- sensitivity_env$create_study_cohorts(
    connection = connection,
    config = washout_config
  )

  sensitivity_env$validate_outcome_cohort_tables(
    washout_cohorts
  )

  covariate_settings <-
    sensitivity_env$create_baseline_covariate_settings()

  washout_covariates <- suppressMessages(
    sensitivity_env$extract_study_baseline_covariates(
      connection = connection,
      connection_details = connection_details,
      cdm_database_schema =
        washout_config$project$database_schema,
      cohort_tables = washout_cohorts,
      covariate_settings = covariate_settings,
      cohort_table = paste0(
        "study_sensitivity_cov_365_",
        Sys.getpid()
      )
    )
  )

  on.exit(
    if (
      !is.null(washout_covariates) &&
        Andromeda::isValidAndromeda(
          washout_covariates
        )
    ) {
      Andromeda::close(
        washout_covariates
      )
    },
    add = TRUE
  )

  washout_adjustment <- create_adjustment_objects(
    connection = connection,
    cohort_tables = washout_cohorts,
    covariate_data = washout_covariates,
    config = washout_config,
    tag = "365"
  )

  washout_position <- match(
    "washout_365",
    scenarios$scenarioId
  )

  washout_scenario <- scenarios[
    washout_position,
    ,
    drop = FALSE
  ]

  washout_model <- fit_matched_sensitivity(
    connection = connection,
    cohort_tables = washout_cohorts,
    matched_population =
      washout_adjustment$matched,
    scenario = washout_scenario
  )

  results[[washout_position]] <-
    format_sensitivity_result(
      scenario = washout_scenario,
      model_result = washout_model,
      population_before =
      washout_adjustment$population,
      population_trimmed =
      washout_adjustment$trimmed,
      population_adjusted =
      washout_adjustment$matched,
      balance =
      washout_adjustment$matching_balance,
      threshold = threshold
    )

  output <- do.call(
    rbind,
    results
  )

  output <- output[
    order(output$scenarioOrder),
    ,
    drop = FALSE
  ]

  row.names(output) <- NULL

  validate_sensitivity_output(
    output
  )

  write_sensitivity_summary(
    output = output,
    path = summary_path
  )

  invisible(output)
}

if (sys.nframe() == 0L) {
  result <- run_sensitivity_analysis()

  print(result)
}
