ps_test_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = ps_test_env)
sys.source(here::here("R", "database.R"), envir = ps_test_env)
sys.source(here::here("R", "cohorts.R"), envir = ps_test_env)
sys.source(here::here("R", "covariates.R"), envir = ps_test_env)
sys.source(here::here("R", "propensity_score.R"), envir = ps_test_env)

local_ps_database_file <- function() {
  database_file <- ps_test_env$get_eunomia_database_path()
  testthat::skip_if_not(
    isTRUE(fs::is_file(database_file)),
    "The project-local Eunomia database is not available."
  )
  database_file
}

open_local_ps_database <- function() {
  connection_details <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite",
    server = local_ps_database_file()
  )
  suppressMessages(DatabaseConnector::connect(connection_details))
}

create_synthetic_ps_covariate_data <- function() {
  covariate_data <- FeatureExtraction::createEmptyCovariateData(
    cohortIds = c(1L, 2L),
    aggregated = FALSE,
    temporal = FALSE
  )
  covariate_data$covariates <- data.frame(
    rowId = c(1, 2, 3, 5, 1:8, 3, 7, 1, 1),
    covariateId = c(rep(10, 4), rep(20, 8), rep(30, 2), 50, 60),
    covariateValue = c(rep(1, 4), 1:4, 2:5, 1, 1, 1, 2),
    check.names = FALSE
  )
  covariate_data$covariateRef <- data.frame(
    covariateId = c(10, 20, 30, 40, 50, 60),
    covariateName = c(
      "Known binary",
      "Known continuous",
      "Disappearing binary",
      "Absent binary",
      "Target-only binary",
      "Target-only observed continuous"
    ),
    analysisId = c(100, 200, 100, 100, 100, 300),
    conceptId = rep(NA_real_, 6),
    check.names = FALSE
  )
  covariate_data$analysisRef <- data.frame(
    analysisId = c(300, 100, 200),
    analysisName = c(
      "Observed continuous example",
      "Binary example",
      "Continuous example"
    ),
    domainId = rep("Test", 3),
    startDay = rep(-30, 3),
    endDay = rep(-1, 3),
    isBinary = c("N", "Y", "N"),
    missingMeansZero = c("N", "Y", "N"),
    check.names = FALSE
  )
  covariate_data
}

synthetic_balance <- function() {
  data.frame(
    covariateId = c(10, 20),
    covariateName = c("Known binary", "Known continuous"),
    analysisId = c(100, 200),
    isBinary = c(TRUE, FALSE),
    beforeSmd = c(0.2, -0.15),
    afterSmd = c(0.05, -0.08),
    balanced = c(TRUE, TRUE),
    check.names = FALSE
  )
}

testthat::test_that("primary propensity-score configuration is enforced", {
  config <- ps_test_env$read_study_config()
  testthat::expect_invisible(
    ps_test_env$validate_propensity_score_config(config)
  )

  invalid_configs <- list(
    within(config, propensity_score$method <- "weighting"),
    within(config, propensity_score$estimand <- "ATE"),
    within(config, propensity_score$trim_preference_score <- FALSE),
    within(config, propensity_score$trim_fraction <- 0.1),
    within(config, propensity_score$matching_ratio <- 2L),
    within(config, propensity_score$caliper_scale <- "propensity_score"),
    within(config, balance$absolute_smd_threshold <- 0.2)
  )
  for (invalid_config in invalid_configs) {
    testthat::expect_error(
      ps_test_env$validate_propensity_score_config(invalid_config),
      class = "propensity_score_argument_error"
    )
  }
})

testthat::test_that("baseline archive uses the FeatureExtraction loader", {
  archive_path <- here::here(
    "data",
    "derived",
    "baseline_covariates.rds"
  )
  testthat::skip_if_not(fs::is_file(archive_path))
  loader_state <- new.env(parent = emptyenv())
  loader_state$called <- FALSE
  loader_state$deprecated_argument_supplied <- FALSE
  testthat::local_mocked_bindings(
    loadCovariateData = function(file, ...) {
      arguments <- list(...)
      loader_state$called <- identical(file, archive_path)
      loader_state$deprecated_argument_supplied <- "readOnly" %in% names(arguments)
      structure(list(), class = "CovariateData")
    },
    .package = "FeatureExtraction"
  )

  loaded <- ps_test_env$load_baseline_covariates(archive_path)
  testthat::expect_s3_class(loaded, "CovariateData")
  testthat::expect_true(loader_state$called)
  testthat::expect_false(loader_state$deprecated_argument_supplied)
})

testthat::test_that("baseline archive cannot escape data derived", {
  outside_path <- tempfile(fileext = ".rds")
  saveRDS(list(), outside_path)
  on.exit(unlink(outside_path), add = TRUE)

  testthat::expect_error(
    ps_test_env$load_baseline_covariates(outside_path),
    class = "propensity_score_privacy_error"
  )
  testthat::expect_error(
    ps_test_env$load_baseline_covariates(
      here::here("data", "derived", "..", "escaped.rds")
    ),
    class = "propensity_score_privacy_error"
  )
})

testthat::test_that("treatment rows map one-to-one to baseline covariates", {
  connection <- NULL
  covariate_data <- NULL
  working_table <- "study_ps_cohort_test"
  on.exit({
    if (!is.null(connection)) {
      tryCatch(
        {
          if (!is.null(covariate_data) &&
                Andromeda::isValidAndromeda(covariate_data)) {
            Andromeda::close(covariate_data)
          }
        },
        finally = tryCatch(
          ps_test_env$drop_feature_extraction_cohort_table(
            connection = connection,
            table_name = working_table
          ),
          finally = ps_test_env$disconnect_safely(connection)
        )
      )
    } else if (!is.null(covariate_data) &&
                 Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    }
  }, add = TRUE)

  connection <- open_local_ps_database()
  archive_path <- here::here(
    "data",
    "derived",
    "baseline_covariates.rds"
  )
  testthat::skip_if_not(
    fs::is_file(archive_path),
    "The baseline covariate archive is not available."
  )
  covariate_data <- ps_test_env$load_baseline_covariates(archive_path)
  config <- ps_test_env$read_study_config()
  cohort_tables <- ps_test_env$create_study_cohorts(connection, config)
  population <- ps_test_env$create_propensity_score_population(
    connection = connection,
    cohort_tables = cohort_tables,
    covariate_data = covariate_data,
    table_name = working_table
  )

  testthat::expect_identical(names(population), c("rowId", "treatment"))
  testthat::expect_identical(sum(population$treatment == 1), 1800L)
  testthat::expect_identical(sum(population$treatment == 0), 830L)
  testthat::expect_false(anyDuplicated(population$rowId) > 0L)
  covariate_row_ids <- covariate_data$covariates |>
    dplyr::distinct(.data$rowId) |>
    dplyr::collect() |>
    dplyr::pull(.data$rowId)
  testthat::expect_setequal(population$rowId, covariate_row_ids)

  model_data <- ps_test_env$create_propensity_score_model_data(
    covariate_data = covariate_data,
    population = population
  )
  testthat::expect_s3_class(model_data, "propensity_score_model_data")
  testthat::expect_identical(
    names(model_data),
    c("cyclops_data", "row_ids")
  )
  testthat::expect_setequal(model_data$row_ids, population$rowId)
})

testthat::test_that("PS estimation rejects outcomes and direct identifiers", {
  config <- ps_test_env$read_study_config()
  base_population <- data.frame(
    rowId = 1:4,
    treatment = c(1, 1, 0, 0)
  )
  forbidden_columns <- list(
    outcome = c(0, 1, 0, 1),
    subject_id = 11:14,
    person_id = 21:24,
    index_date = as.Date("2020-01-01") + 0:3
  )
  for (column_name in names(forbidden_columns)) {
    unsafe_population <- base_population
    unsafe_population[[column_name]] <- forbidden_columns[[column_name]]
    testthat::expect_error(
      ps_test_env$estimate_propensity_scores(
        model_data = list(),
        population = unsafe_population,
        config = config
      ),
      class = "propensity_score_privacy_error"
    )
  }

  estimate_arguments <- names(formals(ps_test_env$estimate_propensity_scores))
  testthat::expect_identical(
    estimate_arguments,
    c("model_data", "population", "config")
  )
})

testthat::test_that("preference scores use the odds-based transformation", {
  population <- data.frame(
    rowId = 1:4,
    treatment = c(1, 0, 0, 0),
    propensityScore = c(0.25, 0.5, 0.75, 0.1)
  )
  result <- ps_test_env$calculate_preference_scores(population)
  prevalence <- 0.25
  expected <- stats::plogis(
    stats::qlogis(population$propensityScore) - stats::qlogis(prevalence)
  )

  testthat::expect_equal(result$preferenceScore, expected)
  testthat::expect_true(all(result$preferenceScore >= 0))
  testthat::expect_true(all(result$preferenceScore <= 1))
})

testthat::test_that("preference-score trimming includes its boundaries", {
  population <- data.frame(
    rowId = 1:6,
    treatment = c(1, 1, 1, 0, 0, 0),
    propensityScore = c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9),
    preferenceScore = c(0.049, 0.05, 0.5, 0.95, 0.951, 0.7)
  )
  result <- ps_test_env$trim_propensity_score_population(
    population,
    trim_fraction = 0.05
  )

  testthat::expect_identical(result$rowId, c(2L, 3L, 4L, 6L))
  testthat::expect_true(2L %in% result$rowId)
  testthat::expect_true(4L %in% result$rowId)
})

testthat::test_that("1 to 1 ATT matching is deterministic without reuse", {
  config <- ps_test_env$read_study_config()
  population <- data.frame(
    rowId = c(11, 12, 13, 14, 21, 22, 23, 24),
    treatment = c(1, 1, 1, 1, 0, 0, 0, 0),
    propensityScore = c(0.20, 0.40, 0.60, 0.80, 0.21, 0.41, 0.61, 0.79),
    preferenceScore = c(0.20, 0.40, 0.60, 0.80, 0.21, 0.41, 0.61, 0.79)
  )
  first <- ps_test_env$match_propensity_score_population(population, config)
  second <- ps_test_env$match_propensity_score_population(population, config)

  testthat::expect_identical(first, second)
  testthat::expect_identical(
    names(first),
    c(
      "rowId",
      "treatment",
      "propensityScore",
      "preferenceScore",
      "matchId"
    )
  )
  testthat::expect_false(anyDuplicated(first$rowId) > 0L)
  comparator_rows <- first$rowId[first$treatment == 0]
  testthat::expect_false(anyDuplicated(comparator_rows) > 0L)
  match_counts <- table(first$matchId, first$treatment)
  testthat::expect_true(all(match_counts[, "0"] == 1L))
  testthat::expect_true(all(match_counts[, "1"] == 1L))
})

testthat::test_that("sparse SMDs use analysis flags matched by analysis ID", {
  covariate_data <- create_synthetic_ps_covariate_data()
  on.exit(
    if (Andromeda::isValidAndromeda(covariate_data)) {
      Andromeda::close(covariate_data)
    },
    add = TRUE
  )
  population_before <- data.frame(
    rowId = 1:8,
    treatment = c(1, 1, 1, 1, 0, 0, 0, 0)
  )
  population_after <- population_before[
    population_before$rowId %in% c(1, 2, 5, 6),
    ,
    drop = FALSE
  ]
  balance <- ps_test_env$compute_propensity_score_balance(
    covariate_data = covariate_data,
    population_before = population_before,
    population_after = population_after,
    threshold = 0.1
  )

  binary <- balance[balance$covariateId == 10, ]
  continuous <- balance[balance$covariateId == 20, ]
  disappearing <- balance[balance$covariateId == 30, ]
  absent <- balance[balance$covariateId == 40, ]
  target_only <- balance[balance$covariateId == 50, ]
  observed_only <- balance[balance$covariateId == 60, ]
  testthat::expect_true(binary$isBinary)
  testthat::expect_false(continuous$isBinary)
  testthat::expect_equal(binary$beforeSmd, 0.5 / sqrt(0.1875))
  testthat::expect_equal(binary$afterSmd, 0.5 / sqrt(0.125))
  testthat::expect_equal(continuous$beforeSmd, -1 / sqrt(1.25))
  testthat::expect_equal(continuous$afterSmd, -2)
  testthat::expect_equal(disappearing$afterSmd, 0)
  testthat::expect_equal(absent$beforeSmd, 0)
  testthat::expect_equal(absent$afterSmd, 0)
  testthat::expect_true(is.finite(target_only$beforeSmd))
  testthat::expect_true(is.finite(target_only$afterSmd))
  testthat::expect_true(is.finite(observed_only$beforeSmd))
  testthat::expect_true(is.finite(observed_only$afterSmd))
  testthat::expect_false(binary$balanced)
  testthat::expect_false(continuous$balanced)
  testthat::expect_false(any(grepl("pvalue|p_value", names(balance))))
})

testthat::test_that("aggregate adjustment summary is validated", {
  population_before <- data.frame(
    rowId = 1:6,
    treatment = c(1, 1, 1, 0, 0, 0),
    propensityScore = c(0.2, 0.3, 0.4, 0.25, 0.35, 0.45),
    preferenceScore = c(0.2, 0.3, 0.4, 0.25, 0.35, 0.45)
  )
  population_trimmed <- population_before[c(1, 2, 4, 5), ]
  population_matched <- transform(
    population_trimmed,
    matchId = c(1, 2, 1, 2)
  )
  balance <- synthetic_balance()
  summary <- ps_test_env$summarize_propensity_score_adjustment(
    population_before,
    population_trimmed,
    population_matched,
    balance,
    threshold = 0.1
  )

  testthat::expect_identical(nrow(summary), 1L)
  testthat::expect_identical(summary$target_before, 3L)
  testthat::expect_identical(summary$comparator_before, 3L)
  testthat::expect_identical(summary$matched_pair_count, 2L)
  testthat::expect_identical(summary$unbalanced_before_count, 2L)
  testthat::expect_identical(summary$unbalanced_after_count, 0L)

  unsafe_before <- population_before
  unsafe_before$subject_id <- seq_len(nrow(unsafe_before))
  testthat::expect_error(
    ps_test_env$summarize_propensity_score_adjustment(
      unsafe_before,
      population_trimmed,
      population_matched,
      balance,
      threshold = 0.1
    ),
    class = "propensity_score_privacy_error"
  )
})

testthat::test_that("aggregate writers reject person-level columns", {
  balance <- synthetic_balance()
  unsafe_balance <- transform(balance, person_id = seq_len(nrow(balance)))
  testthat::expect_error(
    ps_test_env$write_covariate_balance(
      unsafe_balance,
      tempfile(fileext = ".csv")
    ),
    class = "propensity_score_output_error"
  )

  population_before <- data.frame(
    rowId = 1:4,
    treatment = c(1, 1, 0, 0),
    propensityScore = c(0.2, 0.4, 0.25, 0.45),
    preferenceScore = c(0.2, 0.4, 0.25, 0.45)
  )
  population_matched <- transform(
    population_before,
    matchId = c(1, 2, 1, 2)
  )
  summary <- ps_test_env$summarize_propensity_score_adjustment(
    population_before,
    population_before,
    population_matched,
    balance,
    threshold = 0.1
  )
  unsafe_summary <- transform(summary, subject_id = 1L)
  testthat::expect_error(
    ps_test_env$write_propensity_score_summary(
      unsafe_summary,
      tempfile(fileext = ".csv")
    ),
    class = "propensity_score_output_error"
  )
})

testthat::test_that("local matched population excludes direct identifiers", {
  population <- data.frame(
    rowId = 1:4,
    treatment = c(1, 1, 0, 0),
    propensityScore = c(0.2, 0.4, 0.25, 0.45),
    preferenceScore = c(0.2, 0.4, 0.25, 0.45),
    matchId = c(1, 2, 1, 2)
  )
  path <- here::here(
    "data",
    "derived",
    paste0("ps_matched_population_test_", Sys.getpid(), ".rds")
  )
  on.exit(unlink(path), add = TRUE)
  saved_path <- ps_test_env$save_local_ps_population(population, path)
  saved <- readRDS(saved_path)
  normalized_names <- ps_test_env$normalize_ps_names(names(saved))

  testthat::expect_identical(saved, population)
  testthat::expect_false(any(grepl(
    "person|subject|date|cohortstart|cohortend",
    normalized_names
  )))

  unsafe_population <- transform(population, index_date = as.Date("2020-01-01"))
  testthat::expect_error(
    ps_test_env$save_local_ps_population(unsafe_population, path),
    class = "propensity_score_privacy_error"
  )
})
