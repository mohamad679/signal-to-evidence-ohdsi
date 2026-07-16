project_root <- Sys.getenv("PHASE16_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE16_IMPLEMENTATION_ROOT")

source(
  file.path(project_root, "R", "propensity_score.R"),
  local = FALSE
)
source(
  file.path(project_root, "R", "outcome.R"),
  local = FALSE
)
source(
  file.path(implementation_root, "R", "sensitivity.R"),
  local = FALSE
)

scenario_builder <- get(
  "create_sensitivity_scenarios",
  mode = "function"
)
weight_calculator <- get(
  "calculate_att_weights",
  mode = "function"
)
ess_calculator <- get(
  "calculate_effective_sample_size",
  mode = "function"
)
weighted_smd_calculator <- get(
  "compute_weighted_sparse_smd",
  mode = "function"
)
weighted_outcome_builder <- get(
  "build_weighted_outcome_population",
  mode = "function"
)
weighted_model_fitter <- get(
  "fit_weighted_outcome_model",
  mode = "function"
)

make_sensitivity_config <- function() {
  list(
    project = list(
      random_seed = 20260714L
    ),
    propensity_score = list(
      method = "matching",
      estimand = "ATT",
      trim_preference_score = TRUE,
      trim_fraction = 0.05,
      matching_ratio = 1L,
      caliper_scale = "standard_deviation"
    ),
    balance = list(
      absolute_smd_threshold = 0.1
    ),
    sensitivity = list(
      risk_windows = list(
        c(1L, 14L),
        c(1L, 30L),
        c(1L, 60L)
      ),
      adjustment_methods = c(
        "matching",
        "weighting"
      ),
      washout_days = c(
        180L,
        365L
      )
    )
  )
}

testthat::test_that(
  "sensitivity scenarios are one-factor-at-a-time",
  {
    scenarios <- scenario_builder(
      make_sensitivity_config()
    )

    testthat::expect_identical(
      scenarios$scenarioId,
      c(
        "primary",
        "risk_1_14",
        "risk_1_60",
        "weighting_att",
        "washout_365"
      )
    )
    testthat::expect_identical(
      scenarios$changedParameter,
      c(
        "none",
        "risk_window",
        "risk_window",
        "adjustment_method",
        "washout_days"
      )
    )
    testthat::expect_true(
      all(scenarios$trimPreferenceScore)
    )
    testthat::expect_equal(
      scenarios$trimFraction,
      rep(0.05, 5L)
    )
    testthat::expect_identical(
      scenarios$estimand,
      rep("ATT", 5L)
    )
  }
)

testthat::test_that(
  "ATT weights use propensity odds for comparators",
  {
    population <- data.frame(
      rowId = 1:4,
      treatment = c(1, 1, 0, 0),
      propensityScore = c(0.6, 0.8, 0.2, 0.4),
      preferenceScore = c(0.5, 0.7, 0.3, 0.4),
      check.names = FALSE
    )
    result <- weight_calculator(population)

    testthat::expect_equal(
      result$analysisWeight,
      c(1, 1, 0.25, 2 / 3)
    )
    testthat::expect_equal(
      ess_calculator(c(1, 1)),
      2
    )
  }
)

testthat::test_that(
  "weighted sparse SMD uses weighted moments",
  {
    population <- data.frame(
      rowId = 1:4,
      treatment = c(1, 1, 0, 0),
      analysisWeight = c(1, 1, 1, 3)
    )
    result <- weighted_smd_calculator(
      values = c(1, 0, 1, 0),
      row_ids = 1:4,
      population = population
    )
    target_mean <- 0.5
    comparator_mean <- 0.25
    target_variance <- 0.25
    comparator_variance <- 0.1875
    expected <-
      (target_mean - comparator_mean) /
      sqrt((target_variance + comparator_variance) / 2)

    testthat::expect_equal(result, expected)
  }
)

testthat::test_that(
  "weighted outcomes exclude identifiers and dates",
  {
    weighted_population <- data.frame(
      rowId = 1:4,
      treatment = c(1, 1, 0, 0),
      propensityScore = c(0.6, 0.7, 0.3, 0.4),
      preferenceScore = c(0.55, 0.65, 0.35, 0.45),
      analysisWeight = c(1, 1, 0.5, 2 / 3),
      check.names = FALSE
    )
    treatment_population <- data.frame(
      rowId = 1:4,
      subjectId = 10:13,
      cohortStartDate = as.Date(rep("2020-01-01", 4L)),
      cohortEndDate = as.Date(rep("2020-01-30", 4L)),
      treatment = c(1, 1, 0, 0)
    )
    outcome_cohort <- data.frame(
      subjectId = c(10, 12),
      cohortStartDate = as.Date(
        c("2020-01-05", "2020-01-20")
      ),
      cohortEndDate = as.Date(
        c("2020-01-05", "2020-01-20")
      )
    )
    result <- weighted_outcome_builder(
      weighted_population = weighted_population,
      treatment_population = treatment_population,
      outcome_cohort = outcome_cohort,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      names(result),
      c(
        "rowId",
        "treatment",
        "analysisWeight",
        "outcome"
      )
    )
    testthat::expect_identical(
      result$outcome,
      c(1L, 0L, 1L, 0L)
    )
    testthat::expect_length(
      intersect(
        names(result),
        c(
          "subjectId",
          "personId",
          "cohortStartDate",
          "cohortEndDate"
        )
      ),
      0L
    )
  }
)

testthat::test_that(
  "weighted model returns aggregate robust inference",
  {
    analysis_population <- data.frame(
      rowId = 1:8,
      treatment = c(1, 1, 1, 1, 0, 0, 0, 0),
      analysisWeight = c(1, 1, 1, 1, 0.5, 1, 1.5, 0.8),
      outcome = c(1, 0, 1, 0, 1, 0, 0, 1),
      check.names = FALSE
    )
    result <- weighted_model_fitter(
      analysis_population = analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_equal(nrow(result), 1L)
    testthat::expect_true(is.finite(result$estimate))
    testthat::expect_gt(result$estimate, 0)
    testthat::expect_true(is.finite(result$standardError))
    testthat::expect_gt(result$standardError, 0)
    testthat::expect_identical(result$subjectCount, 8L)
    testthat::expect_identical(result$eventCount, 4L)
    testthat::expect_identical(
      result$varianceEstimator,
      "individual-level robust CR1"
    )
    testthat::expect_length(
      intersect(
        names(result),
        c(
          "rowId",
          "subjectId",
          "personId",
          "cohortStartDate",
          "cohortEndDate"
        )
      ),
      0L
    )
  }
)
