runner_root <- Sys.getenv(
  "PHASE16_RUNNER_ROOT"
)

runner_environment <- new.env(
  parent = globalenv()
)

sys.source(
  file.path(
    runner_root,
    "scripts",
    "08_run_sensitivity.R"
  ),
  envir = runner_environment
)

population_builder <- get(
  "build_sensitivity_population",
  envir = runner_environment,
  inherits = FALSE
)

linkage_builder <- get(
  "build_sensitivity_linkage",
  envir = runner_environment,
  inherits = FALSE
)

balance_summarizer <- get(
  "summarize_sensitivity_balance",
  envir = runner_environment,
  inherits = FALSE
)

output_validator <- get(
  "validate_sensitivity_output",
  envir = runner_environment,
  inherits = FALSE
)

summary_writer <- get(
  "write_sensitivity_summary",
  envir = runner_environment,
  inherits = FALSE
)

testthat::test_that(
  "sensitivity population has no fixed primary count",
  {
    cohort_rows <- data.frame(
      row_id = 1:4,
      cohort_definition_id = c(
        1,
        1,
        2,
        2
      ),
      subject_id = 101:104,
      check.names = FALSE
    )

    population <- population_builder(
      cohort_rows = cohort_rows,
      covariate_row_ids = 1:4
    )

    testthat::expect_identical(
      names(population),
      c(
        "rowId",
        "treatment"
      )
    )

    testthat::expect_identical(
      population$treatment,
      c(
        1L,
        1L,
        0L,
        0L
      )
    )

    runner_lines <- readLines(
      file.path(
        runner_root,
        "scripts",
        "08_run_sensitivity.R"
      ),
      warn = FALSE
    )

    fixed_count_pattern <- paste(
      c(
        "2630",
        "1800",
        "830"
      ),
      collapse = "|"
    )

    testthat::expect_false(
      any(
        grepl(
          fixed_count_pattern,
          runner_lines
        )
      )
    )
  }
)

testthat::test_that(
  "sensitivity linkage remains internal and treatment-consistent",
  {
    feature_rows <- data.frame(
      row_id = 1:4,
      subject_id = 101:104,
      cohort_start_date = rep(
        "2020-01-01",
        4L
      ),
      cohort_end_date = rep(
        "2020-01-30",
        4L
      ),
      cohort_definition_id = c(
        1,
        1,
        2,
        2
      ),
      check.names = FALSE
    )

    population <- data.frame(
      rowId = 1:4,
      treatment = c(
        1,
        1,
        0,
        0
      ),
      check.names = FALSE
    )

    linkage <- linkage_builder(
      feature_rows = feature_rows,
      population = population
    )

    testthat::expect_identical(
      names(linkage),
      c(
        "rowId",
        "subjectId",
        "cohortStartDate",
        "cohortEndDate",
        "treatment"
      )
    )

    testthat::expect_s3_class(
      linkage$cohortStartDate,
      "Date"
    )

    testthat::expect_identical(
      linkage$treatment,
      population$treatment
    )
  }
)

testthat::test_that(
  "balance summary applies the inclusive threshold",
  {
    balance <- data.frame(
      covariateId = 1:3,
      covariateName = c(
        "a",
        "b",
        "c"
      ),
      analysisId = rep(
        1,
        3L
      ),
      isBinary = c(
        TRUE,
        TRUE,
        FALSE
      ),
      beforeSmd = c(
        0.2,
        0.1,
        0.05
      ),
      afterSmd = c(
        0.1,
        0.09,
        NA_real_
      ),
      balanced = c(
        FALSE,
        TRUE,
        FALSE
      ),
      check.names = FALSE
    )

    result <- balance_summarizer(
      balance = balance,
      threshold = 0.1
    )

    testthat::expect_identical(
      result$residualImbalanceCount,
      2L
    )

    testthat::expect_equal(
      result$maximumAbsoluteSmd,
      0.1
    )
  }
)

make_output <- function() {
  scenario_ids <- c(
    "primary",
    "risk_1_14",
    "risk_1_60",
    "weighting_att",
    "washout_365"
  )

  data.frame(
    scenarioOrder = seq_len(5L),
    scenarioId = scenario_ids,
    changedParameter = c(
      "none",
      "risk_window",
      "risk_window",
      "adjustment_method",
      "washout_days"
    ),
    isPrimary = c(
      TRUE,
      rep(
        FALSE,
        4L
      )
    ),
    adjustmentMethod = c(
      "matching",
      "matching",
      "matching",
      "weighting",
      "matching"
    ),
    estimand = rep(
      "ATT",
      5L
    ),
    washoutDays = c(
      180L,
      180L,
      180L,
      180L,
      365L
    ),
    riskWindowStartDays = rep(
      1L,
      5L
    ),
    riskWindowEndDays = c(
      30L,
      14L,
      60L,
      30L,
      30L
    ),
    trimFraction = rep(
      0.05,
      5L
    ),
    preAdjustmentCount = rep(
      100L,
      5L
    ),
    postTrimCount = rep(
      90L,
      5L
    ),
    adjustedSubjectCount = rep(
      80L,
      5L
    ),
    adjustedTargetCount = rep(
      40L,
      5L
    ),
    adjustedComparatorCount = rep(
      40L,
      5L
    ),
    effectiveSampleSize = rep(
      80,
      5L
    ),
    residualImbalanceCount = rep(
      0L,
      5L
    ),
    maximumAbsoluteSmd = rep(
      0.05,
      5L
    ),
    balanceThreshold = rep(
      0.1,
      5L
    ),
    effectMeasure = rep(
      "odds ratio",
      5L
    ),
    estimate = rep(
      1.2,
      5L
    ),
    ciLower = rep(
      1.0,
      5L
    ),
    ciUpper = rep(
      1.4,
      5L
    ),
    confidenceLevel = rep(
      0.95,
      5L
    ),
    logOddsRatio = rep(
      log(1.2),
      5L
    ),
    standardError = rep(
      0.1,
      5L
    ),
    subjectCount = rep(
      80L,
      5L
    ),
    eventCount = rep(
      20L,
      5L
    ),
    targetSubjectCount = rep(
      40L,
      5L
    ),
    targetEventCount = rep(
      12L,
      5L
    ),
    comparatorSubjectCount = rep(
      40L,
      5L
    ),
    comparatorEventCount = rep(
      8L,
      5L
    ),
    varianceEstimator = rep(
      "robust CR1",
      5L
    ),
    modelConverged = rep(
      TRUE,
      5L
    ),
    zeroCellDetected = rep(
      FALSE,
      5L
    ),
    interpretation = rep(
      "Adjusted observational association.",
      5L
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

testthat::test_that(
  "sensitivity summary is aggregate and writable",
  {
    output <- make_output()

    testthat::expect_invisible(
      output_validator(output)
    )

    path <- tempfile(
      fileext = ".csv"
    )

    testthat::expect_invisible(
      summary_writer(
        output = output,
        path = path
      )
    )

    written <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    testthat::expect_identical(
      written$scenarioId,
      output$scenarioId
    )

    testthat::expect_length(
      intersect(
        names(written),
        c(
          "rowId",
          "matchId",
          "subjectId",
          "personId",
          "cohort_start_date",
          "cohort_end_date"
        )
      ),
      0L
    )
  }
)
