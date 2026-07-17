source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "outcome.R"
  ),
  local = FALSE
)

source(
  testthat::test_path(
    "..",
    "..",
    "R",
    "subgroup.R"
  ),
  local = FALSE
)

synthetic_subgroup_population <- function() {
  data.frame(
    rowId = 1:8,
    treatment = rep(
      c(1, 0),
      4
    ),
    matchId = rep(
      1:4,
      each = 2
    ),
    outcome = c(
      1, 0,
      0, 1,
      1, 1,
      0, 0
    ),
    propensityScore = c(
      0.55, 0.52,
      0.61, 0.60,
      0.47, 0.45,
      0.58, 0.57
    ),
    sex = c(
      "FEMALE", "FEMALE",
      "FEMALE", "MALE",
      "MALE", "FEMALE",
      "MALE", "MALE"
    ),
    age = c(
      31L, 32L,
      40L, 41L,
      45L, 44L,
      46L, 43L
    ),
    ageGroup = rep(
      "<65",
      8
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

testthat::test_that(
  "subgroup population validation accepts valid rows",
  {
    population <-
      synthetic_subgroup_population()

    testthat::expect_invisible(
      validate_subgroup_analysis_population(
        population
      )
    )
  }
)

testthat::test_that(
  "the complete younger subgroup reproduces the primary model",
  {
    population <-
      synthetic_subgroup_population()

    primary_population <- population[
      ,
      c(
        "rowId",
        "treatment",
        "matchId",
        "outcome"
      ),
      drop = FALSE
    ]

    primary_result <-
      fit_matched_outcome_model(
        analysis_population =
        primary_population,
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ) # nolint: object_usage_linter.

    subgroup_result <-
      fit_subgroup_outcome_model(
        subgroup_population =
        population,
        subgroup_type = "age",
        subgroup_level = "<65",
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      )

    testthat::expect_identical(
      subgroup_result$estimabilityStatus,
      "ESTIMABLE"
    )

    testthat::expect_equal(
      subgroup_result$estimate,
      primary_result$estimate,
      tolerance = 1e-12
    )

    testthat::expect_equal(
      subgroup_result$standardError,
      primary_result$standardError,
      tolerance = 1e-12
    )

    testthat::expect_identical(
      subgroup_result$subjectCount,
      8L
    )

    testthat::expect_identical(
      subgroup_result$completePairCount,
      4L
    )

    testthat::expect_identical(
      subgroup_result$singletonClusterCount,
      0L
    )
  }
)

testthat::test_that(
  "an empty older subgroup is explicitly non-estimable",
  {
    result <- fit_subgroup_outcome_model(
      subgroup_population =
        synthetic_subgroup_population(),
      subgroup_type = "age",
      subgroup_level = ">=65",
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$estimabilityStatus,
      "NOT_ESTIMABLE"
    )

    testthat::expect_identical(
      result$subjectCount,
      0L
    )

    testthat::expect_match(
      result$estimabilityReason,
      "No subjects"
    )

    testthat::expect_true(
      is.na(result$estimate)
    )
  }
)

testthat::test_that(
  "sex subgroups retain singleton matched clusters",
  {
    population <-
      synthetic_subgroup_population()

    female_result <-
      fit_subgroup_outcome_model(
        subgroup_population =
        population,
        subgroup_type = "sex",
        subgroup_level = "FEMALE",
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      )

    male_result <-
      fit_subgroup_outcome_model(
        subgroup_population =
        population,
        subgroup_type = "sex",
        subgroup_level = "MALE",
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      )

    testthat::expect_identical(
      female_result$estimabilityStatus,
      "ESTIMABLE"
    )

    testthat::expect_identical(
      male_result$estimabilityStatus,
      "ESTIMABLE"
    )

    testthat::expect_identical(
      female_result$subjectCount,
      4L
    )

    testthat::expect_identical(
      female_result$completePairCount,
      1L
    )

    testthat::expect_identical(
      female_result$singletonClusterCount,
      2L
    )

    testthat::expect_identical(
      male_result$completePairCount,
      1L
    )

    testthat::expect_identical(
      male_result$singletonClusterCount,
      2L
    )
  }
)

testthat::test_that(
  "a one-treatment subgroup is non-estimable",
  {
    population <-
      synthetic_subgroup_population()

    population$sex[
      population$treatment == 0
    ] <- "OTHER"

    result <- fit_subgroup_outcome_model(
      subgroup_population =
        population,
      subgroup_type = "sex",
      subgroup_level = "FEMALE",
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$estimabilityStatus,
      "NOT_ESTIMABLE"
    )

    testthat::expect_match(
      result$estimabilityReason,
      "Both treatment groups"
    )
  }
)

testthat::test_that(
  "prespecified models include sex and both age categories",
  {
    result <- run_prespecified_subgroup_models(
      subgroup_population =
        synthetic_subgroup_population(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$subgroupType,
      c(
        "sex",
        "sex",
        "age",
        "age"
      )
    )

    testthat::expect_identical(
      result$subgroupLevel,
      c(
        "FEMALE",
        "MALE",
        "<65",
        ">=65"
      )
    )

    older_row <- result$subgroupLevel ==
      ">=65"

    testthat::expect_identical(
      result$estimabilityStatus[older_row],
      "NOT_ESTIMABLE"
    )

    prohibited_fields <- c(
      "rowId",
      "matchId",
      "subjectId",
      "personId",
      "row_id",
      "match_id",
      "subject_id",
      "person_id"
    )

    testthat::expect_length(
      intersect(
        names(result),
        prohibited_fields
      ),
      0L
    )
  }
)

testthat::test_that(
  "overlap diagnostics are bounded and aggregate",
  {
    result <- fit_subgroup_outcome_model(
      subgroup_population =
        synthetic_subgroup_population(),
      subgroup_type = "age",
      subgroup_level = "<65",
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_true(
      result$overlapAvailable
    )

    testthat::expect_gte(
      result$overlapLower,
      0
    )

    testthat::expect_lte(
      result$overlapUpper,
      1
    )

    testthat::expect_gte(
      result$overlapWidth,
      0
    )
  }
)
