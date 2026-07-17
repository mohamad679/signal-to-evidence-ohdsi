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

synthetic_balance_population <- function() {
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
      1, 0,
      0, 1
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

fake_balance_calculation <- function(
    covariate_data,
    population_before,
    population_after,
    threshold) {
  testthat::expect_identical(
    covariate_data,
    "synthetic-covariates"
  )

  testthat::expect_identical(
    population_before,
    population_after
  )

  testthat::expect_identical(
    names(population_before),
    c(
      "rowId",
      "treatment"
    )
  )

  testthat::expect_true(
    all(
      c(0, 1) %in%
        population_before$treatment
    )
  )

  testthat::expect_equal(
    threshold,
    0.1
  )

  data.frame(
    covariateId = 1:3,
    afterSmd = c(
      0.05,
      0.15,
      NA_real_
    ),
    balanced = c(
      TRUE,
      FALSE,
      FALSE
    )
  )
}

testthat::test_that(
  "subgroup balance reports all prespecified categories",
  {
    result <-
      calculate_subgroup_balance_summary(
        covariate_data =
        "synthetic-covariates",
        subgroup_population =
        synthetic_balance_population(),
        threshold = 0.1,
        balance_function =
        fake_balance_calculation
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

    older <- result$subgroupLevel ==
      ">=65"

    testthat::expect_identical(
      result$balanceStatus[older],
      "NOT_EVALUABLE"
    )

    testthat::expect_identical(
      result$balanceSubjectCount[older],
      0L
    )
  }
)

testthat::test_that(
  "evaluable balance summarizes SMD diagnostics",
  {
    result <-
      calculate_subgroup_balance_summary(
        covariate_data =
        "synthetic-covariates",
        subgroup_population =
        synthetic_balance_population(),
        balance_function =
        fake_balance_calculation
      )

    evaluable <- result$balanceStatus ==
      "EVALUABLE"

    testthat::expect_true(
      all(
        result$covariateCount[
          evaluable
        ] == 3L
      )
    )

    testthat::expect_true(
      all(
        result$evaluableCovariateCount[
          evaluable
        ] == 2L
      )
    )

    testthat::expect_true(
      all(
        result$unbalancedCovariateCount[
          evaluable
        ] == 2L
      )
    )

    testthat::expect_equal(
      result$maximumAbsoluteSmd[
        evaluable
      ],
      rep(
        0.15,
        sum(evaluable)
      )
    )
  }
)

testthat::test_that(
  "subgroup balance output remains aggregate",
  {
    result <-
      calculate_subgroup_balance_summary(
        covariate_data =
        "synthetic-covariates",
        subgroup_population =
        synthetic_balance_population(),
        balance_function =
        fake_balance_calculation
      )

    prohibited <- c(
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
        prohibited
      ),
      0L
    )
  }
)
