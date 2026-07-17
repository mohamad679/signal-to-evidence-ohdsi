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

valid_subgroup_config <- function() {
  list(
    subgroups = list(
      sex = TRUE,
      age_cutoff = 65L
    )
  )
}

synthetic_demographic_tables <- function() {
  analysis_ref <- data.frame(
    analysisId = c(1, 2, 99),
    analysisName = c(
      "DemographicsGender",
      "DemographicsAge",
      "Measurements"
    ),
    stringsAsFactors = FALSE
  )

  covariate_ref <- data.frame(
    covariateId = c(
      8507001,
      8532001,
      1002,
      999001
    ),
    covariateName = c(
      "gender = MALE",
      "gender = FEMALE",
      "age in years",
      "Sexual orientation"
    ),
    analysisId = c(
      1,
      1,
      2,
      99
    ),
    stringsAsFactors = FALSE
  )

  covariate_rows <- data.frame(
    rowId = c(
      1, 2, 3, 4,
      1, 2, 3, 4,
      1
    ),
    covariateId = c(
      1002, 1002, 1002, 1002,
      8507001, 8532001,
      8507001, 8532001,
      999001
    ),
    covariateValue = c(
      31, 46, 65, 70,
      1, 1, 1, 1,
      1
    )
  )

  list(
    covariate_rows = covariate_rows,
    covariate_ref = covariate_ref,
    analysis_ref = analysis_ref
  )
}

synthetic_matched_data <- function() {
  matched_population <- data.frame(
    rowId = 1:8,
    treatment = rep(
      c(1, 0),
      4
    ),
    propensityScore = c(
      0.55, 0.52,
      0.61, 0.60,
      0.47, 0.45,
      0.58, 0.57
    ),
    preferenceScore = c(
      0.54, 0.51,
      0.60, 0.59,
      0.46, 0.44,
      0.57, 0.56
    ),
    matchId = rep(
      1:4,
      each = 2
    )
  )

  analysis_population <- data.frame(
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
      0, 0,
      1, 1
    )
  )

  demographics <- data.frame(
    rowId = 1:8,
    sex = c(
      "FEMALE", "FEMALE",
      "FEMALE", "MALE",
      "MALE", "MALE",
      "FEMALE", "MALE"
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
    stringsAsFactors = FALSE
  )

  list(
    matched_population = matched_population,
    analysis_population = analysis_population,
    demographics = demographics
  )
}

testthat::test_that(
  "validate_subgroup_config accepts the frozen specification",
  {
    result <- validate_subgroup_config(
      valid_subgroup_config()
    )

    testthat::expect_identical(
      result,
      65L
    )
  }
)

testthat::test_that(
  "validate_subgroup_config rejects unsupported settings",
  {
    invalid_sex <- valid_subgroup_config()
    invalid_sex$subgroups$sex <- FALSE

    testthat::expect_error(
      validate_subgroup_config(invalid_sex),
      "must be enabled"
    )

    invalid_age <- valid_subgroup_config()
    invalid_age$subgroups$age_cutoff <- 65.5

    testthat::expect_error(
      validate_subgroup_config(invalid_age),
      "positive whole number"
    )
  }
)

testthat::test_that(
  "build_subgroup_demographics uses exact analysis identifiers",
  {
    tables <- synthetic_demographic_tables()

    result <- build_subgroup_demographics(
      covariate_rows =
        tables$covariate_rows,
      covariate_ref =
        tables$covariate_ref,
      analysis_ref =
        tables$analysis_ref,
      age_cutoff = 65L
    )

    testthat::expect_identical(
      names(result),
      c(
        "rowId",
        "sex",
        "age",
        "ageGroup"
      )
    )

    testthat::expect_identical(
      result$rowId,
      as.numeric(1:4)
    )

    testthat::expect_identical(
      result$sex,
      c(
        "MALE",
        "FEMALE",
        "MALE",
        "FEMALE"
      )
    )

    testthat::expect_identical(
      result$age,
      c(
        31L,
        46L,
        65L,
        70L
      )
    )

    testthat::expect_identical(
      result$ageGroup,
      c(
        "<65",
        "<65",
        ">=65",
        ">=65"
      )
    )

    testthat::expect_false(
      any(
        grepl(
          "orientation",
          result$sex,
          ignore.case = TRUE
        )
      )
    )
  }
)

testthat::test_that(
  "build_subgroup_demographics rejects duplicate gender assignments",
  {
    tables <- synthetic_demographic_tables()

    duplicate_row <- data.frame(
      rowId = 1,
      covariateId = 8532001,
      covariateValue = 1
    )

    tables$covariate_rows <- rbind(
      tables$covariate_rows,
      duplicate_row
    )

    testthat::expect_error(
      build_subgroup_demographics(
        covariate_rows =
          tables$covariate_rows,
        covariate_ref =
          tables$covariate_ref,
        analysis_ref =
          tables$analysis_ref
      ),
      "one active binary category"
    )
  }
)

testthat::test_that(
  "attach_subgroup_demographics preserves all matched rows",
  {
    data <- synthetic_matched_data()

    result <- attach_subgroup_demographics(
      analysis_population =
        data$analysis_population,
      matched_population =
        data$matched_population,
      demographics =
        data$demographics
    )

    testthat::expect_identical(
      nrow(result),
      8L
    )

    testthat::expect_identical(
      result$rowId,
      data$matched_population$rowId
    )

    testthat::expect_identical(
      result$matchId,
      data$matched_population$matchId
    )

    testthat::expect_identical(
      result$propensityScore,
      data$matched_population$propensityScore
    )

    testthat::expect_identical(
      result$sex,
      data$demographics$sex
    )
  }
)

testthat::test_that(
  "attach_subgroup_demographics rejects incomplete linkage",
  {
    data <- synthetic_matched_data()

    data$demographics <- data$demographics[
      -1,
      ,
      drop = FALSE
    ]

    testthat::expect_error(
      attach_subgroup_demographics(
        analysis_population =
          data$analysis_population,
        matched_population =
          data$matched_population,
        demographics =
          data$demographics
      ),
      "Every matched row"
    )
  }
)

testthat::test_that(
  "summarize_subgroup_membership reports discordant and empty strata",
  {
    data <- synthetic_matched_data()

    subgroup_population <-
      attach_subgroup_demographics(
        analysis_population =
        data$analysis_population,
        matched_population =
        data$matched_population,
        demographics =
        data$demographics
      )

    result <- summarize_subgroup_membership(
      subgroup_population
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

    female_row <- result$subgroupLevel ==
      "FEMALE"

    male_row <- result$subgroupLevel ==
      "MALE"

    younger_row <- result$subgroupLevel ==
      "<65"

    older_row <- result$subgroupLevel ==
      ">=65"

    testthat::expect_identical(
      result$subjectCount[female_row],
      4L
    )

    testthat::expect_identical(
      result$completePairCount[female_row],
      1L
    )

    testthat::expect_identical(
      result$singletonClusterCount[
        female_row
      ],
      2L
    )

    testthat::expect_identical(
      result$subjectCount[male_row],
      4L
    )

    testthat::expect_identical(
      result$completePairCount[male_row],
      1L
    )

    testthat::expect_identical(
      result$subjectCount[younger_row],
      8L
    )

    testthat::expect_identical(
      result$completePairCount[
        younger_row
      ],
      4L
    )

    testthat::expect_identical(
      result$subjectCount[older_row],
      0L
    )

    testthat::expect_identical(
      result$representedClusterCount[
        older_row
      ],
      0L
    )
  }
)
