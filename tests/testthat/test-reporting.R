project_root <- Sys.getenv("PHASE17_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE17_IMPLEMENTATION_ROOT")

source(
  file.path(implementation_root, "R", "plots.R"),
  local = FALSE
)

testthat::test_that(
  "reporting artifact contract contains all aggregate inputs",
  {
    paths <- reporting_table_paths()

    testthat::expect_length(paths, 15L)
    testthat::expect_true(
      all(startsWith(paths, "results/tables/"))
    )
    testthat::expect_true(
      all(endsWith(paths, ".csv"))
    )
    testthat::expect_identical(anyDuplicated(paths), 0L)
  }
)

testthat::test_that(
  "real aggregate reporting inputs satisfy the frozen contract",
  {
    inputs <- validate_reporting_inputs(project_root)

    testthat::expect_s3_class(
      inputs,
      "validated_reporting_inputs"
    )
    testthat::expect_length(inputs, 15L)
    testthat::expect_equal(
      nrow(inputs$outcome_analysis_summary),
      1L
    )
    testthat::expect_equal(
      nrow(inputs$subgroup_analysis_summary),
      4L
    )
    testthat::expect_equal(
      nrow(inputs$sensitivity_analysis_summary),
      5L
    )
  }
)

testthat::test_that(
  "disclosure-safety validation rejects person-level columns",
  {
    unsafe <- data.frame(
      personId = 1L,
      estimate = 1,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      validate_disclosure_safe_reporting_table(
        unsafe,
        "unsafe"
      ),
      "prohibited person-level columns"
    )
  }
)

testthat::test_that(
  "required-column validation identifies missing columns",
  {
    table <- data.frame(
      estimate = 1,
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      require_reporting_columns(
        table,
        c("estimate", "ciLower"),
        "example"
      ),
      "missing required columns: ciLower"
    )
  }
)

testthat::test_that(
  "unique-key validation rejects duplicated scenarios",
  {
    table <- data.frame(
      scenarioId = c("primary", "primary"),
      stringsAsFactors = FALSE
    )

    testthat::expect_error(
      validate_reporting_unique_key(
        table,
        "scenarioId",
        "sensitivity"
      ),
      "duplicated key rows"
    )
  }
)

testthat::test_that(
  "estimate intervals are formatted deterministically",
  {
    testthat::expect_identical(
      format_estimate_ci(
        0.931219271765986,
        0.60293993815296,
        1.43823501684903
      ),
      "0.93 (0.60 to 1.44)"
    )

    testthat::expect_error(
      format_estimate_ci(1, 2, 3),
      "invalid"
    )
  }
)

testthat::test_that(
  "effect plot data preserves non-estimable subgroups",
  {
    inputs <- validate_reporting_inputs(project_root)
    plot_data <- prepare_effect_estimate_plot_data(inputs)

    testthat::expect_equal(nrow(plot_data), 10L)
    testthat::expect_true(
      any(
        plot_data$label == "age: >=65" &
          plot_data$estimabilityStatus == "NOT_ESTIMABLE"
      )
    )
    testthat::expect_true(
      all(
        c(
          "section",
          "label",
          "estimate",
          "ciLower",
          "ciUpper",
          "estimabilityStatus"
        ) %in% names(plot_data)
      )
    )
  }
)

testthat::test_that(
  "balance plot data contains before and after values",
  {
    inputs <- validate_reporting_inputs(project_root)
    plot_data <- prepare_balance_plot_data(inputs)
    balance_rows <- nrow(inputs$covariate_balance)

    testthat::expect_equal(
      nrow(plot_data),
      balance_rows * 2L
    )
    testthat::expect_setequal(
      unique(plot_data$stage),
      c("Before adjustment", "After adjustment")
    )
    testthat::expect_true(
      all(plot_data$absoluteSmd >= 0)
    )
  }
)

testthat::test_that(
  "plot preparation requires validated inputs",
  {
    testthat::expect_error(
      prepare_effect_estimate_plot_data(list()),
      "validated reporting inputs"
    )
    testthat::expect_error(
      prepare_balance_plot_data(list()),
      "validated reporting inputs"
    )
  }
)
