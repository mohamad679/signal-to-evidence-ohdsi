project_root <- Sys.getenv("PHASE17_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE17_IMPLEMENTATION_ROOT")

source(
  file.path(implementation_root, "R", "plots.R"),
  local = FALSE
)

runner_environment <- new.env(parent = globalenv())
sys.source(
  file.path(
    implementation_root,
    "scripts",
    "09_export_results.R"
  ),
  envir = runner_environment
)

testthat::test_that(
  "reporting runner validates real aggregate inputs without rendering",
  {
    result <- runner_environment$run_reporting_pipeline(
      project_root = project_root,
      render = FALSE
    )

    testthat::expect_false(result$rendered)
    testthat::expect_s3_class(
      result$reporting_inputs,
      "validated_reporting_inputs"
    )
    testthat::expect_equal(
      nrow(result$reporting_inputs$outcome_analysis_summary),
      1L
    )
  }
)

testthat::test_that(
  "report sources contain required interpretation and limitations",
  {
    report_paths <- file.path(
      project_root,
      "reports",
      c("index.qmd", "executive_summary.qmd")
    )
    report_text <- lapply(
      report_paths,
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    )
    combined <- paste(unlist(report_text), collapse = "\n")

    testthat::expect_match(
      combined,
      "Adjusted observational association under the stated design assumptions",
      fixed = TRUE
    )
    testthat::expect_match(
      combined,
      "Limitations",
      fixed = TRUE
    )
    testthat::expect_match(
      combined,
      "NOT_ESTIMABLE",
      fixed = TRUE
    )
  }
)

testthat::test_that(
  "reporting runner rejects missing source files",
  {
    temporary_root <- tempfile("phase17-reporting-")
    dir.create(temporary_root)

    testthat::expect_error(
      runner_environment$validate_reporting_sources(
        temporary_root
      ),
      "Required reporting source files are missing"
    )
  }
)

testthat::test_that(
  "quarto binary resolution preserves an explicit path",
  {
    explicit <- "/tmp/quarto-test-binary"

    testthat::expect_identical(
      runner_environment$resolve_quarto_binary(explicit),
      explicit
    )
  }
)
