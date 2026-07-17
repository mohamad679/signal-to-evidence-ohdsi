project_root <- Sys.getenv("PHASE18_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE18_IMPLEMENTATION_ROOT")

pipeline_environment <- new.env(
  parent = globalenv()
)
sys.source(
  file.path(
    implementation_root,
    "scripts",
    "11_ci_full_pipeline.R"
  ),
  envir = pipeline_environment
)

testthat::test_that(
  "full pipeline discovers exactly stages 01 through 09",
  {
    scripts <- pipeline_environment$discover_pipeline_scripts(
      project_root
    )

    testthat::expect_length(
      scripts,
      9L
    )
    testthat::expect_identical(
      sub(
        "_.*$",
        "",
        basename(scripts)
      ),
      sprintf(
        "%02d",
        seq_len(9L)
      )
    )
  }
)

testthat::test_that(
  "full pipeline dry run does not execute stages",
  {
    result <- pipeline_environment$run_full_pipeline(
      project_root = project_root,
      execute = FALSE
    )

    testthat::expect_false(
      result$executed
    )
    testthat::expect_length(
      result$scripts,
      9L
    )
  }
)

testthat::test_that(
  "pipeline validates aggregate tables and plot-data contracts",
  {
    artifacts <- pipeline_environment$validate_pipeline_artifacts(
      project_root
    )

    testthat::expect_length(
      artifacts$csv_files,
      15L
    )
    testthat::expect_gt(
      artifacts$effect_plot_rows,
      0L
    )
    testthat::expect_gt(
      artifacts$balance_plot_rows,
      0L
    )
  }
)

testthat::test_that(
  "pipeline artifact privacy rejects person identifiers",
  {
    temporary <- tempfile(
      fileext = ".csv"
    )
    writeLines(
      c(
        "personId,value",
        "1,2"
      ),
      temporary
    )

    testthat::expect_error(
      pipeline_environment$validate_pipeline_privacy(
        list(
          csv_files = temporary
        )
      ),
      "prohibited field"
    )
  }
)

testthat::test_that(
  "CI workflow runs the full pipeline",
  {
    workflow <- paste(
      readLines(
        file.path(
          project_root,
          ".github",
          "workflows",
          "ci.yml"
        ),
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )

    testthat::expect_match(
      workflow,
      "--mode=pipeline",
      fixed = TRUE
    )
  }
)
