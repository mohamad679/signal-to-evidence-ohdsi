project_root <- Sys.getenv("PHASE18_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE18_IMPLEMENTATION_ROOT")

ci_environment <- new.env(
  parent = globalenv()
)
sys.source(
  file.path(
    implementation_root,
    "scripts",
    "10_ci_validate.R"
  ),
  envir = ci_environment
)

testthat::test_that(
  "CI workflow contains required quality gates",
  {
    workflow_path <- file.path(
      project_root,
      ".github",
      "workflows",
      "ci.yml"
    )
    workflow <- paste(
      readLines(
        workflow_path,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )

    required <- c(
      "permissions:",
      "contents: read",
      "timeout-minutes: 90",
      "actions/checkout@v4",
      "r-lib/actions/setup-r@v2",
      "r-version: renv",
      "quarto-dev/quarto-actions/setup@v2",
      "r-lib/actions/setup-renv@v2",
      "--mode=parse",
      "--mode=lint",
      "--mode=test",
      "--mode=report",
      "actions/upload-artifact@v4"
    )

    for (entry in required) {
      testthat::expect_match(
        workflow,
        entry,
        fixed = TRUE
      )
    }

    testthat::expect_false(
      grepl(
        "pull_request_target",
        workflow,
        fixed = TRUE
      )
    )
  }
)

testthat::test_that(
  "CI mode parsing accepts only supported modes",
  {
    testthat::expect_identical(
      ci_environment$resolve_ci_mode("--mode=parse"),
      "parse"
    )
    testthat::expect_identical(
      ci_environment$resolve_ci_mode(
        c(
          "--mode",
          "report"
        )
      ),
      "report"
    )
    testthat::expect_error(
      ci_environment$resolve_ci_mode("--mode=release"),
      "Use --mode"
    )
  }
)

testthat::test_that(
  "CI runner discovers repository R sources",
  {
    files <- ci_environment$collect_r_files(project_root)

    testthat::expect_true(
      length(files) >= 15L
    )
    testthat::expect_true(
      all(
        file.exists(files)
      )
    )
    testthat::expect_true(
      any(
        grepl(
          "scripts/10_ci_validate.R",
          files,
          fixed = TRUE
        )
      )
    )
  }
)

testthat::test_that(
  "CI workflow is valid YAML",
  {
    workflow <- yaml::read_yaml(
      file.path(
        project_root,
        ".github",
        "workflows",
        "ci.yml"
      )
    )

    testthat::expect_type(
      workflow,
      "list"
    )
    testthat::expect_true(
      "quality" %in% names(workflow$jobs)
    )
  }
)
