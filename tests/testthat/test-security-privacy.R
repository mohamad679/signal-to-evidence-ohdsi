project_root <- Sys.getenv("PHASE18_PROJECT_ROOT")
implementation_root <- Sys.getenv("PHASE18_IMPLEMENTATION_ROOT")

security_environment <- new.env(
  parent = globalenv()
)
sys.source(
  file.path(
    implementation_root,
    "scripts",
    "12_security_privacy_validate.R"
  ),
  envir = security_environment
)

testthat::test_that(
  "tracked path policy rejects row-level data",
  {
    testthat::expect_error(
      security_environment$validate_tracked_path_policy(
        "data/raw/person.csv"
      ),
      "prohibited"
    )
  }
)

testthat::test_that(
  "secret detector rejects a private key marker",
  {
    marker <- paste0(
      "-----BEGIN ",
      "PRIVATE KEY-----"
    )

    testthat::expect_error(
      security_environment$detect_secret_patterns(
        marker,
        artifact_name = "fixture"
      ),
      "private_key"
    )
  }
)

testthat::test_that(
  "secret detector accepts ordinary configuration text",
  {
    testthat::expect_invisible(
      security_environment$detect_secret_patterns(
        "database_schema: main",
        artifact_name = "fixture"
      )
    )
  }
)

testthat::test_that(
  "aggregate privacy contract validates all reporting tables",
  {
    tables <- security_environment$validate_aggregate_privacy(
      project_root
    )

    testthat::expect_length(
      tables,
      15L
    )
  }
)

testthat::test_that(
  "workflow security validation handles multiple action refs",
  {
    actions <- security_environment$validate_workflow_security(
      project_root
    )

    testthat::expect_gt(
      length(actions),
      1L
    )
  }
)

testthat::test_that(
  "CI workflow invokes the security mode",
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
      "--mode=security",
      fixed = TRUE
    )
  }
)
