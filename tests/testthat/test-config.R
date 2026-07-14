config_env <- new.env(parent = globalenv())
sys.source(here::here("R", "config.R"), envir = config_env)
read_study_config <- config_env$read_study_config

valid_study_config <- function() {
  list(
    project = list(
      name = "signal-to-evidence-ohdsi",
      random_seed = 20260714L,
      database_schema = "main"
    ),
    database = list(source = "Eunomia", dataset_name = NULL),
    design = list(
      study_type = "new-user active-comparator cohort",
      washout_days = 180L,
      minimum_prior_observation_days = 180L,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L,
      exclude_prior_outcome = TRUE,
      allow_multiple_entries = FALSE
    ),
    cohorts = list(
      target = list(concept_id = NULL, concept_name = NULL),
      comparator = list(concept_id = NULL, concept_name = NULL),
      outcome = list(concept_id = NULL, concept_name = NULL)
    ),
    feasibility = list(
      minimum_subjects_per_arm = 100L,
      minimum_total_outcomes = 20L,
      minimum_outcomes_per_arm = 5L
    ),
    propensity_score = list(
      method = "matching",
      estimand = "ATT",
      trim_preference_score = TRUE,
      trim_fraction = 0.05,
      matching_ratio = 1L,
      caliper_scale = "standard_deviation"
    ),
    balance = list(absolute_smd_threshold = 0.1),
    subgroups = list(sex = TRUE, age_cutoff = 65L),
    sensitivity = list(
      risk_windows = list(c(1L, 14L), c(1L, 30L), c(1L, 60L)),
      adjustment_methods = c("matching", "weighting"),
      washout_days = c(180L, 365L)
    )
  )
}

write_temp_study_config <- function(config) {
  path <- tempfile(fileext = ".yml")
  yaml::write_yaml(config, path)
  path
}

expect_config_error <- function(config, regexp) {
  path <- write_temp_study_config(config)
  on.exit(unlink(path))

  testthat::expect_error(
    read_study_config(path),
    regexp,
    class = "study_config_validation_error"
  )
}

testthat::test_that("read_study_config returns a valid configuration", {
  path <- write_temp_study_config(valid_study_config())
  on.exit(unlink(path))

  config <- read_study_config(path)

  testthat::expect_type(config, "list")
  testthat::expect_equal(config$project$database_schema, "main")
  testthat::expect_equal(config$design$study_type, "new-user active-comparator cohort")
  testthat::expect_equal(config$propensity_score$method, "matching")
  testthat::expect_equal(config$propensity_score$trim_fraction, 0.05)
  testthat::expect_equal(config$balance$absolute_smd_threshold, 0.1)
  testthat::expect_true(config$subgroups$sex)
  testthat::expect_equal(config$subgroups$age_cutoff, 65L)
  testthat::expect_null(config$cohorts$target$concept_id)
  testthat::expect_null(config$cohorts$comparator$concept_name)
  testthat::expect_null(config$cohorts$outcome$concept_id)
})

testthat::test_that("read_study_config rejects a missing file", {
  path <- tempfile(fileext = ".yml")

  testthat::expect_error(
    read_study_config(path),
    "does not exist or is not readable",
    class = "study_config_file_error"
  )
})

testthat::test_that("read_study_config requires the exact top-level sections", {
  missing_config <- valid_study_config()
  missing_config$database <- NULL
  expect_config_error(missing_config, "missing required top-level section.*database")

  extra_config <- valid_study_config()
  extra_config$diagnostics <- list(absolute_smd_threshold = 0.1)
  expect_config_error(extra_config, "unexpected top-level section.*diagnostics")
})

testthat::test_that("read_study_config rejects an empty database schema", {
  config <- valid_study_config()
  config$project$database_schema <- ""

  expect_config_error(config, "project.database_schema.*non-empty string")
})

testthat::test_that("read_study_config rejects a negative washout", {
  config <- valid_study_config()
  config$design$washout_days <- -1L

  expect_config_error(config, "design.washout_days.*non-negative whole number")
})

testthat::test_that("read_study_config rejects a reversed design risk window", {
  config <- valid_study_config()
  config$design$risk_window_start_days <- 30L
  config$design$risk_window_end_days <- 1L

  expect_config_error(config, "risk_window_end_days.*must not be earlier")
})

testthat::test_that("read_study_config rejects an invalid random seed", {
  config <- valid_study_config()
  config$project$random_seed <- 0L

  expect_config_error(config, "project.random_seed.*positive whole number")
})

testthat::test_that("read_study_config validates the trim fraction boundaries", {
  config <- valid_study_config()
  config$propensity_score$trim_fraction <- -0.01
  expect_config_error(config, "propensity_score.trim_fraction.*between 0 and 0.5")

  config$propensity_score$trim_fraction <- 0.51
  expect_config_error(config, "propensity_score.trim_fraction.*between 0 and 0.5")

  config$propensity_score$trim_fraction <- 0.5
  path <- write_temp_study_config(config)
  on.exit(unlink(path))
  testthat::expect_no_error(read_study_config(path))
})

testthat::test_that("read_study_config rejects an invalid matching ratio", {
  config <- valid_study_config()
  config$propensity_score$matching_ratio <- 1.5

  expect_config_error(config, "propensity_score.matching_ratio.*positive whole number")
})

testthat::test_that("read_study_config rejects an invalid SMD threshold", {
  config <- valid_study_config()
  config$balance$absolute_smd_threshold <- 1.1

  expect_config_error(config, "balance.absolute_smd_threshold.*between 0 and 1")
})

testthat::test_that("read_study_config rejects an invalid subgroup age cutoff", {
  config <- valid_study_config()
  config$subgroups$age_cutoff <- 0L

  expect_config_error(config, "subgroups.age_cutoff.*positive whole number")
})

testthat::test_that("read_study_config validates sensitivity risk-window shape", {
  config <- valid_study_config()
  config$sensitivity$risk_windows[[1L]] <- c(1L, 14L, 30L)

  expect_config_error(
    config,
    "sensitivity.risk_windows\\[\\[1\\]\\].*exactly two whole numbers"
  )
})

testthat::test_that("read_study_config rejects reversed sensitivity risk windows", {
  config <- valid_study_config()
  config$sensitivity$risk_windows[[2L]] <- c(30L, 1L)

  expect_config_error(
    config,
    "sensitivity.risk_windows\\[\\[2\\]\\].*end must not be earlier"
  )
})
