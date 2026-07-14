#' Read and validate the study configuration
#'
#' Reads the study YAML configuration and validates the parameters needed before
#' the study is run. Cohort concept identifiers and names may remain `NULL` at
#' this early configuration stage.
#'
#' @param path Path to the study configuration YAML file.
#'
#' @return A validated named configuration list.
#' @export
read_study_config <- function(path = here::here("config", "study_config.yml")) {
  if (!checkmate::test_file_exists(path, access = "r")) {
    cli::cli_abort(
      "Study configuration file does not exist or is not readable: {.path {path}}.",
      class = "study_config_file_error"
    )
  }

  config <- yaml::read_yaml(path)
  if (!checkmate::test_list(config, names = "unique")) {
    cli::cli_abort(
      "Study configuration must be a named YAML mapping.",
      class = "study_config_validation_error"
    )
  }

  required_sections <- c(
    "project",
    "database",
    "design",
    "cohorts",
    "feasibility",
    "propensity_score",
    "balance",
    "subgroups",
    "sensitivity"
  )
  missing_sections <- required_sections[
    !vapply(required_sections, function(section) {
      section %in% names(config) && !is.null(config[[section]])
    }, logical(1L))
  ]

  if (length(missing_sections) > 0L) {
    cli::cli_abort(
      paste0(
        "Study configuration is missing required top-level section(s): ",
        paste(missing_sections, collapse = ", "),
        "."
      ),
      class = "study_config_validation_error"
    )
  }

  unexpected_sections <- setdiff(names(config), required_sections)
  if (length(unexpected_sections) > 0L) {
    cli::cli_abort(
      paste0(
        "Study configuration contains unexpected top-level section(s): ",
        paste(unexpected_sections, collapse = ", "),
        "."
      ),
      class = "study_config_validation_error"
    )
  }

  validate_value <- function(is_valid, field, requirement) {
    if (!isTRUE(is_valid)) {
      cli::cli_abort(
        "{.field {field}} {requirement}.",
        class = "study_config_validation_error"
      )
    }
  }

  validate_value(
    checkmate::test_string(config$project$database_schema, min.chars = 1L) &&
      nzchar(trimws(config$project$database_schema)),
    "project.database_schema",
    "must be a non-empty string"
  )
  validate_value(
    checkmate::test_string(config$design$study_type, min.chars = 1L),
    "design.study_type",
    "must be a non-empty string"
  )
  validate_value(
    checkmate::test_integerish(
      config$design$washout_days,
      lower = 0,
      len = 1L,
      any.missing = FALSE
    ),
    "design.washout_days",
    "must be a non-negative whole number"
  )
  validate_value(
    checkmate::test_integerish(
      config$design$minimum_prior_observation_days,
      lower = 0,
      len = 1L,
      any.missing = FALSE
    ),
    "design.minimum_prior_observation_days",
    "must be a non-negative whole number"
  )
  validate_value(
    checkmate::test_integerish(
      config$design$risk_window_start_days,
      len = 1L,
      any.missing = FALSE
    ),
    "design.risk_window_start_days",
    "must be a whole number"
  )
  validate_value(
    checkmate::test_integerish(
      config$design$risk_window_end_days,
      len = 1L,
      any.missing = FALSE
    ),
    "design.risk_window_end_days",
    "must be a whole number"
  )
  validate_value(
    config$design$risk_window_end_days >= config$design$risk_window_start_days,
    "design.risk_window_end_days",
    "must not be earlier than design.risk_window_start_days"
  )
  validate_value(
    checkmate::test_integerish(
      config$project$random_seed,
      lower = 1,
      len = 1L,
      any.missing = FALSE
    ),
    "project.random_seed",
    "must be a positive whole number"
  )
  validate_value(
    checkmate::test_string(config$propensity_score$method, min.chars = 1L),
    "propensity_score.method",
    "must be a non-empty string"
  )
  validate_value(
    checkmate::test_number(
      config$propensity_score$trim_fraction,
      lower = 0,
      upper = 0.5,
      finite = TRUE
    ),
    "propensity_score.trim_fraction",
    "must be between 0 and 0.5"
  )
  validate_value(
    checkmate::test_integerish(
      config$propensity_score$matching_ratio,
      lower = 1,
      len = 1L,
      any.missing = FALSE
    ),
    "propensity_score.matching_ratio",
    "must be a positive whole number"
  )
  validate_value(
    checkmate::test_number(
      config$balance$absolute_smd_threshold,
      lower = 0,
      upper = 1,
      finite = TRUE
    ),
    "balance.absolute_smd_threshold",
    "must be between 0 and 1"
  )
  validate_value(
    checkmate::test_flag(config$subgroups$sex),
    "subgroups.sex",
    "must be true or false"
  )
  validate_value(
    checkmate::test_integerish(
      config$subgroups$age_cutoff,
      lower = 1,
      len = 1L,
      any.missing = FALSE
    ),
    "subgroups.age_cutoff",
    "must be a positive whole number"
  )
  validate_value(
    checkmate::test_list(config$sensitivity$risk_windows),
    "sensitivity.risk_windows",
    "must be a list of risk windows"
  )

  for (index in seq_along(config$sensitivity$risk_windows)) {
    risk_window <- config$sensitivity$risk_windows[[index]]
    field <- paste0("sensitivity.risk_windows[[", index, "]]")

    validate_value(
      checkmate::test_integerish(
        risk_window,
        len = 2L,
        any.missing = FALSE
      ),
      field,
      "must contain exactly two whole numbers"
    )
    validate_value(
      risk_window[[2L]] >= risk_window[[1L]],
      field,
      "end must not be earlier than start"
    )
  }

  config
}
