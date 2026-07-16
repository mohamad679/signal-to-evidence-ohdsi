project_root_default <- function() {
  root <- Sys.getenv("PHASE17_PROJECT_ROOT", unset = getwd())

  normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )
}

resolve_quarto_binary <- function(quarto_bin = NULL) {
  if (
    is.character(quarto_bin) &&
      length(quarto_bin) == 1L &&
      !is.na(quarto_bin) &&
      nzchar(quarto_bin)
  ) {
    return(quarto_bin)
  }

  configured <- Sys.getenv("PHASE17_QUARTO_BIN", unset = "")

  if (nzchar(configured)) {
    return(configured)
  }

  unname(Sys.which("quarto"))
}

validate_reporting_sources <- function(project_root) {
  required <- c(
    "_quarto.yml",
    "reports/index.qmd",
    "reports/executive_summary.qmd",
    "reports/references.bib",
    "R/plots.R"
  )
  missing <- required[
    !file.exists(file.path(project_root, required))
  ]

  if (length(missing) > 0L) {
    stop(
      sprintf(
        "Required reporting source files are missing: %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(required)
}

run_reporting_pipeline <- function(
    project_root = project_root_default(),
    render = !identical(
      tolower(Sys.getenv("PHASE17_SKIP_RENDER", unset = "false")),
      "true"
    ),
    quarto_bin = NULL) {
  valid_root <- is.character(project_root) &&
    length(project_root) == 1L &&
    !is.na(project_root) &&
    nzchar(project_root) &&
    dir.exists(project_root)

  if (!valid_root) {
    stop(
      "`project_root` must identify an existing directory.",
      call. = FALSE
    )
  }

  root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  source(
    file.path(root, "R", "plots.R"),
    local = FALSE
  )

  input_validator <- get(
    "validate_reporting_inputs",
    mode = "function"
  )

  validate_reporting_sources(root)
  inputs <- input_validator(root)

  if (!isTRUE(render)) {
    return(
      invisible(
        list(
          project_root = root,
          reporting_inputs = inputs,
          rendered = FALSE
        )
      )
    )
  }

  binary <- resolve_quarto_binary(quarto_bin)

  if (!nzchar(binary) || !file.exists(binary)) {
    stop(
      "Quarto executable was not found.",
      call. = FALSE
    )
  }

  old_directory <- setwd(root)
  on.exit(setwd(old_directory), add = TRUE)

  output <- system2(
    binary,
    args = "render",
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")

  if (is.null(status)) {
    status <- 0L
  }

  if (!identical(as.integer(status), 0L)) {
    stop(
      paste(
        c("Quarto render failed.", output),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  expected_outputs <- file.path(
    root,
    "_site",
    "reports",
    c("index.html", "executive_summary.html")
  )

  if (!all(file.exists(expected_outputs))) {
    stop(
      "Quarto completed without producing both expected HTML reports.",
      call. = FALSE
    )
  }

  invisible(
    list(
      project_root = root,
      reporting_inputs = inputs,
      rendered = TRUE,
      outputs = expected_outputs,
      quarto_output = output
    )
  )
}

if (sys.nframe() == 0L) {
  run_reporting_pipeline()
}
