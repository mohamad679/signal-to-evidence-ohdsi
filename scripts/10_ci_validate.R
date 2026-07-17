project_root_default <- function() {
  candidate <- Sys.getenv(
    "PHASE18_PROJECT_ROOT",
    unset = getwd()
  )

  if (
    !is.character(candidate) ||
      length(candidate) != 1L ||
      is.na(candidate) ||
      !nzchar(candidate) ||
      !dir.exists(candidate)
  ) {
    stop(
      "PHASE18_PROJECT_ROOT must identify an existing directory.",
      call. = FALSE
    )
  }

  normalizePath(
    candidate,
    winslash = "/",
    mustWork = TRUE
  )
}

resolve_ci_mode <- function(args = commandArgs(trailingOnly = TRUE)) {
  inline <- grep(
    "^--mode=",
    args,
    value = TRUE
  )

  if (length(inline) == 1L) {
    mode <- sub(
      "^--mode=",
      "",
      inline
    )
  } else {
    position <- match(
      "--mode",
      args
    )

    if (is.na(position) || position == length(args)) {
      stop(
        "Use --mode=parse, lint, test, pipeline, security, or report.",
        call. = FALSE
      )
    }

    mode <- args[[position + 1L]]
  }

  allowed <- c(
    "parse",
    "lint",
    "test",
    "pipeline",
    "security",
    "report"
  )

  if (
    length(mode) != 1L ||
      is.na(mode) ||
      !mode %in% allowed
  ) {
    stop(
      "Use --mode=parse, lint, test, pipeline, security, or report.",
      call. = FALSE
    )
  }

  mode
}

collect_r_files <- function(project_root) {
  directories <- file.path(
    project_root,
    c(
      "R",
      "scripts",
      "tests/testthat"
    )
  )

  files <- unlist(
    lapply(
      directories,
      list.files,
      pattern = "\\.R$",
      recursive = TRUE,
      full.names = TRUE
    ),
    use.names = FALSE
  )

  files <- sort(
    unique(files)
  )

  if (length(files) == 0L) {
    stop(
      "No R source files were found.",
      call. = FALSE
    )
  }

  files
}

run_parse_validation <- function(project_root) {
  files <- collect_r_files(project_root)

  invisible(
    lapply(
      files,
      parse
    )
  )

  message(
    sprintf(
      "Parsed %d R files.",
      length(files)
    )
  )

  invisible(files)
}

run_lint_validation <- function(project_root) {
  if (!requireNamespace("lintr", quietly = TRUE)) {
    stop(
      "The lintr package is required.",
      call. = FALSE
    )
  }

  files <- collect_r_files(project_root)
  lints <- do.call(
    c,
    lapply(
      files,
      lintr::lint
    )
  )

  if (length(lints) > 0L) {
    print(lints)

    stop(
      sprintf(
        "Lint reported %d issue(s).",
        length(lints)
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "Lint passed for %d R files.",
      length(files)
    )
  )

  invisible(files)
}

discover_test_environment <- function(project_root) {
  test_files <- list.files(
    file.path(
      project_root,
      "tests",
      "testthat"
    ),
    pattern = "\\.R$",
    full.names = TRUE
  )
  lines <- unlist(
    lapply(
      test_files,
      readLines,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    use.names = FALSE
  )
  matches <- regmatches(
    lines,
    gregexpr(
      "PHASE[0-9]+_[A-Z0-9_]+",
      lines,
      perl = TRUE
    )
  )

  sort(
    unique(
      unlist(
        matches,
        use.names = FALSE
      )
    )
  )
}

configure_test_environment <- function(project_root) {
  variables <- discover_test_environment(project_root)
  quarto <- unname(
    Sys.which("quarto")
  )
  rscript <- file.path(
    R.home("bin"),
    "Rscript"
  )
  unsupported <- character()

  for (variable in variables) {
    value <- NULL

    if (grepl("_ROOT$", variable)) {
      value <- project_root
    } else if (grepl("_RSCRIPT$|_R_SCRIPT$", variable)) {
      value <- rscript
    } else if (grepl("_QUARTO_BIN$", variable)) {
      value <- quarto
    } else if (grepl("_SKIP_RENDER$", variable)) {
      value <- "true"
    } else {
      unsupported <- c(
        unsupported,
        variable
      )
    }

    if (!is.null(value)) {
      do.call(
        Sys.setenv,
        stats::setNames(
          list(value),
          variable
        )
      )
    }
  }

  if (length(unsupported) > 0L) {
    stop(
      sprintf(
        "Unsupported test environment variable(s): %s.",
        paste(
          unsupported,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(variables)
}

is_git_tracked <- function(project_root, path) {
  status <- system2(
    "git",
    c(
      "-C",
      project_root,
      "ls-files",
      "--error-unmatch",
      path
    ),
    stdout = FALSE,
    stderr = FALSE
  )

  identical(
    as.integer(status),
    0L
  )
}

cleanup_test_diagnostics <- function(project_root) {
  diagnostic <- "tests/testthat/errorReportSql.txt"
  full_path <- file.path(
    project_root,
    diagnostic
  )

  if (
    file.exists(full_path) &&
      !is_git_tracked(
        project_root,
        diagnostic
      )
  ) {
    unlink(
      full_path,
      force = TRUE
    )
  }

  invisible(full_path)
}

run_test_validation <- function(project_root) {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop(
      "The testthat package is required.",
      call. = FALSE
    )
  }

  configure_test_environment(project_root)
  on.exit(
    cleanup_test_diagnostics(project_root),
    add = TRUE
  )

  old_directory <- setwd(project_root)
  on.exit(
    setwd(old_directory),
    add = TRUE
  )

  testthat::test_dir(
    file.path(
      project_root,
      "tests",
      "testthat"
    ),
    reporter = "summary",
    stop_on_failure = TRUE
  )

  invisible(TRUE)
}

read_file_raw <- function(path) {
  if (!file.exists(path)) {
    return(raw())
  }

  readBin(
    path,
    what = "raw",
    n = file.info(path)$size
  )
}

restore_file_raw <- function(path, content, existed) {
  if (!isTRUE(existed)) {
    unlink(
      path,
      force = TRUE
    )

    return(
      invisible(path)
    )
  }

  connection <- file(
    path,
    open = "wb"
  )
  on.exit(
    close(connection),
    add = TRUE
  )
  writeBin(
    content,
    connection
  )

  invisible(path)
}

cleanup_quarto_diagnostics <- function(project_root) {
  unlink(
    file.path(
      project_root,
      ".quarto"
    ),
    recursive = TRUE,
    force = TRUE
  )

  notebooks <- list.files(
    project_root,
    pattern = "\\.quarto_ipynb$",
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  if (length(notebooks) > 0L) {
    unlink(
      notebooks,
      force = TRUE
    )
  }

  invisible(notebooks)
}

validate_rendered_reports <- function(project_root) {
  outputs <- file.path(
    project_root,
    "_site",
    "reports",
    c(
      "index.html",
      "executive_summary.html"
    )
  )

  if (
    !all(file.exists(outputs)) ||
      !all(file.info(outputs)$size > 5000)
  ) {
    stop(
      "Both rendered reports must exist and be non-empty.",
      call. = FALSE
    )
  }

  text <- lapply(
    outputs,
    readLines,
    warn = FALSE,
    encoding = "UTF-8"
  )
  required <- c(
    "Adjusted observational association under the stated design assumptions.",
    "Limitations",
    "NOT_ESTIMABLE"
  )

  for (phrase in required) {
    present <- vapply(
      text,
      function(lines) {
        any(
          grepl(
            phrase,
            lines,
            fixed = TRUE
          )
        )
      },
      logical(1)
    )

    if (!all(present)) {
      stop(
        sprintf(
          "Rendered reports are missing required text: %s",
          phrase
        ),
        call. = FALSE
      )
    }
  }

  prohibited <- paste(
    c(
      "personId",
      "rowId",
      "subjectId",
      "indexDate",
      "cohortStartDate",
      "cohortEndDate",
      "eventDate",
      "drugExposureStartDate"
    ),
    collapse = "|"
  )

  if (
    any(
      vapply(
        text,
        function(lines) {
          any(
            grepl(
              prohibited,
              lines,
              ignore.case = TRUE,
              perl = TRUE
            )
          )
        },
        logical(1)
      )
    )
  ) {
    stop(
      "Rendered reports contain prohibited person-level fields.",
      call. = FALSE
    )
  }

  study_report <- paste(
    text[[1L]],
    collapse = "\n"
  )
  embedded_images <- gregexpr(
    "data:image/(png|jpeg|svg\\+xml);base64,",
    study_report,
    perl = TRUE
  )[[1L]]
  embedded_image_count <- if (
    length(embedded_images) == 1L &&
      identical(embedded_images[[1L]], -1L)
  ) {
    0L
  } else {
    length(embedded_images)
  }

  if (embedded_image_count < 2L) {
    stop(
      sprintf(
        "Study report must contain at least two embedded figures; found %d.",
        embedded_image_count
      ),
      call. = FALSE
    )
  }

  invisible(outputs)
}

run_report_validation <- function(project_root) {
  quarto <- unname(
    Sys.which("quarto")
  )

  if (!nzchar(quarto) || !file.exists(quarto)) {
    stop(
      "Quarto executable was not found.",
      call. = FALSE
    )
  }

  gitignore <- file.path(
    project_root,
    ".gitignore"
  )
  gitignore_existed <- file.exists(gitignore)
  gitignore_content <- read_file_raw(gitignore)

  on.exit(
    restore_file_raw(
      gitignore,
      gitignore_content,
      gitignore_existed
    ),
    add = TRUE
  )
  on.exit(
    cleanup_quarto_diagnostics(project_root),
    add = TRUE
  )

  Sys.setenv(
    PHASE17_PROJECT_ROOT = project_root,
    PHASE17_IMPLEMENTATION_ROOT = project_root,
    PHASE17_QUARTO_BIN = quarto,
    PHASE17_SKIP_RENDER = "false"
  )

  environment <- new.env(
    parent = globalenv()
  )
  sys.source(
    file.path(
      project_root,
      "scripts",
      "09_export_results.R"
    ),
    envir = environment
  )

  result <- environment$run_reporting_pipeline(
    project_root = project_root,
    render = TRUE,
    quarto_bin = quarto
  )

  if (!isTRUE(result$rendered)) {
    stop(
      "Reporting pipeline did not record a completed render.",
      call. = FALSE
    )
  }

  outputs <- validate_rendered_reports(project_root)

  message(
    sprintf(
      "Validated %d rendered reports.",
      length(outputs)
    )
  )

  invisible(outputs)
}

assert_git_clean <- function(project_root) {
  status <- system2(
    "git",
    c(
      "-C",
      project_root,
      "status",
      "--porcelain=v1",
      "--untracked-files=all"
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  if (length(status) > 0L) {
    stop(
      paste(
        c(
          "CI validation modified the repository:",
          status
        ),
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

run_full_pipeline_validation <- function(project_root) {
  environment <- new.env(
    parent = globalenv()
  )
  sys.source(
    file.path(
      project_root,
      "scripts",
      "11_ci_full_pipeline.R"
    ),
    envir = environment
  )

  environment$run_full_pipeline(
    project_root = project_root
  )
}

run_security_privacy_validation <- function(project_root) {
  environment <- new.env(
    parent = globalenv()
  )
  sys.source(
    file.path(
      project_root,
      "scripts",
      "12_security_privacy_validate.R"
    ),
    envir = environment
  )

  environment$run_security_privacy_gates(
    project_root = project_root
  )
}

run_ci_validation <- function(
    mode,
    project_root = project_root_default()) {
  switch(
    mode,
    parse = run_parse_validation(project_root),
    lint = run_lint_validation(project_root),
    test = run_test_validation(project_root),
    pipeline = run_full_pipeline_validation(project_root),
    security = run_security_privacy_validation(project_root),
    report = run_report_validation(project_root)
  )

  enforce_clean <- identical(
    tolower(
      Sys.getenv(
        "CI_ENFORCE_CLEAN",
        unset = "false"
      )
    ),
    "true"
  )

  if (enforce_clean) {
    assert_git_clean(project_root)
  }

  message(
    sprintf(
      "CI mode '%s' passed.",
      mode
    )
  )

  invisible(TRUE)
}

main <- function() {
  mode <- resolve_ci_mode()

  run_ci_validation(mode)
}

if (sys.nframe() == 0L) {
  main()
}
