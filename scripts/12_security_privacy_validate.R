security_project_root <- function() {
  root <- Sys.getenv(
    "PHASE18_PROJECT_ROOT",
    unset = getwd()
  )

  normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )
}

run_git <- function(
    project_root,
    arguments) {
  output <- system2(
    "git",
    c(
      "-C",
      project_root,
      arguments
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(
    output,
    "status"
  )

  if (
    !is.null(status) &&
      !identical(
        as.integer(status),
        0L
      )
  ) {
    stop(
      paste(
        output,
        collapse = "\n"
      ),
      call. = FALSE
    )
  }

  output
}

tracked_files <- function(project_root) {
  paths <- run_git(
    project_root,
    "ls-files"
  )
  paths[
    nzchar(paths)
  ]
}

tracked_symlinks <- function(project_root) {
  entries <- run_git(
    project_root,
    c(
      "ls-files",
      "-s"
    )
  )
  entries[
    grepl(
      "^120000 ",
      entries
    )
  ]
}

validate_tracked_path_policy <- function(paths) {
  normalized <- gsub(
    "\\\\",
    "/",
    paths
  )
  allowed_keeps <- c(
    "data/raw/.gitkeep",
    "data/derived/.gitkeep"
  )
  private_directories <- grepl(
    paste0(
      "^(",
      "results/private/",
      "|secrets/",
      "|credentials/",
      ")"
    ),
    normalized,
    perl = TRUE
  )
  raw_or_derived <- grepl(
    "^data/(raw|derived)/",
    normalized,
    perl = TRUE
  ) &
    !normalized %in% allowed_keeps
  prohibited_suffix <- grepl(
    paste0(
      "(",
      "\\.env",
      "|\\.pem",
      "|\\.key",
      "|\\.p12",
      "|\\.pfx",
      "|\\.jks",
      "|\\.sqlite",
      "|\\.sqlite3",
      "|\\.duckdb",
      "|\\.db",
      "|\\.rds",
      "|\\.RDS",
      ")$"
    ),
    normalized,
    perl = TRUE
  )
  prohibited <- normalized[
    private_directories |
      raw_or_derived |
      prohibited_suffix
  ]

  if (length(prohibited) > 0L) {
    stop(
      sprintf(
        "Tracked private or credential-like paths are prohibited: %s.",
        paste(
          prohibited,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

is_scannable_text_path <- function(path) {
  basename_value <- basename(path)
  extension <- tolower(
    tools::file_ext(path)
  )
  extension %in% c(
    "r",
    "rmd",
    "qmd",
    "md",
    "yml",
    "yaml",
    "json",
    "toml",
    "txt",
    "csv",
    "bib"
  ) ||
    basename_value %in% c(
      ".gitignore",
      ".lintr",
      ".Rprofile",
      "renv.lock"
    )
}

read_text_safely <- function(path) {
  size <- file.info(path)$size

  if (
    is.na(size) ||
      size > 5 * 1024 * 1024
  ) {
    return("")
  }

  bytes <- readBin(
    path,
    what = "raw",
    n = size
  )

  if (
    length(bytes) == 0L ||
      any(bytes == as.raw(0L))
  ) {
    return("")
  }

  rawToChar(bytes)
}

secret_pattern_contract <- function() {
  c(
    private_key = paste0(
      "-----BEGIN ",
      "(?:RSA |EC |OPENSSH )?",
      "PRIVATE KEY-----"
    ),
    aws_access_key = paste0(
      "\\b(?:AKIA|ASIA)",
      "[0-9A-Z]{16}\\b"
    ),
    github_token = paste0(
      "\\bgh[pousr]_[A-Za-z0-9]{30,}\\b",
      "|\\bgithub_pat_[A-Za-z0-9_]{30,}\\b"
    ),
    slack_token = paste0(
      "\\bxox[baprs]-",
      "[A-Za-z0-9-]{20,}\\b"
    ),
    google_api_key = paste0(
      "\\bAIza",
      "[0-9A-Za-z_-]{35}\\b"
    ),
    jwt = paste0(
      "\\beyJ[A-Za-z0-9_-]{10,}\\.",
      "[A-Za-z0-9_-]{10,}\\.",
      "[A-Za-z0-9_-]{10,}\\b"
    ),
    credential_url = paste0(
      "[a-z][a-z0-9+.-]*://",
      "[^[:space:]/:@]+:",
      "[^[:space:]@/]+@"
    )
  )
}

detect_secret_patterns <- function(
    text,
    artifact_name = "text") {
  patterns <- secret_pattern_contract()
  detected <- names(patterns)[
    vapply(
      patterns,
      function(pattern) {
        grepl(
          pattern,
          text,
          perl = TRUE
        )
      },
      logical(1)
    )
  ]

  if (length(detected) > 0L) {
    stop(
      sprintf(
        "Potential secret material detected in %s: %s.",
        artifact_name,
        paste(
          detected,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

validate_tracked_secret_content <- function(
    project_root,
    paths) {
  candidates <- paths[
    vapply(
      paths,
      is_scannable_text_path,
      logical(1)
    )
  ]

  for (relative_path in candidates) {
    absolute_path <- file.path(
      project_root,
      relative_path
    )

    if (!file.exists(absolute_path)) {
      next
    }

    text <- read_text_safely(
      absolute_path
    )
    detect_secret_patterns(
      text,
      artifact_name = relative_path
    )
  }

  invisible(candidates)
}

validate_gitignore_privacy <- function(project_root) {
  path <- file.path(
    project_root,
    ".gitignore"
  )
  lines <- trimws(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
  )
  required <- c(
    "data/raw/*",
    "data/derived/*",
    "results/private/",
    "*.rds",
    "*.RDS",
    ".env",
    ".env.*",
    "*.pem",
    "*.key",
    "secrets/",
    "credentials/"
  )
  missing <- setdiff(
    required,
    lines
  )

  if (length(missing) > 0L) {
    stop(
      sprintf(
        ".gitignore is missing privacy protections: %s.",
        paste(
          missing,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

collect_workflow_uses <- function(value) {
  result <- character()

  if (is.list(value)) {
    if (
      !is.null(names(value)) &&
        "uses" %in% names(value) &&
        is.character(value$uses)
    ) {
      result <- c(
        result,
        value$uses
      )
    }

    for (item in value) {
      result <- c(
        result,
        collect_workflow_uses(item)
      )
    }
  }

  unique(result)
}

validate_workflow_security <- function(project_root) {
  workflow_path <- file.path(
    project_root,
    ".github",
    "workflows",
    "ci.yml"
  )
  workflow <- yaml::read_yaml(
    workflow_path
  )
  permissions <- workflow$permissions

  if (
    !is.list(permissions) ||
      !identical(
        permissions$contents,
        "read"
      )
  ) {
    stop(
      "CI workflow must declare contents: read permissions.",
      call. = FALSE
    )
  }

  permission_values <- unlist(
    permissions,
    use.names = FALSE
  )

  if (
    any(
      grepl(
        "write",
        permission_values,
        fixed = TRUE
      )
    )
  ) {
    stop(
      "CI workflow must not request write permissions.",
      call. = FALSE
    )
  }

  uses <- collect_workflow_uses(
    workflow
  )
  floating <- uses[
    grepl(
      "@(main|master|latest)$",
      uses,
      perl = TRUE
    ) |
      !grepl(
        "@",
        uses,
        fixed = TRUE
      )
  ]

  if (length(floating) > 0L) {
    stop(
      sprintf(
        "Workflow actions must not use floating refs: %s.",
        paste(
          floating,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  invisible(uses)
}

validate_aggregate_privacy <- function(project_root) {
  environment <- new.env(
    parent = globalenv()
  )
  sys.source(
    file.path(
      project_root,
      "R",
      "plots.R"
    ),
    envir = environment
  )
  relative_paths <- unname(
    environment$reporting_table_paths()
  )

  if (length(relative_paths) != 15L) {
    stop(
      sprintf(
        "Expected exactly 15 aggregate reporting tables; found %d.",
        length(relative_paths)
      ),
      call. = FALSE
    )
  }

  tables <- environment$validate_reporting_inputs(
    project_root
  )

  for (name in names(tables)) {
    environment$validate_disclosure_safe_reporting_table(
      tables[[name]],
      artifact_name = name
    )
  }

  invisible(tables)
}

run_security_privacy_gates <- function(
    project_root = security_project_root()) {
  root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
  paths <- tracked_files(root)
  symlinks <- tracked_symlinks(root)

  if (length(symlinks) > 0L) {
    stop(
      sprintf(
        "Tracked symbolic links are prohibited: %s.",
        paste(
          symlinks,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  validate_tracked_path_policy(paths)
  validate_tracked_secret_content(
    root,
    paths
  )
  validate_gitignore_privacy(root)
  actions <- validate_workflow_security(root)
  tables <- validate_aggregate_privacy(root)

  message(
    sprintf(
      paste0(
        "Security/privacy gates passed for %d tracked files, ",
        "%d actions, and %d aggregate tables."
      ),
      length(paths),
      length(actions),
      length(tables)
    )
  )

  invisible(
    list(
      tracked_files = paths,
      workflow_actions = actions,
      aggregate_tables = tables
    )
  )
}

if (sys.nframe() == 0L) {
  run_security_privacy_gates()
}
