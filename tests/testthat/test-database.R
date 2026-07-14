database_env <- new.env(parent = globalenv())
sys.source(here::here("R", "database.R"), envir = database_env)

testthat::test_that("default Eunomia database path uses GiBleed", {
  testthat::expect_identical(
    database_env$get_eunomia_database_path(),
    here::here("data", "raw", "eunomia", "GiBleed_5.3.sqlite")
  )
})

testthat::test_that("Eunomia database paths are dataset-specific", {
  testthat::expect_identical(
    database_env$get_eunomia_database_path("SynPUF"),
    here::here("data", "raw", "eunomia", "SynPUF_5.3.sqlite")
  )
})

testthat::test_that("invalid dataset names are rejected", {
  invalid_dataset_names <- list(
    "",
    " ",
    "../GiBleed",
    "Gi/Bleed",
    "Gi.Bleed",
    NA_character_,
    c("GiBleed", "SynPUF"),
    1L
  )

  for (dataset_name in invalid_dataset_names) {
    testthat::expect_error(
      database_env$get_eunomia_database_path(dataset_name),
      class = "eunomia_argument_error"
    )
  }
})

testthat::test_that("invalid database files are rejected", {
  invalid_database_files <- list("", " ", NA_character_, c("one", "two"), 1L)

  for (database_file in invalid_database_files) {
    testthat::expect_error(
      database_env$create_eunomia_connection_details(database_file = database_file),
      class = "eunomia_argument_error"
    )
  }
})

testthat::test_that("absolute database paths outside the data directory are rejected", {
  unsafe_database_file <- fs::path_temp("GiBleed_5.3.sqlite")

  testthat::expect_error(
    database_env$create_eunomia_connection_details(
      database_file = unsafe_database_file
    ),
    class = "eunomia_argument_error"
  )
})

testthat::test_that("parent-directory traversal database paths are rejected", {
  traversal_database_file <- file.path(
    here::here("data", "raw", "eunomia"),
    "..",
    "GiBleed_5.3.sqlite"
  )

  testthat::expect_error(
    database_env$create_eunomia_connection_details(
      database_file = traversal_database_file
    ),
    class = "eunomia_argument_error"
  )
})

testthat::test_that("the Eunomia data directory is not accepted as a database file", {
  testthat::expect_error(
    database_env$create_eunomia_connection_details(
      database_file = here::here("data", "raw", "eunomia")
    ),
    class = "eunomia_argument_error"
  )
})

testthat::test_that("symbolic-link database paths cannot escape the data directory", {
  allowed_directory <- here::here("data", "raw", "eunomia")
  outside_directory <- tempfile("eunomia-symlink-escape-")
  link_path <- fs::path(
    allowed_directory,
    paste0("symlink-escape-", basename(outside_directory))
  )
  fs::dir_create(outside_directory)
  on.exit(fs::dir_delete(outside_directory), add = TRUE)

  link_created <- tryCatch(
    {
      fs::link_create(outside_directory, link_path, symbolic = TRUE)
      TRUE
    },
    error = function(error) FALSE
  )
  testthat::skip_if_not(
    link_created,
    "Symbolic-link creation is unsupported on this system."
  )
  on.exit(
    if (fs::link_exists(link_path)) {
      fs::link_delete(link_path)
    },
    add = TRUE
  )

  testthat::expect_error(
    database_env$create_eunomia_connection_details(
      database_file = fs::path(link_path, "GiBleed_5.3.sqlite")
    ),
    class = "eunomia_argument_error"
  )
  testthat::expect_false(
    fs::file_exists(fs::path(outside_directory, "GiBleed_5.3.sqlite"))
  )
})

testthat::test_that("datasets cannot reuse another dataset filename", {
  testthat::expect_error(
    database_env$create_eunomia_connection_details(
      dataset_name = "SynPUF",
      database_file = database_env$get_eunomia_database_path("GiBleed")
    ),
    class = "eunomia_argument_error"
  )
})

testthat::test_that("an existing safe database file is reused", {
  calls <- new.env(parent = emptyenv())
  dataset_name <- paste0("ExistingSafeTest", Sys.getpid())
  database_file <- database_env$get_eunomia_database_path(dataset_name)
  original_contents <- charToRaw("existing database contents")
  fs::dir_create(dirname(database_file))
  writeBin(original_contents, database_file)
  on.exit(
    if (fs::file_exists(database_file)) {
      fs::file_delete(database_file)
    },
    add = TRUE
  )

  testthat::local_mocked_bindings(
    getEunomiaConnectionDetails = function(...) {
      stop("download attempted")
    },
    getDatabaseFile = function(...) {
      stop("download attempted")
    },
    .package = "Eunomia"
  )
  testthat::local_mocked_bindings(
    createConnectionDetails = function(...) {
      calls$create_connection_details <- list(...)
      calls$create_connection_details
    },
    .package = "DatabaseConnector"
  )

  connection_details <- database_env$create_eunomia_connection_details(
    dataset_name = dataset_name,
    database_file = database_file
  )

  testthat::expect_identical(connection_details$dbms, "sqlite")
  testthat::expect_identical(connection_details$server, database_file)
  testthat::expect_identical(readBin(database_file, "raw", n = 100L), original_contents)
})

testthat::test_that("non-GiBleed datasets use the generic Eunomia download", {
  calls <- new.env(parent = emptyenv())
  database_file <- database_env$get_eunomia_database_path("SynPUF")
  cache_directory <- here::here("data", "raw", "eunomia", ".cache")
  cache_existed <- fs::dir_exists(cache_directory)

  on.exit(
    if (!cache_existed && fs::dir_exists(cache_directory)) {
      fs::dir_delete(cache_directory)
    },
    add = TRUE
  )

  testthat::local_mocked_bindings(
    getDatabaseFile = function(...) {
      calls$get_database_file <- list(...)
      calls$get_database_file$databaseFile
    },
    .package = "Eunomia"
  )
  testthat::local_mocked_bindings(
    createConnectionDetails = function(...) {
      calls$create_connection_details <- list(...)
      calls$create_connection_details
    },
    .package = "DatabaseConnector"
  )

  connection_details <- database_env$create_eunomia_connection_details(
    dataset_name = "SynPUF"
  )

  testthat::expect_identical(
    calls$get_database_file$datasetName,
    "SynPUF"
  )
  testthat::expect_identical(
    calls$get_database_file$databaseFile,
    database_file
  )
  testthat::expect_identical(connection_details$dbms, "sqlite")
  testthat::expect_identical(connection_details$server, database_file)
})

testthat::test_that("non-GiBleed download cache differs from final database", {
  calls <- new.env(parent = emptyenv())
  dataset_name <- paste0("SeparateCacheTest", Sys.getpid())
  database_file <- database_env$get_eunomia_database_path(dataset_name)
  cache_directory <- here::here("data", "raw", "eunomia", ".cache")
  cache_existed <- fs::dir_exists(cache_directory)

  on.exit(
    {
      if (fs::file_exists(database_file)) {
        fs::file_delete(database_file)
      }
      if (!cache_existed && fs::dir_exists(cache_directory)) {
        fs::dir_delete(cache_directory)
      }
    },
    add = TRUE
  )

  testthat::local_mocked_bindings(
    getDatabaseFile = function(...) {
      calls$get_database_file <- list(...)
      calls$get_database_file$databaseFile
    },
    .package = "Eunomia"
  )
  testthat::local_mocked_bindings(
    createConnectionDetails = function(...) {
      calls$create_connection_details <- list(...)
      calls$create_connection_details
    },
    .package = "DatabaseConnector"
  )

  connection_details <- database_env$create_eunomia_connection_details(
    dataset_name = dataset_name
  )
  cache_source_file <- database_env$canonicalize_path(
    fs::path(cache_directory, basename(database_file))
  )
  final_database_file <- database_env$canonicalize_path(database_file)

  testthat::expect_identical(
    calls$get_database_file$pathToData,
    cache_directory
  )
  testthat::expect_identical(
    calls$get_database_file$databaseFile,
    database_file
  )
  testthat::expect_false(identical(cache_source_file, final_database_file))
  testthat::expect_identical(connection_details$server, database_file)
})

testthat::test_that("empty database schemas are rejected", {
  for (database_schema in c("", " ")) {
    testthat::expect_error(
      database_env$validate_required_omop_tables(
        connection = NULL,
        database_schema = database_schema
      ),
      class = "eunomia_argument_error"
    )
  }
})

testthat::test_that("missing database schemas are rejected", {
  for (database_schema in list(NULL, NA_character_)) {
    testthat::expect_error(
      database_env$validate_required_omop_tables(
        connection = NULL,
        database_schema = database_schema
      ),
      class = "eunomia_argument_error"
    )
  }
})

testthat::test_that("project Eunomia database passes required table validation", {
  connection <- database_env$connect_eunomia()
  on.exit(database_env$disconnect_safely(connection), add = TRUE)

  table_names <- DatabaseConnector::getTableNames(
    connection = connection,
    databaseSchema = "main"
  )

  testthat::expect_setequal(
    database_env$get_required_omop_tables(),
    intersect(database_env$get_required_omop_tables(), tolower(table_names))
  )

  testthat::expect_invisible(
    database_env$validate_required_omop_tables(connection, database_schema = "main")
  )

  original_helper <- database_env$get_required_omop_tables
  on.exit(
    assign(
      "get_required_omop_tables",
      original_helper,
      envir = database_env
    ),
    add = TRUE
  )
  replacement_helper <- (function(helper) {
    function() c(helper(), "deliberately_nonexistent_table")
  })(original_helper)
  assign(
    "get_required_omop_tables",
    replacement_helper,
    envir = database_env
  )

  testthat::expect_error(
    database_env$validate_required_omop_tables(connection, database_schema = "main"),
    "deliberately_nonexistent_table",
    class = "missing_omop_tables_error"
  )
})
