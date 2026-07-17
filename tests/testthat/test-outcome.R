source(
  testthat::test_path("..", "..", "R", "outcome.R"),
  local = FALSE
)

make_valid_matched_population <- function() {
  data.frame(
    rowId = c(101L, 102L, 201L, 202L),
    treatment = c(1, 1, 0, 0),
    propensityScore = c(0.61, 0.57, 0.59, 0.55),
    preferenceScore = c(0.53, 0.51, 0.52, 0.50),
    matchId = c(1L, 2L, 1L, 2L)
  )
}

testthat::test_that(
  "matched population validation accepts valid pairs",
  {
    matched_population <- make_valid_matched_population()

    testthat::expect_invisible(
      validate_matched_population(matched_population)
    )
  }
)

testthat::test_that(
  "matched population validation enforces the privacy schema",
  {
    matched_population <- make_valid_matched_population()
    matched_population$personId <- c(1L, 2L, 3L, 4L)

    testthat::expect_error(
      validate_matched_population(matched_population),
      "must contain exactly these columns"
    )
  }
)

testthat::test_that(
  "matched population validation rejects invalid matched pairs",
  {
    matched_population <- make_valid_matched_population()
    matched_population$treatment[3] <- 1

    testthat::expect_error(
      validate_matched_population(matched_population),
      "exactly one target and one comparator"
    )
  }
)

testthat::test_that(
  "matched population validation rejects invalid scores",
  {
    matched_population <- make_valid_matched_population()
    matched_population$propensityScore[1] <- Inf

    testthat::expect_error(
      validate_matched_population(matched_population),
      "finite numeric values"
    )
  }
)

testthat::test_that(
  "matched population loader validates an RDS artifact",
  {
    matched_population <- make_valid_matched_population()
    artifact <- tempfile(fileext = ".rds")
    on.exit(unlink(artifact), add = TRUE)

    saveRDS(matched_population, artifact)

    testthat::expect_identical(
      load_matched_population(artifact),
      matched_population
    )
  }
)

testthat::test_that(
  "matched population loader rejects a missing artifact",
  {
    missing_artifact <- tempfile(fileext = ".rds")

    testthat::expect_error(
      load_matched_population(missing_artifact),
      "does not exist"
    )
  }
)

make_valid_treatment_population <- function() {
  data.frame(
    rowId = c(101L, 102L, 201L, 202L),
    subjectId = c(1001L, 1002L, 1003L, 1004L),
    cohortStartDate = as.Date(rep("2020-01-01", 4L)),
    cohortEndDate = as.Date(rep("2020-01-30", 4L)),
    treatment = c(1, 1, 0, 0)
  )
}

make_risk_window_outcome_cohort <- function() {
  outcome_dates <- as.Date(
    c(
      "2020-01-02",
      "2020-01-31",
      "2020-01-01",
      "2020-02-01"
    )
  )

  data.frame(
    subjectId = c(1001L, 1002L, 1003L, 1004L),
    cohortStartDate = outcome_dates,
    cohortEndDate = outcome_dates
  )
}

testthat::test_that(
  "matched outcome population applies the inclusive risk window",
  {
    result <- build_matched_outcome_population(
      matched_population = make_valid_matched_population(),
      treatment_population = make_valid_treatment_population(),
      outcome_cohort = make_risk_window_outcome_cohort(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$outcome,
      c(1L, 1L, 0L, 0L)
    )
  }
)

testthat::test_that(
  "matched outcome population exports no subject identifiers or dates",
  {
    result <- build_matched_outcome_population(
      matched_population = make_valid_matched_population(),
      treatment_population = make_valid_treatment_population(),
      outcome_cohort = make_risk_window_outcome_cohort(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      names(result),
      c("rowId", "treatment", "matchId", "outcome")
    )

    testthat::expect_false(
      any(grepl(
        "subject|person|date",
        names(result),
        ignore.case = TRUE
      ))
    )
  }
)

testthat::test_that(
  "matched outcome population permits an empty outcome cohort",
  {
    empty_outcome_cohort <- data.frame(
      subjectId = integer(),
      cohortStartDate = as.Date(character()),
      cohortEndDate = as.Date(character())
    )

    result <- build_matched_outcome_population(
      matched_population = make_valid_matched_population(),
      treatment_population = make_valid_treatment_population(),
      outcome_cohort = empty_outcome_cohort,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$outcome,
      rep(0L, 4L)
    )
  }
)

testthat::test_that(
  "matched outcome population collapses repeated episodes to binary",
  {
    outcome_cohort <- make_risk_window_outcome_cohort()

    repeated_episode <- data.frame(
      subjectId = 1001L,
      cohortStartDate = as.Date("2020-01-10"),
      cohortEndDate = as.Date("2020-01-10")
    )

    outcome_cohort <- rbind(outcome_cohort, repeated_episode)

    result <- build_matched_outcome_population(
      matched_population = make_valid_matched_population(),
      treatment_population = make_valid_treatment_population(),
      outcome_cohort = outcome_cohort,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(result$outcome[[1]], 1L)
    testthat::expect_identical(sum(result$outcome), 2L)
  }
)

testthat::test_that(
  "matched outcome population requires complete row linkage",
  {
    treatment_population <- make_valid_treatment_population()
    treatment_population <- treatment_population[-4L, , drop = FALSE]

    testthat::expect_error(
      build_matched_outcome_population(
        matched_population = make_valid_matched_population(),
        treatment_population = treatment_population,
        outcome_cohort = make_risk_window_outcome_cohort(),
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ),
      "Every matched `rowId` must exist"
    )
  }
)

testthat::test_that(
  "matched outcome population rejects treatment disagreement",
  {
    treatment_population <- make_valid_treatment_population()
    treatment_population$treatment[[1]] <- 0

    testthat::expect_error(
      build_matched_outcome_population(
        matched_population = make_valid_matched_population(),
        treatment_population = treatment_population,
        outcome_cohort = make_risk_window_outcome_cohort(),
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ),
      "Treatment assignments disagree"
    )
  }
)

testthat::test_that(
  "matched outcome population rejects a reversed risk window",
  {
    testthat::expect_error(
      build_matched_outcome_population(
        matched_population = make_valid_matched_population(),
        treatment_population = make_valid_treatment_population(),
        outcome_cohort = make_risk_window_outcome_cohort(),
        risk_window_start_days = 30L,
        risk_window_end_days = 1L
      ),
      "must not precede"
    )
  }
)

make_valid_feature_table_rows <- function() {
  data.frame(
    row_id = c(101L, 102L, 201L, 202L),
    subject_id = c(1001L, 1002L, 1003L, 1004L),
    cohort_start_date = as.Date(rep("2020-01-01", 4L)),
    cohort_end_date = as.Date(rep("2020-01-30", 4L)),
    treatment = c(1, 1, 0, 0)
  )
}

testthat::test_that(
  "feature-table rows create matched treatment linkage",
  {
    result <- build_treatment_from_feature_rows(
      feature_rows = make_valid_feature_table_rows(),
      matched_population = make_valid_matched_population()
    )

    testthat::expect_identical(
      names(result),
      c(
        "rowId",
        "subjectId",
        "cohortStartDate",
        "cohortEndDate",
        "treatment"
      )
    )

    testthat::expect_identical(
      result$rowId,
      make_valid_matched_population()$rowId
    )

    testthat::expect_identical(
      result$treatment,
      c(1, 1, 0, 0)
    )
  }
)

testthat::test_that(
  "feature-table adapter restores matched row order",
  {
    feature_rows <- make_valid_feature_table_rows()
    feature_rows <- feature_rows[c(4L, 2L, 1L, 3L), , drop = FALSE]

    result <- build_treatment_from_feature_rows(
      feature_rows,
      make_valid_matched_population()
    )

    testthat::expect_identical(
      result$subjectId,
      c(1001L, 1002L, 1003L, 1004L)
    )
  }
)

testthat::test_that(
  "feature-table adapter accepts additional source columns",
  {
    feature_rows <- make_valid_feature_table_rows()
    feature_rows$cohort_definition_id <- 999L

    result <- build_treatment_from_feature_rows(
      feature_rows,
      make_valid_matched_population()
    )

    testthat::expect_identical(nrow(result), 4L)
    testthat::expect_false(
      "cohort_definition_id" %in% names(result)
    )
  }
)

testthat::test_that(
  "feature-table adapter requires complete row linkage",
  {
    feature_rows <- make_valid_feature_table_rows()
    feature_rows <- feature_rows[-4L, , drop = FALSE]

    testthat::expect_error(
      build_treatment_from_feature_rows(
        feature_rows,
        make_valid_matched_population()
      ),
      "Every matched `rowId` must exist"
    )
  }
)

testthat::test_that(
  "feature-table adapter rejects duplicate row identifiers",
  {
    feature_rows <- make_valid_feature_table_rows()
    feature_rows$row_id[[2]] <- feature_rows$row_id[[1]]

    testthat::expect_error(
      build_treatment_from_feature_rows(
        feature_rows,
        make_valid_matched_population()
      ),
      "must be unique"
    )
  }
)

testthat::test_that(
  "feature-table adapter rejects treatment disagreement",
  {
    feature_rows <- make_valid_feature_table_rows()
    feature_rows$treatment[[1]] <- 0

    testthat::expect_error(
      build_treatment_from_feature_rows(
        feature_rows,
        make_valid_matched_population()
      ),
      "Treatment assignments disagree"
    )
  }
)

testthat::test_that(
  "cohort-table loader returns matched treatment linkage",
  {
    calls <- new.env(parent = emptyenv())
    calls$created <- FALSE
    calls$dropped <- FALSE
    calls$queried <- FALSE

    create_table <- function(
        connection,
        cohort_tables,
        table_name) {
      calls$created <- identical(connection, "connection") &&
        identical(cohort_tables, list(target = "t", comparator = "c")) &&
        identical(table_name, "temporary_feature_table")
    }

    drop_table <- function(connection, table_name) {
      calls$dropped <- identical(connection, "connection") &&
        identical(table_name, "temporary_feature_table")
    }

    query_rows <- function(connection, table_name) {
      calls$queried <- identical(connection, "connection") &&
        identical(table_name, "temporary_feature_table")

      make_valid_feature_table_rows()
    }

    result <- load_treatment_from_cohort_tables(
      connection = "connection",
      cohort_tables = list(target = "t", comparator = "c"),
      matched_population = make_valid_matched_population(),
      table_name = "temporary_feature_table",
      create_table = create_table,
      drop_table = drop_table,
      query_rows = query_rows
    )

    testthat::expect_true(calls$created)
    testthat::expect_true(calls$queried)
    testthat::expect_true(calls$dropped)
    testthat::expect_identical(nrow(result), 4L)
    testthat::expect_identical(
      result$rowId,
      make_valid_matched_population()$rowId
    )
  }
)

testthat::test_that(
  "cohort-table loader drops its table after query failure",
  {
    calls <- new.env(parent = emptyenv())
    calls$dropped <- FALSE

    create_table <- function(
        connection,
        cohort_tables,
        table_name) {
      invisible(NULL)
    }

    drop_table <- function(connection, table_name) {
      calls$dropped <- TRUE
      invisible(NULL)
    }

    query_rows <- function(connection, table_name) {
      stop("synthetic query failure", call. = FALSE)
    }

    testthat::expect_error(
      load_treatment_from_cohort_tables(
        connection = "connection",
        cohort_tables = list(target = "t", comparator = "c"),
        matched_population = make_valid_matched_population(),
        table_name = "temporary_feature_table",
        create_table = create_table,
        drop_table = drop_table,
        query_rows = query_rows
      ),
      "synthetic query failure"
    )

    testthat::expect_true(calls$dropped)
  }
)

testthat::test_that(
  "cohort-table loader validates callback functions",
  {
    testthat::expect_error(
      load_treatment_from_cohort_tables(
        connection = "connection",
        cohort_tables = list(target = "t", comparator = "c"),
        matched_population = make_valid_matched_population(),
        table_name = "temporary_feature_table",
        create_table = "not a function",
        drop_table = function(...) invisible(NULL),
        query_rows = function(...) make_valid_feature_table_rows()
      ),
      "callbacks must be functions"
    )
  }
)

testthat::test_that(
  "cohort-table loader rejects an empty table name",
  {
    testthat::expect_error(
      load_treatment_from_cohort_tables(
        connection = "connection",
        cohort_tables = list(target = "t", comparator = "c"),
        matched_population = make_valid_matched_population(),
        table_name = ""
      ),
      "one non-empty character value"
    )
  }
)

make_valid_outcome_table_rows <- function() {
  data.frame(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1001L, 1002L),
    cohort_start_date = as.Date(
      c("2020-01-02", "2020-01-31")
    ),
    cohort_end_date = as.Date(
      c("2020-01-02", "2020-01-31")
    )
  )
}

testthat::test_that(
  "outcome rows normalize to the internal cohort schema",
  {
    result <- normalize_outcome_cohort_rows(
      make_valid_outcome_table_rows()
    )

    testthat::expect_identical(
      names(result),
      c(
        "subjectId",
        "cohortStartDate",
        "cohortEndDate"
      )
    )

    testthat::expect_identical(
      result$subjectId,
      c(1001L, 1002L)
    )

    testthat::expect_false(
      "cohort_definition_id" %in% names(result)
    )
  }
)

testthat::test_that(
  "outcome row normalization accepts character dates",
  {
    outcome_rows <- make_valid_outcome_table_rows()
    outcome_rows$cohort_start_date <- as.character(
      outcome_rows$cohort_start_date
    )
    outcome_rows$cohort_end_date <- as.character(
      outcome_rows$cohort_end_date
    )

    result <- normalize_outcome_cohort_rows(outcome_rows)

    testthat::expect_s3_class(result$cohortStartDate, "Date")
    testthat::expect_s3_class(result$cohortEndDate, "Date")
  }
)

testthat::test_that(
  "outcome row normalization supports an empty cohort",
  {
    outcome_rows <- data.frame(
      cohort_definition_id = integer(),
      subject_id = integer(),
      cohort_start_date = as.Date(character()),
      cohort_end_date = as.Date(character())
    )

    result <- normalize_outcome_cohort_rows(outcome_rows)

    testthat::expect_identical(nrow(result), 0L)
  }
)

testthat::test_that(
  "outcome row normalization rejects invalid dates",
  {
    outcome_rows <- make_valid_outcome_table_rows()
    outcome_rows$cohort_start_date <- c(
      "not-a-date",
      "2020-01-31"
    )

    testthat::expect_error(
      normalize_outcome_cohort_rows(outcome_rows),
      "invalid or missing dates"
    )
  }
)

testthat::test_that(
  "outcome-table loader queries the configured table",
  {
    calls <- new.env(parent = emptyenv())
    calls$queried <- FALSE

    query_rows <- function(connection, outcome_table) {
      calls$queried <- identical(connection, "connection") &&
        identical(outcome_table, "study_outcome_cohort")

      make_valid_outcome_table_rows()
    }

    result <- load_outcome_from_cohort_table(
      connection = "connection",
      outcome_table = "study_outcome_cohort",
      query_rows = query_rows
    )

    testthat::expect_true(calls$queried)
    testthat::expect_identical(nrow(result), 2L)
  }
)

testthat::test_that(
  "outcome-table loader rejects unsafe table names",
  {
    testthat::expect_error(
      load_outcome_from_cohort_table(
        connection = "connection",
        outcome_table = "outcome; DROP TABLE person",
        query_rows = function(...) {
          make_valid_outcome_table_rows()
        }
      ),
      "unsupported characters"
    )
  }
)

testthat::test_that(
  "loaded outcome rows integrate with the risk window",
  {
    outcome_cohort <- load_outcome_from_cohort_table(
      connection = "connection",
      outcome_table = "study_outcome_cohort",
      query_rows = function(...) {
        make_valid_outcome_table_rows()
      }
    )

    result <- build_matched_outcome_population(
      matched_population = make_valid_matched_population(),
      treatment_population = make_valid_treatment_population(),
      outcome_cohort = outcome_cohort,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$outcome,
      c(1L, 1L, 0L, 0L)
    )
  }
)

make_outcome_cohort_tables <- function() {
  list(
    target = "target_cohort",
    comparator = "comparator_cohort",
    outcome = "outcome_cohort"
  )
}

testthat::test_that(
  "outcome orchestration returns matched analysis rows",
  {
    calls <- new.env(parent = emptyenv())
    calls$dropped <- FALSE

    result <- build_matched_outcome_from_tables(
      connection = "connection",
      cohort_tables = make_outcome_cohort_tables(),
      matched_population = make_valid_matched_population(),
      feature_table_name = "temporary_feature_table",
      risk_window_start_days = 1L,
      risk_window_end_days = 30L,
      create_table = function(...) {
        invisible(NULL)
      },
      drop_table = function(...) {
        calls$dropped <- TRUE
        invisible(NULL)
      },
      treatment_query = function(...) {
        make_valid_feature_table_rows()
      },
      outcome_query = function(...) {
        make_valid_outcome_table_rows()
      }
    )

    testthat::expect_true(calls$dropped)

    testthat::expect_identical(
      names(result),
      c(
        "rowId",
        "treatment",
        "matchId",
        "outcome"
      )
    )

    testthat::expect_identical(
      result$outcome,
      c(1L, 1L, 0L, 0L)
    )
  }
)

testthat::test_that(
  "matched outcome summary returns aggregate counts",
  {
    analysis_population <- data.frame(
      rowId = c(101L, 102L, 201L, 202L),
      treatment = c(1, 1, 0, 0),
      matchId = c(1L, 2L, 1L, 2L),
      outcome = c(1L, 0L, 0L, 1L)
    )

    result <- summarize_matched_outcomes(
      analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      names(result),
      c(
        "group",
        "treatment",
        "subjectCount",
        "eventCount",
        "riskWindowStartDays",
        "riskWindowEndDays"
      )
    )

    testthat::expect_identical(
      result$subjectCount,
      c(2L, 2L)
    )

    testthat::expect_identical(
      result$eventCount,
      c(1L, 1L)
    )
  }
)

testthat::test_that(
  "matched outcome summary excludes person-level columns",
  {
    analysis_population <- data.frame(
      rowId = c(101L, 102L, 201L, 202L),
      treatment = c(1, 1, 0, 0),
      matchId = c(1L, 2L, 1L, 2L),
      outcome = c(1L, 0L, 0L, 1L)
    )

    result <- summarize_matched_outcomes(
      analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    prohibited_columns <- c(
      "rowId",
      "matchId",
      "subjectId",
      "personId",
      "cohortStartDate",
      "cohortEndDate"
    )

    testthat::expect_length(
      intersect(names(result), prohibited_columns),
      0L
    )
  }
)

testthat::test_that(
  "matched outcome summary validates matched sets",
  {
    analysis_population <- data.frame(
      rowId = c(101L, 102L, 201L, 202L),
      treatment = c(1, 1, 1, 0),
      matchId = c(1L, 2L, 1L, 2L),
      outcome = c(1L, 0L, 0L, 1L)
    )

    testthat::expect_error(
      summarize_matched_outcomes(
        analysis_population,
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ),
      "one target and one comparator"
    )
  }
)

testthat::test_that(
  "matched outcome summary remains deterministic",
  {
    analysis_population <- data.frame(
      rowId = c(101L, 102L, 201L, 202L),
      treatment = c(1, 1, 0, 0),
      matchId = c(1L, 2L, 1L, 2L),
      outcome = c(1L, 0L, 0L, 1L)
    )

    first <- summarize_matched_outcomes(
      analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    second <- summarize_matched_outcomes(
      analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(first, second)
  }
)

make_model_analysis_population <- function() {
  target_outcome <- c(
    1L, 1L, 1L, 1L, 1L,
    1L, 0L, 0L, 0L, 0L
  )

  comparator_outcome <- c(
    1L, 1L, 1L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L
  )

  data.frame(
    rowId = seq_len(20L),
    treatment = c(
      rep(1, 10L),
      rep(0, 10L)
    ),
    matchId = c(
      seq_len(10L),
      seq_len(10L)
    ),
    outcome = c(
      target_outcome,
      comparator_outcome
    )
  )
}

testthat::test_that(
  "matched outcome model returns the protocol odds ratio",
  {
    result <- fit_matched_outcome_model(
      analysis_population =
        make_model_analysis_population(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(
      result$effectMeasure,
      "odds ratio"
    )

    testthat::expect_equal(
      result$estimate,
      3.5,
      tolerance = sqrt(.Machine$double.eps)
    )

    testthat::expect_true(
      result$ciLower < result$estimate
    )

    testthat::expect_true(
      result$ciUpper > result$estimate
    )

    testthat::expect_identical(
      result$confidenceLevel,
      0.95
    )
  }
)

testthat::test_that(
  "matched outcome model reports aggregate diagnostics",
  {
    result <- fit_matched_outcome_model(
      analysis_population =
        make_model_analysis_population(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(result$subjectCount, 20L)
    testthat::expect_identical(result$eventCount, 9L)
    testthat::expect_identical(result$targetEventCount, 6L)
    testthat::expect_identical(result$comparatorEventCount, 3L)
    testthat::expect_identical(result$matchedSetCount, 10L)

    testthat::expect_identical(
      result$varianceEstimator,
      "matched-set cluster-robust CR1"
    )

    testthat::expect_true(result$modelConverged)
    testthat::expect_false(result$zeroCellDetected)
  }
)

testthat::test_that(
  "matched outcome model rejects separated data",
  {
    analysis_population <- data.frame(
      rowId = seq_len(8L),
      treatment = c(
        rep(1, 4L),
        rep(0, 4L)
      ),
      matchId = c(
        seq_len(4L),
        seq_len(4L)
      ),
      outcome = c(
        rep(1L, 4L),
        rep(0L, 4L)
      )
    )

    testthat::expect_error(
      fit_matched_outcome_model(
        analysis_population = analysis_population,
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ),
      "zero cell"
    )
  }
)

testthat::test_that(
  "matched outcome model requires two matched sets",
  {
    analysis_population <- data.frame(
      rowId = c(1L, 2L),
      treatment = c(1, 0),
      matchId = c(1L, 1L),
      outcome = c(1L, 0L)
    )

    testthat::expect_error(
      fit_matched_outcome_model(
        analysis_population = analysis_population,
        risk_window_start_days = 1L,
        risk_window_end_days = 30L
      ),
      "At least two matched sets"
    )
  }
)

testthat::test_that(
  "matched outcome model validates confidence level",
  {
    testthat::expect_error(
      fit_matched_outcome_model(
        analysis_population =
          make_model_analysis_population(),
        risk_window_start_days = 1L,
        risk_window_end_days = 30L,
        confidence_level = 1
      ),
      "strictly between 0 and 1"
    )
  }
)

testthat::test_that(
  "matched outcome model exports aggregate fields only",
  {
    result <- fit_matched_outcome_model(
      analysis_population =
        make_model_analysis_population(),
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    prohibited_columns <- c(
      "rowId",
      "matchId",
      "subjectId",
      "personId",
      "cohortStartDate",
      "cohortEndDate"
    )

    testthat::expect_length(
      intersect(names(result), prohibited_columns),
      0L
    )

    testthat::expect_identical(nrow(result), 1L)

    testthat::expect_identical(
      result$interpretation,
      paste(
        "Adjusted observational association",
        "under the stated design assumptions."
      )
    )
  }
)

testthat::test_that(
  "matched outcome model is deterministic",
  {
    analysis_population <-
      make_model_analysis_population()

    first <- fit_matched_outcome_model(
      analysis_population = analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    second <- fit_matched_outcome_model(
      analysis_population = analysis_population,
      risk_window_start_days = 1L,
      risk_window_end_days = 30L
    )

    testthat::expect_identical(first, second)
  }
)

testthat::test_that(
  "feature cohort ids map to validated treatment groups",
  {
    feature_rows <- data.frame(
      row_id = c(1L, 2L),
      cohort_definition_id = c(1L, 2L),
      subject_id = c(101L, 202L),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      ))
    )

    result <- normalize_feature_treatment_rows(
      feature_rows
    )

    testthat::expect_identical(
      result$treatment,
      c(1, 0)
    )

    testthat::expect_false(
      "cohort_definition_id" %in% names(result)
    )
  }
)

testthat::test_that(
  "feature cohort normalization preserves treatment schema",
  {
    feature_rows <- data.frame(
      row_id = c(1L, 2L),
      subject_id = c(101L, 202L),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      )),
      treatment = c(1, 0)
    )

    testthat::expect_identical(
      normalize_feature_treatment_rows(
        feature_rows
      ),
      feature_rows
    )
  }
)

testthat::test_that(
  "feature cohort normalization rejects unknown ids",
  {
    feature_rows <- data.frame(
      row_id = 1L,
      cohort_definition_id = 3L,
      subject_id = 101L,
      cohort_start_date = as.Date("2020-01-01"),
      cohort_end_date = as.Date("2020-01-01")
    )

    testthat::expect_error(
      normalize_feature_treatment_rows(
        feature_rows
      ),
      "Only cohort_definition_id 1"
    )
  }
)

testthat::test_that(
  "feature cohort normalization accepts database column order",
  {
    feature_rows <- data.frame(
      row_id = c(1L, 2L),
      subject_id = c(101L, 202L),
      cohort_start_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      )),
      cohort_end_date = as.Date(c(
        "2020-01-01",
        "2020-02-01"
      )),
      cohort_definition_id = c(1L, 2L)
    )

    result <- normalize_feature_treatment_rows(
      feature_rows
    )

    testthat::expect_identical(
      result$treatment,
      c(1, 0)
    )

    testthat::expect_identical(
      names(result),
      c(
        "row_id",
        "subject_id",
        "cohort_start_date",
        "cohort_end_date",
        "treatment"
      )
    )
  }
)
