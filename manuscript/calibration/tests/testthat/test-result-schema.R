schema_path <- file.path("..", "..", "R", "schema.R")
testthat::expect_true(file.exists(schema_path))

schema_env <- new.env(parent = globalenv())
sys.source(schema_path, envir = schema_env)

scenario_fixture <- function() {
  scenario_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "config", "scenarios.R"), envir = scenario_env)
  scenario_env$calibration_scenarios()
}

completed_replicate_fixture <- function() {
  schema_env$new_calibration_replicate(
    scenario_id = "two_sample_smoke",
    replicate_id = 1L,
    analysis_family = "two_sample",
    endpoint = "mean_difference",
    design_layer = "core",
    truth_class = "null",
    target_conclusion = "non_significant",
    screening_conclusion = "non_significant",
    selected = TRUE,
    analysis_conclusion = list(significant = FALSE, direction = NA_character_),
    original_p = 0.4,
    effective_p = 0.4,
    jackknife_stability = 95,
    fragility_component = 80,
    fragility_k = 5L,
    fragility_pct = 10,
    bootstrap_reproducibility = 90,
    overall_score = 88,
    assigned_label = "Robust",
    n = 50L,
    replicate_seed = 1101001L,
    bootstrap_seed = 1101001001L,
    runtime_seconds = 0.2,
    status = "completed",
    failure_stage = NA_character_,
    failure_class = NA_character_,
    failure_message = NA_character_
  )
}

testthat::test_that("replicate constructor emits the common schema and preserves values", {
  replicate <- completed_replicate_fixture()
  expected_columns <- c(
    "scenario_id", "replicate_id", "analysis_family", "endpoint", "design_layer",
    "truth_class", "target_conclusion", "screening_conclusion", "selected",
    "analysis_conclusion", "original_p", "effective_p", "jackknife_stability",
    "fragility_component", "fragility_k", "fragility_pct", "bootstrap_reproducibility",
    "overall_score", "assigned_label", "n", "replicate_seed", "bootstrap_seed",
    "runtime_seconds", "status", "failure_stage", "failure_class", "failure_message"
  )

  testthat::expect_s3_class(replicate, "tbl_df")
  testthat::expect_identical(names(replicate), expected_columns)
  testthat::expect_identical(replicate$replicate_id, 1L)
  testthat::expect_identical(
    replicate$analysis_conclusion[[1L]],
    list(significant = FALSE, direction = NA_character_)
  )
})

testthat::test_that("scenario validation rejects malformed registries and accepts Task 1 registry", {
  scenarios <- scenario_fixture()
  testthat::expect_true(schema_env$validate_calibration_scenarios(scenarios))

  testthat::expect_error(
    schema_env$validate_calibration_scenarios(scenarios[, -1L]),
    "missing required scenario columns"
  )

  duplicate_ids <- scenarios
  duplicate_ids$scenario_id[[2L]] <- duplicate_ids$scenario_id[[1L]]
  testthat::expect_error(
    schema_env$validate_calibration_scenarios(duplicate_ids),
    "scenario_id.*unique"
  )

  invalid_layer <- scenarios
  invalid_layer$design_layer[[1L]] <- "unknown"
  testthat::expect_error(
    schema_env$validate_calibration_scenarios(invalid_layer),
    "design_layer"
  )

  invalid_boot <- scenarios
  invalid_boot$n_boot[[1L]] <- 0L
  testthat::expect_error(
    schema_env$validate_calibration_scenarios(invalid_boot),
    "n_boot"
  )

  invalid_removal <- scenarios
  invalid_removal$max_removal_pct[[1L]] <- 0
  testthat::expect_error(
    schema_env$validate_calibration_scenarios(invalid_removal),
    "max_removal_pct"
  )

  invalid_split <- scenarios
  invalid_split$training_split[[1L]] <- 1
  testthat::expect_error(
    schema_env$validate_calibration_scenarios(invalid_split),
    "training_split"
  )
})

testthat::test_that("replicate validation rejects malformed completed artifacts", {
  replicate <- completed_replicate_fixture()
  testthat::expect_true(schema_env$validate_calibration_replicates(replicate))

  testthat::expect_error(
    schema_env$validate_calibration_replicates(replicate[, -1L]),
    "missing required replicate columns"
  )

  duplicate_pair <- rbind(replicate, replicate)
  testthat::expect_error(
    schema_env$validate_calibration_replicates(duplicate_pair),
    "scenario_id.*replicate_id.*unique"
  )

  invalid_score <- replicate
  invalid_score$overall_score[[1L]] <- 101
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_score),
    "overall_score"
  )

  invalid_status <- replicate
  invalid_status$status[[1L]] <- "done"
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_status),
    "status"
  )

  non_finite_metric <- replicate
  non_finite_metric$bootstrap_reproducibility[[1L]] <- Inf
  testthat::expect_error(
    schema_env$validate_calibration_replicates(non_finite_metric),
    "finite"
  )
})

testthat::test_that("failure constructor records an auditable failure row", {
  scenarios <- scenario_fixture()
  failure <- schema_env$new_calibration_failure(
    scenarios[1L, , drop = FALSE],
    replicate_id = 7L,
    stage = "analysis",
    condition = simpleError("bootstrap failed")
  )

  testthat::expect_identical(names(failure), schema_env$CALIBRATION_REPLICATE_COLUMNS)
  testthat::expect_identical(failure$scenario_id, scenarios$scenario_id[[1L]])
  testthat::expect_identical(failure$replicate_id, 7L)
  testthat::expect_identical(failure$status, "failed")
  testthat::expect_identical(failure$failure_stage, "analysis")
  testthat::expect_identical(failure$failure_class, "simpleError")
  testthat::expect_identical(failure$failure_message, "bootstrap failed")
  testthat::expect_true(is.na(failure$overall_score))
  testthat::expect_true(is.na(failure$bootstrap_reproducibility))
  testthat::expect_true(schema_env$validate_calibration_replicates(failure))
})
