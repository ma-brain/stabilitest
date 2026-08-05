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

  float_overshoot <- replicate
  float_overshoot$original_p[[1L]] <- 1 + .Machine$double.eps
  float_overshoot$effective_p[[1L]] <- 1 + .Machine$double.eps
  testthat::expect_true(schema_env$validate_calibration_replicates(float_overshoot))

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

  for (invalid_integer_metric in list(
    list(column = "fragility_k", value = 1.5),
    list(column = "n", value = 50.5),
    list(column = "replicate_seed", value = 1101001.5),
    list(column = "bootstrap_seed", value = 1101001001.5),
    list(column = "replicate_seed", value = -1L),
    list(column = "bootstrap_seed", value = -1L)
  )) {
    invalid_metric <- replicate
    invalid_metric[[invalid_integer_metric$column]][[1L]] <- invalid_integer_metric$value
    testthat::expect_error(
      schema_env$validate_calibration_replicates(invalid_metric),
      invalid_integer_metric$column
    )
  }
})

testthat::test_that("replicate validation enforces ID, metadata, and conclusion types", {
  replicate <- completed_replicate_fixture()

  invalid_character_id <- replicate
  invalid_character_id$replicate_id[[1L]] <- "1"
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_character_id),
    "replicate_id"
  )

  invalid_fractional_id <- replicate
  invalid_fractional_id$replicate_id[[1L]] <- 1.5
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_fractional_id),
    "replicate_id"
  )

  invalid_negative_id <- replicate
  invalid_negative_id$replicate_id[[1L]] <- -1L
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_negative_id),
    "replicate_id"
  )

  invalid_list_id <- replicate
  invalid_list_id$replicate_id <- list(1L)
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_list_id),
    "replicate_id"
  )

  invalid_metadata <- replicate
  invalid_metadata$analysis_family[[1L]] <- NA_character_
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_metadata),
    "analysis_family"
  )

  invalid_metadata <- replicate
  invalid_metadata$endpoint[[1L]] <- ""
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_metadata),
    "endpoint"
  )

  invalid_metadata <- replicate
  invalid_metadata$design_layer[[1L]] <- "other"
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_metadata),
    "design_layer"
  )

  invalid_screening_type <- replicate
  invalid_screening_type$screening_conclusion <- 1
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_screening_type),
    "screening_conclusion"
  )

  invalid_analysis_type <- replicate
  invalid_analysis_type$analysis_conclusion <- list(1)
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_analysis_type),
    "analysis_conclusion"
  )

  invalid_label_type <- replicate
  invalid_label_type$assigned_label <- factor("Robust")
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_label_type),
    "assigned_label"
  )
})

testthat::test_that("failed rows cannot carry selected or analysis results", {
  scenarios <- scenario_fixture()
  failure <- schema_env$new_calibration_failure(
    scenarios[1L, , drop = FALSE],
    replicate_id = 7L,
    stage = "analysis",
    condition = simpleError("bootstrap failed"),
    replicate_seed = 701L,
    bootstrap_seed = 702L
  )
  testthat::expect_identical(failure$replicate_seed, 701L)
  testthat::expect_identical(failure$bootstrap_seed, 702L)

  contaminated <- failure
  contaminated$selected[[1L]] <- FALSE
  testthat::expect_error(
    schema_env$validate_calibration_replicates(contaminated),
    "selected"
  )

  contaminated <- failure
  contaminated$overall_score[[1L]] <- 50
  testthat::expect_error(
    schema_env$validate_calibration_replicates(contaminated),
    "failed.*overall_score"
  )

  contaminated <- failure
  contaminated$overall_score[[1L]] <- NaN
  testthat::expect_error(
    schema_env$validate_calibration_replicates(contaminated),
    "failed.*overall_score"
  )

  contaminated <- failure
  contaminated$analysis_conclusion <- list(list(significant = FALSE))
  testthat::expect_error(
    schema_env$validate_calibration_replicates(contaminated),
    "failed.*analysis_conclusion"
  )

  contaminated <- failure
  contaminated$assigned_label[[1L]] <- "Fragile"
  testthat::expect_error(
    schema_env$validate_calibration_replicates(contaminated),
    "failed.*assigned_label"
  )

  invalid_seed <- failure
  invalid_seed$replicate_seed[[1L]] <- NaN
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_seed),
    "failed.*replicate_seed"
  )
  testthat::expect_error(
    schema_env$new_calibration_failure(
      scenarios[1L, , drop = FALSE], 9L, "analysis", simpleError("bad seed"),
      replicate_seed = NaN
    ),
    "replicate_seed"
  )

  invalid_runtime <- failure
  invalid_runtime$runtime_seconds[[1L]] <- NaN
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_runtime),
    "failed runtime_seconds"
  )
})

testthat::test_that("empty artifacts still have valid column classes", {
  empty <- completed_replicate_fixture()[FALSE, , drop = FALSE]
  invalid_empty <- empty
  invalid_empty$replicate_id <- character()
  testthat::expect_error(
    schema_env$validate_calibration_replicates(invalid_empty),
    "replicate_id"
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

  invalid_scenario <- scenarios[1L, , drop = FALSE]
  invalid_scenario$endpoint[[1L]] <- ""
  testthat::expect_error(
    schema_env$new_calibration_failure(
      invalid_scenario, 8L, "analysis", simpleError("bad metadata")
    ),
    "scenario endpoint"
  )
})
