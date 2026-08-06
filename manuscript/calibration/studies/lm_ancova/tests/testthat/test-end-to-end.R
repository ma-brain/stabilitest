.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.fixture_path <- function(name) {
  file.path(.study_root(), "tests", "fixtures", name)
}

.load_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_study(project_root = .project_root(), envir = env)
  for (tool in c("assemble_replicates.R", "freeze_and_publish.R")) {
    sys.source(file.path(.study_root(), "tools", tool), envir = env)
  }
  sys.source(file.path(.study_root(), "analyse_calibration.R"), envir = env)
  env
}

test_that("reduced ANCOVA workflow is locked by committed fixtures", {
  training_path <- .fixture_path("training-replicates.rds")
  validation_path <- .fixture_path("validation-replicates.rds")
  testthat::expect_true(file.exists(training_path))
  testthat::expect_true(file.exists(validation_path))

  env <- .load_study_env()
  scenarios <- env$lm_ancova_scenarios()
  testthat::expect_true(env$validate_calibration_scenarios(scenarios))

  training <- readRDS(training_path)
  validation <- readRDS(validation_path)
  scenario_hash <- env$calibration_scenario_hash(scenarios)
  training_hash <- env$calibration_hash_object(training)
  validation_hash <- env$calibration_hash_object(validation)

  result <- env$analyse_lm_ancova_calibration(
    training = training,
    validation = validation,
    scenario_manifest_hash = scenario_hash,
    training_manifest_hash = training_hash,
    validation_manifest_hash = validation_hash,
    cluster_B = 40L,
    cluster_seed = 20260806L
  )

  testthat::expect_identical(result$validation_refit, FALSE)
  testthat::expect_false(isTRUE(result$validation$validation_refit))
  testthat::expect_identical(result$frozen$cutoffs, c(50L, 70L))
  testthat::expect_identical(result$status, "validated_method_specific")
  testthat::expect_true(nzchar(result$frozen$candidate_hash))

  destination <- tempfile("ancova-e2e-publish-")
  ledger <- env$lm_ancova_publish_atomic(
    list(
      completed_training = training,
      completed_validation = validation,
      audit_training = training,
      audit_validation = validation,
      occupancy = data.frame(
        scenario_id = unique(c(training$scenario_id, validation$scenario_id)),
        completed = 100L,
        stringsAsFactors = FALSE
      ),
      failures = data.frame(scenario_id = character(), failure_rate = numeric(),
                            stringsAsFactors = FALSE),
      power_verification = data.frame(scenario_id = "fixture", achieved_power = 0.6,
                                      stringsAsFactors = FALSE),
      candidate = result$frozen,
      validation = result$validation,
      registry = data.frame(
        calibration_unit = "lm_ancova",
        status = result$status,
        cutoff_fragile = result$frozen$cutoffs[[1L]],
        cutoff_robust = result$frozen$cutoffs[[2L]],
        stringsAsFactors = FALSE
      ),
      training_manifest = list(scenario_manifest_hash = scenario_hash,
                               artifact_hash = training_hash),
      validation_manifest = list(scenario_manifest_hash = scenario_hash,
                                 artifact_hash = validation_hash)
    ),
    destination = destination
  )

  testthat::expect_true(file.exists(file.path(destination, "hash_ledger.rds")))
  again <- readRDS(file.path(destination, "hash_ledger.rds"))
  testthat::expect_identical(
    vapply(ledger, function(x) x$hash, character(1)),
    vapply(again, function(x) x$hash, character(1))
  )
  testthat::expect_false("hash_ledger.rds" %in% names(again))
  testthat::expect_true(all(c(
    "completed_training.rds", "completed_validation.rds",
    "candidate.rds", "validation.rds", "registry.csv"
  ) %in% names(again)))
  testthat::expect_identical(result$frozen$candidate_hash, {
    rerun <- env$analyse_lm_ancova_calibration(
      training = training,
      validation = validation,
      scenario_manifest_hash = scenario_hash,
      training_manifest_hash = training_hash,
      validation_manifest_hash = validation_hash,
      cluster_B = 40L,
      cluster_seed = 20260806L
    )
    rerun$frozen$candidate_hash
  })
})
