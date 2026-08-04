# Task 12 contract tests.  The fixtures are deliberately tiny so this suite
# exercises policy decisions without requiring a publication-sized run.
test_project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
pkgload::load_all(test_project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

threshold_env <- new.env(parent = globalenv())
for (file in c("schema.R", "manifest.R", "thresholds.R")) {
  path <- file.path("..", "..", "R", file)
  if (file.exists(path)) sys.source(path, envir = threshold_env)
}

fixture_path <- function(name) file.path("..", "fixtures", name)

testthat::test_that("shared bands are evaluated before candidate family mappings", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  result <- threshold_env$fit_calibration_candidates(training, shared_cutoffs = c(55L, 70L))
  testthat::expect_identical(result$shared_cutoffs, c(55L, 70L))
  testthat::expect_true(isTRUE(result$shared_evaluated))
  calibrated <- result$registry[result$registry$status != "uncalibrated", , drop = FALSE]
  testthat::expect_true(all(calibrated$lower_cutoff <= calibrated$upper_cutoff))
  testthat::expect_true(all(calibrated$lower_cutoff >= 0 & calibrated$upper_cutoff <= 100))
  testthat::expect_error(
    threshold_env$fit_calibration_candidates(validation, shared_cutoffs = c(55L, 70L)),
    "training|validation"
  )
})

testthat::test_that("family-specific mapping requires both improvement and material difference", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  candidates <- threshold_env$fit_calibration_candidates(training)
  evaluated <- threshold_env$validate_calibration_candidates(candidates, validation)
  testthat::expect_true(all(evaluated$registry$status %in%
    c("validated", "family_specific", "uncalibrated", "shared")))
  family_row <- evaluated[evaluated$analysis_family == "fake_family", , drop = FALSE]
  testthat::expect_true(nrow(family_row) == 1L)
  testthat::expect_identical(family_row$status[[1L]], "family_specific")
  testthat::expect_true(abs(family_row$lower_cutoff - 55L) >= 5L ||
    abs(family_row$upper_cutoff - 70L) >= 5L)
  testthat::expect_gte(family_row$heldout_improvement[[1L]], 0.05)
})

testthat::test_that("failed families become uncalibrated rather than silently shared", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  candidates <- threshold_env$fit_calibration_candidates(training)
  evaluated <- threshold_env$validate_calibration_candidates(candidates, validation)
  row <- evaluated[evaluated$analysis_family == "failed_family", , drop = FALSE]
  testthat::expect_identical(row$status[[1L]], "uncalibrated")
  testthat::expect_true(is.na(row$lower_cutoff[[1L]]))
})

testthat::test_that("shared mapping validation publishes shared cutoffs, not training candidates", {
  # Training-style candidate proposes a non-shared pair (55/75). Held-out data
  # still validates the shared mapping; exported cutoffs must become (55/70).
  set.seed(42L)
  n_per <- 120L
  make_stratum <- function(truth, lo, hi) {
    data.frame(
      analysis_family = "shared_override",
      truth_class = truth,
      overall_score = runif(n_per, lo, hi),
      status = "completed",
      design_layer = "core",
      stringsAsFactors = FALSE
    )
  }
  validation <- rbind(
    make_stratum("null", 10, 40),
    make_stratum("borderline", 56, 68),
    make_stratum("clear", 80, 95)
  )
  candidates <- data.frame(
    analysis_family = "shared_override",
    lower_cutoff = 55L, upper_cutoff = 75L,
    shared_lower = 55L, shared_upper = 70L,
    training_balanced_accuracy = 0.9,
    training_false_reassurance = 0.01,
    training_robust_identification = 0.9,
    training_false_reassurance_upper = 0.05,
    training_robust_identification_lower = 0.8,
    shared_balanced_accuracy = 0.9,
    shared_false_reassurance = 0.01,
    shared_robust_identification = 0.9,
    shared_false_reassurance_upper = 0.05,
    shared_robust_identification_lower = 0.8,
    heldout_balanced_accuracy = NA_real_, shared_heldout_accuracy = NA_real_,
    heldout_improvement = NA_real_, material_difference = NA_integer_,
    heldout_false_reassurance_upper = NA_real_,
    heldout_robust_identification_lower = NA_real_,
    median_ordering_ok = NA, stratum_complete = NA,
    improvement_direction_ok = NA,
    status = "candidate", reason = NA_character_,
    stringsAsFactors = FALSE
  )
  evaluated <- threshold_env$validate_calibration_candidates(
    candidates, validation, shared_cutoffs = c(55L, 70L), minimum_stratum_n = 100L
  )
  row <- evaluated[evaluated$analysis_family == "shared_override", , drop = FALSE]
  testthat::expect_identical(nrow(row), 1L)
  testthat::expect_identical(row$status[[1L]], "validated")
  testthat::expect_identical(row$reason[[1L]], "shared_mapping_validated")
  testthat::expect_identical(row$lower_cutoff[[1L]], 55L)
  testthat::expect_identical(row$upper_cutoff[[1L]], 70L)
})

testthat::test_that("analysis is deterministic and freezes a hashed registry", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  first <- threshold_env$analyse_calibration(training, validation)
  second <- threshold_env$analyse_calibration(training, validation)
  testthat::expect_identical(first$candidate_hash, second$candidate_hash)
  testthat::expect_identical(first$registry, second$registry)
  testthat::expect_identical(first$validation$refit, FALSE)
  testthat::expect_error(
    threshold_env$analyse_calibration(training, training),
    "disjoint|overlap|held.?out"
  )
})

testthat::test_that("manifest provenance is validated and preserved", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  training_manifest <- list(manifest_version = "calibration-1", scenario_manifest_hash = "same",
                           options = list(split = "training", validation_only = FALSE))
  validation_manifest <- list(manifest_version = "calibration-1", scenario_manifest_hash = "same",
                             options = list(split = "validation", validation_only = TRUE))
  result <- threshold_env$analyse_calibration(training, validation, training_manifest, validation_manifest)
  testthat::expect_true(all(result$registry$training_manifest_hash == "same"))
  testthat::expect_true(all(result$registry$validation_manifest_hash == "same"))
  bad <- validation_manifest; bad$scenario_manifest_hash <- "different"
  testthat::expect_error(threshold_env$analyse_calibration(training, validation, training_manifest, bad), "hash")
  bad <- validation_manifest; bad$manifest_version <- "future-format"
  testthat::expect_error(threshold_env$analyse_calibration(training, validation, training_manifest, bad), "version")
})

testthat::test_that("held-out acceptance records Wilson bounds and stratum checks", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  result <- threshold_env$analyse_calibration(training, validation)
  fake <- result$registry[result$registry$analysis_family == "fake_family", , drop = FALSE]
  testthat::expect_lte(fake$heldout_false_reassurance_upper[[1L]], 0.10)
  testthat::expect_gte(fake$heldout_robust_identification_lower[[1L]], 0.60)
  testthat::expect_true(isTRUE(fake$stratum_complete[[1L]]))
  testthat::expect_true(isTRUE(fake$median_ordering_ok[[1L]]))
  short <- validation[seq_len(6L), , drop = FALSE]
  short_result <- threshold_env$analyse_calibration(training, short,
                                                     minimum_stratum_n = 1L)
  testthat::expect_true(any(!short_result$registry$stratum_complete))
})

testthat::test_that("scenario-level overlap and malformed result schemas are rejected", {
  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  validation$scenario_id <- sub("_heldout$", "_training", validation$scenario_id)
  testthat::expect_error(threshold_env$analyse_calibration(training, validation), "scenario_id")
  malformed <- readRDS(fixture_path("validation-replicates.rds"))
  malformed$overall_score[[1L]] <- Inf
  testthat::expect_error(threshold_env$analyse_calibration(training, malformed), "finite|overall_score")
})
