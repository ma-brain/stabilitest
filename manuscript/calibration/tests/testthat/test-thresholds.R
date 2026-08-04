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
  training$overall_score[training$analysis_family == "failed_family"] <- NA_real_
  candidates <- threshold_env$fit_calibration_candidates(training)
  evaluated <- threshold_env$validate_calibration_candidates(candidates, validation)
  row <- evaluated[evaluated$analysis_family == "failed_family", , drop = FALSE]
  testthat::expect_identical(row$status[[1L]], "uncalibrated")
  testthat::expect_true(is.na(row$lower_cutoff[[1L]]))
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
