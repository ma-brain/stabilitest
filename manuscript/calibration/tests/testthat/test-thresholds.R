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

# The historical compact fixtures predate the applicable-conclusion schema.
# Give their null rows an observed significant result so they exercise the
# false-reassurance denominator under the corrected estimand while retaining
# their original scores and broad-family labels.
readRDS <- local({
  base_read <- base::readRDS
  function(file, ...) {
    value <- base_read(file, ...)
    if (is.data.frame(value) && all(c("truth_class", "screening_conclusion",
                                      "target_conclusion") %in% names(value))) {
      null <- value$truth_class == "null"
      value$screening_conclusion[null] <- "significant"
      value$target_conclusion[null] <- "non_significant"
    }
    value
  }
})

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

.threshold_candidate_row <- function(family, lower, upper, status = "candidate") {
  data.frame(
    analysis_family = family,
    lower_cutoff = as.integer(lower), upper_cutoff = as.integer(upper),
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
    status = status, reason = NA_character_,
    stringsAsFactors = FALSE
  )
}

testthat::test_that("shared acceptance requires usable ordinal discrimination, not only FR/RI", {
  # Shared 55/70 can pass FR/RI + median ordering while mapping every borderline
  # score into robust (balanced ordinal accuracy = 2/3). That must not count as
  # a validated shared policy, or family-specific alternatives stay blocked.
  n_per <- 120L
  validation <- data.frame(
    analysis_family = "moderate_dump",
    truth_class = rep(c("null", "borderline", "clear"), each = n_per),
    overall_score = c(rep(40, n_per), rep(75, n_per), rep(90, n_per)),
    status = "completed",
    design_layer = "core",
    stringsAsFactors = FALSE
  )
  shared_metrics <- threshold_env$.threshold_metrics(validation, c(55L, 70L))
  testthat::expect_lte(shared_metrics$false_reassurance, 0.05)
  testthat::expect_gte(shared_metrics$robust_identification, 0.70)
  testthat::expect_lt(shared_metrics$balanced_ordinal_accuracy, 0.70)
  testthat::expect_identical(
    threshold_env$classify_score_band(75, c(55L, 70L)), "robust"
  )

  candidates <- .threshold_candidate_row("moderate_dump", 55L, 70L)
  evaluated <- threshold_env$validate_calibration_candidates(
    candidates, validation, shared_cutoffs = c(55L, 70L), minimum_stratum_n = 100L
  )
  row <- evaluated[evaluated$analysis_family == "moderate_dump", , drop = FALSE]
  testthat::expect_identical(nrow(row), 1L)
  testthat::expect_false(identical(row$status[[1L]], "validated"))
  testthat::expect_identical(row$status[[1L]], "uncalibrated")
  testthat::expect_match(row$reason[[1L]], "shared|ordinal|balanced|moderate|borderline")
})

testthat::test_that("failed shared ordinal gate still allows family-specific acceptance", {
  n_per <- 120L
  validation <- data.frame(
    analysis_family = "family_rescue",
    truth_class = rep(c("null", "borderline", "clear"), each = n_per),
    overall_score = c(rep(40, n_per), rep(75, n_per), rep(90, n_per)),
    status = "completed",
    design_layer = "core",
    stringsAsFactors = FALSE
  )
  # Family pair keeps borderline in moderate while remaining materially different.
  candidates <- .threshold_candidate_row("family_rescue", 55L, 85L)
  evaluated <- threshold_env$validate_calibration_candidates(
    candidates, validation, shared_cutoffs = c(55L, 70L), minimum_stratum_n = 100L
  )
  row <- evaluated[evaluated$analysis_family == "family_rescue", , drop = FALSE]
  testthat::expect_identical(row$status[[1L]], "family_specific")
  testthat::expect_gte(row$heldout_improvement[[1L]], 0.05)
  testthat::expect_gte(row$material_difference[[1L]], 5L)
})

testthat::test_that("Phase 6 public status vocabulary is mapped from internal statuses", {
  map <- threshold_env$map_calibration_status
  testthat::expect_identical(map("validated"), "validated_shared")
  testthat::expect_identical(map("family_specific"), "validated_family_specific")
  testthat::expect_identical(map("uncalibrated"), "uncalibrated")
  testthat::expect_identical(map("bands_not_applicable"), "bands_not_applicable")
  testthat::expect_identical(
    map(c("validated", "family_specific", "uncalibrated")),
    c("validated_shared", "validated_family_specific", "uncalibrated")
  )

  training <- readRDS(fixture_path("training-replicates.rds"))
  validation <- readRDS(fixture_path("validation-replicates.rds"))
  result <- threshold_env$analyse_calibration(training, validation)
  testthat::expect_true("status_public" %in% names(result$registry))
  testthat::expect_identical(
    result$registry$status_public,
    map(result$registry$status)
  )
  testthat::expect_true(all(result$registry$status_public %in%
    c("validated_shared", "validated_family_specific",
      "uncalibrated", "bands_not_applicable")))
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

testthat::test_that("threshold metrics use only applicable superiority conclusions", {
  fixture <- data.frame(
    calibration_unit = "lm_ancova",
    design_layer = "core",
    truth_class = c(rep("null", 200), rep("clear", 100)),
    screening_conclusion = c(rep("non_significant", 100),
                             rep("significant", 100), rep("significant", 100)),
    target_conclusion = c(rep("non_significant", 200), rep("significant", 100)),
    overall_score = c(rep(95, 100), rep(40, 100), rep(85, 100)),
    status = "completed", stringsAsFactors = FALSE
  )
  metrics <- threshold_env$threshold_metrics_for_applicable(fixture, c(55, 70))
  testthat::expect_equal(metrics$false_reassurance, 0)
  testthat::expect_equal(metrics$robust_identification, 1)
  testthat::expect_equal(metrics$false_reassurance_n, 100L)
  testthat::expect_equal(metrics$robust_identification_n, 100L)
  testthat::expect_equal(metrics$n, 200L)
})

testthat::test_that("TOST metrics exclude unsuccessful conclusions from bands", {
  fixture <- data.frame(
    calibration_unit = "tost_mean",
    design_layer = "core",
    truth_class = c(rep("clear", 100), rep("null", 100)),
    screening_conclusion = c(rep("equivalent", 100), rep("not_equivalent", 100)),
    target_conclusion = c(rep("equivalent", 100), rep("not_equivalent", 100)),
    overall_score = c(rep(90, 100), rep(10, 100)),
    status = "completed", stringsAsFactors = FALSE
  )
  metrics <- threshold_env$threshold_metrics_for_applicable(fixture, c(55, 70))
  testthat::expect_equal(metrics$n, 100L)
  testthat::expect_equal(metrics$false_reassurance_n, 0L)
  testthat::expect_equal(metrics$robust_identification_n, 100L)
  testthat::expect_equal(metrics$robust_identification, 1)
})

testthat::test_that("candidate fitting groups by calibration unit", {
  fixture <- data.frame(
    analysis_family = rep(c("two_sample", "two_sample"), each = 6),
    calibration_unit = rep(c("welch_unpaired", "paired_t"), each = 6),
    truth_class = rep(c("null", "borderline", "clear"), 4),
    screening_conclusion = rep(c("significant", "significant", "significant"), 4),
    target_conclusion = rep(c("non_significant", "significant", "significant"), 4),
    overall_score = rep(c(20, 60, 90), 4),
    design_layer = "core", status = "completed", stringsAsFactors = FALSE
  )
  result <- threshold_env$fit_calibration_candidates(fixture)
  testthat::expect_true("calibration_unit" %in% names(result$registry))
  testthat::expect_setequal(result$registry$calibration_unit,
                            c("paired_t", "welch_unpaired"))
  testthat::expect_false("analysis_engine" %in% names(result$registry))
})
