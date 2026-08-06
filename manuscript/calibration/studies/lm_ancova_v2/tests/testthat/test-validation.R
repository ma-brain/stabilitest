.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_v2_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_v2_study(project_root = .project_root(), envir = env)
  analyse <- file.path(.study_root(), "analyse_calibration.R")
  if (file.exists(analyse)) {
    sys.source(analyse, envir = env)
  }
  env
}

.ancova_v2_score_rows <- function(scenario_id, truth_class, scores,
                                  sample_size = 80L, baseline_r2 = 0.40,
                                  diagnostic_only = FALSE) {
  n <- length(scores)
  data.frame(
    scenario_id = scenario_id,
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    sample_size = as.integer(sample_size),
    baseline_r2 = as.numeric(baseline_r2),
    diagnostic_only = isTRUE(diagnostic_only),
    status = "completed",
    stringsAsFactors = FALSE
  )
}

.feasible_heldout_block_v2 <- function(prefix, n = 80L, r2 = 0.40, n_per = 100L) {
  null_scores <- c(rep(c(10, 20, 30, 40, 45), length.out = n_per - 10L), rep(50, 10L))
  clear_scores <- c(rep(51, as.integer(round(0.75 * n_per))),
                    rep(40, n_per - as.integer(round(0.75 * n_per))))
  bord_scores <- rep(c(50, 51, 52, 53), length.out = n_per)
  rbind(
    .ancova_v2_score_rows(paste0(prefix, "_null"), "null", null_scores, n, r2),
    .ancova_v2_score_rows(
      paste0(prefix, "_bord"), "borderline", bord_scores, n, r2,
      diagnostic_only = TRUE
    ),
    .ancova_v2_score_rows(paste0(prefix, "_clear"), "clear", clear_scores, n, r2)
  )
}

test_that("ANCOVA v2 cluster bootstrap is deterministic and scenario-based", {
  env <- .load_lm_ancova_v2_study_env()
  data <- rbind(
    .ancova_v2_score_rows("s1", "null", c(10, 20, 30)),
    .ancova_v2_score_rows("s2", "null", c(40, 50, 60)),
    .ancova_v2_score_rows("s3", "null", c(70, 80, 90))
  )
  statistic <- function(rows) mean(rows$overall_score)
  a <- env$ancova_v2_cluster_bound(data, statistic, side = "upper", B = 50L, seed = 99L)
  b <- env$ancova_v2_cluster_bound(data, statistic, side = "upper", B = 50L, seed = 99L)
  testthat::expect_identical(a, b)
  testthat::expect_identical(sort(a$clusters), c("s1", "s2", "s3"))
  testthat::expect_identical(a$n_clusters, 3L)
  testthat::expect_identical(a$cluster, "scenario")
})

test_that("freeze and validation enforce no-refit held-out gates for single L", {
  env <- .load_lm_ancova_v2_study_env()
  training <- .feasible_heldout_block_v2("train", n = 80L, r2 = 0.40, n_per = 40L)
  fit <- env$fit_lm_ancova_v2_cutoffs(training)
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$cutoff, 50L)

  scenario_manifest_hash <- "scenario-hash-fixture-v2"
  training_manifest_hash <- "training-hash-fixture-v2"
  frozen <- env$freeze_lm_ancova_v2_candidate(
    fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  testthat::expect_true(is.character(frozen$candidate_hash) && nzchar(frozen$candidate_hash))
  testthat::expect_identical(frozen$cutoff, 50L)
  testthat::expect_false(isTRUE(frozen$held_out_opened))
  testthat::expect_false(isTRUE(frozen$validation_refit))

  mutated <- frozen
  mutated$cutoff <- mutated$cutoff + 1L
  # Re-freeze from a fit-like object with mutated cutoff to show hash depends on L.
  mutated_fit <- fit
  mutated_fit$cutoff <- fit$cutoff + 1L
  mutated_hash <- env$freeze_lm_ancova_v2_candidate(
    mutated_fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )$candidate_hash
  testthat::expect_false(identical(frozen$candidate_hash, mutated_hash))

  validation <- rbind(
    .feasible_heldout_block_v2("val60", n = 60L, r2 = 0.25, n_per = 100L),
    .feasible_heldout_block_v2("val120", n = 120L, r2 = 0.55, n_per = 100L)
  )
  # Diagnostic poison must not enter acceptance.
  poison <- .ancova_v2_score_rows(
    "val_stress_clear", "clear", rep(5, 100L),
    sample_size = 120L, baseline_r2 = 0.55, diagnostic_only = TRUE
  )
  validation <- rbind(validation, poison)

  ok <- env$validate_lm_ancova_v2_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture-v2",
    cluster_B = 50L,
    cluster_seed = 17L
  )
  testthat::expect_identical(ok$status, "validated_method_specific")
  testthat::expect_false(isTRUE(ok$validation_refit))
  testthat::expect_identical(ok$cutoff, frozen$cutoff)
  testthat::expect_true(isTRUE(ok$held_out_opened))

  # Validation must not call the fitting path.
  env$fit_lm_ancova_v2_cutoffs <- function(...) {
    stop("fit_lm_ancova_v2_cutoffs must not be called during validation", call. = FALSE)
  }
  again <- env$validate_lm_ancova_v2_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture-v2",
    cluster_B = 50L,
    cluster_seed = 17L
  )
  testthat::expect_identical(again$status, "validated_method_specific")

  bad_validation <- validation
  bad_validation$overall_score[bad_validation$truth_class == "null"] <- 90
  failed <- env$validate_lm_ancova_v2_candidate(
    frozen,
    bad_validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture-v2",
    cluster_B = 20L,
    cluster_seed = 17L
  )
  testthat::expect_identical(failed$status, "uncalibrated")
  testthat::expect_identical(failed$cutoff, frozen$cutoff)
  testthat::expect_false(isTRUE(failed$validation_refit))

  uncalibrated_fit <- list(
    status = "uncalibrated",
    reason = "no_feasible_thresholds",
    cutoff = NA_integer_
  )
  unopened <- env$freeze_lm_ancova_v2_candidate(
    uncalibrated_fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  testthat::expect_identical(unopened$status, "uncalibrated")
  testthat::expect_identical(unopened$reason, "no_feasible_thresholds")
  testthat::expect_identical(unopened$held_out_opened, FALSE)
})

test_that("analyse_lm_ancova_v2_calibration validates frozen L once without refit", {
  env <- .load_lm_ancova_v2_study_env()
  testthat::expect_true(is.function(env$analyse_lm_ancova_v2_calibration))

  training <- .feasible_heldout_block_v2("train", n = 80L, r2 = 0.40, n_per = 40L)
  validation <- rbind(
    .feasible_heldout_block_v2("val60", n = 60L, r2 = 0.25, n_per = 100L),
    .feasible_heldout_block_v2("val120", n = 120L, r2 = 0.55, n_per = 100L)
  )

  fit_calls <- 0L
  real_fit <- env$fit_lm_ancova_v2_cutoffs
  env$fit_lm_ancova_v2_cutoffs <- function(...) {
    fit_calls <<- fit_calls + 1L
    real_fit(...)
  }

  result <- env$analyse_lm_ancova_v2_calibration(
    training,
    validation = validation,
    scenario_manifest_hash = "scenario-hash-fixture-v2",
    training_manifest_hash = "training-hash-fixture-v2",
    validation_manifest_hash = "validation-hash-fixture-v2",
    cluster_B = 30L,
    cluster_seed = 17L
  )
  testthat::expect_identical(fit_calls, 1L)
  testthat::expect_identical(result$status, "validated_method_specific")
  testthat::expect_false(isTRUE(result$validation_refit))
  testthat::expect_identical(result$frozen$cutoff, 50L)
  testthat::expect_identical(result$validation$cutoff, 50L)

  bad_training <- .feasible_heldout_block_v2("bad", n_per = 40L)
  bad_training$overall_score[bad_training$truth_class == "null"] <- 95
  bad_training$overall_score[bad_training$truth_class == "clear"] <- 5
  closed <- env$analyse_lm_ancova_v2_calibration(
    bad_training,
    validation = validation,
    scenario_manifest_hash = "scenario-hash-fixture-v2",
    training_manifest_hash = "training-hash-fixture-v2",
    validation_manifest_hash = "validation-hash-fixture-v2"
  )
  testthat::expect_identical(closed$status, "uncalibrated")
  testthat::expect_identical(closed$frozen$reason, "no_feasible_thresholds")
  testthat::expect_null(closed$validation)
  testthat::expect_false(isTRUE(closed$frozen$held_out_opened))
})
