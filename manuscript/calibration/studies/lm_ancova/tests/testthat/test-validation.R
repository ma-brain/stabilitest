.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_study(project_root = .project_root(), envir = env)
  env
}

.ancova_score_rows <- function(scenario_id, truth_class, scores,
                               sample_size = 80L, baseline_r2 = 0.40) {
  n <- length(scores)
  data.frame(
    scenario_id = scenario_id,
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    sample_size = as.integer(sample_size),
    baseline_r2 = as.numeric(baseline_r2),
    status = "completed",
    stringsAsFactors = FALSE
  )
}

.feasible_heldout_block <- function(prefix, n = 80L, r2 = 0.40, n_per = 100L) {
  null_scores <- rep(c(10, 20, 30, 40, 45, 48, 49, 50), length.out = n_per)
  bord_scores <- rep(c(51, 55, 58, 60, 62, 65, 68, 70), length.out = n_per)
  clear_scores <- rep(c(71, 75, 78, 80, 82, 85, 90, 95), length.out = n_per)
  rbind(
    .ancova_score_rows(paste0(prefix, "_null"), "null", null_scores, n, r2),
    .ancova_score_rows(paste0(prefix, "_bord"), "borderline", bord_scores, n, r2),
    .ancova_score_rows(paste0(prefix, "_clear"), "clear", clear_scores, n, r2)
  )
}

test_that("ANCOVA cluster bootstrap is deterministic and scenario-based", {
  env <- .load_lm_ancova_study_env()
  data <- rbind(
    .ancova_score_rows("s1", "null", c(10, 20, 30)),
    .ancova_score_rows("s2", "null", c(40, 50, 60)),
    .ancova_score_rows("s3", "null", c(70, 80, 90))
  )
  statistic <- function(rows) mean(rows$overall_score)
  a <- env$ancova_cluster_bound(data, statistic, side = "upper", B = 50L, seed = 99L)
  b <- env$ancova_cluster_bound(data, statistic, side = "upper", B = 50L, seed = 99L)
  testthat::expect_identical(a, b)
  testthat::expect_identical(sort(a$clusters), c("s1", "s2", "s3"))
  testthat::expect_identical(a$n_clusters, 3L)

  # Resampling whole scenarios changes the mean more than resampling rows of one
  # scenario alone would when clusters are unequal; the helper must report
  # scenario clusters as the resampling units.
  testthat::expect_identical(a$cluster, "scenario")
})

test_that("freeze and validation enforce no-refit held-out gates", {
  env <- .load_lm_ancova_study_env()
  training <- .feasible_heldout_block("train", n = 80L, r2 = 0.40, n_per = 40L)
  fit <- env$fit_lm_ancova_cutoffs(training)
  testthat::expect_identical(fit$status, "candidate")

  scenario_manifest_hash <- "scenario-hash-fixture"
  training_manifest_hash <- "training-hash-fixture"
  frozen <- env$freeze_lm_ancova_candidate(
    fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  testthat::expect_true(is.character(frozen$candidate_hash) && nzchar(frozen$candidate_hash))
  mutated <- frozen
  mutated$cutoffs[[1L]] <- mutated$cutoffs[[1L]] + 1L
  mutated_hash <- env$freeze_lm_ancova_candidate(
    mutated,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )$candidate_hash
  testthat::expect_false(identical(frozen$candidate_hash, mutated_hash))

  validation <- rbind(
    .feasible_heldout_block("val60", n = 60L, r2 = 0.25, n_per = 100L),
    .feasible_heldout_block("val120", n = 120L, r2 = 0.55, n_per = 100L)
  )
  ok <- env$validate_lm_ancova_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture",
    cluster_B = 50L,
    cluster_seed = 17L
  )
  testthat::expect_identical(ok$status, "validated_method_specific")
  testthat::expect_false(isTRUE(ok$validation_refit))
  testthat::expect_identical(ok$cutoffs, frozen$cutoffs)

  # Validation must not call the fitting path.
  env$fit_lm_ancova_cutoffs <- function(...) {
    stop("fit_lm_ancova_cutoffs must not be called during validation", call. = FALSE)
  }
  again <- env$validate_lm_ancova_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture",
    cluster_B = 50L,
    cluster_seed = 17L
  )
  testthat::expect_identical(again$status, "validated_method_specific")

  # Held-out failure stays uncalibrated without replacement cutoffs.
  bad_validation <- validation
  bad_validation$overall_score[bad_validation$truth_class == "null"] <- 90
  failed <- env$validate_lm_ancova_candidate(
    frozen,
    bad_validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = "validation-hash-fixture",
    cluster_B = 20L,
    cluster_seed = 17L
  )
  testthat::expect_identical(failed$status, "uncalibrated")
  testthat::expect_identical(failed$cutoffs, frozen$cutoffs)
  testthat::expect_false(isTRUE(failed$validation_refit))

  uncalibrated_fit <- list(
    status = "uncalibrated",
    reason = "no_feasible_thresholds",
    cutoffs = c(NA_integer_, NA_integer_)
  )
  unopened <- env$freeze_lm_ancova_candidate(
    uncalibrated_fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  testthat::expect_identical(unopened$status, "uncalibrated")
  testthat::expect_identical(unopened$held_out_opened, FALSE)
})
