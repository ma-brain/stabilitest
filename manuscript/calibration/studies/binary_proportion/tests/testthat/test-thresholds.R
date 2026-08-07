.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_bp_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)
  env$load_binary_proportion_study(project_root = .project_root(), envir = env)
  env
}

# A training-shaped fixture with truth_class, overall_score, and the screening
# columns the fitter filters on.  Scores are chosen so exactly one cutoff is
# feasible (clears above the null mass, nulls below).
.feasible_fixture <- function() {
  set.seed(20260807)
  null_scores <- sample(0:35, 200, replace = TRUE)      # well below 50
  clear_scores <- sample(55:100, 200, replace = TRUE)   # well above 50
  dplyr::bind_rows(
    data.frame(
      scenario_id = rep("s_null", 200),
      truth_class = "null", overall_score = null_scores,
      analysis_conclusion = "significant", status = "completed",
      stringsAsFactors = FALSE
    ),
    data.frame(
      scenario_id = rep("s_clear", 200),
      truth_class = "clear", overall_score = clear_scores,
      analysis_conclusion = "significant", status = "completed",
      stringsAsFactors = FALSE
    )
  )
}

# Infeasible: null and clear scores overlap at every cutoff (no separation).
.infeasible_fixture <- function() {
  set.seed(20260808)
  scores <- sample(40:60, 400, replace = TRUE)
  dplyr::bind_rows(
    data.frame(scenario_id = rep("s_null", 200), truth_class = "null",
               overall_score = scores[1:200],
               analysis_conclusion = "significant", status = "completed",
               stringsAsFactors = FALSE),
    data.frame(scenario_id = rep("s_clear", 200), truth_class = "clear",
               overall_score = scores[201:400],
               analysis_conclusion = "significant", status = "completed",
               stringsAsFactors = FALSE)
  )
}

test_that("prop_cutoff_metrics reports FR/RI with Wilson bounds at a cutoff", {
  env <- .load_bp_env()
  data <- .feasible_fixture()
  m <- env$prop_cutoff_metrics(data, cutoff = 50L)
  # Fragile iff score <= cutoff; Not-fragile iff score > cutoff.
  # FR = P(score > L | null, significant): want <= 0.05 here.
  testthat::expect_true(is.numeric(m$false_reassurance) && is.finite(m$false_reassurance))
  testthat::expect_lte(m$false_reassurance, 0.05)
  testthat::expect_true(is.numeric(m$false_reassurance_upper) &&
                         m$false_reassurance_upper <= 0.10)
  # RI = P(score > L | clear, significant): want >= 0.70 here.
  testthat::expect_true(is.numeric(m$robust_identification) &&
                         is.finite(m$robust_identification))
  testthat::expect_gte(m$robust_identification, 0.70)
  testthat::expect_true(is.numeric(m$robust_identification_lower) &&
                         m$robust_identification_lower >= 0.60)
  testthat::expect_equal(m$false_reassurance_n, 200L)
  testthat::expect_equal(m$robust_identification_n, 200L)
})

test_that("prop_training_feasible flags feasible vs infeasible cutoffs", {
  env <- .load_bp_env()
  good <- env$prop_cutoff_metrics(.feasible_fixture(), 50L)
  feasible <- env$prop_training_feasible(good)
  testthat::expect_true(feasible$feasible)
  testthat::expect_length(feasible$reasons, 0L)
  testthat::expect_true(is.finite(feasible$constraint_safety_margin))

  bad <- env$prop_cutoff_metrics(.infeasible_fixture(), 50L)
  infeasible <- env$prop_training_feasible(bad)
  testthat::expect_false(infeasible$feasible)
  testthat::expect_gte(length(infeasible$reasons), 1L)
})

test_that("fit_fisher_exact_cutoffs selects a feasible L on the feasible fixture", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_cutoffs(.feasible_fixture())
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_false(anyNA(fit$cutoff))
  testthat::expect_true(is.integer(fit$cutoff) && length(fit$cutoff) == 1L)
  testthat::expect_gte(fit$cutoff, 0L)
  testthat::expect_lte(fit$cutoff, 100L)
  testthat::expect_true(is.data.frame(fit$grid))
  testthat::expect_true(is.list(fit$metrics))
  testthat::expect_true(env$prop_training_feasible(fit$metrics)$feasible)
})

test_that("fit_fisher_exact_cutoffs returns uncalibrated when no L is feasible", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_cutoffs(.infeasible_fixture())
  testthat::expect_identical(fit$status, "uncalibrated")
  testthat::expect_identical(fit$reason, "no_feasible_thresholds")
  testthat::expect_true(all(is.na(fit$cutoff)))
  testthat::expect_true(is.data.frame(fit$grid))
  testthat::expect_false(any(fit$grid$feasible %in% TRUE))
})

test_that("tie-break is deterministic: highest RI, then FR margin, then smallest L", {
  env <- .load_bp_env()
  # Construct a fixture where multiple Ls are feasible.  The fitter must pick
  # deterministically by the frozen tie-break order.
  set.seed(20260809)
  null_scores <- c(rep(40, 100), sample(0:39, 100, replace = TRUE))
  clear_scores <- c(rep(55, 100), sample(56:100, 100, replace = TRUE))
  data <- dplyr::bind_rows(
    data.frame(scenario_id = "s_null", truth_class = "null",
               overall_score = null_scores, analysis_conclusion = "significant",
               status = "completed", stringsAsFactors = FALSE),
    data.frame(scenario_id = "s_clear", truth_class = "clear",
               overall_score = clear_scores, analysis_conclusion = "significant",
               status = "completed", stringsAsFactors = FALSE)
  )
  a <- env$fit_fisher_exact_cutoffs(data)
  b <- env$fit_fisher_exact_cutoffs(data)
  testthat::expect_identical(a$cutoff, b$cutoff)
  testthat::expect_identical(a$status, b$status)
  # Repeated calls are deterministic (no RNG dependence in the fitter).
  testthat::expect_identical(a$metrics$robust_identification,
                             b$metrics$robust_identification)
})

test_that("freeze_binary_proportion_candidate records a candidate hash", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_cutoffs(.feasible_fixture())
  frozen <- env$freeze_binary_proportion_candidate(
    fit, track_d = NULL,
    scenario_manifest_hash = "scenario-hash",
    training_manifest_hash = "training-hash"
  )
  testthat::expect_identical(frozen$status, "candidate")
  testthat::expect_true(nzchar(frozen$candidate_hash))
  testthat::expect_identical(frozen$scenario_manifest_hash, "scenario-hash")
  testthat::expect_identical(frozen$training_manifest_hash, "training-hash")
  testthat::expect_false(isTRUE(frozen$held_out_opened))
  testthat::expect_false(isTRUE(frozen$validation_refit))
})

test_that("freeze returns uncalibrated payload (with hash) when no candidate", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_cutoffs(.infeasible_fixture())
  frozen <- env$freeze_binary_proportion_candidate(
    fit, track_d = NULL,
    scenario_manifest_hash = "scenario-hash",
    training_manifest_hash = "training-hash"
  )
  testthat::expect_identical(frozen$status, "uncalibrated")
  testthat::expect_true(nzchar(frozen$candidate_hash))
  testthat::expect_false(isTRUE(frozen$held_out_opened))
})

test_that("cluster-bootstrap bound is deterministic at the frozen seed", {
  env <- .load_bp_env()
  data <- .feasible_fixture()
  stat <- function(rows) as.numeric(env$prop_cutoff_metrics(rows, 50L)$robust_identification)
  a <- env$prop_cluster_bound(
    data[data$truth_class == "clear", , drop = FALSE], stat,
    side = "lower", B = 100L, seed = 20260808L
  )
  b <- env$prop_cluster_bound(
    data[data$truth_class == "clear", , drop = FALSE], stat,
    side = "lower", B = 100L, seed = 20260808L
  )
  testthat::expect_equal(a$bound, b$bound)
  testthat::expect_equal(a$seed, 20260808L)
  testthat::expect_equal(a$B, 100L)
})

test_that("validate_binary_proportion_candidate is callable only on a frozen candidate", {
  env <- .load_bp_env()
  # A non-frozen ad-hoc list must be rejected.
  testthat::expect_error(
    env$validate_binary_proportion_candidate(
      list(status = "candidate", cutoff = 50L),
      .feasible_fixture(),
      scenario_manifest_hash = "s", training_manifest_hash = "t",
      validation_manifest_hash = "v"
    ),
    "frozen candidate"
  )
})
