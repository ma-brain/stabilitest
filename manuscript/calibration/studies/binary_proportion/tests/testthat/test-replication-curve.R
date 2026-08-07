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

# A fixture of completed significant rows with a score and a binary replication
# outcome.  Score is strongly predictive of replication here.
.replication_fixture <- function(n = 400) {
  set.seed(20260807)
  score <- sample(0:100, n, replace = TRUE)
  # Replication probability increasing in score (well-calibrated logistic).
  prob <- 1 / (1 + exp(-(-3 + 0.08 * score)))
  replicated <- stats::rbinom(n, 1L, prob)
  data.frame(
    scenario_id = rep(c("s1", "s2", "s3", "s4"), each = n / 4),
    overall_score = as.numeric(score),
    replication_significant = as.integer(replicated),
    original_p = runif(n, 0, 0.05),
    stringsAsFactors = FALSE
  )
}

test_that("fit_fisher_exact_replication_curve fits a logistic replication model", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_replication_curve(.replication_fixture())
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_true(is.numeric(fit$intercept) && is.finite(fit$intercept))
  testthat::expect_true(is.numeric(fit$slope) && is.finite(fit$slope))
  testthat::expect_true(is.list(fit$calibration))
  testthat::expect_true(is.function(fit$predict))
  # predict maps a score to a replication probability in [0,1].
  p <- fit$predict(50)
  testthat::expect_true(is.numeric(p) && length(p) == 1L && p >= 0 && p <= 1)
})

test_that("replication curve returns the p-only reference map", {
  env <- .load_bp_env()
  fit <- env$fit_fisher_exact_replication_curve(.replication_fixture())
  testthat::expect_true(is.function(fit$p_only_reference))
  p0 <- fit$p_only_reference(0.01)
  testthat::expect_true(is.numeric(p0) && length(p0) == 1L && p0 >= 0 && p0 <= 1)
})

test_that("replication_curve_held_out_diagnostics computes the four gate quantities", {
  env <- .load_bp_env()
  data <- .replication_fixture()
  fit <- env$fit_fisher_exact_replication_curve(data)
  diag <- env$replication_curve_held_out_diagnostics(fit, data)
  testthat::expect_true(is.numeric(diag$intercept_logit_abs) &&
                         diag$intercept_logit_abs >= 0)
  testthat::expect_true(is.numeric(diag$slope) && is.finite(diag$slope))
  testthat::expect_true(is.numeric(diag$max_abs_bin_error) &&
                         diag$max_abs_bin_error >= 0)
  testthat::expect_true(is.numeric(diag$brier_score) && diag$brier_score >= 0)
  testthat::expect_true(is.numeric(diag$brier_p_only) && diag$brier_p_only >= 0)
  testthat::expect_true(is.numeric(diag$brier_improvement))
})

test_that("replication_curve_passes gates a well-calibrated fixture", {
  env <- .load_bp_env()
  # Large n, strongly calibrated: gates should pass.
  data <- .replication_fixture(2000)
  fit <- env$fit_fisher_exact_replication_curve(data)
  verdict <- env$replication_curve_passes(fit, data)
  testthat::expect_true(is.logical(verdict$passes) && length(verdict$passes) == 1L)
  testthat::expect_true(is.character(verdict$reasons))
})

test_that("replication curve fitting is deterministic for identical inputs", {
  env <- .load_bp_env()
  data <- .replication_fixture(400)
  a <- env$fit_fisher_exact_replication_curve(data)
  b <- env$fit_fisher_exact_replication_curve(data)
  testthat::expect_equal(a$intercept, b$intercept)
  testthat::expect_equal(a$slope, b$slope)
})
