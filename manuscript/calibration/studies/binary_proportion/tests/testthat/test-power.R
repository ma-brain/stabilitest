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

test_that("solve_prop_effect returns p0 for the null target", {
  env <- .load_bp_env()
  for (p0 in c(0.10, 0.25, 0.50)) {
    for (n in c(25, 50, 100, 200)) {
      p1 <- env$solve_prop_effect(n = n, p0 = p0, target = 0)
      testthat::expect_equal(p1, p0, tolerance = 1e-12,
                              info = sprintf("n=%d p0=%.2f", n, p0))
    }
  }
})

test_that("solve_prop_effect is monotone non-decreasing in the target", {
  env <- .load_bp_env()
  for (p0 in c(0.10, 0.25, 0.50)) {
    for (n in c(25, 50, 100, 200)) {
      p1_lo <- env$solve_prop_effect(n = n, p0 = p0, target = 0.60)
      p1_hi <- env$solve_prop_effect(n = n, p0 = p0, target = 0.95)
      testthat::expect_true(p1_hi >= p1_lo,
                            info = sprintf("n=%d p0=%.2f", n, p0))
      testthat::expect_true(p1_lo >= p0,
                            info = sprintf("n=%d p0=%.2f", n, p0))
    }
  }
})

test_that("solve_prop_effect makes enumerated exact Fisher power equal target", {
  env <- .load_bp_env()
  # Reproduce the projection-script enumeration independently of the solver.
  fisher_p_matrix <- function(n) {
    P <- matrix(NA_real_, n + 1, n + 1)
    for (x0 in 0:n) for (x1 in 0:n) {
      P[x0 + 1, x1 + 1] <- stats::fisher.test(
        matrix(c(x1, n - x1, x0, n - x0), 2)
      )$p.value
    }
    P
  }
  exact_power <- function(P, n, p0, p1, alpha = 0.05) {
    w <- outer(stats::dbinom(0:n, n, p0), stats::dbinom(0:n, n, p1))
    sum(w[P <= alpha])
  }
  for (target in c(0.60, 0.95)) {
    for (n in c(25, 50, 100)) {
      for (p0 in c(0.10, 0.25, 0.50)) {
        p1 <- env$solve_prop_effect(n = n, p0 = p0, target = target)
        testthat::expect_false(is.na(p1),
                               info = sprintf("target=%.2f n=%d p0=%.2f", target, n, p0))
        P <- fisher_p_matrix(n)
        achieved <- exact_power(P, n, p0, p1)
        # The SAP contracts the solver to reproduce exact Fisher power equal to
        # the target within 1e-6 (absolute).  Fisher power is a discrete step
        # function of p1, so the comparison is absolute, not mean-scaled.
        testthat::expect_true(abs(achieved - target) <= 1e-6,
                              info = sprintf(
                                "target=%.2f n=%d p0=%.2f p1=%.4f achieved=%.6f",
                                target, n, p0, p1, achieved))
      }
    }
  }
})

test_that("exact_fisher_power matches a brute-force Monte Carlo at large n", {
  env <- .load_bp_env()
  set.seed(20260807)
  n <- 60; p0 <- 0.25; p1 <- 0.50
  enumerated <- env$exact_fisher_power(n = n, p0 = p0, p1 = p1, alpha = 0.05)
  draws <- 40000
  hit <- 0L
  for (i in seq_len(draws)) {
    x0 <- stats::rbinom(1, n, p0)
    x1 <- stats::rbinom(1, n, p1)
    p <- stats::fisher.test(matrix(c(x1, n - x1, x0, n - x0), 2))$p.value
    if (p < 0.05) hit <- hit + 1L
  }
  mc <- hit / draws
  testthat::expect_equal(enumerated, mc, tolerance = 0.02)
})
