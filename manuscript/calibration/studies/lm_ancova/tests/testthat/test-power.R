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

test_that("ANCOVA effect solver reaches frozen power targets", {
  env <- .load_lm_ancova_study_env()
  for (n in c(40L, 80L, 160L, 240L)) {
    for (target in c(0.60, 0.90)) {
      beta <- env$solve_ancova_effect(n, target, alpha = 0.05, residual_sd = 1)
      testthat::expect_equal(
        env$ancova_nominal_power(beta, n, alpha = 0.05, residual_sd = 1),
        target,
        tolerance = 1e-8
      )
    }
  }
  testthat::expect_identical(
    env$solve_ancova_effect(80L, 0, alpha = 0.05, residual_sd = 1),
    0
  )
  testthat::expect_gt(
    env$solve_ancova_effect(80L, 0.90, alpha = 0.05, residual_sd = 1),
    env$solve_ancova_effect(80L, 0.60, alpha = 0.05, residual_sd = 1)
  )
})

test_that("ANCOVA power increases monotonically with absolute effect", {
  env <- .load_lm_ancova_study_env()
  betas <- seq(0, 1.5, by = 0.25)
  powers <- vapply(
    betas,
    function(beta) env$ancova_nominal_power(beta, n = 80L, alpha = 0.05, residual_sd = 1),
    numeric(1)
  )
  testthat::expect_true(all(diff(powers) >= -1e-12))
})
