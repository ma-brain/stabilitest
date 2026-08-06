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

test_that("generate_lm_ancova encodes balance, R2, and seed reproducibility", {
  env <- .load_lm_ancova_study_env()
  scenario <- list(
    parameters = list(
      generator = list(
        n = 80L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1
      )
    )
  )

  a <- env$generate_lm_ancova(scenario, seed = 4401L)
  b <- env$generate_lm_ancova(scenario, seed = 4401L)
  c <- env$generate_lm_ancova(scenario, seed = 4402L)

  testthat::expect_identical(a$status, "ok")
  testthat::expect_true(is.data.frame(a$data))
  testthat::expect_true(all(c(".row_id", "outcome", "treatment", "baseline") %in% names(a$data)))
  testthat::expect_identical(levels(a$data$treatment), c("A", "B"))
  testthat::expect_identical(as.integer(table(a$data$treatment)), c(40L, 40L))
  testthat::expect_equal(a$truth$baseline_r2, 0.40, tolerance = 1e-12)
  testthat::expect_equal(
    a$truth$gamma,
    sqrt(0.40 / (1 - 0.40)),
    tolerance = 1e-12
  )
  testthat::expect_equal(a$data, b$data)
  testthat::expect_false(isTRUE(all.equal(a$data$outcome, c$data$outcome)))
})

test_that("verify_ancova_power uses only the primary ANCOVA test", {
  env <- .load_lm_ancova_study_env()
  scenario <- list(
    parameters = list(
      generator = list(
        n = 80L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1
      ),
      analysis = list(
        formula = "outcome ~ treatment + baseline",
        term = "treatmentB",
        alpha = 0.05
      )
    )
  )

  verification <- env$verify_ancova_power(scenario, draws = 2000L, seed = 5501L)
  testthat::expect_true(is.list(verification))
  testthat::expect_true(is.numeric(verification$achieved_power))
  testthat::expect_equal(verification$target_power, 0.60, tolerance = 1e-12)
  testthat::expect_lt(abs(verification$achieved_power - 0.60), 0.04)
  testthat::expect_false(isTRUE(verification$used_robustness_score))
})
