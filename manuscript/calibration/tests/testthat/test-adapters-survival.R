testthat::test_that("Cox calibration adapter screens single and multi-df terms", {
  testthat::skip_if_not_installed("survival")
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  dat <- env$generate_cox(n = 80, hazard_ratio = 1.6, seed = 11,
                          distribution = "weibull", shape = 1.2)
  one <- env$screen_cox(dat, term = "treatment", alpha = .05)
  full <- stabilitest::robustness_surv(survival::Surv(time, event) ~ treatment,
                                       dat, term = "treatment", n_boot = 5,
                                       max_removal_pct = .1, seed = 12)
  testthat::expect_equal(one$p, full$original_p, tolerance = 1e-12)
  testthat::expect_identical(one$conclusion, if (full$original_p < .05) "significant" else "non_significant")

  dat$arm3 <- factor(sample(c("A", "B", "C"), nrow(dat), replace = TRUE))
  dat$event <- as.integer(dat$event)
  multi <- env$screen_cox(dat, formula = survival::Surv(time, event) ~ arm3,
                          term = "arm3")
  full_multi <- stabilitest::robustness_surv(survival::Surv(time, event) ~ arm3,
                                             dat, term = "arm3", n_boot = 5,
                                             max_removal_pct = .1, seed = 12)
  testthat::expect_equal(multi$p, full_multi$original_p, tolerance = 1e-12)
  testthat::expect_identical(multi$test, "joint_LRT")
  testthat::expect_error(env$screen_cox(dat, alpha = 0), "alpha")
  testthat::expect_error(env$screen_cox(dat, alpha = NA_real_), "alpha")
})

testthat::test_that("Cox calibration adapter records explicit failure classes", {
  testthat::skip_if_not_installed("survival")
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  no_event <- env$generate_cox(n = 30, event_rate = 0, seed = 1)
  failed <- env$run_cox_adapter(no_event, term = "treatment", n_boot = 5,
                                 max_removal_pct = .1)
  testthat::expect_identical(failed$status, "failed")
  testthat::expect_identical(failed$failure_class, "no_event")
  all_censored <- env$generate_cox(n = 30, censoring_rate = 1, seed = 1)
  failed2 <- env$run_cox_adapter(all_censored, term = "treatment", n_boot = 5,
                                  max_removal_pct = .1)
  testthat::expect_identical(failed2$failure_class, "all_censored")
  testthat::expect_error(env$screen_cox(no_event, term = "treatment"), "event")
})

testthat::test_that("Cox generator interprets censoring_rate as the censored fraction", {
  testthat::skip_if_not_installed("survival")
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  dat <- env$generate_cox(n = 200, hazard_ratio = 1, censoring_rate = .2, seed = 41)
  metadata <- attr(dat, "calibration_metadata")
  testthat::expect_equal(mean(dat$event == 0L), .2, tolerance = 1 / nrow(dat))
  testthat::expect_equal(metadata$realized_censoring_rate, mean(dat$event == 0L))

  same <- env$generate_cox(n = 80, censoring_rate = .2, seed = 41,
                           censoring_seed = 99)
  same_again <- env$generate_cox(n = 80, censoring_rate = .2, seed = 41,
                                 censoring_seed = 99)
  different <- env$generate_cox(n = 80, censoring_rate = .2, seed = 41,
                                censoring_seed = 100)
  testthat::expect_identical(same, same_again)
  testthat::expect_false(identical(same$event, different$event))
})

testthat::test_that("Cox generator rejects malformed shape, size, and seed inputs", {
  testthat::skip_if_not_installed("survival")
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  testthat::expect_error(env$generate_cox(shape = 0), "shape must be positive")
  testthat::expect_error(env$generate_cox(n = NA_integer_), "integer >= 10")
  testthat::expect_error(env$generate_cox(seed = -1), "non-negative integer")
  testthat::expect_error(env$generate_cox(censoring_seed = 1.5), "non-negative integer")
})
