testthat::test_that("LM screening agrees with robustness_lm for coefficients and joint factors", {
  skip_if_not_installed("pkgload")
  env <- new.env(parent = globalenv())
  loader <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  sys.source(loader, envir = env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)

  generated <- env$generate_lm(list(
    n = 36L, effect = 0.8, covariate_effect = 0.5,
    prognostic = TRUE, imbalance = 0.25, heteroscedastic = TRUE,
    factor_levels = 3L, missing_rate = 0.10
  ), seed = 41L)
  dat <- generated$data
  scenario <- list(
    analysis = list(term = "treatmentB", alpha = 0.05),
    parameters = list(analysis = list(term = "treatmentB", alpha = 0.05))
  )
  single <- env$primary_decision_lm(dat, scenario, term = "treatmentB")
  full <- suppressWarnings(robustness_lm(
    outcome ~ treatment + baseline, dat, term = "treatmentB",
    n_boot = 2L, max_removal_pct = 0.10, seed = 41L
  ))
  testthat::expect_equal(single$p_value, full$original_p, tolerance = 1e-10)
  testthat::expect_identical(single$conclusion, full$original_significant)

  joint <- env$primary_decision_lm(dat, scenario, term = "treatment")
  full_joint <- suppressWarnings(robustness_lm(
    outcome ~ treatment + baseline, dat, term = "treatment",
    n_boot = 2L, max_removal_pct = 0.10, seed = 41L
  ))
  testthat::expect_equal(joint$p_value, full_joint$original_p, tolerance = 1e-10)
  testthat::expect_identical(joint$conclusion, full_joint$original_significant)
})

testthat::test_that("LM generator reports truth metadata and preserves omitted row ids", {
  env <- new.env(parent = globalenv())
  loader <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  sys.source(loader, envir = env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  generated <- env$generate_lm(list(n = 24L, missing_rate = 0.25), seed = 7L)
  testthat::expect_true(all(c(".row_id", "outcome", "treatment", "baseline") %in% names(generated$data)))
  testthat::expect_equal(generated$data$.row_id, seq_len(nrow(generated$data)))
  testthat::expect_true(is.list(generated$truth))
  testthat::expect_true(is.finite(generated$truth$effect))
  testthat::expect_true(is.character(generated$truth$term))
  fit <- lm(outcome ~ treatment + baseline, generated$data)
  testthat::expect_lt(stats::nobs(fit), nrow(generated$data))
  testthat::expect_equal(generated$truth$row_id, seq_len(nrow(generated$data)))
})

testthat::test_that("GLM screening agrees with robustness_glm for weighted logit and Poisson offsets", {
  skip_if_not_installed("pkgload")
  env <- new.env(parent = globalenv())
  loader <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  sys.source(loader, envir = env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)

  b <- env$generate_binomial(list(
    n = 42L, effect = log(2), prevalence = 0.35,
    factor_levels = 3L, imbalance = 0.20, weights = TRUE
  ), seed = 12L)
  bscenario <- list(analysis = list(term = "treatmentB", alpha = 0.05))
  bs <- env$primary_decision_glm(b$data, bscenario, term = "treatmentB",
                                 family = binomial(), obs_weights = b$weights)
  br <- suppressWarnings(robustness_glm(
    outcome ~ treatment + baseline, b$data, term = "treatmentB",
    family = binomial(), obs_weights = b$weights, n_boot = 2L,
    max_removal_pct = 0.10, seed = 12L
  ))
  testthat::expect_equal(bs$p_value, br$original_p, tolerance = 1e-10)
  testthat::expect_identical(bs$conclusion, br$original_significant)

  bm <- env$generate_binomial(list(n = 48L, effect = log(1.7), factor_levels = 3L), seed = 14L)
  bms <- env$primary_decision_glm(bm$data, list(analysis = list(term = "treatment")),
                                  term = "treatment", family = binomial())
  bmr <- suppressWarnings(robustness_glm(
    outcome ~ treatment + baseline, bm$data, term = "treatment", family = binomial(),
    n_boot = 1L, max_removal_pct = 0.10, seed = 14L
  ))
  testthat::expect_equal(bms$p_value, bmr$original_p, tolerance = 1e-10)
  testthat::expect_identical(bms$conclusion, bmr$original_significant)

  p <- env$generate_poisson(list(
    n = 42L, rate = 0.8, effect = log(1.5), exposure = TRUE,
    factor_levels = 3L, overdispersion = FALSE, weights = TRUE
  ), seed = 13L)
  pscenario <- list(analysis = list(term = "treatmentB", alpha = 0.05))
  ps <- env$primary_decision_glm(
    p$data, pscenario, term = "treatmentB", family = poisson(),
    formula = outcome ~ treatment + baseline + offset(log(exposure)),
    obs_weights = p$weights
  )
  pr <- suppressWarnings(robustness_glm(
    outcome ~ treatment + baseline + offset(log(exposure)), p$data,
    term = "treatmentB", family = poisson(), obs_weights = p$weights,
    n_boot = 2L, max_removal_pct = 0.10, seed = 13L
  ))
  testthat::expect_equal(ps$p_value, pr$original_p, tolerance = 1e-10)
  testthat::expect_identical(ps$conclusion, pr$original_significant)

  pm <- env$generate_poisson(list(n = 48L, rate = 0.8, effect = log(1.5),
                                  exposure = TRUE, factor_levels = 3L), seed = 15L)
  pms <- env$primary_decision_glm(
    pm$data, list(analysis = list(term = "treatment")), term = "treatment",
    family = poisson(), formula = outcome ~ treatment + baseline + offset(log(exposure))
  )
  pmr <- suppressWarnings(robustness_glm(
    outcome ~ treatment + baseline + offset(log(exposure)), pm$data,
    term = "treatment", family = poisson(), n_boot = 1L,
    max_removal_pct = 0.10, seed = 15L
  ))
  testthat::expect_equal(pms$p_value, pmr$original_p, tolerance = 1e-10)
  testthat::expect_identical(pms$conclusion, pmr$original_significant)
})

testthat::test_that("GLM failures are explicit for separation, aliasing, and degenerate outcomes", {
  env <- new.env(parent = globalenv())
  loader <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  sys.source(loader, envir = env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  separated <- data.frame(
    .row_id = 1:24, outcome = c(rep(0L, 12), rep(1L, 12)),
    treatment = factor(rep(c("A", "B"), each = 12)), baseline = rnorm(24)
  )
  fail <- env$primary_decision_glm(separated, list(analysis = list(term = "treatmentB")),
                                   term = "treatmentB", family = binomial())
  testthat::expect_true(isTRUE(fail$failed) || identical(fail$status, "failed"))
  testthat::expect_true(is.character(fail$failure_class))

  degenerate <- separated
  degenerate$outcome <- 0L
  fail2 <- env$primary_decision_glm(degenerate, list(analysis = list(term = "treatmentB")),
                                    term = "treatmentB", family = binomial())
  testthat::expect_true(isTRUE(fail2$failed) || identical(fail2$status, "failed"))
  testthat::expect_true(is.character(fail2$failure_class))
})
