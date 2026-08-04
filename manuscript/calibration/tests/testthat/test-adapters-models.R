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

testthat::test_that("model scenarios include null, moderate, and held-out strata", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  scenarios <- env$calibration_scenarios()
  model <- scenarios[scenarios$analysis_family %in% c("lm", "binomial", "poisson"), ]
  testthat::expect_true(all(c("lm", "binomial", "poisson") %in% model$analysis_family))
  testthat::expect_gte(sum(model$truth_class == "null"), 3L)
  testthat::expect_gte(sum(model$design_layer == "validation"), 3L)
  testthat::expect_true(all(vapply(model$parameters, function(x) is.list(x$generator), logical(1))))
})

testthat::test_that("frozen smoke log-scale aliases control binomial and Poisson truth", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  b <- env$generate_binomial(list(n = 2000L, baseline_log_odds = -2, treatment_log_odds = 0,
                                   covariate_effect = 0), seed = 21L)
  testthat::expect_equal(b$truth$intercept, -2, tolerance = 0)
  testthat::expect_equal(b$truth$effect, 0, tolerance = 0)
  p <- env$generate_poisson(list(n = 2000L, baseline_log_rate = -1, treatment_log_rate = 0,
                                 exposure = FALSE, covariate_effect = 0), seed = 22L)
  testthat::expect_equal(p$truth$intercept, -1, tolerance = 0)
  testthat::expect_equal(p$truth$effect, 0, tolerance = 0)
})

testthat::test_that("one-row scenario data frames dispatch Poisson full adapters", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  scenario <- env$calibration_scenarios()
  scenario <- scenario[scenario$scenario_id == "poisson_core_offset", , drop = FALSE]
  generated <- env$generate_poisson(scenario$parameters[[1L]], seed = 31L)
  fit <- suppressWarnings(env$run_robustness_glm(
    generated$data, scenario, n_boot = 1L, max_removal_pct = 0.10, seed = 31L
  ))
  testthat::expect_s3_class(fit, "robustness_model")
  testthat::expect_identical(fit$family, "poisson")
  testthat::expect_identical(fit$link, "log")
})

testthat::test_that("nested list scenarios dispatch generic robustness to Poisson", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  scenario <- list(parameters = list(
    generator = list(n = 42L, rate = 0.8, effect = log(1.4), exposure = TRUE),
    analysis = list(family = "poisson", term = "treatmentB", alpha = 0.05)
  ))
  generated <- env$generate_poisson(scenario, seed = 32L)
  screen <- env$primary_decision(generated$data, scenario)
  fit <- suppressWarnings(env$calibration_robustness_analysis(
    generated$data, scenario, n_boot = 1L, max_removal_pct = 0.10, seed = 32L
  ))
  testthat::expect_s3_class(fit, "robustness_model")
  testthat::expect_identical(fit$family, "poisson")
  testthat::expect_equal(screen$p_value, fit$original_p, tolerance = 1e-10)
})

testthat::test_that("LM and GLM aliases and non-convergence are auditable failures", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  lm_data <- data.frame(outcome = rnorm(24), treatment = factor(rep(c("A", "B"), 12)),
                        baseline = rnorm(24), alias = rnorm(24))
  lm_data$alias <- lm_data$baseline
  lm_fail <- env$primary_decision_lm(lm_data, list(analysis = list(term = "treatmentB")),
                                    formula = outcome ~ treatment + baseline + alias)
  testthat::expect_identical(lm_fail$status, "failed")
  testthat::expect_identical(lm_fail$failure_class, "aliased_term")

  glm_data <- data.frame(outcome = c(rep(0L, 20), rep(1L, 20)),
                         treatment = factor(rep(c("A", "B"), each = 20)),
                         baseline = rnorm(40))
  glm_data$alias <- glm_data$baseline
  glm_alias <- env$primary_decision_glm(glm_data, list(analysis = list(term = "treatmentB")),
                                        family = binomial(), formula = outcome ~ treatment + baseline + alias)
  testthat::expect_identical(glm_alias$status, "failed")
  testthat::expect_identical(glm_alias$failure_class, "aliased_term")

  nonconv <- env$primary_decision_glm(glm_data, list(analysis = list(term = "treatmentB")),
                                      family = binomial(), formula = outcome ~ treatment,
                                      control = glm.control(maxit = 1L))
  testthat::expect_identical(nonconv$status, "failed")
  testthat::expect_identical(nonconv$failure_class, "non_convergence")
})

testthat::test_that("screening rejects unsupported GLM links explicitly", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  dat <- env$generate_binomial(list(n = 30L), seed = 51L)$data
  fail <- env$primary_decision_glm(dat, list(analysis = list(term = "treatmentB")),
                                   family = binomial(link = "probit"))
  testthat::expect_identical(fail$status, "failed")
  testthat::expect_identical(fail$failure_class, "unsupported_link")
  testthat::expect_match(fail$failure_message, "logit")
  full <- env$run_robustness_glm(dat, list(analysis = list(term = "treatmentB")),
                                 family = binomial(link = "probit"), n_boot = 1L)
  testthat::expect_identical(full$status, "failed")
  testthat::expect_identical(full$failure_class, "unsupported_link")
})

testthat::test_that("model generators and screeners validate weights, alpha, levels, and row ids", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  no_weights <- env$generate_binomial(list(n = 30L, weights = FALSE), seed = 61L)
  testthat::expect_null(no_weights$weights)
  testthat::expect_error(env$generate_lm(list(n = 30L, factor_levels = 7L)), "factor_levels")

  dat <- no_weights$data
  testthat::expect_identical(
    env$primary_decision_lm(dat, list(analysis = list(term = "treatmentB", alpha = 0)),
                            alpha = 0)$failure_class,
    "invalid_alpha"
  )
  dup <- dat
  dup$.row_id[[2L]] <- dup$.row_id[[1L]]
  testthat::expect_identical(
    env$primary_decision_glm(dup, list(analysis = list(term = "treatmentB")), family = binomial())$failure_class,
    "duplicate_row_id"
  )
})

testthat::test_that("generated weights and inferred unsupported links propagate through generic adapters", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  generated <- env$generate_binomial(list(n = 42L, effect = log(1.5), weights = TRUE), seed = 62L)
  scenario <- list(analysis = list(family = "binomial", link = "logit", term = "treatmentB"))
  screen <- env$primary_decision(generated, scenario)
  testthat::expect_identical(screen$status, "ok")
  testthat::expect_equal(screen$weights, generated$weights)
  bad <- env$primary_decision(generated$data,
                              list(analysis = list(family = "binomial", link = "probit", term = "treatmentB")))
  testthat::expect_identical(bad$failure_class, "unsupported_link")
})

testthat::test_that("malformed inferred links return explicit adapter failures", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  generated <- env$generate_binomial(list(n = 36L), seed = 63L)
  for (link in list(NA_character_, character(), c("logit", "probit"), "bogus")) {
    scenario <- list(analysis = list(family = "binomial", link = link, term = "treatmentB"))
    screen <- env$primary_decision(generated$data, scenario)
    testthat::expect_identical(screen$status, "failed")
    testthat::expect_true(screen$failure_class %in% c("invalid_link", "unsupported_link"))
    full <- env$calibration_robustness_analysis(generated$data, scenario, n_boot = 1L)
    testthat::expect_identical(full$status, "failed")
    testthat::expect_true(full$failure_class %in% c("invalid_link", "unsupported_link"))
  }
  adapter <- env$calibration_model_adapters()$binomial
  bad <- adapter$primary_decision(generated$data,
                                  list(analysis = list(family = "binomial", link = "probit", term = "treatmentB")))
  testthat::expect_identical(bad$failure_class, "unsupported_link")
})

testthat::test_that("factory links are preserved when scenarios omit link settings", {
  env <- new.env(parent = globalenv())
  sys.source(normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE), env)
  env$load_calibration(project_root = normalizePath(file.path("..", "..", "..", "..")), envir = env)
  generated <- env$generate_binomial(list(n = 36L), seed = 64L)
  adapter <- env$glm_model_adapter(binomial(link = "probit"))
  bad <- adapter$primary_decision(generated$data, list(analysis = list(term = "treatmentB")))
  testthat::expect_identical(bad$status, "failed")
  testthat::expect_identical(bad$failure_class, "unsupported_link")
  adapter_p <- env$glm_model_adapter(poisson(link = "identity"))
  generated_p <- env$generate_poisson(list(n = 36L), seed = 65L)
  bad_p <- adapter_p$primary_decision(generated_p$data, list(analysis = list(term = "treatmentB")))
  testthat::expect_identical(bad_p$status, "failed")
  testthat::expect_identical(bad_p$failure_class, "unsupported_link")
  row <- env$calibration_scenarios()
  row <- row[row$scenario_id == "binomial_core_logit", , drop = FALSE]
  row$parameters[[1L]]$analysis$link <- NULL
  bad_row <- env$glm_model_adapter(binomial(link = "probit"))$primary_decision(
    generated$data, row
  )
  testthat::expect_identical(bad_row$failure_class, "unsupported_link")
})
