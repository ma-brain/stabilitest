test_that("case study reproduces published deterministic metrics", {
  res <- robustness_analysis(pain_treatment, pain_placebo,
                             test_type = "t.test", n_boot = 100, seed = 1)
  expect_s3_class(res, "robustness_analysis")
  # Welch test on the fixed dataset (deterministic)
  expect_equal(res$original_p, 0.00182, tolerance = 1e-2)
  expect_equal(unname(res$original_statistic), -3.286, tolerance = 1e-3)
  expect_equal(res$original_mean_diff, -11.40, tolerance = 1e-2)
  # deterministic components
  expect_equal(res$robustness_metrics$jackknife_conclusion_stability, 100)
  expect_equal(res$worstcase$fragility_index, 6L)
  expect_equal(res$robustness_metrics$p_at_fragility, 0.0602, tolerance = 1e-3)
})

test_that("score is bounded and weights are validated", {
  res <- robustness_analysis(pain_treatment, pain_placebo, n_boot = 50)
  expect_gte(res$robustness_metrics$overall_robustness, 0)
  expect_lte(res$robustness_metrics$overall_robustness, 100)
  expect_error(robustness_analysis(pain_treatment, pain_placebo,
                                   weights = c(0.5, 0.5, 0.5)))
})

test_that("all continuous test types run", {
  set.seed(42)
  x <- rnorm(20); y <- rnorm(20, 1)
  for (tt in c("t.test", "wilcoxon", "brunner_munzel")) {
    expect_s3_class(robustness_analysis(x, y, test_type = tt, n_boot = 30),
                    "robustness_analysis")
  }
  for (tt in c("paired.t.test", "wilcoxon.paired")) {
    expect_s3_class(robustness_analysis(x, y, test_type = tt, n_boot = 30),
                    "robustness_analysis")
  }
})

test_that("significant default Welch has narrow calibrated metadata", {
  set.seed(41)
  result <- robustness_analysis(
    rep(3, 12) + rnorm(12, 0, .1),
    rep(0, 12) + rnorm(12, 0, .1),
    test_type = "t.test", n_boot = 10, seed = 41
  )

  expect_identical(result$calibration$calibration_unit, "welch_unpaired")
  expect_identical(result$calibration$endpoint, "mean_difference")
  expect_true(result$calibration$applicable)
  expect_identical(result$calibration$status, "validated_method_specific")
  expect_match(result$robustness_interpretation,
               "Robust|Moderately Robust|Fragile")
})

test_that("non-Welch two-vector methods keep scores but suppress labels", {
  fx <- list(
    g1 = c(rep(1, 12), rep(0, 3)),
    g2 = c(rep(1, 3), rep(0, 12))
  )
  set.seed(42)
  x <- rnorm(12, 3)
  y <- rnorm(12)
  bm_x <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
  bm_y <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
  methods <- list(
    paired.t.test = list(g1 = x, g2 = y),
    wilcoxon = list(g1 = x, g2 = y),
    wilcoxon.paired = list(g1 = x, g2 = y),
    brunner_munzel = list(g1 = bm_x, g2 = bm_y),
    fisher = fx,
    chisq = fx,
    prop = fx
  )

  results <- lapply(names(methods), function(method) {
    args <- c(
      list(group1 = methods[[method]]$g1, group2 = methods[[method]]$g2),
      list(test_type = method, n_boot = 10, seed = 42)
    )
    do.call(robustness_analysis, args)
  })
  expect_true(all(vapply(results, function(x) {
    is.numeric(x$robustness_metrics$overall_robustness)
  }, logical(1))))
  expect_true(all(vapply(results, function(x) {
    is.na(x$robustness_interpretation)
  }, logical(1))))
  expect_false(any(vapply(results, function(x) {
    identical(x$calibration$calibration_unit, "two_sample")
  }, logical(1))))
})

test_that("print and narrative output expose calibration status", {
  x <- c(2, 2.1, 1.9, 2.2, 2.0, 2.1, 1.8, 2.2, 2.1, 1.9)
  y <- c(0, 0.1, -0.1, 0.2, 0.0, 0.15, -0.2, 0.25, 0.1, -0.1)

  calibrated <- robustness_analysis(x, y, test_type = "t.test",
                                    n_boot = 10, seed = 42,
                                    interpret = TRUE)
  calibrated_text <- capture.output(print(calibrated))
  expect_true(any(grepl("Welch calibration", calibrated_text)))
  expect_true(any(grepl("Robust|Moderately Robust|Fragile",
                         calibrated_text)))

  uncalibrated <- robustness_analysis(x, y, test_type = "paired.t.test",
                                      n_boot = 10, seed = 42,
                                      interpret = TRUE)
  uncalibrated_text <- capture.output(print(uncalibrated))
  expect_true(any(grepl("OVERALL ROBUSTNESS: [0-9.]+/100",
                        uncalibrated_text)))
  expect_true(any(grepl("categorical bands not calibrated for this method",
                        uncalibrated_text)))
  expect_false(any(grepl("\\((Robust|Moderately Robust|Fragile)\\)",
                         uncalibrated_text)))
  expect_true(grepl("categorical bands not calibrated for this method",
                    uncalibrated$interpretation$overall))
  expect_true(grepl("component metrics",
                    uncalibrated$interpretation$recommendation))
})

test_that("inapplicable conclusions have a distinct display status", {
  result <- robustness_analysis(
    seq(-1, 1, length.out = 12), seq(-1, 1, length.out = 12),
    test_type = "t.test", n_boot = 10, seed = 41, interpret = TRUE
  )
  text <- capture.output(print(result))
  expect_true(any(grepl(
    "no categorical band assigned because observed conclusion is not eligible",
    text
  )))
  expect_false(any(grepl(
    "categorical bands not calibrated for this method", text
  )))
  expect_true(grepl(
    "observed conclusion is not eligible",
    result$interpretation$recommendation
  ))
})

test_that("Welch labels are suppressed outside the validated design", {
  set.seed(41)
  x <- rep(3, 12) + rnorm(12, 0, .1)
  y <- rep(0, 12) + rnorm(12, 0, .1)

  non_significant <- robustness_analysis(
    seq(-1, 1, length.out = 12), seq(-1, 1, length.out = 12),
    test_type = "t.test", n_boot = 10, seed = 41
  )
  expect_false(non_significant$original_significant)
  expect_identical(non_significant$calibration$status, "bands_not_applicable")
  expect_true(is.na(non_significant$robustness_interpretation))

  custom_weights <- robustness_analysis(
    x, y, test_type = "t.test", n_boot = 10, seed = 41,
    weights = c(jackknife = 0.3, fragility = 0.5, bootstrap = 0.2)
  )
  expect_true(custom_weights$original_significant)
  expect_identical(custom_weights$calibration$status, "uncalibrated")
  expect_true(is.na(custom_weights$robustness_interpretation))

  custom_budget <- robustness_analysis(
    x, y, test_type = "t.test", n_boot = 10, seed = 41,
    max_removal_pct = 0.25
  )
  expect_true(custom_budget$original_significant)
  expect_identical(custom_budget$calibration$status, "uncalibrated")
  expect_true(is.na(custom_budget$robustness_interpretation))
})

test_that("wilcoxon and brunner_munzel report Hodges-Lehmann shift", {
  set.seed(42)
  x <- rnorm(20); y <- rnorm(20, 1)
  hl <- as.numeric(median(outer(x, y, `-`)))

  rw <- robustness_analysis(x, y, test_type = "wilcoxon", n_boot = 20, seed = 1)
  expect_equal(rw$original_mean_diff, hl)
  expect_identical(rw$sample_info$effect_type, "hodges_lehmann")
  expect_true(all(is.finite(rw$original_ci)))

  rb <- robustness_analysis(x, y, test_type = "brunner_munzel",
                            n_boot = 20, seed = 1)
  expect_equal(rb$original_mean_diff, hl)
  expect_identical(rb$sample_info$effect_type, "hodges_lehmann")
  expect_true(is.finite(rb$sample_info$stochastic_superiority))
  expect_true(rb$sample_info$stochastic_superiority > 0.5)
  expect_true(all(is.na(rb$original_ci)))
})

test_that("brunner_munzel matches Neubert-Brunner pain-score reference", {
  # Brunner & Munzel (2000) / Neubert & Brunner (2007) example
  Y <- c(1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 4, 1, 1)
  N <- c(3, 3, 4, 3, 1, 2, 3, 1, 1, 5, 4)
  res <- robustness_analysis(Y, N, test_type = "brunner_munzel",
                             n_boot = 20, seed = 2)
  expect_equal(unname(res$original_statistic), 3.1375, tolerance = 1e-3)
  expect_equal(res$original_p, 0.005786, tolerance = 1e-4)
  expect_equal(res$sample_info$stochastic_superiority, 0.788961, tolerance = 1e-5)
})

test_that("bootstrap is reproducible under a fixed seed", {
  a <- robustness_analysis(pain_treatment, pain_placebo, n_boot = 50, seed = 7)
  b <- robustness_analysis(pain_treatment, pain_placebo, n_boot = 50, seed = 7)
  expect_equal(a$robustness_metrics$overall_robustness,
               b$robustness_metrics$overall_robustness)
})

test_that("invalid Wilcoxon bootstrap replicates are counted and excluded", {
  res <- suppressWarnings(robustness_analysis(
    c(0, 0, 0, 0, 1), c(0, 0, 0, 1, 1),
    test_type = "wilcoxon", n_boot = 500, seed = 9
  ))

  expect_true(is.finite(res$robustness_metrics$overall_robustness))
  expect_gt(res$bootstrap$n_failed, 0L)
  expect_equal(res$bootstrap$n_valid + res$bootstrap$n_failed, 500L)
  expect_equal(nrow(res$bootstrap$results), 500L)
})

test_that("interpretation text generates without error", {
  res <- robustness_analysis(pain_treatment, pain_placebo,
                             n_boot = 50, interpret = TRUE)
  expect_type(res$interpretation$report, "character")
  expect_match(res$interpretation$overall, "Welch")
})

test_that("robustness_lm works on an ANCOVA term", {
  set.seed(2026)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    baseline = rnorm(n, 60, 12))
  dat$change <- -5 - 8 * (dat$arm == "A") - 0.3 * (dat$baseline - 60) +
    rnorm(n, 0, 8)
  res <- robustness_lm(change ~ arm + baseline, dat, term = "armA",
                       n_boot = 50)
  expect_s3_class(res, "robustness_model")
  expect_true(res$metrics$worstcase_fragility_k >= 1)
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$test, "wald_t")
  expect_false(is.na(res$original_estimate))
})

test_that("robustness_lm supports multi-df factor terms via joint F", {
  set.seed(2026)
  n <- 60
  dat <- data.frame(
    arm = factor(rep(c("P", "A", "B"), each = n / 3), levels = c("P", "A", "B")),
    baseline = rnorm(n, 60, 12)
  )
  dat$change <- -5 - 8 * (dat$arm == "A") - 4 * (dat$arm == "B") -
    0.3 * (dat$baseline - 60) + rnorm(n, 0, 8)

  res <- robustness_lm(change ~ arm + baseline, dat, term = "arm",
                       n_boot = 40, seed = 11)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$term_info$type, "joint")
  expect_identical(res$term_info$test, "joint_F")
  expect_identical(res$term_info$ndf, 2L)
  expect_true(is.na(res$original_estimate))
  expect_true(is.finite(res$original_p))
  expect_true(res$original_significant)
  expect_equal(res$original_p < 0.05, res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
  expect_true(is.na(res$metrics$estimate_range_jackknife_lo))

  # Matches drop1 on the full-data fit
  fit <- lm(change ~ arm + baseline, dat)
  d1 <- drop1(fit, scope = ~ arm, test = "F")
  expect_equal(res$original_p, unname(d1["arm", "Pr(>F)"]), tolerance = 1e-10)
  expect_equal(res$term_info$statistic, unname(d1["arm", "F value"]),
               tolerance = 1e-10)

  # Single-coef string still works on the same 3-level factor
  res1 <- robustness_lm(change ~ arm + baseline, dat, term = "armA",
                        n_boot = 30, seed = 11)
  expect_identical(res1$term_info$type, "single")
  expect_false(is.na(res1$original_estimate))
  ct <- summary(fit)$coefficients
  expect_equal(res1$original_p, unname(ct["armA", "Pr(>|t|)"]),
               tolerance = 1e-10)

  # Seed reproducibility for joint
  a <- robustness_lm(change ~ arm + baseline, dat, term = "arm",
                     n_boot = 25, seed = 99)
  b <- robustness_lm(change ~ arm + baseline, dat, term = "arm",
                     n_boot = 25, seed = 99)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)

  out <- capture.output(print(res))
  expect_true(any(grepl("joint F", out, fixed = TRUE)))
  expect_true(any(grepl("ndf = 2", out, fixed = TRUE)))
  expect_true(any(grepl("estimate = NA", out, fixed = TRUE)))
})

test_that("robustness_lm binary factor term label resolves to single coef", {
  set.seed(3)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n),
    y = rnorm(n)
  )
  res <- robustness_lm(y ~ arm + x, dat, term = "arm", n_boot = 20, seed = 3)
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$coef_name, "armA")
  expect_false(is.na(res$original_estimate))
})

test_that("canonical ANCOVA produces an eligible structural profile", {
  set.seed(101)
  dat <- data.frame(
    outcome = rnorm(80),
    treatment = factor(rep(c("A", "B"), each = 40)),
    baseline = rnorm(80)
  )
  fit <- lm(outcome ~ treatment + baseline, dat)
  term <- stabilitest:::resolve_model_term(fit, "treatmentB")

  profile <- stabilitest:::lm_calibration_profile(
    fit, term, original_n = nrow(dat), alpha = 0.05,
    n_boot = 1000L,
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    max_removal_pct = 0.30
  )

  expect_true(profile$canonical_ancova)
  expect_identical(profile$term_type, "single")
  expect_identical(profile$term_df, 1L)
  expect_identical(profile$treatment_levels, 2L)
  expect_identical(profile$baseline_count, 1L)
  expect_false(profile$omitted_rows)
  expect_identical(profile$version, "lm-profile-1")
  expect_identical(profile$n, 80L)
  expect_identical(profile$n_boot, 1000L)

  res <- robustness_lm(
    outcome ~ treatment + baseline, dat, term = "treatmentB",
    n_boot = 1000, seed = 101
  )
  expect_true(is.list(res$analysis_profile))
  expect_true(res$analysis_profile$canonical_ancova)
  expect_identical(res$n_boot, 1000L)
})

test_that("noncanonical LM profiles are marked ineligible", {
  set.seed(102)
  dat <- data.frame(
    outcome = rnorm(80),
    treatment = factor(rep(c("A", "B"), each = 40)),
    baseline = rnorm(80),
    extra = rnorm(80)
  )
  weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

  profile_for <- function(formula, term, data = dat, original_n = nrow(data)) {
    fit <- lm(formula, data)
    term_spec <- stabilitest:::resolve_model_term(fit, term)
    stabilitest:::lm_calibration_profile(
      fit, term_spec, original_n = original_n, alpha = 0.05,
      n_boot = 1000L, weights = weights, max_removal_pct = 0.30
    )
  }

  expect_false(profile_for(outcome ~ treatment + baseline, "baseline")$canonical_ancova)
  expect_false(
    profile_for(
      outcome ~ arm + baseline,
      "armC",
      data = transform(dat, arm = factor(rep(c("A", "B", "C"), length.out = 80)))
    )$canonical_ancova
  )
  expect_false(
    profile_for(outcome ~ treatment * baseline, "treatmentB")$canonical_ancova
  )
  expect_false(
    profile_for(outcome ~ treatment + baseline + extra, "treatmentB")$canonical_ancova
  )
  expect_false(
    profile_for(outcome ~ treatment + I(baseline^2), "treatmentB")$canonical_ancova
  )

  dat_na <- dat
  dat_na$baseline[1] <- NA_real_
  omitted <- profile_for(
    outcome ~ treatment + baseline, "treatmentB",
    data = dat_na, original_n = nrow(dat_na)
  )
  expect_true(omitted$omitted_rows)
  expect_false(omitted$canonical_ancova)
})

test_that("active lm_ancova remains uncalibrated despite canonical profiles", {
  set.seed(103)
  dat <- data.frame(
    outcome = rnorm(80, mean = rep(c(0, 1), each = 40)),
    treatment = factor(rep(c("A", "B"), each = 40)),
    baseline = rnorm(80)
  )
  res <- robustness_lm(
    outcome ~ treatment + baseline, dat, term = "treatmentB",
    n_boot = 1000, seed = 103
  )
  expect_true(res$analysis_profile$canonical_ancova)
  expect_identical(res$calibration$calibration_unit, "lm_ancova")
  expect_identical(res$calibration$status, "uncalibrated")
  expect_false(res$calibration$applicable)
  expect_true(is.na(res$interpretation_label))
})

test_that("robustness_surv works on a Cox term", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 50
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = c(rexp(n / 2, rate = 0.2), rexp(n / 2, rate = 0.04)),
    event = sample(0:1, n, replace = TRUE, prob = c(0.2, 0.8))
  )
  res <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                         term = "armA", n_boot = 40, seed = 42)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$type, "Cox proportional hazards")
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$test, "wald_z")
  expect_true(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
  expect_true(res$metrics$worstcase_fragility_k >= 1)
})

test_that("robustness_surv is reproducible under a fixed seed", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 50
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = c(rexp(n / 2, rate = 0.2), rexp(n / 2, rate = 0.04)),
    event = sample(0:1, n, replace = TRUE, prob = c(0.2, 0.8))
  )
  a <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                       term = "armA", n_boot = 30, seed = 7)
  b <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                       term = "armA", n_boot = 30, seed = 7)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)
})

test_that("robustness_surv handles a non-significant Cox term", {
  skip_if_not_installed("survival")
  set.seed(101)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = rexp(n, rate = 0.1),
    event = 1L
  )
  res <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                         term = "armA", n_boot = 30, seed = 101)
  expect_s3_class(res, "robustness_model")
  expect_false(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

test_that("robustness_surv rejects too-small samples", {
  skip_if_not_installed("survival")
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = 4), levels = c("P", "A")),
    time = rexp(8),
    event = 1L
  )
  expect_error(
    robustness_surv(survival::Surv(time, event) ~ arm, dat,
                    term = "armA", n_boot = 5),
    "Need at least 10 rows"
  )
})

test_that("robustness_glm works on a binomial logit term", {
  set.seed(2026)
  n <- 60
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  eta <- -1.5 + 2.5 * (dat$arm == "A") + 0.3 * dat$x
  dat$y <- rbinom(n, 1, plogis(eta))
  res <- robustness_glm(y ~ arm + x, dat, term = "armA",
                        family = binomial(), n_boot = 40, seed = 2026)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$type, "GLM (binomial, logit)")
  expect_identical(res$family, "binomial")
  expect_identical(res$link, "logit")
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$test, "wald_z")
  expect_true(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
  expect_true(res$metrics$worstcase_fragility_k >= 1)
})

test_that("robustness_glm is reproducible under a fixed seed", {
  set.seed(7)
  n <- 50
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  eta <- -1 + 2 * (dat$arm == "A") + 0.2 * dat$x
  dat$y <- rbinom(n, 1, plogis(eta))
  a <- robustness_glm(y ~ arm + x, dat, term = "armA",
                      family = binomial(), n_boot = 30, seed = 7)
  b <- robustness_glm(y ~ arm + x, dat, term = "armA",
                      family = binomial(), n_boot = 30, seed = 7)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)
})

test_that("robustness_glm handles a non-significant term", {
  set.seed(101)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  dat$y <- rbinom(n, 1, 0.5)
  res <- robustness_glm(y ~ arm + x, dat, term = "armA",
                        family = binomial(), n_boot = 30, seed = 101)
  expect_s3_class(res, "robustness_model")
  expect_false(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

test_that("robustness_glm accepts obs_weights", {
  set.seed(3)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  eta <- -1 + 2.2 * (dat$arm == "A")
  dat$y <- rbinom(n, 1, plogis(eta))
  # Integer case weights avoid the binomial "non-integer #successes" warning
  w <- sample(1:3, n, replace = TRUE)
  res <- robustness_glm(y ~ arm + x, dat, term = "armA",
                        family = binomial(), obs_weights = w,
                        n_boot = 25, seed = 3)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$type, "GLM (binomial, logit)")
})

test_that("robustness_glm works on a poisson log term with offset", {
  set.seed(2026)
  n <- 60
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n),
    exptime = runif(n, 0.5, 2)
  )
  eta <- -0.3 + 1.0 * (dat$arm == "A") + 0.2 * dat$x + log(dat$exptime)
  dat$y <- rpois(n, exp(eta))
  res <- robustness_glm(y ~ arm + x + offset(log(exptime)), dat, term = "armA",
                        family = poisson(), n_boot = 40, seed = 2026)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$type, "GLM (poisson, log)")
  expect_identical(res$family, "poisson")
  expect_identical(res$link, "log")
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$test, "wald_z")
  expect_true(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)

  fit <- glm(y ~ arm + x + offset(log(exptime)), dat, family = poisson())
  ct <- summary(fit)$coefficients
  expect_equal(res$original_p, unname(ct["armA", "Pr(>|z|)"]),
               tolerance = 1e-10)
  expect_equal(res$original_estimate, unname(ct["armA", "Estimate"]),
               tolerance = 1e-10)

  out <- capture.output(print(res))
  expect_true(any(grepl("IRR =", out, fixed = TRUE)))
})

test_that("robustness_glm supports multi-df factor terms via joint LRT", {
  set.seed(2026)
  n <- 90
  dat <- data.frame(
    arm = factor(rep(c("P", "A", "B"), each = n / 3), levels = c("P", "A", "B")),
    x = rnorm(n)
  )
  eta <- -1.2 + 2.0 * (dat$arm == "A") + 1.2 * (dat$arm == "B") + 0.2 * dat$x
  dat$y <- rbinom(n, 1, plogis(eta))

  res <- robustness_glm(y ~ arm + x, dat, term = "arm",
                        family = binomial(), n_boot = 35, seed = 11)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$term_info$type, "joint")
  expect_identical(res$term_info$test, "joint_LRT")
  expect_identical(res$term_info$ndf, 2L)
  expect_true(is.na(res$original_estimate))
  expect_true(is.finite(res$original_p))
  expect_true(res$original_significant)
  expect_true(is.na(res$metrics$estimate_range_jackknife_lo))

  fit <- glm(y ~ arm + x, dat, family = binomial())
  d1 <- drop1(fit, scope = ~ arm, test = "Chisq")
  expect_equal(res$original_p, unname(d1["arm", "Pr(>Chi)"]), tolerance = 1e-10)
  expect_equal(res$term_info$statistic, unname(d1["arm", "LRT"]),
               tolerance = 1e-10)

  res1 <- robustness_glm(y ~ arm + x, dat, term = "armA",
                         family = binomial(), n_boot = 25, seed = 11)
  expect_identical(res1$term_info$type, "single")
  expect_false(is.na(res1$original_estimate))

  a <- robustness_glm(y ~ arm + x, dat, term = "arm",
                      family = binomial(), n_boot = 20, seed = 99)
  b <- robustness_glm(y ~ arm + x, dat, term = "arm",
                      family = binomial(), n_boot = 20, seed = 99)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)

  out <- capture.output(print(res))
  expect_true(any(grepl("joint LRT", out, fixed = TRUE)))
  expect_true(any(grepl("ndf = 2", out, fixed = TRUE)))
  expect_true(any(grepl("estimate = NA", out, fixed = TRUE)))
})

test_that("robustness_surv supports multi-df factor terms via joint LRT", {
  skip_if_not_installed("survival")
  set.seed(2026)
  n <- 90
  dat <- data.frame(
    arm = factor(rep(c("P", "A", "B"), each = n / 3), levels = c("P", "A", "B")),
    x = rnorm(n),
    event = 1L
  )
  dat$time <- rexp(n) * exp(-0.8 * (dat$arm == "A") - 0.5 * (dat$arm == "B") -
                              0.1 * dat$x)

  res <- robustness_surv(survival::Surv(time, event) ~ arm + x, dat,
                         term = "arm", n_boot = 35, seed = 11)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$term_info$type, "joint")
  expect_identical(res$term_info$test, "joint_LRT")
  expect_identical(res$term_info$ndf, 2L)
  expect_true(is.na(res$original_estimate))
  expect_true(is.finite(res$original_p))
  expect_true(res$original_significant)
  expect_true(is.na(res$metrics$estimate_range_jackknife_lo))

  fit <- survival::coxph(survival::Surv(time, event) ~ arm + x, data = dat)
  d1 <- drop1(fit, scope = ~ arm, test = "Chisq")
  expect_equal(res$original_p, unname(d1["arm", "Pr(>Chi)"]), tolerance = 1e-10)
  expect_equal(res$term_info$statistic, unname(d1["arm", "LRT"]),
               tolerance = 1e-10)

  res1 <- robustness_surv(survival::Surv(time, event) ~ arm + x, dat,
                          term = "armA", n_boot = 25, seed = 11)
  expect_identical(res1$term_info$type, "single")
  expect_identical(res1$term_info$test, "wald_z")
  expect_false(is.na(res1$original_estimate))

  a <- robustness_surv(survival::Surv(time, event) ~ arm + x, dat,
                       term = "arm", n_boot = 20, seed = 99)
  b <- robustness_surv(survival::Surv(time, event) ~ arm + x, dat,
                       term = "arm", n_boot = 20, seed = 99)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)

  out <- capture.output(print(res))
  expect_true(any(grepl("joint LRT", out, fixed = TRUE)))
  expect_true(any(grepl("ndf = 2", out, fixed = TRUE)))
  expect_false(any(grepl("HR =", out, fixed = TRUE)))
})

test_that("robustness_surv binary factor term label resolves to single coef", {
  skip_if_not_installed("survival")
  set.seed(3)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = rexp(n),
    event = 1L
  )
  res <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                         term = "arm", n_boot = 20, seed = 3)
  expect_identical(res$term_info$type, "single")
  expect_identical(res$term_info$coef_name, "armA")
  expect_false(is.na(res$original_estimate))
})

test_that("plot.robustness_analysis returns ggplot panels", {
  res <- robustness_analysis(pain_treatment, pain_placebo,
                             n_boot = 40, seed = 1)
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  pl <- plot(res)
  expect_type(pl, "list")
  expect_named(pl, c("trajectories", "bootstrap"))
  expect_s3_class(pl$trajectories, "ggplot")
  expect_s3_class(pl$bootstrap, "ggplot")
})

test_that("non-significant two-sample results are handled", {
  set.seed(1)
  x <- rnorm(20, 0, 1)
  y <- rnorm(20, 0.05, 1)
  res <- robustness_analysis(x, y, n_boot = 40, seed = 1)
  expect_false(res$original_significant)
  expect_gte(res$robustness_metrics$overall_robustness, 0)
  expect_lte(res$robustness_metrics$overall_robustness, 100)
  expect_true(is.na(res$robustness_interpretation))
  expect_identical(res$calibration$status, "bands_not_applicable")
})

test_that("borderline significant results can be fragile", {
  set.seed(2)
  x <- rnorm(10, 0, 1)
  y <- rnorm(10, 1.1, 1)
  res <- robustness_analysis(x, y, n_boot = 40, seed = 2)
  expect_true(res$original_significant)
  expect_lte(res$worstcase$fragility_index, 2L)
  expect_identical(res$robustness_interpretation, "Fragile")
})

# --- proportion / binary two-sample tests -------------------------------------

.prop_fixture <- function() {
  # Clear imbalance: 12/15 vs 3/15 successes
  list(
    g1 = c(rep(1, 12), rep(0, 3)),
    g2 = c(rep(1, 3), rep(0, 12))
  )
}

test_that("fisher / chisq / prop run and match base R p-values", {
  fx <- .prop_fixture()
  tab <- matrix(c(sum(fx$g1), length(fx$g1) - sum(fx$g1),
                  sum(fx$g2), length(fx$g2) - sum(fx$g2)), nrow = 2)

  rf <- robustness_analysis(fx$g1, fx$g2, test_type = "fisher",
                            n_boot = 40, seed = 3)
  expect_s3_class(rf, "robustness_analysis")
  expect_equal(rf$original_p, fisher.test(tab)$p.value)
  expect_equal(rf$original_mean_diff, mean(fx$g1) - mean(fx$g2))
  expect_true(is.na(rf$original_statistic))
  expect_equal(rf$sample_info$group1_prop, mean(fx$g1))
  expect_gte(rf$robustness_metrics$overall_robustness, 0)
  expect_lte(rf$robustness_metrics$overall_robustness, 100)

  rc <- robustness_analysis(fx$g1, fx$g2, test_type = "chisq",
                            n_boot = 40, seed = 3, correct = TRUE)
  expect_equal(rc$original_p,
               suppressWarnings(chisq.test(tab, correct = TRUE)$p.value))
  expect_equal(unname(rc$original_statistic),
               unname(suppressWarnings(chisq.test(tab, correct = TRUE)$statistic)))

  rp <- robustness_analysis(fx$g1, fx$g2, test_type = "prop",
                            n_boot = 40, seed = 3, correct = TRUE)
  expect_equal(
    rp$original_p,
    suppressWarnings(
      prop.test(c(sum(fx$g1), sum(fx$g2)),
                c(length(fx$g1), length(fx$g2)),
                correct = TRUE)$p.value
    )
  )
})

test_that("chisq continuity correction is configurable", {
  fx <- .prop_fixture()
  tab <- matrix(c(sum(fx$g1), length(fx$g1) - sum(fx$g1),
                  sum(fx$g2), length(fx$g2) - sum(fx$g2)), nrow = 2)
  with_c <- robustness_analysis(fx$g1, fx$g2, test_type = "chisq",
                                correct = TRUE, n_boot = 20, seed = 1)
  no_c <- robustness_analysis(fx$g1, fx$g2, test_type = "chisq",
                              correct = FALSE, n_boot = 20, seed = 1)
  expect_equal(with_c$original_p,
               suppressWarnings(chisq.test(tab, correct = TRUE)$p.value))
  expect_equal(no_c$original_p,
               suppressWarnings(chisq.test(tab, correct = FALSE)$p.value))
  expect_false(isTRUE(all.equal(with_c$original_p, no_c$original_p)))
})

test_that("proportion tests accept logical vectors", {
  g1 <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  g2 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  res <- robustness_analysis(g1, g2, test_type = "fisher", n_boot = 30, seed = 4)
  expect_s3_class(res, "robustness_analysis")
  expect_equal(res$sample_info$group1_prop, mean(as.numeric(g1)))
})

test_that("proportion tests reject non-binary input", {
  expect_error(
    robustness_analysis(c(0, 1, 0, 1, 2), c(0, 0, 1, 1, 0),
                        test_type = "fisher", n_boot = 5),
    "binary"
  )
  expect_error(
    robustness_analysis(c(0, 0.5, 1, 1), c(0, 0, 1, 1),
                        test_type = "chisq", n_boot = 5),
    "binary"
  )
  expect_error(
    robustness_analysis(c(0, 1, NA, 1), c(0, 0, 1, 1),
                        test_type = "prop", n_boot = 5),
    "missing"
  )
  expect_error(
    robustness_analysis(letters[1:8], rep(0:1, 4),
                        test_type = "fisher", n_boot = 5),
    "numeric 0/1 or logical"
  )
})

test_that("proportion test interpretation names the test", {
  fx <- .prop_fixture()
  res <- robustness_analysis(fx$g1, fx$g2, test_type = "fisher",
                             n_boot = 30, interpret = TRUE, seed = 5)
  expect_match(res$interpretation$overall, "Fisher")
  expect_output(print(res), "p1 =")
})

# --- proportion analysis profile (Task 1) ------------------------------------

.canonical_prop_data <- function(n1 = 100, n2 = 100, p1 = 0.45, p2 = 0.20) {
  set.seed(20260806)
  list(
    g1 = rbinom(n1, 1, p1),
    g2 = rbinom(n2, 1, p2)
  )
}

test_that("canonical Fisher result carries an eligible analysis profile", {
  dat <- .canonical_prop_data()
  # Eligibility is a property of the input contract, not the bootstrap budget,
  # so the canonical assertions come from the profile builder directly with the
  # frozen n_boot = 1000.
  profile <- stabilitest:::prop_calibration_profile(
    test_type = "fisher", group1 = dat$g1, group2 = dat$g2, alpha = 0.05,
    n_boot = 1000L, weights = c(jackknife = 0.4, fragility = 0.4,
                                 bootstrap = 0.2),
    max_removal_pct = 0.30, correct = TRUE
  )
  expect_identical(profile$version, "prop-profile-1")
  expect_identical(profile$calibration_unit, "fisher_exact")
  expect_true(profile$canonical_fisher)
  expect_true(profile$two_arm_individual_level)
  expect_true(profile$complete_cases)
  expect_identical(profile$n1, 100L)
  expect_identical(profile$n2, 100L)
  expect_identical(profile$allocation_ratio, 1)
  expect_identical(profile$events1, as.integer(sum(dat$g1)))
  expect_identical(profile$non_events1, as.integer(length(dat$g1) - sum(dat$g1)))
  expect_equal(profile$rate1, mean(dat$g1))
  expect_identical(profile$events2, as.integer(sum(dat$g2)))
  expect_identical(profile$non_events2, as.integer(length(dat$g2) - sum(dat$g2)))
  expect_equal(profile$rate2, mean(dat$g2))
  expect_equal(profile$alpha, 0.05)
  expect_identical(profile$n_boot, 1000L)
  expect_true(profile$correct)
  expect_equal(profile$weights, c(jackknife = 0.4, fragility = 0.4,
                                  bootstrap = 0.2))
  expect_equal(profile$max_removal_pct, 0.30)
})

test_that("robustness_analysis records the analysis profile end-to-end", {
  dat <- .canonical_prop_data()
  res <- robustness_analysis(dat$g1, dat$g2, test_type = "fisher",
                             n_boot = 40, seed = 7)
  profile <- res$analysis_profile
  expect_true(is.list(profile))
  expect_identical(profile$version, "prop-profile-1")
  expect_identical(profile$calibration_unit, "fisher_exact")
  # A budget below the frozen n_boot = 1000 keeps the profile recorded but
  # non-canonical: eligibility is intentionally not satisfied by a quick run.
  expect_identical(profile$n_boot, 40L)
  expect_false(profile$canonical_fisher)
})

test_that("noncanonical proportion profiles are marked ineligible", {
  profile_for <- function(g1, g2, test_type = "fisher",
                          weights = c(jackknife = 0.4, fragility = 0.4,
                                      bootstrap = 0.2),
                          max_removal_pct = 0.30, alpha = 0.05,
                          n_boot = 1000, correct = TRUE) {
    stabilitest:::prop_calibration_profile(
      test_type = test_type, group1 = g1, group2 = g2, alpha = alpha,
      n_boot = as.integer(n_boot), weights = weights,
      max_removal_pct = max_removal_pct, correct = correct
    )
  }
  big <- .canonical_prop_data(n1 = 60, n2 = 60, p1 = 0.5, p2 = 0.3)

  # Wrong unit: chi_square / two_sample_prop are never canonical Fisher.
  expect_false(profile_for(big$g1, big$g2, test_type = "chisq")$canonical_fisher)
  expect_false(profile_for(big$g1, big$g2, test_type = "prop")$canonical_fisher)

  # Per-arm n below 25: not eligible.
  small <- list(g1 = rep(c(1, 0), c(8, 16)), g2 = rep(c(1, 0), c(4, 20)))
  expect_false(profile_for(small$g1, small$g2)$canonical_fisher)

  # Unbalanced 2:1 allocation.
  unbal <- .canonical_prop_data(n1 = 60, n2 = 30)
  expect_false(profile_for(unbal$g1, unbal$g2)$canonical_fisher)

  # Control-arm event rate 0.02 (below the 0.08 transportability bound).
  rare <- list(g1 = rep(c(1, 0), c(20, 80)),
               g2 = rep(c(1, 0), c(2, 98)))
  expect_false(profile_for(rare$g1, rare$g2)$canonical_fisher)

  # Fewer than 3 control-arm events.
  few_evt <- list(g1 = rep(c(1, 0), c(20, 80)),
                  g2 = rep(c(1, 0), c(2, 98)))
  expect_false(profile_for(few_evt$g1, few_evt$g2)$canonical_fisher)

  # Non-default alpha / n_boot / weights / budget.
  expect_false(profile_for(big$g1, big$g2, alpha = 0.01)$canonical_fisher)
  expect_false(profile_for(big$g1, big$g2, n_boot = 500)$canonical_fisher)
  expect_false(profile_for(big$g1, big$g2,
                           weights = c(jackknife = 0, fragility = 0.5,
                                       bootstrap = 0.5))$canonical_fisher)
  expect_false(profile_for(big$g1, big$g2, max_removal_pct = 0.20)$canonical_fisher)
})

test_that("analysis_profile is recorded on every two-sample test type", {
  dat <- .canonical_prop_data(n1 = 60, n2 = 60, p1 = 0.5, p2 = 0.3)
  for (tt in c("t.test", "wilcoxon", "fisher", "chisq", "prop")) {
    res <- robustness_analysis(dat$g1, dat$g2, test_type = tt,
                               n_boot = 20, seed = 9)
    expect_true(is.list(res$analysis_profile), info = tt)
    expect_identical(res$analysis_profile$version, "prop-profile-1", info = tt)
    expect_identical(res$analysis_profile$calibration_unit,
                     calibration_unit_for_test(tt), info = tt)
  }
})

test_that("Welch robustness is unaffected by the proportion profile field", {
  set.seed(11)
  g1 <- rnorm(60, 6, 2)
  g2 <- rnorm(60, 4, 2)
  res <- robustness_analysis(g1, g2, test_type = "t.test", n_boot = 30, seed = 11)
  expect_true(res$calibration$applicable)
  expect_identical(res$calibration$status, "validated_method_specific")
  expect_false(is.na(res$interpretation_label))
})
