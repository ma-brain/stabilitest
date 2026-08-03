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

test_that("all four test types run", {
  set.seed(42)
  x <- rnorm(20); y <- rnorm(20, 1)
  for (tt in c("t.test", "wilcoxon")) {
    expect_s3_class(robustness_analysis(x, y, test_type = tt, n_boot = 30),
                    "robustness_analysis")
  }
  for (tt in c("paired.t.test", "wilcoxon.paired")) {
    expect_s3_class(robustness_analysis(x, y, test_type = tt, n_boot = 30),
                    "robustness_analysis")
  }
})

test_that("bootstrap is reproducible under a fixed seed", {
  a <- robustness_analysis(pain_treatment, pain_placebo, n_boot = 50, seed = 7)
  b <- robustness_analysis(pain_treatment, pain_placebo, n_boot = 50, seed = 7)
  expect_equal(a$robustness_metrics$overall_robustness,
               b$robustness_metrics$overall_robustness)
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
})
