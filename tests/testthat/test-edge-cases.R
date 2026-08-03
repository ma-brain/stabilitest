# ==============================================================================
# Edge-case coverage for argument validation, score bands, model engines,
# paired tests, and print methods
# ==============================================================================

# --- argument validation: robustness_analysis ---------------------------------

test_that("robustness_analysis rejects invalid group inputs", {
  expect_error(
    robustness_analysis(numeric(0), numeric(0), n_boot = 5),
    "Each group must have at least 4 observations"
  )
  expect_error(
    robustness_analysis(1:3, 1:10, n_boot = 5),
    "Each group must have at least 4 observations"
  )
  expect_error(
    robustness_analysis(letters[1:10], 1:10, n_boot = 5),
    "is.numeric\\(group1\\) is not TRUE"
  )
  expect_error(
    robustness_analysis(c(1:9, NA_real_), 1:10, n_boot = 5)
  )
  expect_error(
    robustness_analysis(rep(5, 10), rep(5, 10), n_boot = 5),
    "data are essentially constant"
  )
})

test_that("robustness_analysis rejects bad alpha, weights, and test_type", {
  g1 <- 1:10
  g2 <- 11:20
  expect_error(
    robustness_analysis(g1, g2, alpha = 0, n_boot = 5),
    "alpha must be in \\(0, 1\\)"
  )
  expect_error(
    robustness_analysis(g1, g2, alpha = 1, n_boot = 5),
    "alpha must be in \\(0, 1\\)"
  )
  expect_error(
    robustness_analysis(g1, g2, alpha = -0.1, n_boot = 5),
    "alpha must be in \\(0, 1\\)"
  )
  expect_error(
    robustness_analysis(g1, g2,
                        weights = c(jackknife = 0.5, fragility = 0.5, bootstrap = 0.5),
                        n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_analysis(g1, g2,
                        weights = c(jackknife = 0.5, fragility = 0.6, bootstrap = -0.1),
                        n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_analysis(g1, g2,
                        weights = c(jackknife = 1, fragility = 0),
                        n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_analysis(g1, g2, test_type = "anova", n_boot = 5),
    "should be one of"
  )
})

test_that("custom named weights that sum to 1 are accepted", {
  set.seed(11)
  res <- robustness_analysis(
    pain_treatment, pain_placebo, n_boot = 30, seed = 11,
    weights = c(jackknife = 1, fragility = 0, bootstrap = 0)
  )
  expect_equal(res$robustness_metrics$overall_robustness,
               res$robustness_metrics$jackknife_conclusion_stability)
})

# --- paired edges -------------------------------------------------------------

test_that("paired tests reject unequal lengths and tiny samples", {
  expect_error(
    robustness_analysis(1:10, 1:9, test_type = "paired.t.test", n_boot = 5),
    "Paired tests require equal length vectors"
  )
  expect_error(
    robustness_analysis(1:3, 2:4, test_type = "paired.t.test", n_boot = 5),
    "Paired tests require at least 4 pairs"
  )
  expect_error(
    robustness_analysis(1:10, 1:9, test_type = "wilcoxon.paired", n_boot = 5),
    "Paired tests require equal length vectors"
  )
  expect_error(
    robustness_analysis(1:3, 2:4, test_type = "wilcoxon.paired", n_boot = 5),
    "Paired tests require at least 4 pairs"
  )
})

test_that("paired t and Wilcoxon signed-rank handle clear paired effects", {
  set.seed(33)
  x <- rnorm(16, 0, 1)
  y <- x + rnorm(16, 0.8, 0.4)

  rt <- robustness_analysis(x, y, test_type = "paired.t.test",
                            n_boot = 40, seed = 33)
  expect_s3_class(rt, "robustness_analysis")
  expect_identical(rt$sample_info$test_type, "paired.t.test")
  expect_identical(rt$sample_info$n_pairs, 16L)
  expect_true(is.finite(rt$original_mean_diff))
  expect_gte(rt$robustness_metrics$overall_robustness, 0)
  expect_lte(rt$robustness_metrics$overall_robustness, 100)

  rw <- robustness_analysis(x, y, test_type = "wilcoxon.paired",
                            n_boot = 40, seed = 33)
  expect_s3_class(rw, "robustness_analysis")
  expect_identical(rw$sample_info$test_type, "wilcoxon.paired")
  expect_true(is.na(rw$original_mean_diff))
  expect_gte(rw$robustness_metrics$overall_robustness, 0)
  expect_lte(rw$robustness_metrics$overall_robustness, 100)
})

# --- score band boundaries via generate_interpretation ------------------------

.make_mock_analysis <- function(score) {
  label <- dplyr::case_when(
    score > 70 ~ "Robust",
    score > 55 ~ "Moderately Robust",
    TRUE ~ "Fragile"
  )
  structure(
    list(
      original_p = 0.01,
      original_significant = TRUE,
      alpha = 0.05,
      robustness_metrics = tibble::tibble(
        overall_robustness = score,
        jackknife_conclusion_stability = 90,
        jackknife_p_range_lo = 0.01,
        jackknife_p_range_hi = 0.04,
        worstcase_fragility_pct = 5,
        bootstrap_reproducibility = 80,
        bootstrap_p_mean = 0.02,
        bootstrap_p_sd = 0.01
      ),
      robustness_interpretation = label,
      weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
      sample_info = list(test_type = "t.test"),
      jackknife = list(n_influential = 0L),
      worstcase = list(fragility_index = 3L, p_at_fragility = 0.06),
      max_k = 5L,
      max_removal_pct = 0.30,
      bootstrap = list(
        results = data.frame(p_value = runif(10)),
        p_percentile_interval = c(0.01, 0.05)
      )
    ),
    class = c("robustness_analysis", "list")
  )
}

test_that("score bands: exactly 55 is Fragile; exactly 70 is Moderately Robust", {
  # Boundaries documented in robustness_analysis():
  #   > 70 Robust; (55, 70] Moderately Robust; ≤ 55 Fragile
  expect_identical(.make_mock_analysis(55)$robustness_interpretation, "Fragile")
  expect_identical(.make_mock_analysis(55.0001)$robustness_interpretation,
                   "Moderately Robust")
  expect_identical(.make_mock_analysis(70)$robustness_interpretation,
                   "Moderately Robust")
  expect_identical(.make_mock_analysis(70.0001)$robustness_interpretation,
                   "Robust")

  frag <- stabilitest:::generate_interpretation(.make_mock_analysis(55))
  expect_match(frag$recommendation, "fragile", ignore.case = TRUE)

  mod <- stabilitest:::generate_interpretation(.make_mock_analysis(70))
  expect_match(mod$recommendation, "moderate robustness", ignore.case = TRUE)

  rob <- stabilitest:::generate_interpretation(.make_mock_analysis(70.0001))
  expect_match(rob$recommendation, "reported with confidence", ignore.case = TRUE)
})

# --- print methods ------------------------------------------------------------

test_that("print.robustness_analysis smoke test", {
  res <- robustness_analysis(pain_treatment, pain_placebo,
                             n_boot = 30, seed = 1, interpret = TRUE)
  out <- capture.output(print(res))
  expect_true(length(out) > 5)
  expect_true(any(grepl("ROBUSTNESS ANALYSIS SUMMARY", out)))
  expect_true(any(grepl("OVERALL ROBUSTNESS", out)))
  expect_true(any(grepl("Jackknife", out)))
})

test_that("print.robustness_model smoke tests for lm and surv", {
  set.seed(2026)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    baseline = rnorm(n, 60, 12)
  )
  dat$change <- -5 - 8 * (dat$arm == "A") - 0.3 * (dat$baseline - 60) +
    rnorm(n, 0, 8)
  rlm <- robustness_lm(change ~ arm + baseline, dat, term = "armA",
                       n_boot = 25, seed = 2026)
  out_lm <- capture.output(print(rlm))
  expect_true(any(grepl("MODEL-BASED ROBUSTNESS ANALYSIS", out_lm)))
  expect_true(any(grepl("Linear model", out_lm)))
  expect_true(any(grepl("OVERALL ROBUSTNESS", out_lm)))

  skip_if_not_installed("survival")
  set.seed(1)
  n <- 50
  sdat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = c(rexp(n / 2, rate = 0.2), rexp(n / 2, rate = 0.04)),
    event = sample(0:1, n, replace = TRUE, prob = c(0.2, 0.8))
  )
  rsurv <- robustness_surv(survival::Surv(time, event) ~ arm, sdat,
                           term = "armA", n_boot = 25, seed = 42)
  out_s <- capture.output(print(rsurv))
  expect_true(any(grepl("Cox proportional hazards", out_s)))
  expect_true(any(grepl("HR =", out_s)))
})

# --- robustness_lm edges ------------------------------------------------------

test_that("robustness_lm rejects small n, bad weights, and missing terms", {
  set.seed(7)
  tiny <- data.frame(
    y = rnorm(8),
    arm = factor(rep(c("P", "A"), each = 4), levels = c("P", "A")),
    x = rnorm(8)
  )
  expect_error(
    robustness_lm(y ~ arm + x, tiny, term = "armA", n_boot = 5),
    "Need at least 10 rows"
  )

  ok <- data.frame(
    y = rnorm(30),
    arm = factor(rep(c("P", "A"), each = 15), levels = c("P", "A")),
    x = rnorm(30)
  )
  expect_error(
    robustness_lm(y ~ arm + x, ok, term = "armA",
                  weights = c(jackknife = 0.5, fragility = 0.5, bootstrap = 0.5),
                  n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_lm(y ~ arm + x, ok, term = "armA",
                  weights = c(jackknife = 0.5, fragility = 0.6, bootstrap = -0.1),
                  n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_lm(y ~ arm + x, ok, term = "armZ", n_boot = 5),
    "Model could not be fitted on the full dataset"
  )
  expect_error(
    robustness_lm(y ~ arm + x, ok, term = "armA", alpha = 0, n_boot = 5),
    "alpha must be in \\(0, 1\\)"
  )
})

test_that("robustness_lm handles collinear covariates and missing factor levels", {
  set.seed(1)
  d <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30),
    arm = factor(rep(c("P", "A"), 15), levels = c("P", "A"))
  )
  d$x2 <- d$x1
  res <- robustness_lm(y ~ arm + x1 + x2, d, term = "armA",
                       n_boot = 20, seed = 1)
  expect_s3_class(res, "robustness_model")
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)

  one_level <- data.frame(
    y = rnorm(20),
    arm = factor(rep("A", 20), levels = c("P", "A")),
    x = rnorm(20)
  )
  expect_error(
    robustness_lm(y ~ arm + x, one_level, term = "armA", n_boot = 5),
    "contrasts can be applied only to factors with 2 or more levels"
  )
})

test_that("robustness_lm works at the minimum sample size", {
  set.seed(9)
  d <- data.frame(
    y = c(rnorm(5, 0), rnorm(5, 2)),
    arm = factor(rep(c("P", "A"), each = 5), levels = c("P", "A")),
    x = rnorm(10)
  )
  res <- robustness_lm(y ~ arm + x, d, term = "armA", n_boot = 20, seed = 9)
  expect_s3_class(res, "robustness_model")
  expect_identical(res$n, 10L)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

# --- robustness_surv edges ----------------------------------------------------

test_that("robustness_surv rejects all-censored data", {
  skip_if_not_installed("survival")
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = 10), levels = c("P", "A")),
    time = rexp(20),
    event = 0L
  )
  expect_error(
    robustness_surv(survival::Surv(time, event) ~ arm, dat,
                    term = "armA", n_boot = 5),
    "Model could not be fitted on the full dataset"
  )
})

test_that("robustness_surv handles tied event times", {
  skip_if_not_installed("survival")
  set.seed(42)
  n <- 40
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = NA_real_,
    event = 1L
  )
  dat$time[dat$arm == "P"] <- rep(1:5, each = 4)
  dat$time[dat$arm == "A"] <- rep(c(4, 5, 6, 8, 10), each = 4)
  expect_lt(length(unique(dat$time)), n)

  res <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                         term = "armA", n_boot = 30, seed = 42)
  expect_s3_class(res, "robustness_model")
  expect_true(res$original_significant)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

test_that("robustness_surv rejects bad weights", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 30
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    time = c(rexp(n / 2, 0.3), rexp(n / 2, 0.05)),
    event = 1L
  )
  expect_error(
    robustness_surv(survival::Surv(time, event) ~ arm, dat, term = "armA",
                    weights = c(jackknife = 0.5, fragility = 0.5, bootstrap = 0.5),
                    n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
})

test_that("wilcoxon two-sample runs on overlapping distributions", {
  set.seed(55)
  x <- rnorm(18, 0, 1)
  y <- rnorm(18, 0.4, 1)
  res <- robustness_analysis(x, y, test_type = "wilcoxon",
                             n_boot = 40, seed = 55)
  expect_s3_class(res, "robustness_analysis")
  expect_true(is.na(res$original_mean_diff))
  expect_gte(res$robustness_metrics$overall_robustness, 0)
  expect_lte(res$robustness_metrics$overall_robustness, 100)
})
