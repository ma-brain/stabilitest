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
    robustness_analysis(c(1:9, NA_real_), 1:10, n_boot = 5),
    "must not contain missing values"
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
  expect_error(
    robustness_analysis(rep(0:1, 5), rep(0:1, 5),
                        test_type = "chisq", correct = NA, n_boot = 5),
    "correct must be a single non-missing logical"
  )
  expect_error(
    robustness_analysis(c(0, 1, 0), c(1, 0, 1),
                        test_type = "fisher", n_boot = 5),
    "Each group must have at least 4 observations"
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
  expect_identical(rw$sample_info$effect_type, "hodges_lehmann")
  expect_true(is.finite(rw$original_mean_diff))
  # Paired HL = median of Walsh averages of within-pair differences
  d <- x - y
  n <- length(d)
  idx <- which(lower.tri(matrix(0, n, n), diag = TRUE), arr.ind = TRUE)
  hl_paired <- median((d[idx[, 1]] + d[idx[, 2]]) / 2)
  expect_equal(rw$original_mean_diff, hl_paired)
  expect_gte(rw$robustness_metrics$overall_robustness, 0)
  expect_lte(rw$robustness_metrics$overall_robustness, 100)
})

test_that("brunner_munzel is unpaired only and rejects tiny samples", {
  expect_error(
    robustness_analysis(1:3, 2:4, test_type = "brunner_munzel", n_boot = 5),
    "Each group must have at least 4 observations"
  )
  # match.arg rejects unknown aliases; paired BM is not a valid test_type
  expect_error(
    robustness_analysis(1:10, 1:10, test_type = "brunner_munzel.paired",
                        n_boot = 5),
    "should be one of"
  )
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
    "Term 'armZ' not found"
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
  expect_true(is.finite(res$original_mean_diff))
  expect_equal(res$original_mean_diff, median(outer(x, y, `-`)))
  expect_identical(res$sample_info$effect_type, "hodges_lehmann")
  expect_gte(res$robustness_metrics$overall_robustness, 0)
  expect_lte(res$robustness_metrics$overall_robustness, 100)
})

test_that("brunner_munzel runs under heteroscedasticity", {
  set.seed(56)
  x <- rnorm(20, 0, 1)
  y <- rnorm(20, 0.5, 3)
  res <- robustness_analysis(x, y, test_type = "brunner_munzel",
                             n_boot = 40, seed = 56)
  expect_s3_class(res, "robustness_analysis")
  expect_identical(res$sample_info$test_type, "brunner_munzel")
  expect_true(is.finite(res$original_mean_diff))
  expect_true(is.finite(res$sample_info$stochastic_superiority))
  expect_match(
    capture.output(print(res, show_interpretation = FALSE)),
    "Hodges-Lehmann",
    all = FALSE
  )
  expect_gte(res$robustness_metrics$overall_robustness, 0)
  expect_lte(res$robustness_metrics$overall_robustness, 100)
})

# --- robustness_glm edges -----------------------------------------------------

test_that("robustness_glm rejects small n, bad weights, and missing terms", {
  set.seed(7)
  tiny <- data.frame(
    y = rbinom(8, 1, 0.5),
    arm = factor(rep(c("P", "A"), each = 4), levels = c("P", "A")),
    x = rnorm(8)
  )
  expect_error(
    robustness_glm(y ~ arm + x, tiny, term = "armA", n_boot = 5),
    "Need at least 10 rows"
  )

  ok <- data.frame(
    y = rbinom(30, 1, 0.5),
    arm = factor(rep(c("P", "A"), each = 15), levels = c("P", "A")),
    x = rnorm(30)
  )
  expect_error(
    robustness_glm(y ~ arm + x, ok, term = "armA",
                   weights = c(jackknife = 0.5, fragility = 0.5, bootstrap = 0.5),
                   n_boot = 5),
    "weights must be 3 non-negative values summing to 1"
  )
  expect_error(
    robustness_glm(y ~ arm + x, ok, term = "armZ", n_boot = 5),
    "Term 'armZ' not found"
  )
})

test_that("robustness_glm rejects unsupported families and links", {
  set.seed(1)
  dat <- data.frame(
    y = rbinom(30, 1, 0.5),
    arm = factor(rep(c("P", "A"), each = 15), levels = c("P", "A")),
    x = rnorm(30)
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   family = gaussian(), n_boot = 5),
    "Use robustness_lm\\(\\) for Gaussian linear models"
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   family = quasipoisson(), n_boot = 5),
    "Quasi-families are not supported"
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   family = binomial(link = "probit"), n_boot = 5),
    "logit link only"
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   family = poisson(link = "identity"), n_boot = 5),
    "log link only"
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   family = Gamma(link = "log"), n_boot = 5),
    'binomial\\(link = "logit"\\) and poisson\\(link = "log"\\) only'
  )
})

test_that("robustness_glm rejects bad obs_weights", {
  set.seed(2)
  dat <- data.frame(
    y = rbinom(20, 1, 0.5),
    arm = factor(rep(c("P", "A"), each = 10), levels = c("P", "A")),
    x = rnorm(20)
  )
  expect_error(
    robustness_glm(y ~ arm + x, dat, term = "armA",
                   obs_weights = runif(10), n_boot = 5),
    "obs_weights must be NULL or a numeric vector of length nrow\\(data\\)"
  )
})

test_that("robustness_glm handles all-zero binary outcomes", {
  # stats::glm typically converges with a finite (non-significant) arm p for
  # all-zero y; document that path rather than assuming a hard error.
  set.seed(11)
  dat <- data.frame(
    y = rep(0L, 20),
    arm = factor(rep(c("P", "A"), each = 10), levels = c("P", "A")),
    x = rnorm(20)
  )
  result <- tryCatch(
    list(ok = TRUE, res = suppressWarnings(
      robustness_glm(y ~ arm + x, dat, term = "armA", n_boot = 5, seed = 11)
    )),
    error = function(e) list(ok = FALSE, message = conditionMessage(e))
  )
  if (isTRUE(result$ok)) {
    expect_s3_class(result$res, "robustness_model")
    expect_false(result$res$original_significant)
  } else {
    expect_match(result$message, "Model could not be fitted on the full dataset")
  }
})

test_that("robustness_glm handles complete separation deterministically", {
  # Perfect predictor: arm determines y exactly
  dat <- data.frame(
    y = c(rep(0L, 12), rep(1L, 12)),
    arm = factor(rep(c("P", "A"), each = 12), levels = c("P", "A")),
    x = rnorm(24)
  )
  # Documented behavior: either hard error (NA p / non-converged) or a finite
  # extreme fit returning a robustness_model — assert one of the two.
  result <- tryCatch(
    list(ok = TRUE, res = robustness_glm(y ~ arm + x, dat, term = "armA",
                                         n_boot = 10, seed = 1)),
    error = function(e) list(ok = FALSE, message = conditionMessage(e))
  )
  if (isTRUE(result$ok)) {
    expect_s3_class(result$res, "robustness_model")
    expect_true(is.finite(result$res$original_p))
  } else {
    expect_match(result$message, "Model could not be fitted on the full dataset")
  }
})

test_that("robustness_glm handles collinear covariates", {
  set.seed(1)
  d <- data.frame(
    arm = factor(rep(c("P", "A"), 15), levels = c("P", "A")),
    x1 = rnorm(30)
  )
  d$x2 <- d$x1
  eta <- -0.5 + 1.8 * (d$arm == "A")
  d$y <- rbinom(30, 1, plogis(eta))
  res <- robustness_glm(y ~ arm + x1 + x2, d, term = "armA",
                        family = binomial(), n_boot = 20, seed = 1)
  expect_s3_class(res, "robustness_model")
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

test_that("print.robustness_model shows GLM OR", {
  set.seed(2026)
  n <- 50
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  eta <- -1.2 + 2.2 * (dat$arm == "A") + 0.2 * dat$x
  dat$y <- rbinom(n, 1, plogis(eta))
  res <- robustness_glm(y ~ arm + x, dat, term = "armA",
                        family = binomial(), n_boot = 20, seed = 2026)
  out <- capture.output(print(res))
  expect_true(any(grepl("GLM \\(binomial", out)))
  expect_true(any(grepl("OR =", out)))
})

test_that("print.robustness_model shows GLM IRR for poisson", {
  set.seed(2026)
  n <- 50
  dat <- data.frame(
    arm = factor(rep(c("P", "A"), each = n / 2), levels = c("P", "A")),
    x = rnorm(n)
  )
  eta <- -0.2 + 0.9 * (dat$arm == "A") + 0.15 * dat$x
  dat$y <- rpois(n, exp(eta))
  res <- robustness_glm(y ~ arm + x, dat, term = "armA",
                        family = poisson(), n_boot = 20, seed = 2026)
  out <- capture.output(print(res))
  expect_true(any(grepl("GLM \\(poisson", out)))
  expect_true(any(grepl("IRR =", out)))
  expect_false(any(grepl("OR =", out)))
})

# --- n_boot / max_removal_pct validation (shared across APIs) -----------------

test_that("n_boot must be a positive integer across engines", {
  g1 <- 1:10
  g2 <- 11:20
  expect_error(
    robustness_analysis(g1, g2, n_boot = 0),
    "n_boot must be a single positive integer"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = -1),
    "n_boot must be a single positive integer"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = 1.5),
    "n_boot must be a single positive integer"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = NA_real_),
    "n_boot must be a single positive integer"
  )

  set.seed(1)
  dat <- data.frame(
    y = rnorm(20),
    arm = factor(rep(c("A", "B"), each = 10)),
    x = rnorm(20)
  )
  expect_error(
    robustness_lm(y ~ arm + x, dat, term = "armB", n_boot = 0),
    "n_boot must be a single positive integer"
  )
  expect_error(
    robustness_glm(I(y > 0) ~ arm + x, dat, term = "armB",
                   family = binomial(), n_boot = 0),
    "n_boot must be a single positive integer"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = 5, n_boot = 0),
    "n_boot must be a single positive integer"
  )
})

test_that("max_removal_pct must be in (0, 1] across engines", {
  g1 <- 1:10
  g2 <- 11:20
  expect_error(
    robustness_analysis(g1, g2, n_boot = 5, max_removal_pct = 0),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = 5, max_removal_pct = -0.1),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = 5, max_removal_pct = 1.5),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )
  expect_error(
    robustness_analysis(g1, g2, n_boot = 5, max_removal_pct = NA_real_),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )

  set.seed(2)
  dat <- data.frame(
    y = rnorm(20),
    arm = factor(rep(c("A", "B"), each = 10)),
    x = rnorm(20)
  )
  expect_error(
    robustness_lm(y ~ arm + x, dat, term = "armB",
                  n_boot = 5, max_removal_pct = 2),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = 5,
                    n_boot = 5, max_removal_pct = 0),
    "max_removal_pct must be a single number in \\(0, 1\\]"
  )

  # Boundary max_removal_pct = 1 is allowed
  res <- robustness_analysis(g1, g2, n_boot = 5, max_removal_pct = 1, seed = 1)
  expect_equal(res$max_k, length(g1) + length(g2))
})

test_that("continuous NA is rejected with a clear message (paired + unpaired)", {
  expect_error(
    robustness_analysis(c(1:9, NA_real_), 1:10, n_boot = 5),
    "must not contain missing values"
  )
  expect_error(
    robustness_analysis(1:10, c(1:9, NA_real_),
                        test_type = "paired.t.test", n_boot = 5),
    "must not contain missing values"
  )
  expect_error(
    robustness_tost(c(1:9, NA_real_), 1:10, type = "equivalence",
                    margin = 1, n_boot = 5),
    "must not contain missing values"
  )
})

# --- cross-class schema aliases / shared metric columns -----------------------

.test_shared_metric_cols <- function(metrics) {
  shared <- c(
    "jackknife_conclusion_stability", "jackknife_n_influential",
    "jackknife_pct_influential", "jackknife_p_range_lo", "jackknife_p_range_hi",
    "worstcase_fragility_k", "worstcase_fragility_pct",
    "worstcase_fragility_component", "p_at_fragility",
    "extreme_fragility_k", "extreme_fragility_pct",
    "bootstrap_reproducibility", "bootstrap_p_mean", "bootstrap_p_sd",
    "estimate_range_jackknife_lo", "estimate_range_jackknife_hi",
    "overall_robustness"
  )
  expect_true(all(shared %in% names(metrics)))
}

test_that("result schemas expose shared aliases and metric columns", {
  res_a <- robustness_analysis(pain_treatment, pain_placebo,
                               n_boot = 20, seed = 1)
  expect_identical(res_a$metrics, res_a$robustness_metrics)
  expect_identical(res_a$interpretation_label, res_a$robustness_interpretation)
  expect_identical(res_a$original_estimate, res_a$original_mean_diff)
  expect_null(res_a[["interpretation"]])
  expect_null(res_a$interpretation) # must not partial-match interpretation_label
  expect_true(is.numeric(res_a$n))
  expect_silent(capture.output(print(res_a, show_interpretation = TRUE)))
  .test_shared_metric_cols(res_a$robustness_metrics)
  expect_false(is.na(res_a$robustness_metrics$extreme_fragility_k))
  expect_true(is.na(res_a$robustness_metrics$estimate_range_jackknife_lo))

  set.seed(11)
  dat <- data.frame(
    y = rnorm(30),
    arm = factor(rep(c("P", "A"), each = 15), levels = c("P", "A")),
    x = rnorm(30)
  )
  res_m <- robustness_lm(y ~ arm + x, dat, term = "armA",
                         n_boot = 15, seed = 11)
  expect_identical(res_m$robustness_metrics, res_m$metrics)
  expect_identical(res_m$robustness_interpretation, res_m$interpretation_label)
  expect_identical(res_m$original_mean_diff, res_m$original_estimate)
  expect_true(is.numeric(res_m$max_removal_pct))
  .test_shared_metric_cols(res_m$metrics)
  expect_true(is.na(res_m$metrics$extreme_fragility_k))
  expect_false(is.na(res_m$metrics$worstcase_fragility_component))
  expect_false(is.na(res_m$metrics$bootstrap_p_sd))

  set.seed(12)
  g1 <- rnorm(20)
  g2 <- rnorm(20, 0.05)
  res_t <- robustness_tost(g1, g2, type = "equivalence", margin = 1,
                           n_boot = 15, seed = 12)
  expect_identical(res_t$robustness_metrics, res_t$metrics)
  expect_identical(res_t$robustness_interpretation, res_t$interpretation_label)
  expect_identical(res_t$original_mean_diff, res_t$original_estimate)
  .test_shared_metric_cols(res_t$metrics)
})

test_that("shared scoring helpers match historical band boundaries", {
  expect_identical(stabilitest:::robustness_band_label(55), "Fragile")
  expect_identical(stabilitest:::robustness_band_label(55.0001),
                   "Moderately Robust")
  expect_identical(stabilitest:::robustness_band_label(70),
                   "Moderately Robust")
  expect_identical(stabilitest:::robustness_band_label(70.0001), "Robust")
  expect_equal(
    stabilitest:::fragility_component_score(3L, 10L),
    100 * 3 / 11
  )
  expect_equal(
    stabilitest:::overall_robustness_score(
      100, 50, 80, c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)
    ),
    0.4 * 100 + 0.4 * 50 + 0.2 * 80
  )
})
