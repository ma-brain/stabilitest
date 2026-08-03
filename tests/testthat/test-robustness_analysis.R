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
  expect_match(res$robustness_interpretation,
               "Robust|Moderately Robust|Fragile")
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
