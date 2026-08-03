# ==============================================================================
# TOST equivalence / non-inferiority robustness
# ==============================================================================

test_that("known TOST: nearly equal means are equivalent at a generous margin", {
  set.seed(42)
  # Constructed so mean diff is tiny relative to margin = 1
  g1 <- c(0.0, 0.1, -0.1, 0.05, -0.05, 0.02, -0.02, 0.08, -0.08, 0.0,
          0.03, -0.03, 0.06, -0.06, 0.01, -0.01, 0.04, -0.04, 0.07, -0.07)
  g2 <- c(0.02, -0.02, 0.05, -0.05, 0.0, 0.1, -0.1, 0.03, -0.03, 0.01,
          -0.01, 0.04, -0.04, 0.06, -0.06, 0.08, -0.08, 0.0, 0.02, -0.02)

  # Direct TOST check
  detail <- stabilitest:::tost_t_test(
    g1, g2, type = "equivalence",
    delta_L = -1, delta_U = 1, margin = 1,
    alpha = 0.05, paired = FALSE
  )
  expect_true(detail$concluded)
  expect_true(detail$p_eff < 0.05)
  expect_equal(detail$p_eff, max(detail$p_lower, detail$p_upper))
  expect_true(detail$ci_inside)

  res <- robustness_tost(g1, g2, type = "equivalence", margin = 1,
                         n_boot = 40, seed = 1)
  expect_s3_class(res, "robustness_tost")
  expect_s3_class(res, "robustness_model")
  expect_true(res$original_significant)
  expect_equal(res$original_p, detail$p_eff)
  expect_gte(res$metrics$overall_robustness, 0)
  expect_lte(res$metrics$overall_robustness, 100)
})

test_that("known non-equivalent case fails TOST at a tight margin", {
  # Large, stable mean difference vs tiny margin (tiny noise so Welch runs)
  set.seed(2)
  g1 <- rnorm(20, 0, 0.05)
  g2 <- rnorm(20, 2, 0.05)
  detail <- stabilitest:::tost_t_test(
    g1, g2, type = "equivalence",
    delta_L = -0.2, delta_U = 0.2, margin = 0.2,
    alpha = 0.05, paired = FALSE
  )
  expect_false(detail$concluded)
  expect_true(detail$p_eff >= 0.05)
  expect_false(isTRUE(detail$ci_inside))

  res <- robustness_tost(g1, g2, type = "equivalence", margin = 0.2,
                         n_boot = 30, seed = 2)
  expect_false(res$original_significant)
})

test_that("asymmetric delta_L/delta_U works and matches CI rule", {
  set.seed(7)
  g1 <- rnorm(30, 0.2, 0.5)
  g2 <- rnorm(30, 0, 0.5)
  res <- robustness_tost(g1, g2, type = "equivalence",
                         delta_L = -0.5, delta_U = 1.0,
                         n_boot = 30, seed = 3)
  expect_equal(res$delta_L, -0.5)
  expect_equal(res$delta_U, 1.0)
  # Decision must match both one-sided tests
  expect_equal(res$original_significant,
               res$p_lower < 0.05 && res$p_upper < 0.05)
  expect_equal(res$original_significant, isTRUE(res$ci_inside_bounds))
})

test_that("non-inferiority direction: higher_is_better vs lower_is_better", {
  # group1 clearly larger than group2
  set.seed(8)
  g1 <- rnorm(15, 5, 0.2)
  g2 <- rnorm(15, 0, 0.2)
  margin <- 1

  # Higher-is-better: g1 >> g2 should be NI (diff well above -margin)
  ni_hi <- stabilitest:::tost_t_test(
    g1, g2, type = "noninferiority",
    delta_L = NA_real_, delta_U = NA_real_, margin = margin,
    alpha = 0.05, paired = FALSE, higher_is_better = TRUE
  )
  expect_true(ni_hi$concluded)
  expect_true(ni_hi$p_ni < 0.05)

  # Lower-is-better: g1 >> g2 is worse — should NOT be NI
  ni_lo <- stabilitest:::tost_t_test(
    g1, g2, type = "noninferiority",
    delta_L = NA_real_, delta_U = NA_real_, margin = margin,
    alpha = 0.05, paired = FALSE, higher_is_better = FALSE
  )
  expect_false(ni_lo$concluded)

  # Swap groups: lower-is-better NI should hold when g1 is smaller
  ni_lo2 <- stabilitest:::tost_t_test(
    g2, g1, type = "noninferiority",
    delta_L = NA_real_, delta_U = NA_real_, margin = margin,
    alpha = 0.05, paired = FALSE, higher_is_better = FALSE
  )
  expect_true(ni_lo2$concluded)

  res <- robustness_tost(g1, g2, type = "noninferiority", margin = 1,
                         higher_is_better = TRUE, n_boot = 30, seed = 4)
  expect_true(res$original_significant)
  expect_identical(res$tost_type, "noninferiority")

  res2 <- robustness_tost(g1, g2, type = "noninferiority", margin = 1,
                          higher_is_better = FALSE, n_boot = 30, seed = 4)
  expect_false(res2$original_significant)
})

test_that("paired TOST runs and is seed-reproducible", {
  set.seed(99)
  x <- rnorm(25, 10, 2)
  y <- x + rnorm(25, 0.05, 0.3)
  a <- robustness_tost(x, y, type = "equivalence", margin = 1,
                       paired = TRUE, n_boot = 40, seed = 11)
  b <- robustness_tost(x, y, type = "equivalence", margin = 1,
                       paired = TRUE, n_boot = 40, seed = 11)
  expect_true(a$paired)
  expect_equal(a$metrics$overall_robustness, b$metrics$overall_robustness)
  expect_true(a$original_significant)
})

test_that("p_eff adapter flips with the TOST conclusion under jackknife", {
  # Equivalent full sample; leave-one-out should usually keep equivalence
  set.seed(5)
  g1 <- rnorm(20, 0, 0.3)
  g2 <- rnorm(20, 0.05, 0.3)
  res <- robustness_tost(g1, g2, type = "equivalence", margin = 1.5,
                         n_boot = 40, seed = 5)
  expect_true(res$original_significant)
  # Engine stores significant = p_eff < alpha
  expect_true(all(res$jackknife$significant ==
                    (res$jackknife$p_value < res$alpha)))
  expect_equal(mean(res$jackknife$conclusion_match) * 100,
               res$metrics$jackknife_conclusion_stability)
})

test_that("validation: margins, alpha, min-n, NA, mutual exclusion", {
  g1 <- 1:10
  g2 <- 11:20

  expect_error(
    robustness_tost(g1, g2, type = "equivalence"),
    "Equivalence requires margin"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = 0),
    "margin must be a single positive number"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = -1),
    "margin must be a single positive number"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence",
                    delta_L = 1, delta_U = 0),
    "delta_L must be strictly less than delta_U"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence",
                    margin = 1, delta_L = -1, delta_U = 1),
    "either margin or delta_L/delta_U"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", delta_L = -1),
    "both delta_L and delta_U"
  )
  expect_error(
    robustness_tost(g1, g2, type = "noninferiority",
                    delta_L = -1, delta_U = 1),
    "Non-inferiority uses margin"
  )
  expect_error(
    robustness_tost(g1, g2, type = "noninferiority"),
    "Non-inferiority requires a positive margin"
  )
  expect_error(
    robustness_tost(1:3, 1:10, type = "equivalence", margin = 1, n_boot = 5),
    "Each group must have at least 4 observations"
  )
  expect_error(
    robustness_tost(1:5, 1:4, type = "equivalence", margin = 1,
                    paired = TRUE, n_boot = 5),
    "Paired tests require equal length"
  )
  expect_error(
    robustness_tost(1:3, 1:3, type = "equivalence", margin = 1,
                    paired = TRUE, n_boot = 5),
    "at least 4 pairs"
  )
  expect_error(
    robustness_tost(c(1:9, NA_real_), 1:10, type = "equivalence",
                    margin = 1, n_boot = 5),
    "must not contain missing values"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = 1,
                    alpha = 0, n_boot = 5),
    "alpha must be in \\(0, 1\\)"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", margin = 1,
                    weights = c(0.5, 0.5, 0.5), n_boot = 5),
    "weights must be a named numeric vector containing exactly"
  )
})

test_that("print.robustness_tost works for equivalence and NI", {
  set.seed(3)
  g1 <- rnorm(12)
  g2 <- rnorm(12, 0.1)
  res_eq <- robustness_tost(g1, g2, type = "equivalence", margin = 2,
                            n_boot = 20, seed = 1)
  res_ni <- robustness_tost(g1, g2, type = "noninferiority", margin = 1,
                            n_boot = 20, seed = 1)
  expect_output(print(res_eq), "TOST / NON-INFERIORITY")
  expect_output(print(res_eq), "equivalence")
  expect_output(print(res_ni), "noninferiority")
})

# ==============================================================================
# Binary proportion (RD) and odds-ratio TOST / NI
# ==============================================================================

test_that("Wald RD TOST: similar proportions are equivalent at generous margin", {
  # Nearly equal event rates; margin 0.25 on RD
  g1 <- c(rep(1, 20), rep(0, 20))
  g2 <- c(rep(1, 19), rep(0, 21))
  detail <- stabilitest:::tost_prop_test(
    g1, g2, type = "equivalence",
    delta_L = -0.25, delta_U = 0.25, margin = 0.25, alpha = 0.05
  )
  expect_true(detail$concluded)
  expect_equal(detail$p_eff, max(detail$p_lower, detail$p_upper))
  expect_true(detail$ci_inside)
  expect_equal(detail$estimate, mean(g1) - mean(g2), tolerance = 1e-12)

  res <- robustness_tost(g1, g2, type = "equivalence", endpoint = "prop",
                         margin = 0.25, n_boot = 40, seed = 1)
  expect_identical(res$endpoint, "prop")
  expect_true(res$original_significant)
  expect_equal(res$original_p, detail$p_eff)
  expect_match(res$method, "Wald RD")
})

test_that("Wald RD TOST: large RD fails tight equivalence margin", {
  g1 <- c(rep(1, 30), rep(0, 10))
  g2 <- c(rep(1, 10), rep(0, 30))
  detail <- stabilitest:::tost_prop_test(
    g1, g2, type = "equivalence",
    delta_L = -0.1, delta_U = 0.1, margin = 0.1, alpha = 0.05
  )
  expect_false(detail$concluded)
  expect_false(isTRUE(detail$ci_inside))

  res <- robustness_tost(g1, g2, type = "equivalence", endpoint = "prop",
                         margin = 0.1, n_boot = 30, seed = 2)
  expect_false(res$original_significant)
})

test_that("Wald RD NI respects higher_is_better", {
  # g1 success rate clearly higher
  g1 <- c(rep(1, 35), rep(0, 5))
  g2 <- c(rep(1, 15), rep(0, 25))
  margin <- 0.1

  ni_hi <- stabilitest:::tost_prop_test(
    g1, g2, type = "noninferiority",
    delta_L = NA_real_, delta_U = NA_real_, margin = margin,
    alpha = 0.05, higher_is_better = TRUE
  )
  expect_true(ni_hi$concluded)

  ni_lo <- stabilitest:::tost_prop_test(
    g1, g2, type = "noninferiority",
    delta_L = NA_real_, delta_U = NA_real_, margin = margin,
    alpha = 0.05, higher_is_better = FALSE
  )
  expect_false(ni_lo$concluded)

  res <- robustness_tost(g1, g2, type = "noninferiority", endpoint = "prop",
                         margin = 0.1, higher_is_better = TRUE,
                         n_boot = 30, seed = 3)
  expect_true(res$original_significant)
})

test_that("Wald log(OR) TOST and NI with OR-scale margins", {
  # Large, nearly balanced table so Wald CI sits inside [0.5, 2]
  g1 <- c(rep(1, 50), rep(0, 50))
  g2 <- c(rep(1, 48), rep(0, 52))

  detail <- stabilitest:::tost_or_test(
    g1, g2, type = "equivalence",
    delta_L = 1 / 2, delta_U = 2, margin = 2,
    alpha = 0.05, log_L = -log(2), log_U = log(2)
  )
  expect_true(detail$concluded)
  expect_equal(detail$p_eff, max(detail$p_lower, detail$p_upper))
  expect_gt(detail$estimate, 0)

  res <- robustness_tost(g1, g2, type = "equivalence", endpoint = "or",
                         margin = 2, n_boot = 40, seed = 4)
  expect_identical(res$endpoint, "or")
  expect_equal(res$delta_L, 0.5)
  expect_equal(res$delta_U, 2)
  expect_true(res$original_significant)
  expect_match(res$method, "log\\(OR\\)")

  # Logical inputs accepted; test= alias
  res_l <- robustness_tost(as.logical(g1), as.logical(g2),
                           type = "equivalence", test = "or",
                           margin = 2, n_boot = 20, seed = 4)
  expect_identical(res_l$endpoint, "or")
  expect_equal(res_l$original_p, res$original_p)

  # NI: similar rates -> NI at margin 1.5 (higher better)
  res_ni <- robustness_tost(g1, g2, type = "noninferiority", endpoint = "or",
                            margin = 1.5, higher_is_better = TRUE,
                            n_boot = 30, seed = 5)
  expect_true(res_ni$original_significant)
})

test_that("OR Haldane-Anscombe handles a zero cell", {
  g1 <- c(rep(1, 15), rep(0, 5))
  g2 <- rep(0, 20) # no events in group 2
  detail <- stabilitest:::tost_or_test(
    g1, g2, type = "equivalence",
    delta_L = 0.01, delta_U = 100, margin = 100,
    alpha = 0.05, log_L = log(0.01), log_U = log(100)
  )
  expect_true(is.finite(detail$estimate))
  expect_true(is.finite(detail$p_eff))
})

test_that("binary TOST validation: paired, margin, non-binary, test alias", {
  g1 <- c(1, 0, 1, 0, 1, 0, 1, 0)
  g2 <- c(0, 1, 0, 1, 0, 1, 0, 1)

  expect_error(
    robustness_tost(g1, g2, type = "equivalence", endpoint = "prop",
                    margin = 0.2, paired = TRUE, n_boot = 5),
    "paired = TRUE is only supported"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence", endpoint = "or",
                    margin = 0.9, n_boot = 5),
    "margin must be a single number > 1"
  )
  expect_error(
    robustness_tost(g1, g2, type = "noninferiority", endpoint = "or",
                    margin = 1, n_boot = 5),
    "margin must be a single number > 1"
  )
  expect_error(
    robustness_tost(c(0, 0.5, 1, 1, 0, 0, 1, 1), g2,
                    type = "equivalence", endpoint = "prop",
                    margin = 0.2, n_boot = 5),
    "must be binary"
  )
  expect_error(
    robustness_tost(g1, g2, type = "equivalence",
                    endpoint = "mean", test = "prop",
                    margin = 0.2, n_boot = 5),
    "endpoint and test must agree"
  )
  expect_output(
    print(robustness_tost(g1, g2, type = "equivalence", endpoint = "prop",
                          margin = 0.5, n_boot = 15, seed = 1)),
    "ENDPOINT: prop"
  )
})
