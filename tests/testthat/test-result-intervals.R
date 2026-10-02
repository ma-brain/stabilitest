test_that("Fisher intervals are explicitly on the odds-ratio scale", {
  x <- c(rep(1, 8), rep(0, 4))
  y <- c(rep(1, 3), rep(0, 9))
  result <- robustness_analysis(x, y, test_type = "fisher", n_boot = 10,
                                 max_removal_pct = .1)
  reference <- fisher.test(matrix(c(8, 4, 3, 9), nrow = 2))
  expect_equal(result$original_estimate, 5 / 12)
  expect_length(which(is.finite(result$original_ci)), 0L)
  expect_equal(result$original_odds_ratio, unname(reference$estimate))
  expect_equal(result$original_odds_ratio_ci, reference$conf.int)
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "Odds ratio (g1 vs g2)", fixed = TRUE)
})

test_that("prop.test retains the interval matching the risk-difference estimate", {
  x <- c(rep(1, 8), rep(0, 4))
  y <- c(rep(1, 3), rep(0, 9))
  result <- robustness_analysis(x, y, test_type = "prop", n_boot = 10,
                                 max_removal_pct = .1)
  reference <- prop.test(c(8, 3), c(12, 12))
  expect_equal(result$original_ci, reference$conf.int)
  expect_equal(result$original_estimate, 5 / 12)
})
