summary_env <- new.env(parent = globalenv())
for (file in c("uncertainty.R", "summarise.R")) {
  sys.source(file.path("..", "..", "R", file), envir = summary_env)
}
wilson_interval <- summary_env$wilson_interval
score_operating_characteristics <- summary_env$score_operating_characteristics
check_median_ordering <- summary_env$check_median_ordering
cluster_bootstrap_metrics <- summary_env$cluster_bootstrap_metrics
completion_rates <- summary_env$completion_rates
failure_rates <- summary_env$failure_rates
monte_carlo_target_met <- summary_env$monte_carlo_target_met

testthat::test_that("Wilson one-sided bounds match the hand calculation", {
  expect <- testthat::expect_equal
  ci <- wilson_interval(5, 10, conf_level = 0.95)
  z <- stats::qnorm(0.95)
  centre <- (0.5 + z^2 / (2 * 10)) / (1 + z^2 / 10)
  half <- z / (1 + z^2 / 10) * sqrt(0.5 * 0.5 / 10 + z^2 / (4 * 10^2))
  expect(ci$estimate, 0.5, tolerance = 1e-12)
  expect(ci$lower, centre - half, tolerance = 1e-12)
  expect(ci$upper, centre + half, tolerance = 1e-12)
  expect(wilson_interval(0, 10)$lower, 0)
  expect(wilson_interval(10, 10)$upper, 1)
})

testthat::test_that("operating characteristics report false reassurance and identification", {
  reps <- data.frame(
    scenario_id = rep(c("null-a", "clear-a", "borderline-a"), each = 4),
    truth_class = rep(c("null", "clear", "borderline"), each = 4),
    target_conclusion = rep(c("non_significant", "significant", "significant"), each = 4),
    screening_conclusion = rep(c("non_significant", "significant", "significant"), each = 4),
    analysis_family = "fixture",
    overall_score = c(40, 60, 80, 50, 80, 90, 75, 60, 65, 70, 45, 55),
    status = "completed", stringsAsFactors = FALSE
  )
  out <- score_operating_characteristics(reps, c(55, 70))
  expect_true(is.data.frame(out$by_truth))
  expect_equal(out$cutoffs, c(55, 70))
  expect_equal(out$false_reassurance$count, 2L)
  expect_equal(out$false_reassurance$n, 4L)
  expect_equal(out$robust_identification$count, 3L)
  expect_equal(out$robust_identification$n, 8L)
  expect_equal(out$by_truth$false_reassurance_point[out$by_truth$truth_class == "null"], 0.5)
  expect_true(all(c("false_reassurance_lower", "false_reassurance_upper",
                    "false_reassurance_mc_se", "robust_identification_lower") %in%
                  names(out$by_family)))
  expect_true(all(c("fragile", "moderate", "robust", "calibration_rate_point",
                    "calibration_rate_lower", "calibration_rate_upper",
                    "calibration_rate_mc_se") %in% names(out$by_band)))
  expect_equal(nrow(out$by_truth), 3L)
  expect_true(out$by_truth$present[out$by_truth$truth_class == "null"])
  expect_true(is.finite(out$balanced_ordinal_accuracy))
})

testthat::test_that("ordinal accuracy and median ordering require the core truth strata", {
  incomplete <- data.frame(
    scenario_id = c("n1", "n2"), truth_class = c("null", "clear"),
    design_layer = "core", overall_score = c(20, 80), status = "completed",
    stringsAsFactors = FALSE
  )
  summary <- score_operating_characteristics(incomplete, c(55, 70))
  testthat::expect_false(summary$balanced_ordinal_complete)
  testthat::expect_true(is.na(summary$balanced_ordinal_accuracy))
  ordering <- check_median_ordering(incomplete)
  testthat::expect_false(ordering$ordered)
  testthat::expect_false(ordering$core_ordered)
  testthat::expect_match(ordering$by_layer$missing_truth, "borderline")
})

testthat::test_that("median ordering reports incomplete all-failed inputs", {
  failed <- data.frame(overall_score = NA_real_, truth_class = "null",
                       status = "failed", stringsAsFactors = FALSE)
  result <- check_median_ordering(failed)
  testthat::expect_false(result$complete)
  testthat::expect_identical(result$reason, "no_completed_replicates")
})

testthat::test_that("median ordering is checked by scenario layer", {
  reps <- data.frame(
    scenario_id = rep(c("null", "borderline", "clear"), each = 2),
    truth_class = rep(c("null", "borderline", "clear"), each = 2),
    design_layer = "core", overall_score = c(20, 30, 50, 60, 80, 90),
    status = "completed", stringsAsFactors = FALSE
  )
  ordering <- check_median_ordering(reps)
  testthat::expect_true(ordering$ordered)
  testthat::expect_equal(unname(ordering$medians[c("null", "borderline", "clear")]),
                         c(25, 55, 85))
  reps$overall_score[5:6] <- c(10, 15)
  testthat::expect_false(check_median_ordering(reps)$ordered)
})

testthat::test_that("cluster bootstrap resamples scenarios deterministically", {
  reps <- data.frame(
    scenario_id = rep(c("s1", "s2", "s3"), each = 2),
    overall_score = c(10, 20, 30, 40, 50, 60),
    status = "completed", stringsAsFactors = FALSE
  )
  statistic <- function(x) mean(x$overall_score)
  one <- cluster_bootstrap_metrics(reps, statistic, B = 100, seed = 42)
  two <- cluster_bootstrap_metrics(reps, statistic, B = 100, seed = 42)
  testthat::expect_identical(one$draws, two$draws)
  testthat::expect_equal(one$estimate, 35)
  testthat::expect_equal(length(one$draws), 100L)
  testthat::expect_true(one$lower <= one$estimate && one$estimate <= one$upper)
})

testthat::test_that("completion and failure summaries preserve attempted denominators", {
  reps <- data.frame(
    scenario_id = rep(c("s1", "s2"), each = 3),
    analysis_family = rep(c("lm", "cox"), each = 3),
    truth_class = rep(c("clear", "null"), each = 3),
    design_layer = rep(c("core", "stress"), each = 3),
    status = c("completed", "completed", "failed", "failed", "failed", "completed"),
    failure_class = c(NA, NA, "subset_failure", "no_event", "error", NA),
    stringsAsFactors = FALSE
  )
  completion <- completion_rates(reps)
  failures <- failure_rates(reps)
  testthat::expect_equal(completion$attempted, 6L)
  testthat::expect_equal(completion$completed, 3L)
  testthat::expect_equal(completion$completion_rate, 0.5)
  testthat::expect_equal(failures$failed, 3L)
  testthat::expect_equal(failures$failure_rate, 0.5)
  testthat::expect_equal(failures$by_failure_class$failure_class,
                         c("error", "no_event", "subset_failure"))
})

testthat::test_that("Monte Carlo target checks enforce SAP precision and minima", {
  summary <- list(
    attempted = 510L, completed = 500L,
    heldout_n = 100L, mc_se = 0.015,
    false_reassurance = list(point = 0.04, upper = 0.09),
    robust_identification = list(point = 0.75, lower = 0.62)
  )
  sap <- list(min_completed = 500L, min_heldout = 100L, max_mc_se = 0.02,
              false_reassurance_max = 0.05, false_reassurance_upper_max = 0.10,
              robust_identification_min = 0.70, robust_identification_lower_min = 0.60)
  result <- monte_carlo_target_met(summary, sap)
  testthat::expect_true(result$met)
  testthat::expect_length(result$failed_criteria, 0L)
  summary$heldout_n <- 99L
  result <- monte_carlo_target_met(summary, sap)
  testthat::expect_false(result$met)
  testthat::expect_true("heldout_minimum" %in% result$failed_criteria)
})
