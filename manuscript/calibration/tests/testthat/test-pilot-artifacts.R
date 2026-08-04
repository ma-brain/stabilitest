testthat::test_that("published pilot runtime summary is compact and auditable", {
  path <- file.path("..", "..", "published", "pilot-runtime-summary.csv")
  testthat::expect_true(file.exists(path))
  runtime <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c(
    "scenario_id", "analysis_family", "design_layer", "truth_class",
    "target_conclusion", "sample_size", "n_boot", "pilot_max_screen_draws",
    "screen_draws", "screen_completed", "screen_failed", "selected_replicates",
    "analysis_completed", "analysis_failed", "completion_rate",
    "runtime_total_seconds", "runtime_median_seconds", "runtime_p95_seconds",
    "run_status", "missing_strata"
  )
  testthat::expect_named(runtime, required)
  testthat::expect_equal(nrow(runtime), 13L)
  testthat::expect_true(all(!duplicated(runtime$scenario_id)))
  testthat::expect_true(all(runtime$n_boot == 50L))
  testthat::expect_true(all(runtime$pilot_max_screen_draws == 250L))
  testthat::expect_true(all(runtime$screen_draws == 250L))
  testthat::expect_true(all(runtime$screen_failed == 0L))
  testthat::expect_true(all(runtime$analysis_failed == 0L))
  testthat::expect_true(all(runtime$analysis_completed == runtime$selected_replicates))
  testthat::expect_true(all(runtime$completion_rate == 1))
  testthat::expect_true(all(runtime$runtime_total_seconds >= 0))
  testthat::expect_true(all(runtime$runtime_median_seconds >= 0))
  testthat::expect_true(all(runtime$runtime_p95_seconds >= 0))
  testthat::expect_true(all(runtime$run_status %in% c("complete", "incomplete")))
  # Pilot artifacts must not contain score distributions, labels, or fitted
  # thresholds; those are deliberately withheld until the locked analysis.
  testthat::expect_false(any(grepl("score|cutoff|label|band", names(runtime), ignore.case = TRUE)))
})

testthat::test_that("published pilot failure summary records occupancy gaps", {
  path <- file.path("..", "..", "published", "pilot-failure-summary.csv")
  testthat::expect_true(file.exists(path))
  failures <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  testthat::expect_named(failures, c(
    "scenario_id", "analysis_family", "design_layer", "truth_class",
    "target_conclusion", "failure_stage", "failure_class", "failure_count",
    "denominator", "failure_rate"
  ))
  testthat::expect_true(all(failures$failure_count > 0L))
  testthat::expect_true(all(failures$denominator > 0L))
  testthat::expect_true(all(failures$failure_rate >= 0 & failures$failure_rate <= 1))
  testthat::expect_true(any(failures$failure_class == "quota_incomplete"))
  testthat::expect_false(any(grepl("score|cutoff|label|band", names(failures), ignore.case = TRUE)))
})
