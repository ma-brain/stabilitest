.welch_threshold_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch_threshold_code <- function(envir = parent.frame()) {
  sys.source(
    file.path(.welch_threshold_study_root(), "R", "thresholds.R"),
    envir = envir
  )
}

.welch_score_rows <- function(prefix, truth, scores, archetype = "W01") {
  clusters <- paste0(prefix, "-", rep(1:3, length.out = length(scores)))
  data.frame(
    scenario_id = clusters,
    archetype_id = archetype,
    design_layer = "training",
    truth = truth,
    original_significant = TRUE,
    status = "completed",
    overall_robustness = as.numeric(scores),
    stringsAsFactors = FALSE
  )
}

.separated_welch_scores <- function() {
  rbind(
    .welch_score_rows("null", "null", rep(c(10, 20, 30), 30)),
    .welch_score_rows("border", "borderline", rep(c(50, 55, 60), 30)),
    .welch_score_rows("clear", "clear", rep(c(80, 90, 95), 30))
  )
}

testthat::test_that("a clearly separated three-band mapping passes", {
  .load_welch_threshold_code()
  metrics <- welch_candidate_metrics(
    .separated_welch_scores(), "three_band", lower = 40L, upper = 70L,
    cluster_B = 100L, cluster_seed = 202640001L
  )
  gate <- welch_candidate_gate(metrics, "three_band")

  testthat::expect_true(gate$feasible)
  testthat::expect_equal(metrics$false_reassurance, 0)
  testthat::expect_equal(metrics$clear_identification, 1)
  testthat::expect_equal(metrics$balanced_ordinal_accuracy, 1)
  testthat::expect_true(metrics$minimum_band_occupancy >= 0.05)
})

testthat::test_that("high null scores fail false reassurance", {
  .load_welch_threshold_code()
  data <- .separated_welch_scores()
  data$overall_robustness[data$truth == "null"] <- 90
  metrics <- welch_candidate_metrics(
    data, "three_band", lower = 40L, upper = 70L,
    cluster_B = 50L
  )
  gate <- welch_candidate_gate(metrics, "three_band")

  testthat::expect_false(gate$feasible)
  testthat::expect_true(any(grepl("false_reassurance", gate$reasons)))
})

testthat::test_that("borderline extremes fail balanced ordinal accuracy", {
  .load_welch_threshold_code()
  data <- .separated_welch_scores()
  data$overall_robustness[data$truth == "borderline"] <- 95
  metrics <- welch_candidate_metrics(
    data, "three_band", lower = 40L, upper = 70L,
    cluster_B = 50L
  )
  gate <- welch_candidate_gate(metrics, "three_band")

  testthat::expect_lt(metrics$balanced_ordinal_accuracy, 0.70)
  testthat::expect_false(gate$feasible)
  testthat::expect_true("balanced_ordinal_accuracy" %in% gate$reasons)
})

testthat::test_that("two-band Not fragile is reassurance for null rows", {
  .load_welch_threshold_code()
  data <- rbind(
    .welch_score_rows("null", "null", c(rep(20, 45), rep(80, 45))),
    .welch_score_rows("clear", "clear", rep(90, 90))
  )
  metrics <- welch_candidate_metrics(
    data, "two_band", lower = 50L, cluster_B = 50L
  )

  testthat::expect_equal(metrics$false_reassurance, 0.5)
  testthat::expect_equal(metrics$clear_identification, 1)
  testthat::expect_false(welch_candidate_gate(metrics, "two_band")$feasible)
})

testthat::test_that("fitter returns no feasible thresholds, never nearest failure", {
  .load_welch_threshold_code()
  data <- rbind(
    .welch_score_rows("null", "null", rep(c(80, 90), 45)),
    .welch_score_rows("border", "borderline", rep(c(40, 50), 45)),
    .welch_score_rows("clear", "clear", rep(c(10, 20), 45))
  )
  fit <- fit_welch_candidate(data, cluster_B = 25L)

  testthat::expect_identical(fit$status, "no_feasible_thresholds")
  testthat::expect_true(all(is.na(fit$cutoffs)))
  testthat::expect_null(fit$selected)
})
