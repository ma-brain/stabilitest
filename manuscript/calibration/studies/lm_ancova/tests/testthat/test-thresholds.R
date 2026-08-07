.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_study(project_root = .project_root(), envir = env)
  env
}

.ancova_score_rows <- function(scenario_id, truth_class, scores) {
  data.frame(
    scenario_id = scenario_id,
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    stringsAsFactors = FALSE
  )
}

# Constructed so 50/70 is the unique feasible pair under the frozen ANCOVA gates.
.feasible_ancova_fixture <- function() {
  null_scores <- rep(c(10, 20, 30, 40, 45, 48, 49, 50), length.out = 40)
  bord_scores <- rep(c(51, 55, 58, 60, 62, 65, 68, 70), length.out = 40)
  clear_scores <- rep(c(71, 75, 78, 80, 82, 85, 90, 95), length.out = 40)
  rbind(
    .ancova_score_rows("null_a", "null", null_scores[1:20]),
    .ancova_score_rows("null_b", "null", null_scores[21:40]),
    .ancova_score_rows("bord_a", "borderline", bord_scores[1:20]),
    .ancova_score_rows("bord_b", "borderline", bord_scores[21:40]),
    .ancova_score_rows("clear_a", "clear", clear_scores[1:20]),
    .ancova_score_rows("clear_b", "clear", clear_scores[21:40])
  )
}

# Nulls are high and clears are low, so no ordered pair can satisfy FR and RI.
.infeasible_ancova_fixture <- function() {
  rbind(
    .ancova_score_rows("null_x", "null", rep(c(80, 85, 90, 95), length.out = 40)),
    .ancova_score_rows("bord_x", "borderline", rep(c(40, 45, 50, 55), length.out = 40)),
    .ancova_score_rows("clear_x", "clear", rep(c(10, 15, 20, 25), length.out = 40))
  )
}

test_that("ANCOVA cutoff metrics match frozen definitions", {
  env <- .load_lm_ancova_study_env()
  data <- .feasible_ancova_fixture()
  metrics <- env$ancova_cutoff_metrics(data, c(50L, 70L))

  null_scores <- data$overall_score[data$truth_class == "null"]
  clear_scores <- data$overall_score[data$truth_class == "clear"]
  testthat::expect_equal(
    metrics$false_reassurance,
    mean(null_scores > 50),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    metrics$robust_identification,
    mean(clear_scores > 70),
    tolerance = 1e-12
  )
  testthat::expect_equal(metrics$class_accuracy[["null"]], mean(null_scores <= 50))
  testthat::expect_equal(
    metrics$class_accuracy[["borderline"]],
    mean(data$overall_score[data$truth_class == "borderline"] > 50 &
           data$overall_score[data$truth_class == "borderline"] <= 70)
  )
  testthat::expect_equal(metrics$class_accuracy[["clear"]], mean(clear_scores > 70))
  testthat::expect_equal(
    metrics$balanced_accuracy,
    mean(unlist(metrics$class_accuracy)),
    tolerance = 1e-12
  )
  testthat::expect_true(metrics$median_ordering_ok)
})

test_that("ANCOVA training feasibility rejects each hard constraint", {
  env <- .load_lm_ancova_study_env()
  data <- .feasible_ancova_fixture()

  high_fr <- env$ancova_cutoff_metrics(data, c(10L, 70L))
  testthat::expect_false(env$ancova_training_feasible(high_fr)$feasible)

  low_ri <- env$ancova_cutoff_metrics(data, c(50L, 95L))
  testthat::expect_false(env$ancova_training_feasible(low_ri)$feasible)

  reversed <- .feasible_ancova_fixture()
  reversed$overall_score[reversed$truth_class == "null"] <-
    reversed$overall_score[reversed$truth_class == "null"] + 80
  reversed$overall_score[reversed$truth_class == "clear"] <-
    pmax(0, reversed$overall_score[reversed$truth_class == "clear"] - 80)
  reversed_metrics <- env$ancova_cutoff_metrics(reversed, c(50L, 70L))
  testthat::expect_false(reversed_metrics$median_ordering_ok)
  testthat::expect_false(env$ancova_training_feasible(reversed_metrics)$feasible)
})

test_that("fit_lm_ancova_cutoffs selects the unique feasible pair or fails closed", {
  env <- .load_lm_ancova_study_env()

  fit <- env$fit_lm_ancova_cutoffs(.feasible_ancova_fixture())
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$cutoffs, c(50L, 70L))
  testthat::expect_true(is.data.frame(fit$grid))
  testthat::expect_true(is.list(fit$welch_comparator))
  testthat::expect_identical(fit$welch_comparator$cutoffs, c(55L, 70L))

  again <- env$fit_lm_ancova_cutoffs(.feasible_ancova_fixture())
  testthat::expect_identical(fit$cutoffs, again$cutoffs)
  testthat::expect_equal(fit$metrics$balanced_accuracy, again$metrics$balanced_accuracy)

  none <- env$fit_lm_ancova_cutoffs(.infeasible_ancova_fixture())
  testthat::expect_identical(none$status, "uncalibrated")
  testthat::expect_identical(none$reason, "no_feasible_thresholds")
  testthat::expect_true(all(is.na(none$cutoffs)))
})
