.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_v2_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_v2_study(project_root = .project_root(), envir = env)
  env
}

.ancova_v2_score_rows <- function(scenario_id, truth_class, scores,
                                  diagnostic_only = FALSE,
                                  status = "completed",
                                  design_layer = "core") {
  data.frame(
    scenario_id = scenario_id,
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    diagnostic_only = isTRUE(diagnostic_only),
    status = status,
    design_layer = as.character(design_layer),
    stringsAsFactors = FALSE
  )
}

# Constructed so L = 50 is the unique feasible integer under Track A FR/RI gates.
.feasible_ancova_v2_fixture <- function() {
  null_scores <- c(rep(c(10, 20, 30, 40, 45), length.out = 30), rep(50, 10))
  clear_scores <- c(rep(51, 30), rep(40, 10))
  bord_scores <- rep(c(50, 51, 52, 53), length.out = 40)
  rbind(
    .ancova_v2_score_rows("null_a", "null", null_scores[1:20]),
    .ancova_v2_score_rows("null_b", "null", null_scores[21:40]),
    .ancova_v2_score_rows("bord_a", "borderline", bord_scores[1:20],
                          diagnostic_only = TRUE),
    .ancova_v2_score_rows("bord_b", "borderline", bord_scores[21:40],
                          diagnostic_only = TRUE),
    .ancova_v2_score_rows("clear_a", "clear", clear_scores[1:20]),
    .ancova_v2_score_rows("clear_b", "clear", clear_scores[21:40])
  )
}

.infeasible_ancova_v2_fixture <- function() {
  rbind(
    .ancova_v2_score_rows("null_x", "null", rep(c(80, 85, 90, 95), length.out = 40)),
    .ancova_v2_score_rows("clear_x", "clear", rep(c(10, 15, 20, 25), length.out = 40))
  )
}

test_that("ANCOVA v2 cutoff metrics use single L and exclude diagnostic strata", {
  env <- .load_lm_ancova_v2_study_env()
  data <- .feasible_ancova_v2_fixture()
  metrics <- env$ancova_v2_cutoff_metrics(data, 50L)

  null_scores <- data$overall_score[data$truth_class == "null"]
  clear_scores <- data$overall_score[data$truth_class == "clear"]
  testthat::expect_identical(metrics$cutoff, 50L)
  testthat::expect_equal(
    metrics$false_reassurance,
    mean(null_scores > 50),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    metrics$not_fragile_identification,
    mean(clear_scores > 50),
    tolerance = 1e-12
  )
  testthat::expect_equal(metrics$n, 80L)
  testthat::expect_false("borderline" %in% names(metrics$class_accuracy))
  testthat::expect_equal(
    metrics$class_accuracy[["null"]],
    mean(null_scores <= 50)
  )
  testthat::expect_equal(
    metrics$class_accuracy[["clear"]],
    mean(clear_scores > 50)
  )
})

test_that("ANCOVA v2 training feasibility enforces FR and not-fragile gates", {
  env <- .load_lm_ancova_v2_study_env()
  data <- .feasible_ancova_v2_fixture()

  ok <- env$ancova_v2_cutoff_metrics(data, 50L)
  testthat::expect_true(env$ancova_v2_training_feasible(ok)$feasible)

  high_fr <- env$ancova_v2_cutoff_metrics(data, 40L)
  testthat::expect_false(env$ancova_v2_training_feasible(high_fr)$feasible)
  testthat::expect_true(
    any(grepl("false_reassurance", env$ancova_v2_training_feasible(high_fr)$reasons))
  )

  low_ri <- env$ancova_v2_cutoff_metrics(data, 51L)
  testthat::expect_false(env$ancova_v2_training_feasible(low_ri)$feasible)
  testthat::expect_true(
    any(grepl("not_fragile", env$ancova_v2_training_feasible(low_ri)$reasons))
  )
})

test_that("fit_lm_ancova_v2_cutoffs searches integer L with deterministic ties", {
  env <- .load_lm_ancova_v2_study_env()

  fit <- env$fit_lm_ancova_v2_cutoffs(.feasible_ancova_v2_fixture())
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$cutoff, 50L)
  testthat::expect_true(is.data.frame(fit$grid))
  testthat::expect_true(all(c("cutoff", "feasible") %in% names(fit$grid)))
  testthat::expect_identical(range(fit$grid$cutoff), c(0L, 100L))

  again <- env$fit_lm_ancova_v2_cutoffs(.feasible_ancova_v2_fixture())
  testthat::expect_identical(fit$cutoff, again$cutoff)
  testthat::expect_equal(
    fit$metrics$not_fragile_identification,
    again$metrics$not_fragile_identification
  )

  none <- env$fit_lm_ancova_v2_cutoffs(.infeasible_ancova_v2_fixture())
  testthat::expect_identical(none$status, "uncalibrated")
  testthat::expect_identical(none$reason, "no_feasible_thresholds")
  testthat::expect_true(is.na(none$cutoff))
})

test_that("fit_lm_ancova_v2_cutoffs ignores borderline and diagnostic_only rows", {
  env <- .load_lm_ancova_v2_study_env()
  base <- .feasible_ancova_v2_fixture()
  # Poison clear scores that would break RI if diagnostic_only rows were fitted.
  poison <- .ancova_v2_score_rows(
    "stress_clear", "clear", rep(10, 40), diagnostic_only = TRUE
  )
  # Borderline without the flag must still be excluded by truth class.
  bord_unflagged <- .ancova_v2_score_rows(
    "bord_unflagged", "borderline", rep(99, 40), diagnostic_only = FALSE
  )
  # Stress-layer clears without diagnostic_only must still be excluded.
  stress_clear <- .ancova_v2_score_rows(
    "stress_layer_clear", "clear", rep(10, 40),
    diagnostic_only = FALSE, design_layer = "stress"
  )
  fit <- env$fit_lm_ancova_v2_cutoffs(
    rbind(base, poison, bord_unflagged, stress_clear)
  )
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$cutoff, 50L)
  testthat::expect_identical(fit$metrics$n, 80L)
})

test_that("fit_lm_ancova_v2_cutoffs tie-breaks by RI then FR margin then smallest L", {
  env <- .load_lm_ancova_v2_study_env()

  # Among equal max-RI cutoffs with equal FR margins, prefer smallest L.
  # Nulls peak at 40 ⇒ FR fails for L < 40. Clears keep RI=1 through L=44.
  null_ri <- c(rep(20, 30), rep(40, 10))
  clear_ri <- c(rep(50, 28), rep(45, 12))
  data_ri <- rbind(
    .ancova_v2_score_rows("null_t", "null", null_ri),
    .ancova_v2_score_rows("clear_t", "clear", clear_ri)
  )
  fit_ri <- env$fit_lm_ancova_v2_cutoffs(data_ri)
  testthat::expect_identical(fit_ri$cutoff, 40L)
  testthat::expect_equal(fit_ri$metrics$not_fragile_identification, 1)

  # With equal RI, prefer greater FR safety margin (lower FR upper bound).
  # Only FR=0 is Wilson-feasible at n=40; smallest such L is 60.
  null_fr <- c(rep(20, 38), 60, 60)
  clear_fr <- rep(80, 40)
  data_fr <- rbind(
    .ancova_v2_score_rows("null_u", "null", null_fr),
    .ancova_v2_score_rows("clear_u", "clear", clear_fr)
  )
  fit_fr <- env$fit_lm_ancova_v2_cutoffs(data_fr)
  testthat::expect_identical(fit_fr$cutoff, 60L)

  # With equal RI and equal FR margin, prefer smallest L.
  null_small <- c(rep(20, 30), rep(48, 10))
  clear_small <- rep(60, 40)
  data_small <- rbind(
    .ancova_v2_score_rows("null_v", "null", null_small),
    .ancova_v2_score_rows("clear_v", "clear", clear_small)
  )
  fit_small <- env$fit_lm_ancova_v2_cutoffs(data_small)
  testthat::expect_identical(fit_small$cutoff, 48L)
})

test_that("fit_lm_ancova_v2_cutoffs prefers higher RI over safer FR margin", {
  env <- .load_lm_ancova_v2_study_env()
  # n=80 so a non-zero FR can still clear the Wilson upper gate.
  # L=40: RI=1.0, FR=3/80 (worse FR margin).
  # L=70: RI=0.70, FR=0 (safer FR margin).
  # RI-primary order must pick 40; FR-margin-primary would pick 70.
  null_scores <- c(rep(10, 67), rep(40, 10), rep(70, 3))
  clear_scores <- c(rep(80, 56), rep(45, 24))
  data <- rbind(
    .ancova_v2_score_rows("null_w", "null", null_scores),
    .ancova_v2_score_rows("clear_w", "clear", clear_scores)
  )
  fit <- env$fit_lm_ancova_v2_cutoffs(data)
  testthat::expect_identical(fit$cutoff, 40L)
  testthat::expect_equal(fit$metrics$not_fragile_identification, 1)

  feasible <- fit$grid[isTRUE(fit$grid$feasible) | fit$grid$feasible %in% TRUE,
                       , drop = FALSE]
  fr_primary <- feasible[
    order(-feasible$fr_safety_margin, feasible$cutoff),
    , drop = FALSE
  ]
  testthat::expect_identical(as.integer(fr_primary$cutoff[[1L]]), 70L)
  testthat::expect_gt(
    fit$metrics$not_fragile_identification,
    fr_primary$not_fragile_identification[[1L]]
  )
  testthat::expect_lt(
    fit$grid$fr_safety_margin[fit$grid$cutoff == 40L],
    fit$grid$fr_safety_margin[fit$grid$cutoff == 70L]
  )
})
