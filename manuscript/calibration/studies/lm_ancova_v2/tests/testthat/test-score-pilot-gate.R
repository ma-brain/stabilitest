.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_score_pilot_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_v2_study(project_root = .project_root(), envir = env)
  sys.source(
    file.path(.study_root(), "tools", "score_pilot_gate.R"),
    envir = env
  )
  env
}

.pilot_fixture_rows <- function(n, truth_class, scores,
                                fragility = scores,
                                bootstrap = scores,
                                jackknife = scores,
                                status = "completed",
                                design_layer = "core",
                                diagnostic_only = FALSE) {
  data.frame(
    scenario_id = paste0(truth_class, "_n", n),
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    fragility_component = as.numeric(fragility),
    bootstrap_reproducibility = as.numeric(bootstrap),
    jackknife_stability = as.numeric(jackknife),
    n = as.integer(n),
    diagnostic_only = isTRUE(diagnostic_only),
    status = status,
    design_layer = as.character(design_layer),
    stringsAsFactors = FALSE
  )
}

# Strong separation: delta = 30, overlap = 0, AUC near 1.
.go_fixture <- function() {
  rbind(
    .pilot_fixture_rows(40L, "null", rep(c(30, 35, 40, 45), length.out = 20)),
    .pilot_fixture_rows(80L, "null", rep(c(32, 38, 42, 48), length.out = 20)),
    .pilot_fixture_rows(40L, "clear", rep(c(65, 70, 75, 80), length.out = 20)),
    .pilot_fixture_rows(80L, "clear", rep(c(68, 72, 78, 82), length.out = 20)),
    .pilot_fixture_rows(
      40L, "borderline", rep(50, 10), diagnostic_only = TRUE
    )
  )
}

# Marginal under SAP bands: 15 <= delta < 20 and 0.10 < O <= 0.20.
.marginal_fixture <- function() {
  # median(clear) = 60; median(null) = 43 => delta = 17
  # null scores: 3 of 20 exceed 60 => O = 0.15
  null_scores <- c(rep(30, 8L), rep(43, 9L), 65, 68, 72)
  clear_scores <- c(rep(50, 5L), rep(60, 10L), rep(70, 5L))
  rbind(
    .pilot_fixture_rows(40L, "null", null_scores),
    .pilot_fixture_rows(40L, "clear", clear_scores)
  )
}

# Hard no-go: delta < 15 and O > 0.20.
.hard_nogo_fixture <- function() {
  null_scores <- rep(c(40, 50, 55, 70), length.out = 20)
  clear_scores <- rep(c(45, 50, 52, 55), length.out = 20)
  rbind(
    .pilot_fixture_rows(40L, "null", null_scores),
    .pilot_fixture_rows(40L, "clear", clear_scores)
  )
}

test_that("score pilot metrics compute sealed delta, overlap, and optional AUC", {
  env <- .load_score_pilot_env()
  metrics <- env$ancova_v2_score_pilot_metrics(.go_fixture(), compute_auc = TRUE)

  null_scores <- c(rep(c(30, 35, 40, 45), length.out = 20),
                   rep(c(32, 38, 42, 48), length.out = 20))
  clear_scores <- c(rep(c(65, 70, 75, 80), length.out = 20),
                    rep(c(68, 72, 78, 82), length.out = 20))
  expected_delta <- stats::median(clear_scores) - stats::median(null_scores)
  expected_overlap <- mean(null_scores > stats::median(clear_scores))

  testthat::expect_equal(metrics$delta, expected_delta, tolerance = 1e-12)
  testthat::expect_equal(metrics$overlap, expected_overlap, tolerance = 1e-12)
  testthat::expect_true(isTRUE(metrics$auc_computed))
  testthat::expect_true(is.finite(metrics$auc))
  testthat::expect_gt(metrics$auc, 0.9)
  testthat::expect_equal(metrics$n_null, 40L)
  testthat::expect_equal(metrics$n_clear, 40L)
  # Borderline / diagnostic rows must not enter sealed metrics.
  testthat::expect_equal(metrics$n_total, 80L)
})

test_that("score pilot AUC is omitted from gate when not computed", {
  env <- .load_score_pilot_env()
  metrics <- env$ancova_v2_score_pilot_metrics(.go_fixture(), compute_auc = FALSE)
  testthat::expect_false(isTRUE(metrics$auc_computed))
  testthat::expect_true(is.na(metrics$auc))
})

test_that("score pilot metric bands follow frozen SAP thresholds", {
  env <- .load_score_pilot_env()
  go_bands <- env$ancova_v2_score_pilot_metric_bands(
    delta = 25, overlap = 0.05, auc = 0.80, auc_computed = TRUE
  )
  testthat::expect_identical(go_bands$delta, "go")
  testthat::expect_identical(go_bands$overlap, "go")
  testthat::expect_identical(go_bands$auc, "go")

  marg_bands <- env$ancova_v2_score_pilot_metric_bands(
    delta = 17, overlap = 0.15, auc = 0.72, auc_computed = TRUE
  )
  testthat::expect_identical(marg_bands$delta, "marginal")
  testthat::expect_identical(marg_bands$overlap, "marginal")
  testthat::expect_identical(marg_bands$auc, "marginal")

  hard_bands <- env$ancova_v2_score_pilot_metric_bands(
    delta = 10, overlap = 0.25, auc = 0.60, auc_computed = TRUE
  )
  testthat::expect_identical(hard_bands$delta, "hard_no_go")
  testthat::expect_identical(hard_bands$overlap, "hard_no_go")
  testthat::expect_identical(hard_bands$auc, "hard_no_go")

  no_auc <- env$ancova_v2_score_pilot_metric_bands(
    delta = 25, overlap = 0.05, auc = NA_real_, auc_computed = FALSE
  )
  testthat::expect_null(no_auc$auc)
})

test_that("score pilot decision records go, escalate, and no-go without searching L", {
  env <- .load_score_pilot_env()

  go <- env$ancova_v2_score_pilot_decide(
    env$ancova_v2_score_pilot_metrics(.go_fixture(), compute_auc = TRUE),
    clear_power = 0.90
  )
  testthat::expect_true(go$pass)
  testthat::expect_identical(go$decision, "go")
  testthat::expect_equal(go$frozen_clear_power, 0.90)
  testthat::expect_false(isTRUE(go$searched_L))

  marg <- env$ancova_v2_score_pilot_decide(
    env$ancova_v2_score_pilot_metrics(.marginal_fixture(), compute_auc = FALSE),
    clear_power = 0.90
  )
  testthat::expect_false(marg$pass)
  testthat::expect_identical(marg$decision, "escalate_clear_power")
  testthat::expect_equal(marg$recommended_clear_power, 0.95)
  testthat::expect_true(is.na(marg$frozen_clear_power))

  hard <- env$ancova_v2_score_pilot_decide(
    env$ancova_v2_score_pilot_metrics(.hard_nogo_fixture(), compute_auc = FALSE),
    clear_power = 0.90
  )
  testthat::expect_false(hard$pass)
  testthat::expect_identical(hard$decision, "no_go")
  testthat::expect_false(isTRUE(hard$escalate))

  # After a single 0.95 escalation, marginal/fail remains no-go.
  fail_095 <- env$ancova_v2_score_pilot_decide(
    env$ancova_v2_score_pilot_metrics(.marginal_fixture(), compute_auc = FALSE),
    clear_power = 0.95,
    already_escalated = TRUE
  )
  testthat::expect_false(fail_095$pass)
  testthat::expect_identical(fail_095$decision, "no_go")
})

test_that("score pilot archives by-n quantiles for score and components", {
  env <- .load_score_pilot_env()
  q <- env$ancova_v2_score_pilot_quantiles_by_n(.go_fixture())
  testthat::expect_true(is.data.frame(q))
  testthat::expect_true(all(c(
    "n", "truth_class", "component", "n_obs", "q25", "median", "q75"
  ) %in% names(q)))
  comps <- unique(as.character(q$component))
  testthat::expect_true(all(c(
    "overall_score", "fragility_component", "bootstrap_reproducibility",
    "jackknife_stability"
  ) %in% comps))
  testthat::expect_false("borderline" %in% q$truth_class)
  testthat::expect_true(all(q$n %in% c(40L, 80L)))
})

test_that("score pilot gate writer emits machine-readable SCORE_PILOT_GATE.json", {
  env <- .load_score_pilot_env()
  out_dir <- file.path(tempdir(), "ancova-v2-score-pilot-gate")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "SCORE_PILOT_GATE.json")

  stamp <- env$ancova_v2_write_score_pilot_gate(
    data = .go_fixture(),
    output = out_path,
    clear_power = 0.90,
    compute_auc = TRUE
  )

  testthat::expect_true(file.exists(out_path))
  parsed <- jsonlite::fromJSON(out_path, simplifyVector = FALSE)
  testthat::expect_identical(parsed$gate, "score_pilot")
  testthat::expect_identical(parsed$calibration_unit, "lm_ancova_v2")
  testthat::expect_true(isTRUE(parsed$pass))
  testthat::expect_identical(parsed$decision, "go")
  testthat::expect_equal(as.numeric(parsed$frozen_clear_power), 0.90)
  testthat::expect_false(isTRUE(parsed$searched_L))
  testthat::expect_true(is.list(parsed$metrics))
  testthat::expect_true(is.list(parsed$bands))
  testthat::expect_identical(stamp$decision, "go")
})
