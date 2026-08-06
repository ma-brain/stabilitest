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

test_that("lm_ancova scenarios form the frozen study contract", {
  env <- .load_lm_ancova_study_env()
  scenarios <- env$lm_ancova_scenarios()

  testthat::expect_true(env$validate_calibration_scenarios(scenarios))
  testthat::expect_true(all(scenarios$calibration_unit == "lm_ancova"))
  testthat::expect_false(anyDuplicated(scenarios$scenario_id) > 0L)
  testthat::expect_false(anyDuplicated(scenarios$scenario_seed) > 0L)

  training <- expand.grid(
    n = c(40L, 80L, 160L),
    baseline_r2 = c(0.10, 0.40, 0.70),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  validation <- expand.grid(
    n = c(60L, 120L, 240L),
    baseline_r2 = c(0.25, 0.55),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  core <- scenarios[scenarios$design_layer == "core", , drop = FALSE]
  held_out <- scenarios[scenarios$design_layer == "validation", , drop = FALSE]
  stress <- scenarios[scenarios$design_layer == "stress", , drop = FALSE]

  testthat::expect_identical(nrow(core), 27L)
  testthat::expect_identical(nrow(held_out), 18L)
  testthat::expect_identical(nrow(stress), 6L)
  testthat::expect_true(all(stress$design_layer == "stress"))
  testthat::expect_false(any(stress$design_layer == "validation"))

  core_keys <- data.frame(
    n = vapply(core$parameters, function(p) as.integer(p$generator$n), integer(1)),
    baseline_r2 = vapply(core$parameters, function(p) as.numeric(p$generator$baseline_r2), numeric(1)),
    truth_class = core$truth_class,
    stringsAsFactors = FALSE
  )
  held_keys <- data.frame(
    n = vapply(held_out$parameters, function(p) as.integer(p$generator$n), integer(1)),
    baseline_r2 = vapply(held_out$parameters, function(p) as.numeric(p$generator$baseline_r2), numeric(1)),
    truth_class = held_out$truth_class,
    stringsAsFactors = FALSE
  )

  order_keys <- function(x) {
    x[order(x$n, x$baseline_r2, x$truth_class), , drop = FALSE]
  }
  testthat::expect_equal(
    order_keys(core_keys),
    order_keys(training),
    ignore_attr = TRUE
  )
  testthat::expect_equal(
    order_keys(held_keys),
    order_keys(validation),
    ignore_attr = TRUE
  )

  stress_ids <- sort(stress$scenario_id)
  testthat::expect_identical(
    stress_ids,
    sort(c(
      "lm_ancova_stress_allocation_2to1",
      "lm_ancova_stress_heteroscedastic",
      "lm_ancova_stress_heavy_tails",
      "lm_ancova_stress_missing_baseline",
      "lm_ancova_stress_nonlinear_baseline",
      "lm_ancova_stress_interaction"
    ))
  )
})
