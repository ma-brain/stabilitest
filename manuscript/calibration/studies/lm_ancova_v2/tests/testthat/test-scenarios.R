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

.param_flag <- function(parameters, name) {
  vapply(parameters, function(p) isTRUE(p[[name]]), logical(1))
}

.param_target_power <- function(parameters) {
  vapply(parameters, function(p) as.numeric(p$generator$target_power), numeric(1))
}

test_that("lm_ancova_v2 scenarios form the isolated study contract", {
  env <- .load_lm_ancova_v2_study_env()
  scenarios <- env$lm_ancova_v2_scenarios()

  testthat::expect_true(env$validate_calibration_scenarios(scenarios))
  testthat::expect_true(all(scenarios$calibration_unit == "lm_ancova_v2"))
  testthat::expect_false(anyDuplicated(scenarios$scenario_id) > 0L)
  testthat::expect_false(anyDuplicated(scenarios$scenario_seed) > 0L)
  testthat::expect_true(all(grepl("^lm_ancova_v2_", scenarios$scenario_id)))

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

  borderline <- scenarios[scenarios$truth_class == "borderline", , drop = FALSE]
  fitting <- scenarios[scenarios$truth_class %in% c("null", "clear"), , drop = FALSE]
  testthat::expect_true(nrow(borderline) > 0L)
  testthat::expect_true(all(.param_flag(borderline$parameters, "diagnostic_only")))
  testthat::expect_false(any(.param_flag(fitting$parameters, "diagnostic_only")))

  clear <- scenarios[scenarios$truth_class == "clear", , drop = FALSE]
  testthat::expect_true(all(.param_target_power(clear$parameters) == 0.90))

  clear_95 <- env$lm_ancova_v2_scenarios(clear_target_power = 0.95)
  clear_95_rows <- clear_95[clear_95$truth_class == "clear", , drop = FALSE]
  testthat::expect_true(all(.param_target_power(clear_95_rows$parameters) == 0.95))
  testthat::expect_true(all(clear_95$calibration_unit == "lm_ancova_v2"))

  # Seed ranges stay isolated from the v1 study (31001 / 32001 / 33001).
  testthat::expect_true(min(core$scenario_seed) >= 41001L)
  testthat::expect_true(min(held_out$scenario_seed) >= 42001L)
  testthat::expect_true(min(stress$scenario_seed) >= 43001L)
  testthat::expect_length(
    intersect(scenarios$scenario_seed, 31001L:33006L),
    0L
  )

  stress_ids <- sort(stress$scenario_id)
  testthat::expect_identical(
    stress_ids,
    sort(c(
      "lm_ancova_v2_stress_allocation_2to1",
      "lm_ancova_v2_stress_heteroscedastic",
      "lm_ancova_v2_stress_heavy_tails",
      "lm_ancova_v2_stress_missing_baseline",
      "lm_ancova_v2_stress_nonlinear_baseline",
      "lm_ancova_v2_stress_interaction"
    ))
  )

  # Loader reuses v1 analytic power / generator helpers without mutating v1.
  testthat::expect_true(is.function(env$solve_ancova_effect))
  testthat::expect_true(is.function(env$generate_lm_ancova))
  testthat::expect_true(is.function(env$ancova_nominal_power))
})

test_that("study root cwd fallback never resolves to v1", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)

  v1_root <- normalizePath(
    file.path(.project_root(), "manuscript", "calibration", "studies", "lm_ancova"),
    mustWork = TRUE
  )
  v2_root <- .study_root()
  testthat::expect_true(dir.exists(v1_root))
  testthat::expect_true(
    file.exists(file.path(v1_root, "R", "load_study.R"))
  )
  testthat::expect_true(
    file.exists(file.path(v1_root, "config", "scenarios.R"))
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  # Ambiguous cwd: v1 has the same generic markers as v2.
  setwd(v1_root)
  resolved_from_v1 <- env$.lm_ancova_v2_study_root(script_path = NULL)
  testthat::expect_identical(resolved_from_v1, v2_root)
  testthat::expect_identical(basename(resolved_from_v1), "lm_ancova_v2")
  testthat::expect_false(identical(resolved_from_v1, v1_root))

  # Sibling studies/ cwd must still land on v2, not v1.
  setwd(dirname(v1_root))
  resolved_from_studies <- env$.lm_ancova_v2_study_root(script_path = NULL)
  testthat::expect_identical(resolved_from_studies, v2_root)
})
