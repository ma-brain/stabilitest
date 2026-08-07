.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_bp_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)
  env$load_binary_proportion_study(project_root = .project_root(), envir = env)
  env
}

.bp_scenario <- function(n_per_arm = 100, p0 = 0.25, target_power = 0.95,
                         truth_class = "clear", seed = 61001L) {
  list(
    scenario_id = "test",
    analysis_engine = "proportion",
    calibration_family = "binary_proportion",
    calibration_unit = "fisher_exact",
    endpoint = "risk_difference",
    design_layer = "core",
    truth_class = truth_class,
    target_conclusion = "significant",
    sample_size = as.integer(2L * n_per_arm),
    scenario_seed = as.integer(seed),
    parameters = list(list(
      generator = list(
        n_per_arm = n_per_arm, p0 = p0, target_power = target_power,
        solve_exact_power = TRUE, diagnostic_only = FALSE,
        allocation = 0.5, effect_direction = 1
      ),
      analysis = list(test_type = "fisher", alpha = 0.05,
                      weights = c(jackknife = 0, fragility = 0.5,
                                  bootstrap = 0.5)),
      screening = list(conclusions = "significant", target_n = 100L)
    ))
  )
}

test_that("verify_prop_power confirms clear power 0.95 via fisher.test", {
  env <- .load_bp_env()
  # Unit-test settings: 2000 draws, tolerance 0.04 (per the SAP).
  res <- env$verify_prop_power(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.95,
                 truth_class = "clear"),
    draws = 2000L, seed = 20260808L, tolerance = 0.04
  )
  testthat::expect_equal(res$target_power, 0.95)
  testthat::expect_equal(res$draws, 2000L)
  testthat::expect_equal(res$seed, 20260808L)
  testthat::expect_false(res$used_robustness_score)
  testthat::expect_true(res$within_tolerance,
                        info = sprintf("achieved=%.4f target=%.4f",
                                       res$achieved_power, res$target_power))
})

test_that("verify_prop_power confirms null power ~ alpha", {
  env <- .load_bp_env()
  res <- env$verify_prop_power(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0,
                 truth_class = "null"),
    draws = 2000L, seed = 20260808L, tolerance = 0.04
  )
  testthat::expect_equal(res$target_power, 0)
  # Null achieved power should be near the (conservative) Fisher type-I rate,
  # well below 0.04 + 0 = 0.04.
  testthat::expect_lt(res$achieved_power, 0.06)
  testthat::expect_true(res$within_tolerance)
})

test_that("verify_prop_power uses the master seed deterministically", {
  env <- .load_bp_env()
  scen <- .bp_scenario(n_per_arm = 50, p0 = 0.50, target_power = 0.95,
                       truth_class = "clear")
  a <- env$verify_prop_power(scen, draws = 300L, seed = 20260808L)
  b <- env$verify_prop_power(scen, draws = 300L, seed = 20260808L)
  testthat::expect_equal(a$achieved_power, b$achieved_power)
})
