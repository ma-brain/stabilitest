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

.bp_scenario <- function(n_per_arm = 60, p0 = 0.25, target_power = 0.95,
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
      generator = list(n_per_arm = n_per_arm, p0 = p0, target_power = target_power,
                       solve_exact_power = TRUE, diagnostic_only = FALSE,
                       allocation = 0.5, effect_direction = 1),
      analysis = list(test_type = "fisher", alpha = 0.05,
                      weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)),
      screening = list(conclusions = "significant", target_n = 100L)
    ))
  )
}

test_that("adapter screening decision matches fisher.test parity", {
  env <- .load_bp_env()
  adapter <- env$binary_proportion_adapter()
  scen <- .bp_scenario(n_per_arm = 60, p0 = 0.25, target_power = 0.95)
  generated <- env$generate_binary_proportion(scen, seed = 123L)
  decision <- adapter$primary_decision(generated$data, scen)
  # The decision must equal a direct fisher.test on the generated table to
  # 1e-12 (p, conclusion).
  g1 <- generated$data$group1
  g2 <- generated$data$group2
  tab <- matrix(c(sum(g1), length(g1) - sum(g1),
                  sum(g2), length(g2) - sum(g2)), nrow = 2L)
  expected_p <- stats::fisher.test(tab)$p.value
  testthat::expect_equal(decision$p, expected_p, tolerance = 1e-12)
  testthat::expect_equal(decision$original_p, expected_p, tolerance = 1e-12)
  testthat::expect_equal(decision$significant, expected_p < 0.05)
  testthat::expect_equal(decision$conclusion, expected_p < 0.05)
  testthat::expect_identical(decision$test_type, "fisher")
  testthat::expect_equal(decision$alpha, 0.05)
})

test_that("adapter robustness parity with robustness_analysis at frozen weights", {
  env <- .load_bp_env()
  adapter <- env$binary_proportion_adapter()
  scen <- .bp_scenario(n_per_arm = 50, p0 = 0.25, target_power = 0.95)
  generated <- env$generate_binary_proportion(scen, seed = 777L)
  out <- adapter$run_robustness(generated$data, scen, n_boot = 60L, seed = 777L)
  direct <- stabilitest::robustness_analysis(
    generated$data$group1, generated$data$group2,
    test_type = "fisher",
    weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5),
    n_boot = 60L, seed = 777L
  )
  # p, estimate, conclusion to 1e-12.
  testthat::expect_equal(out$original_p, direct$original_p, tolerance = 1e-12)
  testthat::expect_equal(out$original_mean_diff, direct$original_mean_diff,
                         tolerance = 1e-12)
  testthat::expect_equal(out$original_significant, direct$original_significant)
  # The frozen jackknife-light weights are applied.
  testthat::expect_equal(out$weights, c(jackknife = 0, fragility = 0.5,
                                        bootstrap = 0.5))
})

test_that("adapter exposes the v1-weight comparator column path", {
  env <- .load_bp_env()
  adapter <- env$binary_proportion_adapter()
  testthat::expect_true(is.function(adapter$v1_weight_score))
  scen <- .bp_scenario(n_per_arm = 50, p0 = 0.25, target_power = 0.95)
  generated <- env$generate_binary_proportion(scen, seed = 5L)
  out <- adapter$run_robustness(generated$data, scen, n_boot = 40L, seed = 5L)
  # The v1 comparator score recomputes the composite with 0.4/0.4/0.2 weights
  # from the same component metrics; it is archived, never the fitted score.
  v1 <- adapter$v1_weight_score(out)
  testthat::expect_true(is.numeric(v1) && length(v1) == 1L && is.finite(v1))
  testthat::expect_gte(v1, 0)
  testthat::expect_lte(v1, 100)
})
