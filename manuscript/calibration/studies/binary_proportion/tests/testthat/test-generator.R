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
                         truth_class = "clear", seed = 61001L,
                         allocation = 0.5, effect_direction = 1,
                         stress = NULL, ...) {
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
      generator = c(list(
        n_per_arm = n_per_arm, p0 = p0, target_power = target_power,
        solve_exact_power = TRUE, diagnostic_only = FALSE,
        allocation = allocation, effect_direction = effect_direction,
        stress = stress
      ), list(...)),
      analysis = list(test_type = "fisher", alpha = 0.05,
                      weights = c(jackknife = 0, fragility = 0.5,
                                  bootstrap = 0.5)),
      screening = list(conclusions = "significant", target_n = 100L)
    ))
  )
}

test_that("generate_binary_proportion produces 0/1 vectors per arm", {
  env <- .load_bp_env()
  out <- env$generate_binary_proportion(.bp_scenario(n_per_arm = 50), seed = 1L)
  testthat::expect_true(is.list(out))
  testthat::expect_true(is.list(out$data))
  g1 <- out$data$group1
  g2 <- out$data$group2
  testthat::expect_length(g1, 50L)
  testthat::expect_length(g2, 50L)
  testthat::expect_true(all(g1 %in% c(0, 1)))
  testthat::expect_true(all(g2 %in% c(0, 1)))
  testthat::expect_true(is.integer(g1))
  testthat::expect_true(is.integer(g2))
})

test_that("identical seeds reproduce the generated data exactly", {
  env <- .load_bp_env()
  s <- .bp_scenario(n_per_arm = 60, p0 = 0.25, target_power = 0.95)
  a <- env$generate_binary_proportion(s, seed = 4242L)
  b <- env$generate_binary_proportion(s, seed = 4242L)
  testthat::expect_identical(a$data$group1, b$data$group1)
  testthat::expect_identical(a$data$group2, b$data$group2)
  # Different seed -> different data (with overwhelming probability).
  c <- env$generate_binary_proportion(s, seed = 4243L)
  testthat::expect_false(identical(a$data$group1, c$data$group1))
})

test_that("null scenario uses p1 == p0 and clear uses the solved effect", {
  env <- .load_bp_env()
  null_out <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0,
                 truth_class = "null"),
    seed = 7L
  )
  testthat::expect_equal(null_out$truth$p1, 0.25)
  testthat::expect_equal(null_out$truth$effect, 0)

  clear_out <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.95,
                 truth_class = "clear"),
    seed = 7L
  )
  testthat::expect_gt(clear_out$truth$p1, 0.25)
  testthat::expect_gte(clear_out$truth$effect, 0)
  # The solved p1 must reproduce exact power 0.95.
  achieved <- env$exact_fisher_power(n = 100, p0 = 0.25,
                                     p1 = clear_out$truth$p1, alpha = 0.05)
  testthat::expect_true(abs(achieved - 0.95) <= 1e-6)
})

test_that("generator uses exact rbinom draws (no normal approximation)", {
  env <- .load_bp_env()
  out <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 30, p0 = 0.10, target_power = 0.95,
                 truth_class = "clear"),
    seed = 99L
  )
  # The truth records the underlying probabilities used for rbinom.
  testthat::expect_equal(out$truth$p0, 0.10)
  testthat::expect_gt(out$truth$p1, 0.10)
  # Event counts are consistent with rbinom at the recorded probabilities.
  testthat::expect_equal(sum(out$data$group2), as.integer(out$truth$control_events))
  testthat::expect_equal(sum(out$data$group1), as.integer(out$truth$active_events))
})

test_that("allocation switch produces 2:1 arms", {
  env <- .load_bp_env()
  out <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 90, p0 = 0.25, target_power = 0.60,
                 truth_class = "borderline", allocation = 2 / 3),
    seed = 5L
  )
  # allocation = fraction of the total in the active arm (matching the lm_ancova
  # generator semantics: n_active = round(total * allocation)).  Total =
  # 2 * n_per_arm = 180; allocation 2/3 -> 120 active, 60 control (2:1).
  testthat::expect_length(out$data$group1, 120L)
  testthat::expect_length(out$data$group2, 60L)
  testthat::expect_equal(out$truth$n_active, 120L)
  testthat::expect_equal(out$truth$n_control, 60L)
})

test_that("stress switches are applied and recorded", {
  env <- .load_bp_env()
  # Rare events: p0 driven to 0.03 by the switch regardless of base p0.
  rare <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.60,
                 truth_class = "borderline", stress = "rare_events"),
    seed = 11L
  )
  testthat::expect_equal(rare$truth$p0, 0.03)

  # Misclassification: recorded rate present, flips a fraction of outcomes.
  misc <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.60,
                 truth_class = "borderline", stress = "misclassification",
                 misclassification_rate = 0.05),
    seed = 12L
  )
  testthat::expect_equal(misc$truth$misclassification_rate, 0.05)

  # Overdispersion: recorded rho.
  over <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.60,
                 truth_class = "borderline", stress = "overdispersion",
                 overdispersion_rho = 0.10),
    seed = 13L
  )
  testthat::expect_equal(over$truth$overdispersion_rho, 0.10)

  # Missing outcomes: a fraction of group2 outcomes set to NA.
  miss <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 100, p0 = 0.25, target_power = 0.60,
                 truth_class = "borderline", stress = "missing_outcomes",
                 missing_rate = 0.10),
    seed = 14L
  )
  testthat::expect_gt(length(miss$truth$missing_row_ids), 0L)
  testthat::expect_equal(miss$truth$missing_rate, 0.10)
})

test_that("row-ID structure matches existing generators", {
  env <- .load_bp_env()
  out <- env$generate_binary_proportion(
    .bp_scenario(n_per_arm = 40, p0 = 0.25, target_power = 0.95), seed = 3L
  )
  # Long-form frame carries .row_id and arm (one row per subject).
  lf <- out$data$long_form
  testthat::expect_true(is.data.frame(lf))
  testthat::expect_true(".row_id" %in% names(lf))
  testthat::expect_equal(lf$.row_id, seq_len(80L))
  testthat::expect_true("arm" %in% names(lf))
  testthat::expect_true(all(c("group1", "group2") %in% names(out$data)))
})
