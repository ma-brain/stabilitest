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

test_that("ancova primary decision matches reduced robustness_lm", {
  env <- .load_lm_ancova_study_env()
  scenario <- list(
    parameters = list(
      generator = list(
        n = 80L, baseline_r2 = 0.40, target_power = 0.90,
        allocation = 0.5, residual_sd = 1, effect_direction = 1
      ),
      analysis = list(
        formula = "outcome ~ treatment + baseline",
        term = "treatmentB",
        alpha = 0.05
      )
    )
  )
  generated <- env$generate_lm_ancova(scenario, seed = 6601L)
  screen <- env$ancova_primary_decision(generated$data, scenario)
  full <- stabilitest::robustness_lm(
    outcome ~ treatment + baseline, generated$data,
    term = "treatmentB", n_boot = 1L, seed = 44
  )

  testthat::expect_equal(screen$p_value, full$original_p, tolerance = 1e-12)
  testthat::expect_equal(screen$estimate, full$original_estimate, tolerance = 1e-12)
  testthat::expect_identical(screen$conclusion, full$original_significant)
  testthat::expect_true(all(generated$data$.row_id %in% screen$row_ids))
})

test_that("ancova adapter exposes executor hooks and missing-row metadata", {
  env <- .load_lm_ancova_study_env()
  adapter <- env$lm_ancova_adapter()
  testthat::expect_true(is.function(adapter$generate))
  testthat::expect_true(is.function(adapter$primary_decision))
  testthat::expect_true(is.function(adapter$run_robustness))

  scenario <- list(
    parameters = list(
      generator = list(
        n = 80L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "missing_baseline", missing_rate = 0.10
      ),
      analysis = list(
        formula = "outcome ~ treatment + baseline",
        term = "treatmentB",
        alpha = 0.05
      )
    )
  )
  generated <- adapter$generate(scenario, seed = 6602L)
  screen <- adapter$primary_decision(generated$data, scenario)
  testthat::expect_true(length(screen$omitted_row_ids) > 0L)
  testthat::expect_identical(screen$status, "ok")
})
