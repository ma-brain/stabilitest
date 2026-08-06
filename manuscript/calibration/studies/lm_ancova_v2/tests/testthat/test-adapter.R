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

.canonical_scenario <- function(stress = NULL, missing_rate = NULL) {
  generator <- list(
    n = 80L, baseline_r2 = 0.40, target_power = 0.90,
    allocation = 0.5, residual_sd = 1, effect_direction = 1
  )
  if (!is.null(stress)) {
    generator$stress <- stress
    if (!is.null(missing_rate)) generator$missing_rate <- missing_rate
  }
  list(
    parameters = list(
      generator = generator,
      analysis = list(
        formula = "outcome ~ treatment + baseline",
        term = "treatmentB",
        alpha = 0.05
      )
    )
  )
}

.assemble_score <- function(s_jack, frag_comp, s_boot, weights) {
  weights[["jackknife"]] * s_jack +
    weights[["fragility"]] * frag_comp +
    weights[["bootstrap"]] * s_boot
}

test_that("ancova v2 primary decision matches reduced robustness_lm", {
  env <- .load_lm_ancova_v2_study_env()
  scenario <- .canonical_scenario()
  generated <- env$generate_lm_ancova(scenario, seed = 6601L)
  screen <- env$ancova_v2_primary_decision(generated$data, scenario)
  full <- stabilitest::robustness_lm(
    outcome ~ treatment + baseline, generated$data,
    term = "treatmentB", n_boot = 1L, seed = 44,
    weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
  )

  testthat::expect_equal(screen$p_value, full$original_p, tolerance = 1e-12)
  testthat::expect_equal(screen$estimate, full$original_estimate, tolerance = 1e-12)
  testthat::expect_identical(screen$conclusion, full$original_significant)
  testthat::expect_true(all(generated$data$.row_id %in% screen$row_ids))
})

test_that("ancova v2 adapter exposes executor hooks and missing-row metadata", {
  env <- .load_lm_ancova_v2_study_env()
  adapter <- env$lm_ancova_v2_adapter()
  testthat::expect_true(is.function(adapter$generate))
  testthat::expect_true(is.function(adapter$primary_decision))
  testthat::expect_true(is.function(adapter$run_robustness))

  scenario <- .canonical_scenario(stress = "missing_baseline", missing_rate = 0.10)
  generated <- adapter$generate(scenario, seed = 6602L)
  screen <- adapter$primary_decision(generated$data, scenario)
  testthat::expect_true(length(screen$omitted_row_ids) > 0L)
  testthat::expect_identical(screen$status, "ok")
})

test_that("ancova v2 robustness uses Track A weights and archives v1 comparator", {
  env <- .load_lm_ancova_v2_study_env()
  adapter <- env$lm_ancova_v2_adapter()
  scenario <- .canonical_scenario()
  generated <- adapter$generate(scenario, seed = 6610L)

  result <- adapter$run_robustness(
    generated$data, scenario, n_boot = 40L, seed = 99L
  )

  v2_weights <- c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
  v1_weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

  testthat::expect_equal(
    unname(result$weights[names(v2_weights)]),
    unname(v2_weights),
    tolerance = 1e-8
  )

  s_jack <- result$metrics$jackknife_conclusion_stability
  frag <- result$metrics$worstcase_fragility_component
  s_boot <- result$metrics$bootstrap_reproducibility
  expected_v2 <- .assemble_score(s_jack, frag, s_boot, v2_weights)
  expected_v1 <- .assemble_score(s_jack, frag, s_boot, v1_weights)

  testthat::expect_equal(
    as.numeric(result$metrics$overall_robustness),
    expected_v2,
    tolerance = 1e-8
  )
  testthat::expect_true(is.numeric(result$v1_comparator_score))
  testthat::expect_length(result$v1_comparator_score, 1L)
  testthat::expect_equal(
    as.numeric(result$v1_comparator_score),
    expected_v1,
    tolerance = 1e-8
  )
  # Fitting score is the v2 composite; comparator is archived separately.
  testthat::expect_false(isTRUE(all.equal(
    as.numeric(result$metrics$overall_robustness),
    as.numeric(result$v1_comparator_score),
    tolerance = 1e-8
  )))
  testthat::expect_false(identical(
    unname(result$weights[names(v1_weights)]),
    unname(v1_weights)
  ))
})
