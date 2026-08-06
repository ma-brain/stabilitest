.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_v3_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_v3_study(project_root = .project_root(), envir = env)
  env
}

.assemble_score <- function(s_jack, frag_comp, s_boot, weights) {
  weights[["jackknife"]] * s_jack +
    weights[["fragility"]] * frag_comp +
    weights[["bootstrap"]] * s_boot
}

test_that("ancova v3 primary decision matches reduced robustness_lm", {
  env <- .load_lm_ancova_v3_study_env()
  scenarios <- env$lm_ancova_v3_scenarios()
  scenario <- scenarios[scenarios$scenario_id == "lm_ancova_v3_clean_n80_r2_40_clear", , drop = FALSE]
  generated <- env$generate_lm_ancova_v3(scenario, seed = 7601L)
  screen <- env$ancova_v3_primary_decision(generated$data, scenario)
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

test_that("ancova v3 adapter exposes hooks and routes violation generators", {
  env <- .load_lm_ancova_v3_study_env()
  adapter <- env$lm_ancova_v3_adapter()
  testthat::expect_true(is.function(adapter$generate))
  testthat::expect_true(is.function(adapter$primary_decision))
  testthat::expect_true(is.function(adapter$run_robustness))

  scenarios <- env$lm_ancova_v3_scenarios()
  clean <- scenarios[scenarios$scenario_id == "lm_ancova_v3_clean_n80_r2_40_clear", , drop = FALSE]
  viol <- scenarios[
    scenarios$scenario_id == "lm_ancova_v3_viol_n80_r2_40_clear_missing_baseline",
    ,
    drop = FALSE
  ]
  testthat::expect_identical(
    as.character(viol$parameters[[1L]]$matched_clean_id),
    clean$scenario_id
  )

  generated <- adapter$generate(viol, seed = 7602L)
  screen <- adapter$primary_decision(generated$data, viol)
  testthat::expect_true(length(screen$omitted_row_ids) > 0L)
  testthat::expect_identical(screen$status, "ok")
})

test_that("violated cells share n, R2, and clean-solved effect with matched clean", {
  env <- .load_lm_ancova_v3_study_env()
  scenarios <- env$lm_ancova_v3_scenarios()
  primary <- scenarios[
    vapply(scenarios$parameters, function(p) !isTRUE(p$diagnostic_only), logical(1)),
    ,
    drop = FALSE
  ]
  clean <- primary[
    vapply(primary$parameters, function(p) {
      is.null(p$violation_type) || !nzchar(as.character(p$violation_type))
    }, logical(1)),
    ,
    drop = FALSE
  ]
  violated <- primary[
    vapply(primary$parameters, function(p) {
      !is.null(p$violation_type) && nzchar(as.character(p$violation_type))
    }, logical(1)),
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(clean))) {
    cid <- clean$scenario_id[[i]]
    cg <- clean$parameters[[i]]$generator
    clean_effect <- env$solve_ancova_effect(
      n = as.integer(cg$n),
      target_power = as.numeric(cg$target_power),
      residual_sd = as.numeric(cg$residual_sd),
      allocation = 0.5
    )
    matched <- violated[
      vapply(violated$parameters, function(p) {
        identical(as.character(p$matched_clean_id), cid)
      }, logical(1)),
      ,
      drop = FALSE
    ]
    testthat::expect_identical(nrow(matched), 5L)

    for (j in seq_len(nrow(matched))) {
      vg <- matched$parameters[[j]]$generator
      testthat::expect_identical(as.integer(vg$n), as.integer(cg$n))
      testthat::expect_equal(as.numeric(vg$baseline_r2), as.numeric(cg$baseline_r2))
      testthat::expect_equal(as.numeric(vg$target_power), as.numeric(cg$target_power))
      testthat::expect_equal(as.numeric(vg$allocation), 0.5)

      gen_clean <- env$generate_lm_ancova_v3(clean[i, , drop = FALSE], seed = 8000L + i)
      gen_viol <- env$generate_lm_ancova_v3(matched[j, , drop = FALSE], seed = 9000L + j)
      testthat::expect_equal(
        as.numeric(gen_viol$truth$effect),
        as.numeric(gen_clean$truth$effect),
        tolerance = 1e-10
      )
      testthat::expect_equal(
        as.numeric(gen_clean$truth$effect),
        clean_effect * as.numeric(cg$effect_direction),
        tolerance = 1e-8
      )
    }
  }
})

test_that("ancova v3 robustness uses jackknife-light weights and archives v1 comparator", {
  env <- .load_lm_ancova_v3_study_env()
  adapter <- env$lm_ancova_v3_adapter()
  scenarios <- env$lm_ancova_v3_scenarios()
  scenario <- scenarios[scenarios$scenario_id == "lm_ancova_v3_clean_n80_r2_40_clear", , drop = FALSE]
  generated <- adapter$generate(scenario, seed = 7610L)

  result <- adapter$run_robustness(
    generated$data, scenario, n_boot = 40L, seed = 99L
  )

  v3_weights <- c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
  v1_weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

  testthat::expect_equal(
    unname(result$weights[names(v3_weights)]),
    unname(v3_weights),
    tolerance = 1e-8
  )

  s_jack <- result$metrics$jackknife_conclusion_stability
  frag <- result$metrics$worstcase_fragility_component
  s_boot <- result$metrics$bootstrap_reproducibility
  expected_v3 <- .assemble_score(s_jack, frag, s_boot, v3_weights)
  expected_v1 <- .assemble_score(s_jack, frag, s_boot, v1_weights)

  testthat::expect_equal(
    as.numeric(result$metrics$overall_robustness),
    expected_v3,
    tolerance = 1e-8
  )
  testthat::expect_true(is.numeric(result$v1_comparator_score))
  testthat::expect_equal(
    as.numeric(result$v1_comparator_score),
    expected_v1,
    tolerance = 1e-8
  )
  testthat::expect_false(isTRUE(all.equal(
    as.numeric(result$metrics$overall_robustness),
    as.numeric(result$v1_comparator_score),
    tolerance = 1e-8
  )))
})
