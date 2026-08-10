.welch_adapter_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.welch_adapter_project_root <- function() {
  normalizePath(
    file.path(.welch_adapter_study_root(), "..", "..", "..", ".."),
    mustWork = TRUE
  )
}

.load_welch_adapter_test_code <- function(envir = parent.frame()) {
  devtools::load_all(.welch_adapter_project_root(), quiet = TRUE)
  root <- .welch_adapter_study_root()
  sys.source(file.path(root, "R", "power.R"), envir = envir)
  sys.source(file.path(root, "R", "generator.R"), envir = envir)
  sys.source(file.path(root, "R", "adapter.R"), envir = envir)
  sys.source(file.path(root, "config", "scenarios.R"), envir = envir)
  invisible(root)
}

testthat::test_that("adapter uses the package API and returns the full audit row", {
  root <- .load_welch_adapter_test_code()
  scenario <- welch_scenarios()[
    welch_scenarios()$scenario_id == "WTR-W01-clear", , drop = FALSE
  ]
  scenario$delta <- solve_welch_delta(
    scenario$target_power, scenario$n_group1, scenario$n_group2,
    scenario$sd_group1, scenario$sd_group2, scenario$alpha
  )
  fixture <- utils::read.csv(file.path(root, "tests", "fixtures", "strong-welch.csv"))
  fixture$group <- factor(fixture$group, levels = c("group1", "group2"))
  fixture$latent_value <- fixture$value
  fixture$injected_offset <- 0
  fixture$replicate_seed <- welch_replicate_seed(scenario$seed, 7L)

  row <- welch_score_analysis(
    fixture, scenario, replicate_id = 7L, n_boot = 20L,
    production = FALSE
  )

  testthat::expect_identical(row$status, "completed")
  testthat::expect_identical(row$resolved_method, "welch_unpaired")
  testthat::expect_true(row$original_significant)
  testthat::expect_true(is.finite(row$original_p))
  testthat::expect_true(all(WELCH_METRIC_COLUMNS %in% names(row)))
  finite_metrics <- setdiff(
    WELCH_METRIC_COLUMNS,
    c(
      "p_at_fragility", "estimate_range_jackknife_lo",
      "estimate_range_jackknife_hi"
    )
  )
  testthat::expect_true(all(vapply(
    finite_metrics,
    function(column) is.finite(row[[column]]),
    logical(1)
  )))
  testthat::expect_equal(row$profile_alpha, 0.05)
  testthat::expect_identical(row$profile_n_boot, 20L)
  testthat::expect_equal(row$profile_max_removal_pct, 0.30)
  testthat::expect_equal(
    c(row$weight_jackknife, row$weight_fragility, row$weight_bootstrap),
    c(0.4, 0.4, 0.2)
  )
  testthat::expect_true(row$elapsed_seconds >= 0)
})

testthat::test_that("production adapter rejects any non-frozen profile", {
  .load_welch_adapter_test_code()
  scenario <- welch_scenarios()[1L, , drop = FALSE]

  testthat::expect_error(
    welch_validate_profile(
      scenario, n_boot = 1000L, max_removal_pct = 0.30,
      weights = c(jackknife = 0.5, fragility = 0.3, bootstrap = 0.2),
      production = TRUE
    ),
    "frozen weights"
  )
  testthat::expect_error(
    welch_validate_profile(
      scenario, n_boot = 1000L, max_removal_pct = 0.25,
      weights = WELCH_FROZEN_WEIGHTS, production = TRUE
    ),
    "removal cap"
  )
  testthat::expect_error(
    welch_validate_profile(
      scenario, n_boot = 20L, max_removal_pct = 0.30,
      weights = WELCH_FROZEN_WEIGHTS, production = TRUE
    ),
    "n_boot"
  )
})

testthat::test_that("adapter failures are explicit auditable rows", {
  root <- .load_welch_adapter_test_code()
  scenario <- welch_scenarios()[1L, , drop = FALSE]
  fixture <- utils::read.csv(file.path(root, "tests", "fixtures", "strong-welch.csv"))
  fixture$group <- factor(fixture$group, levels = c("group1", "group2"))
  fixture$value <- 1
  fixture$latent_value <- 1
  fixture$injected_offset <- 0
  fixture$replicate_seed <- welch_replicate_seed(scenario$seed, 9L)

  row <- welch_score_analysis(
    fixture, scenario, replicate_id = 9L, n_boot = 20L,
    production = FALSE
  )
  testthat::expect_identical(row$status, "failed")
  testthat::expect_identical(row$failure_stage, "robustness_analysis")
  testthat::expect_true(nzchar(row$failure_class))
  testthat::expect_true(nzchar(row$failure_message))
  testthat::expect_identical(row$scenario_id, scenario$scenario_id)
  testthat::expect_identical(row$replicate_id, 9L)
  testthat::expect_true(is.finite(row$replicate_seed))
})
