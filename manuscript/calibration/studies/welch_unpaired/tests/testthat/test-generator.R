.welch_generator_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch_generator_test_code <- function(envir = parent.frame()) {
  root <- .welch_generator_study_root()
  sys.source(file.path(root, "R", "power.R"), envir = envir)
  sys.source(file.path(root, "R", "generator.R"), envir = envir)
  sys.source(file.path(root, "config", "scenarios.R"), envir = envir)
  invisible(root)
}

testthat::test_that("Welch generation is reproducible and preserves RNG state", {
  .load_welch_generator_test_code()
  scenario <- welch_scenarios()[
    welch_scenarios()$scenario_id == "WTR-W02-borderline", , drop = FALSE
  ]
  scenario$delta <- solve_welch_delta(
    scenario$target_power, scenario$n_group1, scenario$n_group2,
    scenario$sd_group1, scenario$sd_group2, scenario$alpha
  )

  set.seed(9127)
  state_before <- .Random.seed
  a <- generate_welch_data(scenario, replicate_id = 7L)
  state_after <- .Random.seed
  b <- generate_welch_data(scenario, replicate_id = 7L)

  testthat::expect_identical(a, b)
  testthat::expect_identical(state_after, state_before)
  testthat::expect_identical(
    as.integer(table(a$group)),
    c(scenario$n_group1, scenario$n_group2)
  )
  testthat::expect_true(all(is.finite(a$value)))
  testthat::expect_true(all(is.finite(a$latent_value)))
  testthat::expect_equal(a$value, a$latent_value + a$injected_offset)
  testthat::expect_false(identical(
    a$value,
    generate_welch_data(scenario, replicate_id = 8L)$value
  ))
})

testthat::test_that("all frozen distributions produce finite requested rows", {
  .load_welch_generator_test_code()
  scenarios <- welch_scenarios()
  scenarios <- scenarios[
    !duplicated(scenarios$distribution), , drop = FALSE
  ]

  for (i in seq_len(nrow(scenarios))) {
    scenario <- scenarios[i, , drop = FALSE]
    if (is.na(scenario$delta)) {
      scenario$delta <- solve_welch_delta(
        scenario$target_power, scenario$n_group1, scenario$n_group2,
        scenario$sd_group1, scenario$sd_group2, scenario$alpha
      )
    }
    generated <- generate_welch_data(scenario, replicate_id = 3L)
    testthat::expect_identical(
      nrow(generated),
      scenario$n_group1 + scenario$n_group2,
      info = scenario$distribution
    )
    testthat::expect_true(
      all(is.finite(generated$value)),
      info = scenario$distribution
    )
  }
})

testthat::test_that("gross-error rows retain latent truth and injected values", {
  .load_welch_generator_test_code()
  scenario <- welch_scenarios()[
    welch_scenarios()$scenario_id == "WST-GE-null", , drop = FALSE
  ]
  generated <- generate_welch_data(scenario, replicate_id = 11L)

  treated <- generated$group == "group1"
  testthat::expect_true(any(generated$injected_offset[treated] != 0))
  testthat::expect_true(all(generated$injected_offset[!treated] == 0))
  testthat::expect_setequal(
    unique(generated$injected_offset),
    c(0, scenario$contamination_offset)
  )
  testthat::expect_true(all(generated$latent_delta == 0))
  testthat::expect_true(all(generated$truth == "null"))
  testthat::expect_equal(
    generated$value,
    generated$latent_value + generated$injected_offset
  )
})
