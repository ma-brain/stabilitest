testthat::test_that("calibration scenarios satisfy the frozen schema", {
  scenario_path <- file.path("..", "..", "config", "scenarios.R")
  testthat::expect_true(file.exists(scenario_path))

  scenario_env <- new.env(parent = globalenv())
  sys.source(scenario_path, envir = scenario_env)
  testthat::expect_true(exists("calibration_scenarios", envir = scenario_env, inherits = FALSE))

  scenarios <- scenario_env$calibration_scenarios()
  required_columns <- c(
    "scenario_id", "analysis_family", "endpoint", "design_layer",
    "data_generator", "primary_adapter", "robustness_adapter", "truth_class",
    "target_conclusion", "sample_size", "n_boot", "max_removal_pct",
    "training_split", "scenario_seed", "parameters"
  )

  testthat::expect_s3_class(scenarios, "tbl_df")
  testthat::expect_identical(names(scenarios), required_columns)
  testthat::expect_true(all(!is.na(scenarios$scenario_id)))
  testthat::expect_length(unique(scenarios$scenario_id), nrow(scenarios))
  testthat::expect_setequal(
    scenarios$analysis_family,
    c("two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost")
  )
  testthat::expect_length(scenarios$scenario_id, 7L)
  testthat::expect_identical(
    scenarios$scenario_id,
    c(
      "two_sample_smoke", "proportion_smoke", "lm_smoke", "binomial_smoke",
      "poisson_smoke", "cox_smoke", "tost_smoke"
    )
  )
  testthat::expect_setequal(scenarios$design_layer, c("core", "stress", "validation"))
  testthat::expect_true(all(scenarios$n_boot == 1000L))
  testthat::expect_true(all(scenarios$max_removal_pct > 0 & scenarios$max_removal_pct <= 1))
  testthat::expect_true(all(scenarios$training_split > 0 & scenarios$training_split < 1))
  testthat::expect_true(is.list(scenarios$parameters))
  testthat::expect_true(all(vapply(scenarios$parameters, is.list, logical(1))))
})

testthat::test_that("loader locates the project root when sys.source omits ofile", {
  loader_path <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  loader_env <- new.env(parent = globalenv())

  sys.source(loader_path, envir = loader_env)

  project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  testthat::expect_identical(loader_env$.calibration_project_root(), project_root)
  testthat::expect_identical(loader_env$load_calibration(envir = loader_env), project_root)
  testthat::expect_true(exists("calibration_scenarios", envir = loader_env, inherits = FALSE))
})
