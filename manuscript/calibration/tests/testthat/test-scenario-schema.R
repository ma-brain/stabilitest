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
  testthat::expect_length(unique(scenarios$scenario_seed), nrow(scenarios))
  testthat::expect_setequal(
    scenarios$analysis_family,
    c("two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost")
  )
  testthat::expect_gte(length(scenarios$scenario_id), 7L)
  testthat::expect_true(all(c(
    "two_sample_smoke", "proportion_smoke", "lm_smoke", "binomial_smoke",
    "poisson_smoke", "cox_smoke", "tost_smoke"
  ) %in% scenarios$scenario_id))
  testthat::expect_true(all(vapply(scenarios$parameters, is.list, logical(1))))
  testthat::expect_setequal(scenarios$design_layer, c("core", "stress", "validation"))
  testthat::expect_true(all(scenarios$n_boot == 1000L))
  testthat::expect_true(all(scenarios$max_removal_pct > 0 & scenarios$max_removal_pct <= 1))
  testthat::expect_true(all(scenarios$training_split > 0 & scenarios$training_split < 1))
  testthat::expect_true(is.list(scenarios$parameters))
  testthat::expect_true(all(vapply(scenarios$parameters, is.list, logical(1))))
})

testthat::test_that("adapter metadata names screening helpers and public APIs", {
  scenario_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "config", "scenarios.R"), envir = scenario_env)
  scenarios <- scenario_env$calibration_scenarios()

  expected_primary <- c(
    two_sample = "two_sample_primary_decision",
    proportion = "two_sample_primary_decision",
    lm = "primary_decision_lm",
    binomial = "primary_decision_glm",
    poisson = "primary_decision_glm",
    cox = "screen_cox",
    tost = "screen_tost"
  )
  expected_robustness <- c(
    two_sample = "robustness_analysis",
    proportion = "robustness_analysis",
    lm = "robustness_lm",
    binomial = "robustness_glm",
    poisson = "robustness_glm",
    cox = "robustness_surv",
    tost = "robustness_tost"
  )
  for (family in names(expected_primary)) {
    rows <- scenarios[scenarios$analysis_family == family, , drop = FALSE]
    testthat::expect_true(nrow(rows) > 0L, info = family)
    testthat::expect_true(
      all(rows$primary_adapter == expected_primary[[family]]),
      info = family
    )
    testthat::expect_true(
      all(rows$robustness_adapter == expected_robustness[[family]]),
      info = family
    )
  }

  tost_smoke <- scenarios[scenarios$scenario_id == "tost_smoke", , drop = FALSE]
  testthat::expect_identical(nrow(tost_smoke), 1L)
  analysis <- tost_smoke$parameters[[1L]]$analysis
  testthat::expect_identical(analysis$type, "equivalence")
  testthat::expect_identical(analysis$endpoint, "mean")
  testthat::expect_true(is.numeric(analysis$margin) || is.numeric(analysis$delta_L))
})

testthat::test_that("registry closes Cox, proportion, wilcoxon.paired, and TOST gaps", {
  scenario_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "config", "scenarios.R"), envir = scenario_env)
  scenarios <- scenario_env$calibration_scenarios()

  cox <- scenarios[scenarios$analysis_family == "cox", , drop = FALSE]
  testthat::expect_gte(sum(cox$design_layer == "core"), 3L)
  testthat::expect_true(all(c("null", "borderline", "clear") %in% cox$truth_class[cox$design_layer == "core"]))
  testthat::expect_gte(sum(cox$design_layer == "stress"), 1L)
  testthat::expect_gte(sum(cox$design_layer == "validation"), 1L)

  proportion <- scenarios[scenarios$analysis_family == "proportion", , drop = FALSE]
  testthat::expect_gte(sum(proportion$design_layer == "core"), 1L)
  testthat::expect_gte(sum(proportion$design_layer == "stress"), 1L)
  testthat::expect_gte(sum(proportion$design_layer == "validation"), 1L)
  prop_tests <- vapply(proportion$parameters, function(x) x$analysis$test_type, character(1))
  testthat::expect_true(all(c("prop", "fisher", "chisq") %in% prop_tests))

  paired_wilcoxon <- vapply(scenarios$parameters, function(x) {
    identical(x$analysis$test_type, "wilcoxon.paired")
  }, logical(1))
  testthat::expect_gte(sum(paired_wilcoxon), 1L)
  testthat::expect_true(
    "core" %in% scenarios$design_layer[paired_wilcoxon]
  )

  tost <- scenarios[scenarios$analysis_family == "tost", , drop = FALSE]
  tost_core <- tost[tost$design_layer == "core", , drop = FALSE]
  testthat::expect_gte(nrow(tost_core), 3L)
  testthat::expect_true(all(c("null", "borderline", "clear") %in% tost_core$truth_class))
  testthat::expect_true(
    all(c("equivalent", "not_equivalent") %in% tost_core$target_conclusion)
  )
  testthat::expect_true(all(vapply(tost$parameters, function(x) {
    is.list(x$analysis) && !is.null(x$analysis$type)
  }, logical(1))))
})

testthat::test_that("loader locates the project root when sys.source omits ofile", {
  loader_path <- normalizePath(file.path("..", "..", "R", "load_calibration.R"), mustWork = TRUE)
  loader_env <- new.env(parent = globalenv())

  sys.source(loader_path, envir = loader_env)

  project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  testthat::expect_identical(loader_env$.calibration_project_root(), project_root)
  testthat::expect_identical(loader_env$load_calibration(envir = loader_env), project_root)
  testthat::expect_true(exists("calibration_scenarios", envir = loader_env, inherits = FALSE))

  foreign_loader <- tempfile(fileext = ".R")
  testthat::expect_true(file.copy(loader_path, foreign_loader, overwrite = TRUE))
  foreign_env <- new.env(parent = globalenv())
  sys.source(loader_path, envir = foreign_env)
  foreign_env$.calibration_script_file <- normalizePath(foreign_loader, mustWork = TRUE)
  foreign_env$load_calibration(project_root = project_root, envir = foreign_env)
  testthat::expect_identical(
    foreign_env$.calibration_script_file,
    normalizePath(foreign_loader, mustWork = TRUE)
  )
})
