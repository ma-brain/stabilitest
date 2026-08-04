test_project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
pkgload::load_all(
  test_project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE
)

testthat::test_that("two-sample adapters preserve primary-test parity", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  testthat::expect_true(file.exists(adapter_path))
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)

  adapter <- adapter_env$two_sample_adapter()
  continuous <- list(
    group1 = c(-1.2, -0.4, 0.1, 0.8, 1.4, 0.3, -0.2, 0.6),
    group2 = c(-0.9, -0.1, 0.2, 0.5, 1.0, -0.3, 0.4, 0.7)
  )
  binary <- list(
    group1 = c(1, 1, 1, 0, 0, 1, 0, 1, 0, 0),
    group2 = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0)
  )
  paired <- list(
    group1 = c(10.1, 9.4, 11.2, 10.8, 12.0, 9.8, 10.5, 11.1),
    group2 = c(9.8, 9.5, 10.6, 10.7, 11.4, 9.6, 10.2, 10.9)
  )

  cases <- list(
    welch = list(data = continuous, test_type = "t.test"),
    paired_t = list(data = paired, test_type = "paired.t.test"),
    wilcoxon = list(data = continuous, test_type = "wilcoxon"),
    paired_wilcoxon = list(data = paired, test_type = "wilcoxon.paired"),
    brunner_munzel = list(data = continuous, test_type = "brunner_munzel"),
    fisher = list(data = binary, test_type = "fisher"),
    chisq = list(data = binary, test_type = "chisq", correct = FALSE),
    prop = list(data = binary, test_type = "prop", correct = FALSE)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    scenario <- list(
      parameters = list(analysis = list(
        test_type = case$test_type,
        alpha = 0.05,
        correct = if (is.null(case$correct)) TRUE else case$correct
      )),
      analysis_family = "two_sample",
      scenario_id = paste0("parity_", case_name)
    )
    primary <- adapter$primary_decision(case$data, scenario)
    robust <- suppressWarnings(adapter$run_robustness(
      case$data, scenario, n_boot = 5L, seed = 23L
    ))
    testthat::expect_equal(primary$p, robust$original_p, tolerance = 1e-12,
                           info = case_name)
    testthat::expect_identical(primary$conclusion, robust$original_significant,
                               info = case_name)
  }
})

testthat::test_that("two-sample generators cover paired, contamination, and binary designs", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)

  scenarios <- list(
    list(parameters = list(generator = list(
      n_per_group = 6L, effect_size = 0, distribution = "normal",
      paired = FALSE
    ), analysis = list(test_type = "t.test"))),
    list(parameters = list(generator = list(
      n_per_group = 6L, effect_size = 0.5, distribution = "heavy_tailed",
      paired = TRUE
    ), analysis = list(test_type = "paired.t.test"))),
    list(parameters = list(generator = list(
      n_per_group = 6L, effect_size = 0.5, distribution = "normal",
      sd_control = 1, sd_treatment = 3, contamination = 0.25,
      paired = FALSE
    ), analysis = list(test_type = "brunner_munzel"))),
    list(parameters = list(generator = list(
      n_per_group = 12L, probability_control = 0.2,
      probability_treatment = 0.6, paired = FALSE
    ), analysis = list(test_type = "fisher")))
  )

  generated <- lapply(seq_along(scenarios), function(i) {
    adapter_env$generate_two_sample(scenarios[[i]], seed = 100L + i)
  })
  testthat::expect_true(all(vapply(generated, function(x) {
    is.list(x) && all(c("group1", "group2") %in% names(x))
  }, logical(1))))
  testthat::expect_equal(length(generated[[1]]$group1), 6L)
  testthat::expect_equal(length(generated[[2]]$group1), length(generated[[2]]$group2))
  testthat::expect_true(all(generated[[4]]$group1 %in% c(0, 1)))
  testthat::expect_true(all(generated[[4]]$group2 %in% c(0, 1)))
})

testthat::test_that("adapter failures are explicit and do not substitute tests", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)
  adapter <- adapter_env$two_sample_adapter()
  data <- list(group1 = 1:5, group2 = 2:6)
  scenario <- list(parameters = list(analysis = list(test_type = "not-a-test")))

  failure <- testthat::expect_error(
    adapter$run_robustness(data, scenario, n_boot = 5L),
    class = "two_sample_adapter_error"
  )
  testthat::expect_match(conditionMessage(failure), "unsupported test_type")
})

testthat::test_that("generator supports imbalanced unpaired groups and rejects unequal pairs", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)

  imbalanced <- list(parameters = list(generator = list(
    n_group1 = 7L, n_group2 = 13L, effect_size = 0.4,
    distribution = "normal"
  ), analysis = list(test_type = "t.test")))
  generated <- adapter_env$generate_two_sample(imbalanced, seed = 42L)
  testthat::expect_length(generated$group1, 7L)
  testthat::expect_length(generated$group2, 13L)
  direct_named <- list(generator = list(
    n_group1 = 5L, n_group2 = 9L, effect_size = 0.2,
    distribution = "normal"
  ))
  direct_generated <- adapter_env$generate_two_sample(direct_named, seed = 43L)
  testthat::expect_length(direct_generated$group1, 5L)
  testthat::expect_length(direct_generated$group2, 9L)

  paired_bad <- list(parameters = list(generator = list(
    n_group1 = 7L, n_group2 = 13L, paired = TRUE
  ), analysis = list(test_type = "paired.t.test")))
  testthat::expect_error(
    adapter_env$generate_two_sample(paired_bad, seed = 42L),
    "paired designs require equal group sizes"
  )
})

testthat::test_that("sparse binary scenarios exercise all public proportion tests", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)
  adapter <- adapter_env$two_sample_adapter()
  settings <- list(parameters = list(generator = list(
    n_group1 = 18L, n_group2 = 27L, probability_control = 0.08,
    probability_treatment = 0.18, distribution = "binary"
  )))
  data <- adapter_env$generate_two_sample(settings, seed = 99L)

  for (test_type in c("fisher", "chisq", "prop")) {
    scenario <- list(parameters = list(analysis = list(
      test_type = test_type, alpha = 0.05, correct = TRUE
    )))
    scenario$max_removal_pct <- 0.05
    primary <- adapter$primary_decision(data, scenario)
    robust <- suppressWarnings(adapter$run_robustness(data, scenario, n_boot = 5L, seed = 7L))
    testthat::expect_equal(primary$p, robust$original_p, tolerance = 1e-12,
                           info = test_type)
    testthat::expect_identical(primary$conclusion, robust$original_significant,
                               info = test_type)
  }
})

testthat::test_that("scenario registry includes imbalanced and sparse binary strata", {
  scenario_path <- file.path("..", "..", "config", "scenarios.R")
  scenario_env <- new.env(parent = globalenv())
  sys.source(scenario_path, envir = scenario_env)
  scenarios <- scenario_env$calibration_scenarios()
  two_sample <- scenarios[scenarios$analysis_family == "two_sample", , drop = FALSE]
  testthat::expect_true(any(grepl("imbalanced", two_sample$scenario_id)))
  testthat::expect_true(any(grepl("sparse", two_sample$scenario_id)))
  generators <- two_sample$parameters[vapply(two_sample$parameters, function(x) {
    !is.null(x$generator$n_group1) && !is.null(x$generator$n_group2)
  }, logical(1))]
  testthat::expect_true(length(generators) >= 2L)
  testthat::expect_true(any(vapply(generators, function(x) {
    isTRUE(x$generator$probability_control < 0.1) &&
      isTRUE(x$generator$probability_treatment < 0.2)
  }, logical(1))))
})

testthat::test_that("configured adapter names expose callable wrappers", {
  adapter_path <- file.path("..", "..", "R", "adapters_two_sample.R")
  adapter_env <- new.env(parent = globalenv())
  sys.source(adapter_path, envir = adapter_env)
  testthat::expect_true(is.function(adapter_env$two_sample_primary_decision))
  testthat::expect_true(is.function(adapter_env$run_two_sample_robustness))
  scenario <- list(parameters = list(analysis = list(test_type = "t.test")))
  data <- list(group1 = c(1, 2, 3, 4), group2 = c(1, 2, 2, 3))
  direct <- adapter_env$two_sample_primary_decision(data, scenario)
  adapter <- adapter_env$two_sample_adapter()
  testthat::expect_equal(direct$p, adapter$primary_decision(data, scenario)$p)
})
