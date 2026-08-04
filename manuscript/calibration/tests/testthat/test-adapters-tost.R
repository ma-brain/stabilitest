testthat::test_that("TOST adapter preserves p_eff and conclusion parity", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  cases <- list(
    list(endpoint = "mean", type = "equivalence", margin = .5, paired = FALSE),
    list(endpoint = "mean", type = "noninferiority", margin = .5, paired = TRUE,
         higher_is_better = FALSE),
    list(endpoint = "prop", type = "equivalence", delta_L = -.2, delta_U = .2),
    list(endpoint = "or", type = "noninferiority", margin = 1.5)
  )
  for (spec in cases) {
    dat <- do.call(env$generate_tost, c(list(n_per_group = 30, seed = 5), spec))
    screened <- do.call(env$screen_tost, c(list(data = dat, n_boot = 8,
                                                max_removal_pct = .1, seed = 8), spec))
    analyzed <- do.call(env$run_tost_adapter, c(list(data = dat, n_boot = 8,
                                                       max_removal_pct = .1, seed = 8), spec))
    testthat::expect_equal(screened$p_eff, analyzed$analysis$original_p, tolerance = 1e-12)
    testthat::expect_identical(screened$conclusion, analyzed$screening$conclusion)
    testthat::expect_identical(analyzed$conclusion, analyzed$screening$conclusion)
    testthat::expect_identical(analyzed$analysis_conclusion, analyzed$screening$conclusion)
  }
})

testthat::test_that("executor preserves endpoint-specific TOST conclusions", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  scenario <- list(
    scenario_id = "tost_executor_conclusion", analysis_family = "tost",
    endpoint = "mean", design_layer = "core", truth_class = "clear",
    target_conclusion = "equivalent", sample_size = 40L, n_boot = 5L,
    max_removal_pct = .1, scenario_seed = 1701L,
    parameters = list(
      generator = list(endpoint = "mean", type = "equivalence", n_per_group = 20L,
                       mean_difference = 0, margin = .5),
      analysis = list(endpoint = "mean", type = "equivalence", margin = .5,
                      alpha = .05)
    )
  )
  generated <- env$generate_tost(seed = 101L, endpoint = "mean", type = "equivalence",
                                 n_per_group = 20L, mean_difference = 0, margin = .5)
  adapter <- list(
    primary_decision = function(data, scenario) {
      do.call(env$screen_tost, c(list(data = data), scenario$parameters$analysis))
    },
    run_robustness = function(data, scenario, n_boot, seed) {
      do.call(env$run_tost_adapter, c(list(data = data, n_boot = n_boot, seed = seed),
                                       scenario$parameters$analysis))
    }
  )
  row <- env$run_selected_replicate(
    scenario, adapter, replicate_id = 1L, data = generated,
    replicate_seed = 11L, n_boot = 5L
  )
  testthat::expect_identical(row$status, "completed")
  testthat::expect_identical(row$analysis_conclusion, row$screening_conclusion)
  testthat::expect_true(row$analysis_conclusion %in% c("equivalent", "not_equivalent"))
})

testthat::test_that("TOST screening is primary-test only and validates alpha", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  dat <- env$generate_tost(endpoint = "mean", type = "equivalence",
                           margin = .5, n_per_group = 30, seed = 17)
  screened <- env$screen_tost(dat, n_boot = 0, max_removal_pct = 0, seed = 99)
  testthat::expect_true(is.finite(screened$p_eff))
  testthat::expect_false("score" %in% names(screened$analysis))
  full <- env$run_tost_adapter(dat, n_boot = 2, max_removal_pct = .1, seed = 99)
  testthat::expect_true("bootstrap" %in% names(full$analysis))
  testthat::expect_equal(screened$p_eff, full$analysis$original_p, tolerance = 1e-12)
  testthat::expect_error(env$screen_tost(dat, alpha = 0), "alpha")
  testthat::expect_error(env$screen_tost(dat, alpha = NA_real_), "alpha")
})

testthat::test_that("TOST truth uses configured margins rather than realized estimates", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  testthat::expect_identical(env$classify_tost_truth(0.1, type = "equivalence", margin = .2), "equivalent")
  testthat::expect_identical(env$classify_tost_truth(0.3, type = "equivalence", margin = .2), "not_equivalent")
  testthat::expect_identical(env$classify_tost_truth(-0.2, type = "noninferiority", margin = .1,
                                                     higher_is_better = TRUE), "inferior")
  testthat::expect_identical(env$classify_tost_truth(-0.2, type = "noninferiority", margin = .1,
                                                     higher_is_better = FALSE), "noninferior")
  testthat::expect_error(env$generate_tost(endpoint = "prop", paired = TRUE), "paired")
})

testthat::test_that("TOST generator accepts the scenario equivalence_margin alias", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  dat <- env$generate_tost(endpoint = "mean", type = "equivalence",
                           n_per_group = 20, equivalence_margin = .5, seed = 6)
  testthat::expect_identical(dat$margin, .5)
  testthat::expect_error(
    env$generate_tost(endpoint = "mean", type = "equivalence", margin = .4,
                      equivalence_margin = .5),
    "must agree"
  )
})

testthat::test_that("degenerate proportion TOSTs have an explicit sparse failure class", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  dat <- list(group1 = rep.int(0L, 20), group2 = rep.int(0L, 20),
              endpoint = "prop", type = "equivalence", delta_L = -.2, delta_U = .2)
  failed <- env$run_tost_adapter(dat, n_boot = 5, max_removal_pct = .1)
  testthat::expect_identical(failed$status, "failed")
  testthat::expect_identical(failed$failure_class, "sparse_degenerate")
  testthat::expect_error(env$screen_tost(dat, n_boot = 5, max_removal_pct = .1),
                         "sparse_degenerate")
})

testthat::test_that("TOST generator rejects malformed bounds, sizes, seeds, and scalars", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "load_calibration.R"), env)
  env$load_calibration(envir = env)
  testthat::expect_error(env$generate_tost(endpoint = "mean"), "requires margin")
  testthat::expect_error(env$generate_tost(endpoint = "or", margin = 1), "margin must be > 1")
  testthat::expect_error(env$generate_tost(endpoint = "mean", delta_L = .2, delta_U = -.2), "strictly less")
  testthat::expect_error(env$generate_tost(endpoint = "mean", margin = .5, n = 15), "even integer")
  testthat::expect_error(env$generate_tost(endpoint = "mean", margin = .5, seed = -1), "non-negative integer")
  testthat::expect_error(env$generate_tost(endpoint = "prop", margin = .2,
                                           probability_control = NA_real_), "finite numeric")
})
