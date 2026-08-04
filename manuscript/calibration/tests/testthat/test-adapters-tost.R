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
  }
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

