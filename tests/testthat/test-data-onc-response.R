.pkg_root <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
}

test_that("onc_response_trial matches the frozen synthetic contract", {
  expect_true(exists("onc_response_trial"))
  expect_s3_class(onc_response_trial, "data.frame")
  expect_identical(nrow(onc_response_trial), 120L)
  expect_identical(
    names(onc_response_trial),
    c("subject_id", "arm", "response")
  )
  # arm is a two-level factor Placebo / Active, 60 per arm.
  expect_s3_class(onc_response_trial$arm, "factor")
  expect_identical(levels(onc_response_trial$arm), c("Placebo", "Active"))
  expect_identical(as.integer(table(onc_response_trial$arm)), c(60L, 60L))
  # response is 0/1 integer.
  expect_true(is.integer(onc_response_trial$response))
  expect_true(all(onc_response_trial$response %in% c(0L, 1L)))
  # subject_id is unique.
  expect_identical(
    length(unique(as.character(onc_response_trial$subject_id))), 120L
  )
  # No missing values anywhere.
  expect_false(anyNA(onc_response_trial))
})

test_that("onc_response_trial regenerates identically from the frozen generator", {
  generator <- file.path(.pkg_root(), "data-raw", "onc_response_trial.R")
  skip_if_not(file.exists(generator),
              "data-raw generator not shipped in the package tarball")
  env <- new.env(parent = globalenv())
  sys.source(generator, envir = env)
  expect_true(exists("generate_onc_response_trial", envir = env, inherits = FALSE))
  regenerated <- env$generate_onc_response_trial()
  expect_equal(regenerated, onc_response_trial, tolerance = 1e-12)
  expect_identical(env$ONC_RESPONSE_CASE_SEED, 20260809L)
})

test_that("onc_response_trial seed is absent from binary_proportion ledgers", {
  loader <- file.path(
    .pkg_root(), "manuscript", "calibration", "studies", "binary_proportion",
    "R", "load_study.R"
  )
  skip_if_not(file.exists(loader),
              "binary_proportion study sources are not shipped in the package tarball")
  study_env <- new.env(parent = globalenv())
  sys.source(loader, envir = study_env)
  study_env$load_binary_proportion_study(
    project_root = .pkg_root(), envir = study_env
  )
  scenarios <- study_env$binary_proportion_scenarios()
  expect_false(20260809L %in% as.integer(scenarios$scenario_seed))
  expect_false(any(grepl("onc_response", scenarios$scenario_id, fixed = TRUE)))
})
