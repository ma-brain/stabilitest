.pkg_root <- function() {
  normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
}

test_that("pain_ancova_trial matches the frozen synthetic contract", {
  expect_true(exists("pain_ancova_trial"))
  expect_s3_class(pain_ancova_trial, "data.frame")
  expect_identical(nrow(pain_ancova_trial), 80L)
  expect_identical(
    names(pain_ancova_trial),
    c("subject_id", "arm", "baseline_pain", "week12_pain", "change")
  )
  expect_true(is.character(pain_ancova_trial$subject_id) || is.factor(pain_ancova_trial$subject_id))
  expect_identical(length(unique(as.character(pain_ancova_trial$subject_id))), 80L)
  expect_s3_class(pain_ancova_trial$arm, "factor")
  expect_identical(levels(pain_ancova_trial$arm), c("Placebo", "Active"))
  expect_identical(as.integer(table(pain_ancova_trial$arm)), c(40L, 40L))
  expect_true(is.numeric(pain_ancova_trial$baseline_pain))
  expect_true(is.numeric(pain_ancova_trial$week12_pain))
  expect_true(is.numeric(pain_ancova_trial$change))
  expect_false(anyNA(pain_ancova_trial))
  expect_true(all(pain_ancova_trial$baseline_pain >= 0 & pain_ancova_trial$baseline_pain <= 100))
  expect_true(all(pain_ancova_trial$week12_pain >= 0 & pain_ancova_trial$week12_pain <= 100))
  expect_equal(
    pain_ancova_trial$change,
    round(pain_ancova_trial$week12_pain - pain_ancova_trial$baseline_pain, 1),
    tolerance = 1e-8
  )
})

test_that("pain_ancova_trial regenerates identically from the frozen generator", {
  generator <- file.path(.pkg_root(), "data-raw", "pain_ancova_trial.R")
  skip_if_not(file.exists(generator), "data-raw generator not shipped in the package tarball")
  env <- new.env(parent = globalenv())
  sys.source(generator, envir = env)
  expect_true(exists("generate_pain_ancova_trial", envir = env, inherits = FALSE))
  regenerated <- env$generate_pain_ancova_trial()
  expect_equal(regenerated, pain_ancova_trial, tolerance = 1e-12)
  expect_identical(env$PAIN_ANCOVA_CASE_SEED, 20260806L)
})

test_that("pain_ancova_trial seed is absent from ANCOVA calibration ledgers", {
  loader <- file.path(
    .pkg_root(), "manuscript", "calibration", "studies", "lm_ancova", "R", "load_study.R"
  )
  skip_if_not(file.exists(loader), "ANCOVA study sources are not shipped in the package tarball")
  study_env <- new.env(parent = globalenv())
  sys.source(loader, envir = study_env)
  study_env$load_lm_ancova_study(project_root = .pkg_root(), envir = study_env)
  scenarios <- study_env$lm_ancova_scenarios()
  expect_false(20260806L %in% as.integer(scenarios$scenario_seed))
  expect_false(any(grepl("pain_ancova", scenarios$scenario_id, fixed = TRUE)))
})
