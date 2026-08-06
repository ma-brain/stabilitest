.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_runner_env <- function() {
  env <- new.env(parent = globalenv())
  runner <- file.path(.study_root(), "run_calibration.R")
  sys.source(runner, envir = env)
  env$prepare_lm_ancova_runner(
    project_root = .project_root(),
    study_root = .study_root(),
    envir = env
  )
  env
}

test_that("isolated ANCOVA runner selects study scenarios and adapter", {
  env <- .load_lm_ancova_runner_env()

  smoke <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "smoke", engine = "lm", scenario = NULL, validation_only = FALSE)
  )
  testthat::expect_true(nrow(smoke) >= 1L)
  testthat::expect_true(all(smoke$calibration_unit == "lm_ancova"))
  testthat::expect_true(all(smoke$analysis_engine == "lm"))

  training <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "full", engine = "lm", scenario = NULL, validation_only = FALSE)
  )
  testthat::expect_false(any(training$design_layer == "validation"))
  testthat::expect_true(all(training$calibration_unit == "lm_ancova"))

  validation <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "full", engine = "lm", scenario = NULL, validation_only = TRUE)
  )
  testthat::expect_true(all(validation$design_layer == "validation"))
  testthat::expect_identical(nrow(validation), 18L)

  adapter <- env$.calibration_adapter_for_scenario(smoke[1L, , drop = FALSE], env)
  expected <- env$lm_ancova_adapter()
  testthat::expect_identical(names(adapter), names(expected))
  testthat::expect_true(identical(adapter$generate, expected$generate))
  testthat::expect_true(identical(adapter$primary_decision, expected$primary_decision))
})

test_that("isolated runner hashes study scenarios and records study command", {
  env <- .load_lm_ancova_runner_env()
  scenarios <- env$calibration_scenarios()
  smoke <- env$.calibration_select_scenarios(
    scenarios,
    list(mode = "smoke", engine = "lm", scenario = NULL, validation_only = FALSE)
  )
  study_smoke_hash <- env$calibration_scenario_hash(smoke)

  shared <- new.env(parent = globalenv())
  sys.source(
    file.path(.project_root(), "manuscript", "calibration", "R", "load_calibration.R"),
    envir = shared
  )
  shared$load_calibration(project_root = .project_root(), envir = shared)
  shared_hash <- env$calibration_scenario_hash(shared$calibration_scenarios())
  testthat::expect_false(identical(study_smoke_hash, shared_hash))
  testthat::expect_true(all(smoke$calibration_unit == "lm_ancova"))

  output <- tempfile("lm-ancova-manifest-")
  dir.create(output)
  manifest <- env$run_calibration(
    args = c("--mode", "smoke", "--phase", "screen", "--engine", "lm",
             "--output", output, "--workers", "1"),
    project_root = .project_root(),
    hooks = list(screen = function(...) NULL),
    command = c("Rscript", file.path(.study_root(), "run_calibration.R"),
                "--mode", "smoke")
  )
  testthat::expect_true(any(grepl("studies/lm_ancova/run_calibration.R",
                                  unlist(manifest$command), fixed = TRUE)))
  testthat::expect_identical(manifest$scenario_manifest_hash, study_smoke_hash)
})
