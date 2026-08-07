.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_bp_runner_env <- function() {
  env <- new.env(parent = globalenv())
  runner <- file.path(.study_root(), "run_calibration.R")
  sys.source(runner, envir = env)
  env$prepare_binary_proportion_runner(
    project_root = .project_root(),
    study_root = .study_root(),
    envir = env
  )
  env
}

test_that("isolated binary_proportion runner selects study scenarios and adapter", {
  env <- .load_bp_runner_env()

  smoke <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "smoke", engine = "proportion", scenario = NULL,
         validation_only = FALSE)
  )
  testthat::expect_true(nrow(smoke) >= 1L)
  testthat::expect_true(all(smoke$calibration_unit == "fisher_exact"))
  testthat::expect_true(all(smoke$analysis_engine == "proportion"))

  training <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "full", engine = "proportion", scenario = NULL,
         validation_only = FALSE)
  )
  testthat::expect_false(any(training$design_layer == "validation"))
  testthat::expect_true(all(training$calibration_unit == "fisher_exact"))

  validation <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "full", engine = "proportion", scenario = NULL,
         validation_only = TRUE)
  )
  testthat::expect_true(all(validation$design_layer == "validation"))
  testthat::expect_identical(nrow(validation), 18L)

  # Pilot restricts to core only (no stress, no validation).
  pilot <- env$.calibration_select_scenarios(
    env$calibration_scenarios(),
    list(mode = "pilot", engine = "proportion", scenario = NULL,
         validation_only = FALSE)
  )
  testthat::expect_true(all(pilot$design_layer == "core"))
  testthat::expect_identical(nrow(pilot), 36L)

  adapter <- env$.calibration_adapter_for_scenario(smoke[1L, , drop = FALSE], env)
  expected <- env$binary_proportion_adapter()
  testthat::expect_identical(names(adapter), names(expected))
  testthat::expect_true(identical(adapter$generate, expected$generate))
  testthat::expect_true(identical(adapter$primary_decision, expected$primary_decision))
})

test_that("isolated runner hashes study scenarios distinctly from the shared table", {
  env <- .load_bp_runner_env()
  scenarios <- env$calibration_scenarios()
  smoke <- env$.calibration_select_scenarios(
    scenarios,
    list(mode = "smoke", engine = "proportion", scenario = NULL,
         validation_only = FALSE)
  )
  study_smoke_hash <- env$calibration_scenario_hash(smoke)

  shared <- new.env(parent = globalenv())
  sys.source(
    file.path(.project_root(), "manuscript", "calibration", "R", "load_calibration.R"),
    envir = shared
  )
  shared$load_calibration(project_root = .project_root(), envir = shared)
  shared_hash <- env$calibration_scenario_hash(shared$calibration_scenarios())
  # The study scenario manifest hash must differ from the shared Welch table.
  testthat::expect_false(identical(study_smoke_hash, shared_hash))
  testthat::expect_true(all(smoke$calibration_unit == "fisher_exact"))
})

test_that("runner manifest records the study entry point and scenario hash", {
  env <- .load_bp_runner_env()
  scenarios <- env$calibration_scenarios()
  smoke <- env$.calibration_select_scenarios(
    scenarios,
    list(mode = "smoke", engine = "proportion", scenario = NULL,
         validation_only = FALSE)
  )
  study_smoke_hash <- env$calibration_scenario_hash(smoke)

  output <- tempfile("bp-manifest-")
  dir.create(output)
  manifest <- env$run_calibration(
    args = c("--mode", "smoke", "--phase", "screen", "--engine", "proportion",
             "--output", output, "--workers", "1"),
    project_root = .project_root(),
    hooks = list(screen = function(...) NULL),
    command = c("Rscript", file.path(.study_root(), "run_calibration.R"),
                "--mode", "smoke")
  )
  testthat::expect_true(any(grepl(
    "studies/binary_proportion/run_calibration.R",
    unlist(manifest$command), fixed = TRUE
  )))
  testthat::expect_identical(manifest$scenario_manifest_hash, study_smoke_hash)
})

test_that("shared runner direct-invoke excludes nested study entry points", {
  shared <- new.env(parent = globalenv())
  sys.source(
    file.path(.project_root(), "manuscript", "calibration", "run_calibration.R"),
    envir = shared
  )
  testthat::expect_true(is.function(shared$.calibration_is_direct))
  study_file <- normalizePath(
    file.path(.study_root(), "run_calibration.R"), mustWork = TRUE
  )
  shared_file <- normalizePath(
    file.path(.project_root(), "manuscript", "calibration", "run_calibration.R"),
    mustWork = TRUE
  )
  testthat::expect_true(grepl("studies/.*/run_calibration[.]R$", study_file))
  testthat::expect_true(grepl("manuscript/calibration/run_calibration[.]R$", shared_file))
  testthat::expect_false(grepl(
    paste0(.Platform$file.sep, "studies", .Platform$file.sep),
    shared_file
  ))
})
