.welch3_runner_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch3_runner <- function(envir = parent.frame()) {
  root <- .welch3_runner_root()
  for (file in c("runner.R")) sys.source(file.path(root, "R", file), envir = envir)
  sys.source(file.path(root, "config", "scenarios.R"), envir = envir)
  sys.source(file.path(root, "config", "scenarios_2026_3.R"), envir = envir)
}

testthat::test_that("2026-2 paths are unchanged by default", {
  .load_welch3_runner()
  root <- "/study"
  testthat::expect_identical(welch_artifact_root(root), file.path(root, "artifacts"))
  testthat::expect_identical(
    welch_layer_output_dir(root, "production", "training"),
    file.path(root, "artifacts", "training")
  )
  testthat::expect_identical(
    welch_layer_checkpoint_dir(root, "production", "training"),
    file.path(root, "artifacts", "checkpoints", "training")
  )
  testthat::expect_identical(
    welch_layer_output_dir(root, "smoke", "training"),
    file.path(root, "outputs", "smoke", "training")
  )
})

testthat::test_that("2026-3 paths never collide with 2026-2", {
  .load_welch3_runner()
  root <- "/study"
  for (mode in c("smoke", "production")) {
    for (layer in c("training", "validation")) {
      old <- welch_layer_output_dir(root, mode, layer, "2026-2")
      new <- welch_layer_output_dir(root, mode, layer, "2026-3")
      testthat::expect_false(identical(old, new))
      testthat::expect_false(identical(
        welch_layer_checkpoint_dir(root, mode, layer, "2026-2"),
        welch_layer_checkpoint_dir(root, mode, layer, "2026-3")
      ))
    }
  }
  testthat::expect_identical(
    welch_layer_output_dir(root, "production", "validation", "2026-3"),
    file.path(root, "artifacts", "2026-3", "validation")
  )
  testthat::expect_error(welch_artifact_root(root, "2027-1"), "should be one of")
})

testthat::test_that("validation authority reads the versioned candidate hash", {
  .load_welch3_runner()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "artifacts", "2026-3", "summaries"), recursive = TRUE)
  dir.create(file.path(root, "artifacts", "summaries"), recursive = TRUE)
  writeLines("hash-v3", file.path(root, "artifacts", "2026-3", "summaries",
                                  "candidate-hash.txt"))
  writeLines("hash-v2", file.path(root, "artifacts", "summaries",
                                  "candidate-hash.txt"))
  testthat::expect_true(validate_welch_run_authority(
    "production", "validation", "hash-v3", root, study_version = "2026-3"
  ))
  testthat::expect_error(
    validate_welch_run_authority("production", "validation", "hash-v2", root,
                                 study_version = "2026-3"),
    "does not match"
  )
  testthat::expect_true(validate_welch_run_authority(
    "production", "validation", "hash-v2", root
  ))
})

testthat::test_that("results are read from the versioned directory", {
  .load_welch3_runner()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "artifacts", "2026-3", "training"), recursive = TRUE)
  saveRDS(data.frame(x = 3), file.path(root, "artifacts", "2026-3", "training",
                                       "completed.rds"))
  testthat::expect_equal(
    read_welch_results(root, "training", "training", study_version = "2026-3")$x, 3
  )
  testthat::expect_error(read_welch_results(root, "training", "training"),
                         "missing")
  testthat::expect_error(
    read_welch_results(root, "validation", "training", study_version = "2026-3"),
    "training authority"
  )
})

testthat::test_that("scenario IDs must match the study version", {
  .load_welch3_runner()
  v2 <- welch_scenarios()$scenario_id
  v3 <- welch_scenarios_2026_3()$scenario_id
  testthat::expect_true(.welch_check_scenario_version(v2, "2026-2"))
  testthat::expect_true(.welch_check_scenario_version(v3, "2026-3"))
  testthat::expect_error(.welch_check_scenario_version(v2, "2026-3"),
                         "do not belong")
  testthat::expect_error(.welch_check_scenario_version(v3, "2026-2"),
                         "do not belong")
  testthat::expect_error(.welch_check_scenario_version(c(v2[1], v3[1]), "2026-3"),
                         "do not belong")
})

testthat::test_that("CLI accepts --study-version and defaults to 2026-2", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.welch3_runner_root(), "run_calibration.R"), envir = env)
  testthat::expect_identical(env$parse_welch_cli(character())$study_version,
                             "2026-2")
  testthat::expect_identical(
    env$parse_welch_cli(c("--study-version", "2026-3"))$study_version, "2026-3"
  )
  testthat::expect_error(env$parse_welch_cli(c("--study-version", "bogus")),
                         "must be 2026-2 or 2026-3")
})
