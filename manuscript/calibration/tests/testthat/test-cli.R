testthat::test_that("calibration CLI parses the frozen options", {
  cli_path <- file.path("..", "..", "R", "cli.R")
  testthat::expect_true(file.exists(cli_path))
  cli_env <- new.env(parent = globalenv())
  sys.source(cli_path, envir = cli_env)

  options <- cli_env$parse_calibration_cli(c(
    "--mode", "full", "--phase", "screen", "--engine", "cox",
    "--scenario", "cox_weibull_stress", "--workers", "3", "--resume",
    "--master-seed", "1234", "--output", "/tmp/calibration-output",
    "--allow-dirty"
  ))

  testthat::expect_identical(options$mode, "full")
  testthat::expect_identical(options$phase, "screen")
  testthat::expect_identical(options$engine, "cox")
  testthat::expect_identical(options$scenario, "cox_weibull_stress")
  testthat::expect_identical(options$workers, 3L)
  testthat::expect_true(options$resume)
  testthat::expect_identical(options$master_seed, 1234L)
  testthat::expect_identical(options$output, "/tmp/calibration-output")
  testthat::expect_true(options$allow_dirty)
})

testthat::test_that("calibration CLI rejects malformed and unknown options", {
  cli_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "cli.R"), envir = cli_env)

  expect_error <- function(args, pattern) {
    testthat::expect_error(cli_env$parse_calibration_cli(args), pattern)
  }
  expect_error(character(), "--mode")
  expect_error(c("--mode", "smoke", "--unknown"), "Unknown option")
  expect_error(c("--mode"), "requires a value")
  prefix <- c("--mode", "smoke", "--output", tempfile("out-"))
  expect_error(c(prefix, "--workers", "0"), "positive integer")
  expect_error(c(prefix, "--workers", "1.5"), "positive integer")
  expect_error(c(prefix, "--phase", "bogus"), "phase")
  expect_error(c(prefix, "--engine", "bogus"), "engine")
  expect_error(c(prefix, "--workers", "1", "--workers", "2"),
               "only be supplied once")
  expect_error(c(prefix, "--resume"), "resume")
  expect_error(c("--mode", "full", "--output", tempfile("out-"),
                 "--phase", "screen", "--validation-only"),
               "validation-only")
})

testthat::test_that("mode plans freeze smoke, pilot, and full quotas", {
  cli_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "cli.R"), envir = cli_env)

  smoke <- cli_env$calibration_run_plan(list(mode = "smoke", phase = "all",
                                               engine = "all", workers = 1L,
                                               resume = FALSE,
                                               master_seed = 1L,
                                               output = tempdir(),
                                               allow_dirty = FALSE,
                                               scenario = NULL,
                                               validation_only = FALSE))
  pilot <- cli_env$calibration_run_plan(utils::modifyList(smoke, list(mode = "pilot")))
  full <- cli_env$calibration_run_plan(utils::modifyList(smoke, list(mode = "full")))
  testthat::expect_identical(smoke$n_boot, 5L)
  testthat::expect_identical(smoke$replicates_per_stratum, 2L)
  testthat::expect_identical(pilot$n_boot, 50L)
  testthat::expect_equal(pilot$replicates_per_stratum, c(10L, 25L))
  testthat::expect_identical(full$n_boot, 1000L)
  testthat::expect_true(is.null(full$replicates_per_stratum))
})

testthat::test_that("manifest hashes and records provenance", {
  manifest_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "manifest.R"), envir = manifest_env)
  scenarios <- data.frame(
    scenario_id = c("a", "b"), analysis_family = c("lm", "cox"),
    scenario_seed = c(1L, 2L), stringsAsFactors = FALSE
  )
  first <- manifest_env$calibration_scenario_hash(scenarios)
  second <- manifest_env$calibration_scenario_hash(scenarios)
  testthat::expect_identical(first, second)
  testthat::expect_match(first, "^[0-9a-f]{32}$")
  manifest <- manifest_env$new_calibration_manifest(
    scenarios = scenarios,
    options = list(mode = "smoke", phase = "all", engine = "all", workers = 1L,
                   master_seed = 1L, output = tempdir(), resume = FALSE,
                   allow_dirty = FALSE),
    command = c("Rscript", "run_calibration.R", "--mode", "smoke"),
    project_root = normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  )
  testthat::expect_true(all(c(
    "scenario_manifest_hash", "seed_ledger", "git_commit", "git_dirty",
    "command", "package_versions", "r_session", "start_time", "workers"
  ) %in% names(manifest)))
  testthat::expect_identical(manifest$scenario_manifest_hash, first)
  testthat::expect_true(is.list(manifest$seed_ledger))
})

testthat::test_that("manifest writes an audit file and output hashes", {
  manifest_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "R", "manifest.R"), envir = manifest_env)
  out <- tempfile("calibration-manifest-")
  dir.create(out)
  payload <- file.path(out, "results.csv")
  writeLines(c("x,y", "1,2"), payload)
  manifest <- list(start_time = Sys.time(), output = out)
  written <- manifest_env$write_calibration_manifest(
    manifest, out, output_files = payload
  )
  testthat::expect_true(file.exists(written))
  testthat::expect_true(file.exists(file.path(out, "manifest.rds")))
  testthat::expect_true(file.exists(file.path(out, "manifest.dput")))
  restored <- readRDS(written)
  testthat::expect_true(length(restored$output_hashes) == 1L)
  testthat::expect_match(restored$output_hashes[[1L]]$hash, "^[0-9a-f]{32}$")
})
