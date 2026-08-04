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

testthat::test_that("selection uses design_layer for train and held-out splits", {
  runner_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "run_calibration.R"), envir = runner_env)
  scenarios <- data.frame(
    scenario_id = c("core-a", "stress-a", "val-a", "core-b"),
    analysis_family = c("lm", "lm", "lm", "cox"),
    design_layer = c("core", "stress", "validation", "core"),
    # training_split is schema-only; selection must ignore it.
    training_split = c(0.9, 0.1, 0.5, 0.7),
    stringsAsFactors = FALSE
  )

  pilot <- runner_env$.calibration_select_scenarios(
    scenarios,
    list(mode = "pilot", engine = "all", scenario = NULL, validation_only = FALSE)
  )
  testthat::expect_setequal(pilot$scenario_id, c("core-a", "core-b"))
  testthat::expect_true(all(pilot$design_layer == "core"))

  training <- runner_env$.calibration_select_scenarios(
    scenarios,
    list(mode = "full", engine = "all", scenario = NULL, validation_only = FALSE)
  )
  testthat::expect_setequal(training$scenario_id, c("core-a", "stress-a", "core-b"))
  testthat::expect_false(any(training$design_layer == "validation"))

  held_out <- runner_env$.calibration_select_scenarios(
    scenarios,
    list(mode = "full", engine = "all", scenario = NULL, validation_only = TRUE)
  )
  testthat::expect_identical(held_out$scenario_id, "val-a")
  testthat::expect_true(all(held_out$design_layer == "validation"))
})

testthat::test_that("runner forwards screened records and resume to modular hooks", {
  runner_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "run_calibration.R"), envir = runner_env)
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  output <- tempfile("calibration-runner-")
  calls <- new.env(parent = emptyenv())
  calls$screen <- 0L
  calls$analyse <- 0L
  hooks <- list(
    screen = function(scenario, plan, options, project_root) {
      calls$screen <- calls$screen + 1L
      list(selected = data.frame(replicate_id = 1L), scenario_id = scenario$scenario_id[[1L]])
    },
    analyse = function(scenario, plan, options, project_root, screened) {
      calls$analyse <- calls$analyse + 1L
      list(resume = options$resume, has_screened = !is.null(screened))
    }
  )
  result <- runner_env$run_calibration(
    args = c("--mode", "full", "--phase", "all", "--engine", "lm",
             "--workers", "1", "--resume", "--allow-dirty", "--output", output),
    project_root = root, hooks = hooks
  )
  testthat::expect_equal(calls$screen, 4L)
  testthat::expect_equal(calls$analyse, 4L)
  testthat::expect_true(all(vapply(result$selected_scenario_ids, nzchar, logical(1))))
  testthat::expect_true(file.exists(file.path(output, "manifest.rds")))
})

testthat::test_that("runner hook seam exposes screening and executor arguments", {
  runner_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "run_calibration.R"), envir = runner_env)
  scenario <- data.frame(
    scenario_id = "hook-scenario", analysis_family = "lm",
    truth_class = "null", target_conclusion = "non_significant",
    sample_size = 20L, n_boot = 1000L, stringsAsFactors = FALSE
  )
  plan <- list(mode = "smoke", n_boot = 5L, replicates_per_stratum = 2L,
               max_screen_draws = 2L)
  options <- list(output = "/tmp/calibration-hook", workers = 3L, resume = TRUE)
  hook <- function(scenario, adapter, target_by_stratum, max_draws, workers,
                   checkpoint_root) {
    list(target = target_by_stratum, max_draws = max_draws, workers = workers,
         checkpoint_root = checkpoint_root)
  }
  seen <- runner_env$.calibration_call_hook(
    hook, scenario, plan, options, "/tmp/project", adapter = list()
  )
  testthat::expect_identical(seen$target, c(
    "null::significant" = 2L, "null::non_significant" = 2L
  ))
  testthat::expect_identical(seen$max_draws, 2L)
  testthat::expect_identical(seen$workers, 3L)
  testthat::expect_identical(seen$checkpoint_root, "/tmp/calibration-hook/checkpoints")
})

testthat::test_that("screening targets include both conclusion outcomes per truth", {
  runner_env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "run_calibration.R"), envir = runner_env)
  conventional <- data.frame(truth_class = "borderline", target_conclusion = "significant")
  tost <- data.frame(truth_class = "clear", target_conclusion = "equivalent")
  plan <- list(replicates_per_stratum = 2L)
  testthat::expect_identical(
    runner_env$.calibration_target_by_stratum(conventional, plan),
    c("borderline::significant" = 2L, "borderline::non_significant" = 2L)
  )
  testthat::expect_identical(
    runner_env$.calibration_target_by_stratum(tost, plan),
    c("clear::equivalent" = 2L, "clear::not_equivalent" = 2L)
  )
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
