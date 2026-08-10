.welch_runner_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch_runner_test_code <- function(envir = parent.frame()) {
  root <- .welch_runner_study_root()
  for (file in c("power.R", "generator.R", "adapter.R", "runner.R")) {
    sys.source(file.path(root, "R", file), envir = envir)
  }
  sys.source(file.path(root, "config", "scenarios.R"), envir = envir)
  invisible(root)
}

.mock_welch_adapter <- function(score_counter = NULL, fail_replicate = NULL) {
  row_builder <- get(
    "new_welch_score_row", envir = parent.frame(), inherits = TRUE
  )
  metric_columns <- get(
    "WELCH_METRIC_COLUMNS", envir = parent.frame(), inherits = TRUE
  )
  list(
    primary_decision = function(data, scenario) {
      replicate_id <- unique(data$replicate_id)
      significant <- replicate_id %% 2L == 0L
      list(
        p_value = if (significant) 0.01 else 0.50,
        significant = significant,
        conclusion = if (significant) "significant" else "non_significant"
      )
    },
    score = function(data, scenario, replicate_id, n_boot, production) {
      if (!is.null(score_counter)) score_counter$n <- score_counter$n + 1L
      if (!is.null(fail_replicate) && replicate_id == fail_replicate) {
        return(row_builder(
          scenario, replicate_id, n_boot = n_boot, status = "failed",
          failure_stage = "robustness_analysis",
          failure_class = "fixture_error",
          failure_message = "synthetic failure"
        ))
      }
      row_builder(
        scenario, replicate_id, n_boot = n_boot, status = "completed",
        original_p = 0.01, original_significant = TRUE,
        metrics = stats::setNames(rep(75, length(metric_columns)),
                                  metric_columns),
        elapsed_seconds = 0.01
      )
    }
  )
}

testthat::test_that("runner screens to quota without scoring exclusions", {
  .load_welch_runner_test_code()
  scenario <- welch_scenarios()[1L, , drop = FALSE]
  scenario$delta <- 0
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  output <- tempfile("welch-runner-")
  checkpoint <- tempfile("welch-checkpoint-")

  result <- run_welch_scenario(
    scenario, mode = "smoke", quota = 2L, draw_cap = 10L,
    output_dir = output, checkpoint_dir = checkpoint,
    adapter = .mock_welch_adapter(counter), workers = 1L, resume = FALSE
  )

  testthat::expect_identical(result$occupancy$attempted, 4L)
  testthat::expect_identical(result$occupancy$screened, 4L)
  testthat::expect_identical(result$occupancy$screened_significant, 2L)
  testthat::expect_identical(result$occupancy$excluded, 2L)
  testthat::expect_identical(result$occupancy$completed, 2L)
  testthat::expect_identical(counter$n, 2L)
})

testthat::test_that("resume never duplicates scenario replicate IDs", {
  .load_welch_runner_test_code()
  scenario <- welch_scenarios()[1L, , drop = FALSE]
  scenario$delta <- 0
  output <- tempfile("welch-resume-")
  checkpoint <- tempfile("welch-resume-checkpoint-")
  adapter <- .mock_welch_adapter()

  run_welch_scenario(
    scenario, mode = "smoke", quota = 2L, draw_cap = 10L,
    output_dir = output, checkpoint_dir = checkpoint,
    adapter = adapter, workers = 1L, resume = FALSE
  )
  resumed <- run_welch_scenario(
    scenario, mode = "smoke", quota = 3L, draw_cap = 12L,
    output_dir = output, checkpoint_dir = checkpoint,
    adapter = adapter, workers = 1L, resume = TRUE
  )

  testthat::expect_identical(resumed$occupancy$completed, 3L)
  testthat::expect_identical(
    anyDuplicated(resumed$completed$replicate_id),
    0L
  )
  testthat::expect_identical(
    anyDuplicated(resumed$ledger$replicate_id),
    0L
  )
})

testthat::test_that("layer authority isolates training from validation", {
  root <- .load_welch_runner_test_code()
  scenarios <- welch_scenarios()
  training <- select_welch_scenarios(scenarios, "training")
  validation <- select_welch_scenarios(scenarios, "validation")
  testthat::expect_true(all(training$design_layer == "training"))
  testthat::expect_false(any(training$design_layer == "validation"))
  testthat::expect_true(all(validation$design_layer == "validation"))

  testthat::expect_error(
    validate_welch_run_authority(
      mode = "production", layer = "validation", candidate_hash = NULL,
      study_root = root
    ),
    "candidate hash"
  )
  testthat::expect_error(
    read_welch_results(root, layer = "validation", authority = "training"),
    "training authority"
  )
})

testthat::test_that("runner failures retain the complete audit identity", {
  .load_welch_runner_test_code()
  scenario <- welch_scenarios()[1L, , drop = FALSE]
  scenario$delta <- 0
  result <- run_welch_scenario(
    scenario, mode = "smoke", quota = 2L, draw_cap = 8L,
    output_dir = tempfile("welch-failure-"),
    checkpoint_dir = tempfile("welch-failure-checkpoint-"),
    adapter = .mock_welch_adapter(fail_replicate = 2L),
    workers = 1L, resume = FALSE
  )

  testthat::expect_identical(nrow(result$failures), 1L)
  failure <- result$failures[1L, , drop = FALSE]
  testthat::expect_identical(failure$scenario_id, scenario$scenario_id)
  testthat::expect_identical(failure$replicate_id, 2L)
  testthat::expect_true(is.finite(failure$replicate_seed))
  testthat::expect_identical(failure$failure_stage, "robustness_analysis")
  testthat::expect_identical(failure$failure_message, "synthetic failure")
  testthat::expect_identical(result$occupancy$completed, 2L)
})

testthat::test_that("CLI has a valid worker default when core detection is unavailable", {
  root <- .welch_runner_study_root()
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "run_calibration.R"), envir = env)

  options <- env$parse_welch_cli(c("--mode", "smoke", "--layer", "training"))
  testthat::expect_true(is.finite(options$workers))
  testthat::expect_gte(options$workers, 1L)
})
