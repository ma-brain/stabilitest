testthat::test_that("selected replicate executor emits a complete row", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(
    scenario_id = "fake_success", analysis_family = "fake", endpoint = "mean",
    design_layer = "core", truth_class = "clear", target_conclusion = "significant",
    sample_size = 8L, n_boot = 5L, max_removal_pct = .3
  )
  adapter <- list(
    primary_decision = function(data, scenario) list(p_value = .01, conclusion = "significant"),
    run_robustness = function(data, scenario, n_boot, seed) list(
      status = "completed", original_p = .01, effective_p = .02,
      metrics = list(jackknife_conclusion_stability = 90, worstcase_fragility_component = 80,
                     worstcase_fragility_k = 2L, worstcase_fragility_pct = 25,
                     bootstrap_reproducibility = 95, overall_robustness = 88),
      interpretation_label = "Robust", n = nrow(data),
      analysis_conclusion = list(significant = TRUE)
    )
  )
  row <- env$run_selected_replicate(
    scenario, adapter, replicate_id = 1L, data = data.frame(x = seq_len(8L)),
    replicate_seed = 11L, n_boot = 5L
  )
  testthat::expect_true(env$validate_calibration_replicates(row))
  testthat::expect_identical(row$status, "completed")
  testthat::expect_equal(row$overall_score, 88)
  testthat::expect_identical(row$screening_conclusion, "significant")
  testthat::expect_identical(row$analysis_conclusion[[1L]], list(significant = TRUE))
})

testthat::test_that("executor audits full-fit, subset, and non-finite failures", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(
    scenario_id = "fake_failure", analysis_family = "fake", endpoint = "mean",
    design_layer = "stress", truth_class = "borderline", target_conclusion = "significant",
    sample_size = 4L, n_boot = 5L, max_removal_pct = .3
  )
  data <- data.frame(x = seq_len(4L))
  base <- list(primary_decision = function(...) list(p_value = .2, conclusion = "non_significant"))
  for (case in list(
    list(stage = "robustness", class = "error", adapter = c(base, list(
      run_robustness = function(...) stop("full fit failed")
    ))),
    list(stage = "subset", class = "subset_failure", adapter = c(base, list(
      run_robustness = function(...) list(status = "failed", failure_stage = "subset",
                                          failure_class = "subset_failure", failure_message = "subset failed")
    ))),
    list(stage = "metric_validation", class = "non_finite_metric", adapter = c(base, list(
      run_robustness = function(...) list(status = "completed", original_p = Inf,
                                          effective_p = .2, metrics = list(overall_robustness = 80))
    )))
  )) {
    row <- env$run_selected_replicate(scenario, case$adapter, 1L, data,
                                      replicate_seed = 11L, n_boot = 5L)
    testthat::expect_identical(row$status, "failed")
    testthat::expect_identical(row$failure_stage, case$stage)
    testthat::expect_identical(row$failure_class, case$class)
    testthat::expect_true(is.na(row$selected))
    testthat::expect_true(env$validate_calibration_replicates(row))
  }
})

testthat::test_that("scenario executor resumes checkpoints and is worker-order invariant", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(
    scenario_id = "fake_batch", analysis_family = "fake", endpoint = "mean",
    design_layer = "core", truth_class = "null", target_conclusion = "non_significant",
    sample_size = 3L, n_boot = 2L, max_removal_pct = .3
  )
  adapter <- list(
    generate = function(scenario, seed) {
      set.seed(seed); list(data = data.frame(x = rnorm(3L)))
    },
    primary_decision = function(data, scenario) list(p_value = .5, conclusion = "non_significant"),
    run_robustness = function(data, scenario, n_boot, seed) list(
      status = "completed", original_p = .5, effective_p = .5,
      metrics = list(jackknife_conclusion_stability = 100, worstcase_fragility_component = 100,
                     worstcase_fragility_k = 3L, worstcase_fragility_pct = 100,
                     bootstrap_reproducibility = 100, overall_robustness = 100),
      interpretation_label = "Robust", n = nrow(data)
    )
  )
  root <- tempfile("executor-checkpoint-"); dir.create(root)
  first <- env$run_full_scenario(scenario, adapter, replicate_ids = 1:4,
                                 master_seed = 77L, workers = 1L,
                                 checkpoint_root = root, checkpoint_batch = 2L)
  again <- env$run_full_scenario(scenario, adapter, replicate_ids = 1:4,
                                 master_seed = 77L, workers = 2L,
                                 checkpoint_root = root, checkpoint_batch = 2L,
                                 resume = TRUE)
  testthat::expect_true(env$validate_calibration_replicates(first))
  testthat::expect_identical(first, again)
  testthat::expect_true(file.exists(env$checkpoint_path(root, scenario$scenario_id, "full")))
})

testthat::test_that("timeout and generated-data failures preserve audit status", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(
    scenario_id = "fake_timeout", analysis_family = "fake", endpoint = "mean",
    design_layer = "validation", truth_class = "clear", target_conclusion = "significant",
    sample_size = 2L, n_boot = 2L, max_removal_pct = .3
  )
  adapter <- list(
    generate = function(...) stop("generator unavailable"),
    primary_decision = function(...) list(p_value = .01, conclusion = "significant"),
    run_robustness = function(...) list(status = "timeout", failure_class = "timeout",
                                        failure_stage = "robustness", failure_message = "timed out")
  )
  generated <- env$run_selected_replicate(scenario, adapter, 1L, data = NULL,
                                          replicate_seed = 4L)
  testthat::expect_identical(generated$failure_stage, "generation")
  testthat::expect_identical(generated$failure_class, "error")
  testthat::expect_true(env$validate_calibration_replicates(generated))
})

testthat::test_that("adapter-declared failure fields and timeout conditions are preserved", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(scenario_id = "declared", analysis_family = "fake", endpoint = "mean",
                   design_layer = "core", truth_class = "null", target_conclusion = "non_significant",
                   sample_size = 3L, n_boot = 2L, max_removal_pct = .3)
  screening <- list(conclusion = "non_significant")
  declared <- list(primary_decision = function(...) screening,
                   run_robustness = function(...) list(status = "failed", failure_stage = "subset",
                                                        failure_class = "subset_failure", failure_message = "vanished subset"))
  row <- env$run_selected_replicate(scenario, declared, 1L, data.frame(x = 1:3), replicate_seed = 1L)
  testthat::expect_identical(row$failure_stage, "subset")
  testthat::expect_identical(row$failure_class, "subset_failure")

  timeout_condition <- structure(list(message = "elapsed time limit reached"),
                                 class = c("simpleError", "error", "condition"))
  timeout_adapter <- list(primary_decision = function(...) screening,
                          run_robustness = function(...) stop(timeout_condition))
  timed <- env$run_selected_replicate(scenario, timeout_adapter, 1L,
                                      data.frame(x = 1:3), replicate_seed = 1L)
  testthat::expect_identical(timed$failure_stage, "timeout")
  testthat::expect_identical(timed$failure_class, "error")
})

testthat::test_that("selected adapter screening results with status ok are accepted", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(scenario_id = "status-ok-screen", analysis_family = "fake",
                   endpoint = "mean", design_layer = "core", truth_class = "clear",
                   target_conclusion = "significant", sample_size = 3L, n_boot = 2L,
                   max_removal_pct = .3, scenario_seed = 17L)
  adapter <- list(
    primary_decision = function(...) list(status = "ok", conclusion = "significant"),
    run_robustness = function(...) list(
      status = "completed", original_p = .01, effective_p = .02,
      metrics = list(jackknife_conclusion_stability = 100,
                     worstcase_fragility_component = 80,
                     worstcase_fragility_k = 1L, worstcase_fragility_pct = 33,
                     bootstrap_reproducibility = 100, overall_robustness = 88),
      interpretation_label = "Robust", n = 3L
    )
  )
  row <- env$run_selected_replicate(
    scenario, adapter, 1L, data = data.frame(x = 1:3),
    screening = adapter$primary_decision(), replicate_seed = 11L, n_boot = 2L
  )
  testthat::expect_identical(row$status, "completed")
  testthat::expect_identical(row$failure_stage, NA_character_)
  testthat::expect_identical(row$screening_conclusion, "significant")
})

testthat::test_that("parallel identity is defined for analysis fields while runtime is measured", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(scenario_id = "identity", analysis_family = "fake", endpoint = "mean",
                   design_layer = "core", truth_class = "null", target_conclusion = "non_significant",
                   sample_size = 3L, n_boot = 2L, max_removal_pct = .3)
  adapter <- list(
    generate = function(scenario, seed) { set.seed(seed); list(data = data.frame(x = rnorm(3L))) },
    primary_decision = function(...) list(conclusion = "non_significant"),
    run_robustness = function(data, ...) list(status = "completed", original_p = .5, effective_p = .5,
      metrics = list(jackknife_conclusion_stability = 100, worstcase_fragility_component = 100,
                     worstcase_fragility_k = 3L, worstcase_fragility_pct = 100,
                     bootstrap_reproducibility = 100, overall_robustness = 100),
      interpretation_label = "Robust", n = 3L)
  )
  one <- env$run_full_scenario(scenario, adapter, replicate_ids = 1:4, workers = 1L)
  many <- env$run_full_scenario(scenario, adapter, replicate_ids = 1:4, workers = 2L)
  testthat::expect_identical(one[, setdiff(names(one), "runtime_seconds")],
                             many[, setdiff(names(many), "runtime_seconds")])
  testthat::expect_true(all(one$runtime_seconds >= 0) && all(many$runtime_seconds >= 0))
})

testthat::test_that("resume never returns a checkpoint for a different replicate set", {
  env <- new.env(parent = globalenv())
  for (file in c("schema.R", "seeds.R", "checkpoints.R", "executor.R")) {
    sys.source(file.path("..", "..", "R", file), envir = env)
  }
  scenario <- list(scenario_id = "set-aware", analysis_family = "fake", endpoint = "mean",
                   design_layer = "core", truth_class = "null", target_conclusion = "non_significant",
                   sample_size = 3L, n_boot = 2L, max_removal_pct = .3)
  adapter <- list(
    generate = function(scenario, seed) list(data = data.frame(x = 1:3)),
    primary_decision = function(...) list(conclusion = "non_significant"),
    run_robustness = function(data, ...) list(status = "completed", original_p = .5, effective_p = .5,
      metrics = list(jackknife_conclusion_stability = 100, worstcase_fragility_component = 100,
                     worstcase_fragility_k = 3L, worstcase_fragility_pct = 100,
                     bootstrap_reproducibility = 100, overall_robustness = 100),
      interpretation_label = "Robust", n = 3L)
  )
  root <- tempfile("set-aware-"); dir.create(root)
  env$run_full_scenario(scenario, adapter, replicate_ids = 1:4, checkpoint_root = root)
  resumed <- env$run_full_scenario(scenario, adapter, replicate_ids = 5:8,
                                   checkpoint_root = root, resume = TRUE)
  testthat::expect_identical(resumed$replicate_id, 5:8)
})
