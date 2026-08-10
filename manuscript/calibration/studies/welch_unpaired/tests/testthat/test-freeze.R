.welch_freeze_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch_freeze_code <- function(envir = parent.frame()) {
  sys.source(
    file.path(.welch_freeze_study_root(), "R", "validation.R"),
    envir = envir
  )
}

.welch_hash_context <- function() {
  list(
    study_version = "prospective Welch study 2026-2",
    gates = list(false_reassurance = 0.05, false_reassurance_upper = 0.10),
    scenario_manifest_hash = "scenario-a",
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    sap_hash = "sap-a",
    resolved_scenarios_hash = "resolved-a",
    generator_hash = "generator-a",
    adapter_hash = "adapter-a",
    thresholds_hash = "thresholds-a",
    training_data_hash = "training-a",
    package_version = "0.6.0"
  )
}

testthat::test_that("candidate hash covers every frozen decision input", {
  .load_welch_freeze_code()
  candidate <- list(
    status = "candidate", mapping_type = "three_band",
    cutoffs = c(40L, 70L), metrics = list(balanced_ordinal_accuracy = 1)
  )
  context <- .welch_hash_context()
  baseline <- hash_welch_candidate(candidate, context)

  changed_cutoff <- candidate
  changed_cutoff$cutoffs <- c(41L, 70L)
  changed_gate <- context
  changed_gate$gates$false_reassurance <- 0.04
  changed_scenario <- context
  changed_scenario$scenario_manifest_hash <- "scenario-b"
  changed_weight <- context
  changed_weight$weights[["jackknife"]] <- 0.3
  changed_weight$weights[["fragility"]] <- 0.5
  changed_version <- context
  changed_version$study_version <- "prospective Welch study 2026-3"

  hashes <- c(
    hash_welch_candidate(changed_cutoff, context),
    hash_welch_candidate(candidate, changed_gate),
    hash_welch_candidate(candidate, changed_scenario),
    hash_welch_candidate(candidate, changed_weight),
    hash_welch_candidate(candidate, changed_version)
  )
  testthat::expect_false(any(hashes == baseline))
  testthat::expect_identical(anyDuplicated(c(baseline, hashes)), 0L)
})

testthat::test_that("no-candidate decisions freeze with held-out closed", {
  .load_welch_freeze_code()
  candidate <- list(
    status = "no_feasible_thresholds",
    reason = "no_feasible_thresholds",
    cutoffs = c(NA_integer_, NA_integer_),
    selected = NULL
  )
  frozen <- freeze_welch_candidate(candidate, .welch_hash_context())

  testthat::expect_identical(frozen$status, "no_feasible_thresholds")
  testthat::expect_false(frozen$held_out_opened)
  testthat::expect_false(frozen$validation_refit)
  testthat::expect_true(nzchar(frozen$candidate_hash))
  testthat::expect_identical(
    frozen$candidate_hash,
    recompute_welch_candidate_hash(frozen)
  )
})

testthat::test_that("compact freeze diagnostics are JSON serializable", {
  .load_welch_freeze_code()
  metrics <- list(
    false_reassurance = 0.1,
    band_occupancy = table(c("fragile", "fragile", "robust")),
    bands = rep("fragile", 3L)
  )
  compact <- compact_welch_metrics(metrics)

  testthat::expect_false(inherits(compact$band_occupancy, "table"))
  testthat::expect_false("bands" %in% names(compact))
  testthat::expect_silent(
    jsonlite::toJSON(compact, auto_unbox = TRUE, null = "null")
  )
})
