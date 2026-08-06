.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_publication_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_study(project_root = .project_root(), envir = env)
  for (tool in c("assemble_replicates.R", "freeze_and_publish.R")) {
    sys.source(file.path(.study_root(), "tools", tool), envir = env)
  }
  env
}

.audit_fixture <- function() {
  data.frame(
    scenario_id = c("s1", "s1", "s1", "s2", "s2", "s2", "s3"),
    replicate_id = c(1L, 2L, 3L, 1L, 2L, 3L, 1L),
    status = c("completed", "completed", "failed",
               "completed", "excluded", "completed", "failed"),
    failure_stage = c(NA, NA, "robustness", NA, "screening", NA, "generation"),
    failure_class = c(NA, NA, "error", NA, "quota", NA, "error"),
    failure_message = c(NA, NA, "boom", NA, "skip", NA, "gen"),
    truth_class = c("null", "null", "null", "clear", "clear", "clear", "borderline"),
    analysis_conclusion = c("significant", "significant", NA,
                            "significant", NA, "significant", NA),
    overall_score = c(20, 30, NA, 80, NA, 85, NA),
    stringsAsFactors = FALSE
  )
}

test_that("publication accounting reports attempts before completed-only tables", {
  env <- .load_publication_env()
  audit <- .audit_fixture()
  summary <- env$lm_ancova_publication_accounting(audit, min_quota = 2L)

  testthat::expect_identical(summary$attempted, 7L)
  testthat::expect_identical(summary$completed, 4L)
  testthat::expect_identical(summary$failed, 2L)
  testthat::expect_identical(summary$excluded, 1L)
  testthat::expect_true(is.data.frame(summary$by_scenario))
  testthat::expect_true(all(c("scenario_id", "attempted", "completed",
                              "failed", "failure_rate") %in% names(summary$by_scenario)))
  testthat::expect_equal(
    summary$by_scenario$failure_rate[summary$by_scenario$scenario_id == "s1"],
    1 / 3,
    tolerance = 1e-12
  )
  testthat::expect_identical(nrow(summary$completed_rows), 4L)
})

test_that("publication hard-fails on quota, failure rate, and destination faults", {
  env <- .load_publication_env()
  audit <- .audit_fixture()

  testthat::expect_error(
    env$lm_ancova_assert_publication_ready(audit, min_quota = 3L, max_failure_rate = 0.50),
    "quota shortfall"
  )
  high_failure <- audit[audit$scenario_id == "s1", , drop = FALSE]
  testthat::expect_error(
    env$lm_ancova_assert_publication_ready(
      high_failure, min_quota = 1L, max_failure_rate = 0.05
    ),
    "failure rate"
  )
  testthat::expect_error(
    env$lm_ancova_assert_publication_ready(
      audit, min_quota = 1L, max_failure_rate = 0.50,
      required_scenarios = c("s1", "s2", "missing_scenario")
    ),
    "missing scenario checkpoint"
  )
  testthat::expect_error(
    env$lm_ancova_assert_assembly_status(1L),
    "nonzero assembly"
  )

  destination <- tempfile("ancova-published-")
  dir.create(destination)
  writeLines("stale", file.path(destination, "already-there.txt"))
  testthat::expect_error(
    env$lm_ancova_publish_atomic(
      list(readme = "x"),
      destination = destination,
      allow_overwrite = FALSE
    ),
    "stale artifact"
  )

  testthat::expect_error(
    env$lm_ancova_hash_ledger(list(a = tempfile("missing-"))),
    "missing required hash target"
  )
})

test_that("atomic publication writes compact outputs and a complete hash ledger", {
  env <- .load_publication_env()
  destination <- tempfile("ancova-pub-ok-")
  artifacts <- list(
    completed_training = data.frame(scenario_id = "s1", overall_score = 20, stringsAsFactors = FALSE),
    audit_training = .audit_fixture(),
    occupancy = data.frame(scenario_id = "s1", completed = 2L, stringsAsFactors = FALSE),
    failures = data.frame(scenario_id = "s1", failure_rate = 0.1, stringsAsFactors = FALSE),
    power_verification = data.frame(scenario_id = "s1", achieved_power = 0.6, stringsAsFactors = FALSE),
    candidate = list(status = "candidate", cutoffs = c(50L, 70L)),
    validation = list(status = "validated_method_specific", validation_refit = FALSE),
    registry = data.frame(calibration_unit = "lm_ancova", status = "validated_method_specific",
                          stringsAsFactors = FALSE),
    training_manifest = list(scenario_manifest_hash = "train"),
    validation_manifest = list(scenario_manifest_hash = "val")
  )
  ledger <- env$lm_ancova_publish_atomic(artifacts, destination = destination)
  testthat::expect_true(dir.exists(destination))
  testthat::expect_true(is.list(ledger))
  testthat::expect_true(all(c(
    "completed_training.rds", "audit_training.rds", "occupancy.csv",
    "failures.csv", "power_verification.csv", "candidate.rds",
    "validation.rds", "registry.csv", "registry.rds",
    "training_manifest.rds", "validation_manifest.rds", "hash_ledger.rds"
  ) %in% basename(names(ledger))))
  testthat::expect_true(file.exists(file.path(destination, "hash_ledger.rds")))
})
