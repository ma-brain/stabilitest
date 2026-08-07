.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_bp_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)
  env$load_binary_proportion_study(project_root = .project_root(), envir = env)
  env
}

# Build a synthetic run-results structure mimicking the pilot output shape, with
# controllable null/clear scores so the gate metric is deterministic.
.synthetic_pilot_results <- function(null_scores, clear_scores) {
  mk <- function(scores, truth) {
    dplyr::tibble(
      scenario_id = sprintf("syn_%s", truth),
      truth_class = truth,
      screening_conclusion = "significant",
      status = "completed",
      overall_score = scores,
      replicate_id = seq_along(scores)
    )
  }
  list(screen = NULL, analyse = list(dplyr::bind_rows(
    if (length(null_scores)) mk(null_scores, "null") else NULL,
    if (length(clear_scores)) mk(clear_scores, "clear") else NULL
  )))
}

test_that("score pilot gate returns go when clear separates from null", {
  env <- .load_bp_env()
  # Null scores <= 40, clear scores >= 60: L=50 is FR-safe (FR=0) and RI=1.
  results <- .synthetic_pilot_results(
    null_scores = rep(20:40, each = 5),
    clear_scores = rep(60:100, each = 5)
  )
  tmp <- tempfile("pilot-gate-"); dir.create(file.path(tmp, "outputs", "score-pilot"), recursive = TRUE)
  saveRDS(results, file.path(tmp, "outputs", "score-pilot", "run-results.rds"))
  out_json <- file.path(tmp, "gate.json")
  gate_script <- file.path(.study_root(), "tools", "eval-score-pilot-gate.R")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(gate_script, "--pilot-dir", file.path(tmp, "outputs", "score-pilot"),
      "--out", out_json),
    stdout = TRUE, stderr = TRUE
  )
  attr(status, "status") <- attr(status, "status")
  if (is.null(attr(status, "status"))) attr(status, "status") <- 0L
  testthat::expect_equal(attr(status, "status"), 0L, info = paste(status, collapse = "\n"))
  testthat::expect_true(file.exists(out_json))
  gate <- jsonlite::fromJSON(out_json)
  testthat::expect_identical(gate$verdict, "go")
  testthat::expect_gte(gate$projected_ri, 0.72)
})

test_that("score pilot gate returns hard_no_go when scores overlap", {
  env <- .load_bp_env()
  # Null and clear scores both in 45-55: no FR-safe L achieves RI >= 0.70.
  set.seed(20260810)
  results <- .synthetic_pilot_results(
    null_scores = sample(40:60, 100, replace = TRUE),
    clear_scores = sample(40:60, 100, replace = TRUE)
  )
  tmp <- tempfile("pilot-gate-"); dir.create(file.path(tmp, "outputs", "score-pilot"), recursive = TRUE)
  saveRDS(results, file.path(tmp, "outputs", "score-pilot", "run-results.rds"))
  out_json <- file.path(tmp, "gate.json")
  gate_script <- file.path(.study_root(), "tools", "eval-score-pilot-gate.R")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(gate_script, "--pilot-dir", file.path(tmp, "outputs", "score-pilot"),
      "--out", out_json),
    stdout = TRUE, stderr = TRUE
  )
  attr(status, "status") <- attr(status, "status")
  if (is.null(attr(status, "status"))) attr(status, "status") <- 0L
  testthat::expect_equal(attr(status, "status"), 0L, info = paste(status, collapse = "\n"))
  gate <- jsonlite::fromJSON(out_json)
  testthat::expect_true(gate$verdict %in% c("hard_no_go", "marginal", "no_fr_safe_cutoff"))
})

test_that("score pilot gate reports the archived diagnostics", {
  env <- .load_bp_env()
  results <- .synthetic_pilot_results(
    null_scores = c(10, 20, 30, 40, 45),
    clear_scores = c(55, 65, 75, 85, 95)
  )
  tmp <- tempfile("pilot-gate-"); dir.create(file.path(tmp, "outputs", "score-pilot"), recursive = TRUE)
  saveRDS(results, file.path(tmp, "outputs", "score-pilot", "run-results.rds"))
  out_json <- file.path(tmp, "gate.json")
  gate_script <- file.path(.study_root(), "tools", "eval-score-pilot-gate.R")
  system2(file.path(R.home("bin"), "Rscript"),
          c(gate_script, "--pilot-dir", file.path(tmp, "outputs", "score-pilot"),
            "--out", out_json), stdout = TRUE, stderr = TRUE)
  gate <- jsonlite::fromJSON(out_json)
  testthat::expect_equal(gate$n_null_significant, 5L)
  testthat::expect_equal(gate$n_clear_significant, 5L)
  testthat::expect_false(is.null(gate$diagnostics$pooled_auc_clear_vs_null))
})
