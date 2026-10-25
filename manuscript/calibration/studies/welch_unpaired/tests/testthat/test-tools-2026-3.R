# Tool-script wiring for study 2026-3. The end-to-end test runs the real
# scripts on synthetic scores in a throwaway project copy and is slow (cluster
# bootstraps), so it only runs with WELCH_TOOL_TESTS=1.

.welch_tools_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.welch_tools_project_root <- function() {
  normalizePath(file.path(.welch_tools_study_root(), "..", "..", "..", ".."),
                mustWork = TRUE)
}

# Copy code (never artifacts) into a temporary project mirroring the layout.
.make_welch_tool_project <- function() {
  src_study <- .welch_tools_study_root()
  src_project <- .welch_tools_project_root()
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  study <- file.path(tmp, "manuscript", "calibration", "studies", "welch_unpaired")
  dir.create(study, recursive = TRUE)
  for (d in c("R", "config", "tools")) {
    file.copy(file.path(src_study, d), study, recursive = TRUE)
  }
  file.copy(file.path(src_study, c("CALIBRATION_SAP.md", "CALIBRATION_SAP_2026-3.md")),
            study)
  file.copy(file.path(src_project, "DESCRIPTION"), tmp)
  dir.create(file.path(tmp, "inst", "extdata"), recursive = TRUE)
  file.copy(file.path(src_project, "inst", "extdata", "calibration-registry.csv"),
            file.path(tmp, "inst", "extdata"))
  list(project = tmp, study = study)
}

.write_synthetic_training <- function(artifact_root, ids, seed = 1,
                                      p_clear = c(2.5, 7)) {
  set.seed(seed)
  truth <- sub("^.*-W0[1-6]-", "", ids)
  arch <- sub("^.*-(W0[1-6])-.*$", "\\1", ids)
  completed <- do.call(rbind, lapply(seq_along(ids), function(i) {
    n <- 250L
    p <- switch(truth[[i]],
      null = stats::runif(n, 0, 0.05),
      borderline = 10^(-stats::runif(n, 1.5, 3)),
      clear = 10^(-stats::runif(n, p_clear[[1L]], p_clear[[2L]]))
    )
    mean_score <- c(null = 40, borderline = 60, clear = 90)[[truth[[i]]]]
    data.frame(
      scenario_id = ids[[i]], replicate_id = seq_len(n), archetype_id = arch[[i]],
      design_layer = "training", truth = truth[[i]], original_p = p,
      original_significant = TRUE, status = "completed",
      overall_robustness = pmin(100, pmax(0, round(stats::rnorm(n, mean_score, 8)))),
      stringsAsFactors = FALSE
    )
  }))
  dir <- file.path(artifact_root, "training")
  dir.create(dir, recursive = TRUE)
  saveRDS(completed, file.path(dir, "completed.rds"))
  saveRDS(completed[0, c("scenario_id", "replicate_id")], file.path(dir, "failures.rds"))
  saveRDS(completed[c("scenario_id", "replicate_id")], file.path(dir, "screening-ledger.rds"))
  utils::write.csv(data.frame(
    scenario_id = ids, attempted = 400L, screened = 400L, screening_failed = 0L,
    screened_significant = 250L, excluded = 150L, completed = 250L,
    analysis_failed = 0L, pending = 0L, quota_met = TRUE, occupancy_pass = TRUE
  ), file.path(dir, "occupancy.csv"), row.names = FALSE)
  jsonlite::write_json(
    list(layer = "training", mode = "production", silent_drops = 0L,
         runtime_seconds = 1),
    file.path(dir, "manifest.json"), auto_unbox = TRUE
  )
  invisible(completed)
}

.run_welch_tool <- function(project, tool, ...) {
  script <- file.path(project$study, "tools", tool)
  out <- suppressWarnings(system2(
    "Rscript", c(shQuote(script), ...), stdout = TRUE, stderr = TRUE
  ))
  list(output = out, status = attr(out, "status") %||% 0L)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

testthat::test_that("tool version flag parses and rejects unknown versions", {
  root <- .welch_tools_study_root()
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "R", "runner.R"), envir = env)
  testthat::expect_identical(env$welch_tool_study_version(character()), "2026-2")
  testthat::expect_identical(
    env$welch_tool_study_version(c("--study-version", "2026-3")), "2026-3"
  )
  testthat::expect_error(env$welch_tool_study_version("--study-version"),
                         "requires a value")
  testthat::expect_error(
    env$welch_tool_study_version(c("--study-version", "2027-1")), "must be one of"
  )
})

testthat::test_that("2026-3 closed-at-freeze rule covers both closed outcomes", {
  env <- new.env(parent = globalenv())
  root <- .welch_tools_study_root()
  for (f in c("thresholds.R", "thresholds_2026_3.R")) {
    sys.source(file.path(root, "R", f), envir = env)
  }
  closed_a <- list(status = "no_feasible_thresholds")
  closed_b <- list(status = "candidate",
                   p_comparator = list(status = "no_eligible_comparator"))
  open_c <- list(status = "candidate", p_comparator = list(status = "comparator"))
  testthat::expect_true(env$welch3_closed_at_freeze(closed_a))
  testthat::expect_true(env$welch3_closed_at_freeze(closed_b))
  testthat::expect_false(env$welch3_closed_at_freeze(open_c))
  testthat::expect_identical(env$welch3_closed_reason(closed_a),
                             "no_feasible_thresholds")
  testthat::expect_identical(env$welch3_closed_reason(closed_b),
                             "p_comparator_unavailable")
})

testthat::test_that("fit tool refuses 2026-2 scores under --study-version 2026-3", {
  testthat::skip_if_not(nzchar(Sys.which("Rscript")))
  project <- .make_welch_tool_project()
  root <- file.path(project$study, "artifacts")
  ids <- paste0("WTR-", rep(sprintf("W%02d", 1:6), each = 3), "-",
                rep(c("null", "borderline", "clear"), 6))
  .write_synthetic_training(file.path(root, "2026-3"), ids)
  res <- .run_welch_tool(project, "fit_training_candidate.R",
                         "--study-version", "2026-3")
  testthat::expect_true(res$status != 0L)
  testthat::expect_match(paste(res$output, collapse = "\n"),
                         "do not belong to Welch study 2026-3")
  testthat::expect_false(dir.exists(file.path(root, "2026-3", "summaries")))
})

.welch3_end_to_end <- function(p_clear, expect_closed) {
  testthat::skip_if_not(identical(Sys.getenv("WELCH_TOOL_TESTS"), "1"),
                        "set WELCH_TOOL_TESTS=1 to run slow tool tests")
  project <- .make_welch_tool_project()
  artifacts <- file.path(project$study, "artifacts")
  ids <- paste0("WTR3-", rep(sprintf("W%02d", 1:6), each = 3), "-",
                rep(c("null", "borderline", "clear"), 6))
  .write_synthetic_training(file.path(artifacts, "2026-3"), ids, p_clear = p_clear)
  dir.create(file.path(artifacts, "2026-3", "design"), recursive = TRUE)
  writeLines("id\n1", file.path(artifacts, "2026-3", "design", "resolved-scenarios.csv"))
  writeLines("{}", file.path(artifacts, "2026-3", "design", "design-manifest.json"))

  fit <- .run_welch_tool(project, "fit_training_candidate.R", "--study-version", "2026-3")
  testthat::expect_equal(fit$status, 0L, info = paste(fit$output, collapse = "\n"))
  analysis <- readRDS(file.path(artifacts, "2026-3", "summaries", "training-analysis.rds"))
  testthat::expect_identical(analysis$status, "candidate")
  testthat::expect_identical(analysis$mapping_type, "single_band")
  testthat::expect_false(dir.exists(file.path(artifacts, "training")))

  frz <- .run_welch_tool(project, "freeze_candidate.R", "--study-version", "2026-3")
  testthat::expect_equal(frz$status, 0L, info = paste(frz$output, collapse = "\n"))
  testthat::expect_true(file.exists(
    file.path(artifacts, "2026-3", "summaries", "candidate-hash.txt")
  ))
  diagnostics <- jsonlite::read_json(
    file.path(artifacts, "2026-3", "summaries", "candidate-diagnostics.json")
  )
  testthat::expect_false(diagnostics$held_out_opened)
  testthat::expect_identical(isTRUE(diagnostics$closed_at_freeze), expect_closed)

  pub <- .run_welch_tool(project, "publish_fail_decision.R", "--study-version", "2026-3")
  published <- file.path(project$study, "published")
  if (expect_closed) {
    testthat::expect_equal(pub$status, 0L, info = paste(pub$output, collapse = "\n"))
    verdict <- jsonlite::read_json(file.path(published, "2026-3", "VERDICT.json"))
    testthat::expect_identical(verdict$registry_version, "welch-2026-3")
    testthat::expect_identical(verdict$reason, "p_comparator_unavailable")
    testthat::expect_false(verdict$held_out_opened)
    testthat::expect_false(file.exists(file.path(published, "VERDICT.json")))
  } else {
    testthat::expect_true(pub$status != 0L)
    testthat::expect_match(paste(pub$output, collapse = "\n"), "closed at the freeze")
    testthat::expect_false(dir.exists(file.path(published, "2026-3")))
  }
}

testthat::test_that("fit, freeze, publish: open decision is not published as a fail", {
  .welch3_end_to_end(p_clear = c(2.5, 7), expect_closed = FALSE)
})

testthat::test_that("fit, freeze, publish: unavailable comparator closes at freeze", {
  .welch3_end_to_end(p_clear = c(1.5, 2.5), expect_closed = TRUE)
})
