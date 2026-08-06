#!/usr/bin/env Rscript

# Primary-test-only production power gate for the isolated ANCOVA v2 study.
# Reuses immutable v1 verify_ancova_power(); does not compute robustness scores.

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "lm_ancova_v2"),
    mustWork = TRUE
  )
}

.project_root <- function(study_root = .study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

.frozen_clear_power <- function(study_root) {
  stamp <- file.path(study_root, "artifacts", "summaries", "SCORE_PILOT_GATE.json")
  if (!file.exists(stamp)) {
    stop("SCORE_PILOT_GATE.json missing; freeze clear power before power gate",
         call. = FALSE)
  }
  gate <- jsonlite::fromJSON(stamp)
  clear_power <- as.numeric(gate$frozen_clear_power %||% gate$clear_power_evaluated)
  if (!clear_power %in% c(0.90, 0.95)) {
    stop(
      sprintf("frozen clear power must be 0.90 or 0.95, got %s", clear_power),
      call. = FALSE
    )
  }
  clear_power
}

`%||%` <- function(left, right) if (is.null(left)) right else left

args <- commandArgs(trailingOnly = TRUE)
draws <- 10000L
seed <- 20260806L
tol_power <- 0.02
tol_null <- 0.02
out <- file.path(.study_root(), "artifacts", "summaries", "power-verification.csv")

if (length(args)) {
  if ("--draws" %in% args) {
    draws <- as.integer(args[[which(args == "--draws") + 1L]])
  }
  if ("--seed" %in% args) {
    seed <- as.integer(args[[which(args == "--seed") + 1L]])
  }
  if ("--output" %in% args) {
    out <- args[[which(args == "--output") + 1L]]
  }
}

study_root <- .study_root()
project_root <- .project_root(study_root)
clear_target_power <- .frozen_clear_power(study_root)

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_lm_ancova_v2_study(project_root = project_root, envir = env)

scenarios <- env$lm_ancova_v2_scenarios(clear_target_power = clear_target_power)
canonical <- scenarios[
  scenarios$design_layer %in% c("core", "validation") &
    scenarios$calibration_unit == "lm_ancova_v2",
  ,
  drop = FALSE
]
canonical <- canonical[order(canonical$scenario_id), , drop = FALSE]

rows <- vector("list", nrow(canonical))
failures <- character()
for (i in seq_len(nrow(canonical))) {
  scenario <- canonical[i, , drop = FALSE]
  scenario_id <- as.character(scenario$scenario_id[[1L]])
  truth <- as.character(scenario$truth_class[[1L]])
  target <- switch(
    truth,
    null = 0.05,
    borderline = 0.60,
    clear = clear_target_power,
    stop(sprintf("unknown truth class: %s", truth), call. = FALSE)
  )
  # Independent per-scenario seed derived from the frozen master seed.
  scenario_seed <- as.integer(seed + i - 1L)
  verification <- env$verify_ancova_power(
    scenario,
    draws = draws,
    seed = scenario_seed
  )
  achieved <- as.numeric(verification$achieved_power)
  tol <- if (identical(truth, "null")) tol_null else tol_power
  ok <- abs(achieved - target) <= tol
  if (!ok) {
    failures <- c(
      failures,
      sprintf(
        "%s: achieved=%.4f target=%.2f tol=%.2f",
        scenario_id, achieved, target, tol
      )
    )
  }
  rows[[i]] <- data.frame(
    scenario_id = scenario_id,
    design_layer = as.character(scenario$design_layer[[1L]]),
    truth_class = truth,
    sample_size = as.integer(scenario$sample_size[[1L]]),
    draws = as.integer(draws),
    seed = scenario_seed,
    target = target,
    achieved = achieved,
    abs_error = abs(achieved - target),
    tolerance = tol,
    pass = ok,
    used_robustness_score = isTRUE(verification$used_robustness_score),
    stringsAsFactors = FALSE
  )
  message(sprintf(
    "[%d/%d] %s achieved=%.4f target=%.2f %s",
    i, nrow(canonical), scenario_id, achieved, target,
    if (ok) "PASS" else "FAIL"
  ))
}

table <- do.call(rbind, rows)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(table, out, row.names = FALSE)

meta <- list(
  calibration_unit = "lm_ancova_v2",
  clear_target_power = clear_target_power,
  draws = draws,
  master_seed = seed,
  tol_power = tol_power,
  tol_null = tol_null,
  n_scenarios = nrow(table),
  n_fail = sum(!table$pass),
  any_robustness_score = any(table$used_robustness_score),
  output = normalizePath(out, mustWork = TRUE),
  sha256 = digest::digest(file = out, algo = "sha256")
)
meta_path <- sub("\\.csv$", ".rds", out)
saveRDS(meta, meta_path, version = 2)

message(sprintf(
  "Wrote %s (%d scenarios, %d failures, clear_power=%.2f, sha256=%s)",
  out, nrow(table), meta$n_fail, clear_target_power, meta$sha256
))

if (meta$n_fail > 0L || isTRUE(meta$any_robustness_score)) {
  if (length(failures)) {
    message("Power gate failures:\n", paste(failures, collapse = "\n"))
  }
  if (isTRUE(meta$any_robustness_score)) {
    message("Power gate illegally computed robustness scores")
  }
  quit(status = 1L)
}

quit(status = 0L)
