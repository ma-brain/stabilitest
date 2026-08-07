#!/usr/bin/env Rscript

# Publish the binary-proportion calibration decision.  Opens held-out ONCE,
# validates the frozen candidate (no refit, no second candidate) under the
# conservative-bound rule, and publishes either outcome atomically.

`%||%` <- function(left, right) if (is.null(left)) right else left

study <- normalizePath("manuscript/calibration/studies/binary_proportion", mustWork = TRUE)
project <- normalizePath(".", mustWork = TRUE)
env <- new.env(parent = globalenv())
sys.source(file.path(study, "R", "load_study.R"), envir = env)
env$load_binary_proportion_study(project_root = project, envir = env)
sys.source(file.path(study, "analyse_calibration.R"), envir = env)
sys.source(file.path(study, "tools", "assemble_replicates.R"), envir = env)
sys.source(file.path(study, "tools", "freeze_and_publish.R"), envir = env)

# --- Load the frozen candidate + training artifacts -----------------------
sum_dir <- file.path(study, "artifacts/summaries")
frozen <- readRDS(file.path(sum_dir, "candidate.rds"))
if (is.null(frozen$candidate_hash)) {
  stop("no frozen candidate to validate", call. = FALSE)
}
scenario_hash <- frozen$scenario_manifest_hash
training_hash <- frozen$training_manifest_hash

# --- Assemble held-out replicates -----------------------------------------
val_root <- file.path(study, "artifacts/raw/validation/checkpoints/full/checkpoints")
val_dirs <- list.dirs(val_root, recursive = FALSE)
if (length(val_dirs)) {
  val_parts <- lapply(val_dirs, function(d) {
    obj <- readRDS(file.path(d, "full.rds"))
    if (is.data.frame(obj)) obj else obj$payload$replicates
  })
  all_val <- dplyr::bind_rows(val_parts)
} else {
  rr <- readRDS(file.path(study, "artifacts/raw/validation/run-results.rds"))
  all_val <- do.call(dplyr::bind_rows, lapply(rr$analyse, function(x) {
    if (is.data.frame(x) && nrow(x)) x else NULL
  }))
}
validation <- all_val[all_val$status == "completed" &
                       !is.na(all_val$screening_conclusion) &
                       all_val$screening_conclusion == "significant" &
                       is.finite(all_val$overall_score), , drop = FALSE]
message(sprintf("held-out significant completed: %d", nrow(validation)))
print(table(validation$truth_class))

validation_hash <- env$calibration_hash_object(validation)

# --- Validate the frozen candidate ONCE (no refit) ------------------------
validated <- env$validate_binary_proportion_candidate(
  frozen = frozen,
  validation = validation,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  validation_manifest_hash = validation_hash,
  cluster_B = 1000L,
  cluster_seed = 20260808L
)
message(sprintf("held-out status: %s reason: %s",
                validated$status,
                paste(validated$reason %||% NA, collapse = ",")))

# --- Build the published registry row -------------------------------------
# Either outcome (validated or fail-closed) publishes the same artifact set.
# The active runtime registry is NOT modified here (Gate B is separate
# human-approved work); this is the study's own published registry snapshot.
active_registry <- stabilitest:::load_calibration_registry()
fisher_idx <- active_registry$calibration_unit == "fisher_exact"
published_registry <- active_registry
published_registry$status[fisher_idx] <- validated$status
published_registry$cutoff_fragile[fisher_idx] <- if (identical(validated$status, "validated_method_specific")) {
  as.integer(validated$cutoff)
} else NA_real_
published_registry$cutoff_robust[fisher_idx] <- NA_real_  # two-band: single cutoff L
published_registry$version[fisher_idx] <- if (identical(validated$status, "validated_method_specific")) {
  "fisher-2026-1"
} else "taxonomy-2026-1"
published_registry$source[fisher_idx] <- sprintf("study:binary_proportion@%s",
                                                 substr(frozen$candidate_hash, 1, 12))
published_registry$supported_conditions[fisher_idx] <- if (identical(validated$status, "validated_method_specific")) {
  "canonical significant two-arm fisher_exact (binary_proportion Phase 1)"
} else {
  paste("uncalibrated:", validated$reason %||% "no_feasible_thresholds")
}

# --- Assemble the publication artifact bundle -----------------------------
training_core <- readRDS(file.path(study, "artifacts/raw/training/completed_training_core.rds"))
audit_training <- readRDS(file.path(study, "artifacts/raw/training/audit_training.rds"))
training_manifest <- readRDS(file.path(sum_dir, "training-manifest.rds"))
track_d <- readRDS(file.path(sum_dir, "training-replication-curve.rds"))

# Validation manifest (record that held-out was opened exactly once).
validation_manifest <- list(
  phase = "validation",
  workers = 4L,
  code_commit = system("git rev-parse HEAD", intern = TRUE),
  scenario_manifest_hash = scenario_hash,
  validation_artifact_hash = validation_hash,
  n_validation_significant = nrow(validation),
  candidate_hash = frozen$candidate_hash,
  validation_status = validated$status,
  validation_reason = validated$reason,
  validation_refit = FALSE,
  held_out_opened = TRUE,
  cluster_B = 1000L,
  cluster_seed = 20260808L
)

artifacts <- list(
  completed_training = training_core,
  completed_validation = validation,
  audit_training = audit_training,
  audit_validation = all_val,
  occupancy = read.csv(file.path(sum_dir, "training-occupancy.csv"),
                       stringsAsFactors = FALSE),
  candidate = frozen,
  validation = validated,
  registry = published_registry,
  training_manifest = training_manifest,
  validation_manifest = validation_manifest,
  replication_curve = track_d
)
gate_path <- file.path(sum_dir, "SCORE_PILOT_GATE.json")
if (file.exists(gate_path)) artifacts$score_pilot_gate <- readLines(gate_path)
diag_path <- file.path(sum_dir, "candidate-diagnostics.json")
if (file.exists(diag_path)) artifacts$candidate_diagnostics <- readLines(diag_path)

destination <- file.path(study, "published")
ledger <- env$binary_proportion_publish_atomic(artifacts, destination, allow_overwrite = TRUE)
message(sprintf("published %d artifacts to %s", length(ledger), destination))

# Final summary.
cat(sprintf("\n=== PUBLISHED DECISION ===\n"))
cat(sprintf("candidate_hash: %s\n", frozen$candidate_hash))
cat(sprintf("cutoff L: %s\n", frozen$cutoff))
cat(sprintf("held-out status: %s\n", validated$status))
if (!is.null(validated$reason)) cat(sprintf("reason: %s\n", validated$reason))
cat(sprintf("FR conservative upper: %s\n",
            round(validated$conservative_bounds$false_reassurance_upper, 4)))
cat(sprintf("RI conservative lower: %s\n",
            round(validated$conservative_bounds$robust_identification_lower, 4)))
cat(sprintf("registry fisher_exact status: %s\n", published_registry$status[fisher_idx]))
