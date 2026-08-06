#!/usr/bin/env Rscript

# Assemble completed training checkpoints, collect replication draws, and
# fit/freeze the binary-proportion candidate (Track A'' cutoff + Track D' curve)
# on training only.  Held-out is NOT opened here.

`%||%` <- function(left, right) if (is.null(left)) right else left

study <- normalizePath("manuscript/calibration/studies/binary_proportion", mustWork = TRUE)
project <- normalizePath(".", mustWork = TRUE)
env <- new.env(parent = globalenv())
sys.source(file.path(study, "R", "load_study.R"), envir = env)
env$load_binary_proportion_study(project_root = project, envir = env)
sys.source(file.path(study, "analyse_calibration.R"), envir = env)
sys.source(file.path(study, "tools", "assemble_replicates.R"), envir = env)

# Assemble replicates from per-scenario full checkpoints (robust to a parent
# process that died after writing run-results.rds).
ckpt_root <- file.path(study, "artifacts/raw/training/checkpoints/full/checkpoints")
dirs <- list.dirs(ckpt_root, recursive = FALSE)
if (!length(dirs)) {
  # Fallback: read the monolithic run-results.rds if checkpoints are absent.
  rr_path <- file.path(study, "artifacts/raw/training/run-results.rds")
  if (!file.exists(rr_path)) {
    stop("no training checkpoints or run-results.rds found", call. = FALSE)
  }
  rr <- readRDS(rr_path)
  all_reps <- do.call(dplyr::bind_rows, lapply(rr$analyse, function(x) {
    if (is.data.frame(x) && nrow(x)) x else NULL
  }))
} else {
  parts <- lapply(dirs, function(d) {
    obj <- readRDS(file.path(d, "full.rds"))
    if (is.data.frame(obj)) obj else obj$payload$replicates
  })
  all_reps <- dplyr::bind_rows(parts)
}

training <- all_reps[
  all_reps$design_layer == "core" & all_reps$status == "completed",
  , drop = FALSE
]
message(sprintf("all rows: %d; core completed: %d", nrow(all_reps), nrow(training)))
print(table(training$truth_class))

core_ids <- unique(as.character(training$scenario_id))
env$binary_proportion_assert_publication_ready(
  all_reps[all_reps$design_layer == "core", , drop = FALSE],
  min_quota = 100L,
  max_failure_rate = 0.05,
  required_scenarios = core_ids
)
acct <- env$binary_proportion_publication_accounting(
  all_reps[all_reps$design_layer == "core", , drop = FALSE], min_quota = 100L
)
message("core publication accounting OK")

# Restrict the fitter input to significant completed rows (the SAP screens
# significant-only).  The fitter filters again internally, but pre-filtering
# keeps the training artifact aligned with the screen.
sig_training <- training[
  !is.na(training$screening_conclusion) &
    training$screening_conclusion == "significant",
  , drop = FALSE
]

# --- Collect replication draws (Track D') ---------------------------------
# One primary-test-only replication per completed significant row, dedicated
# seed stream (master 20260808).  This feeds the Track D' curve and is collected
# regardless of whether Track A'' proceeds.
scenarios <- env$binary_proportion_scenarios()
scn_by_id <- setNames(split(scenarios, scenarios$scenario_id),
                      unique(scenarios$scenario_id))
replication_rows <- vector("list", nrow(sig_training))
for (i in seq_len(nrow(sig_training))) {
  row <- sig_training[i, ]
  scn_id <- as.character(row$scenario_id[[1L]])
  rep_id <- as.integer(row$replicate_id[[1L]])
  scn <- scn_by_id[[scn_id]]
  if (is.null(scn)) next
  draw <- env$prop_replication_draw(scn, replicate_id = rep_id)
  replication_rows[[i]] <- data.frame(
    scenario_id = scn_id,
    replicate_id = rep_id,
    overall_score = as.numeric(row$overall_score[[1L]]),
    original_p = as.numeric(row$original_p[[1L]]),
    replication_significant = as.integer(draw$significant),
    replication_p = draw$p,
    replication_seed = draw$replication_seed,
    stringsAsFactors = FALSE
  )
}
replication_data <- dplyr::bind_rows(replication_rows)
message(sprintf("replication draws collected: %d", nrow(replication_data)))

# --- Fit Track A'' + Track D' on training only ----------------------------
scenario_hash <- env$calibration_scenario_hash(scenarios)
training_hash <- env$calibration_hash_object(sig_training)

result <- env$analyse_binary_proportion_calibration(
  training = sig_training,
  validation = NULL,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  replication_data = replication_data
)

message(sprintf("status=%s frozen=%s", result$status, result$frozen$status))
if (!is.null(result$frozen$cutoff)) {
  message(sprintf("cutoff=%s", result$frozen$cutoff))
}
message(sprintf("candidate_hash=%s", result$frozen$candidate_hash %||% NA_character_))
message(sprintf("reason=%s",
                paste(result$frozen$reason %||% result$track_a$reason %||% NA,
                      collapse = ",")))

# Persist compact diagnostics (raw training stays gitignored).
sum_dir <- file.path(study, "artifacts/summaries")
art_dir <- file.path(study, "artifacts/raw/training")
dir.create(sum_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(sig_training, file.path(art_dir, "completed_training_core.rds"), version = 2)
saveRDS(all_reps, file.path(art_dir, "audit_training.rds"), version = 2)
saveRDS(replication_data, file.path(art_dir, "training_replication_draws.rds"), version = 2)
saveRDS(result, file.path(sum_dir, "training-analysis.rds"), version = 2)
saveRDS(result$frozen, file.path(sum_dir, "candidate.rds"), version = 2)
saveRDS(result$track_a, file.path(sum_dir, "training-fit.rds"), version = 2)
saveRDS(result$track_d, file.path(sum_dir, "training-replication-curve.rds"), version = 2)

diag <- list(
  status = result$status,
  frozen_status = result$frozen$status,
  cutoff = result$frozen$cutoff,
  candidate_hash = result$frozen$candidate_hash,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  n_training = nrow(sig_training),
  n_audit = nrow(all_reps),
  n_replication_draws = nrow(replication_data),
  replication_curve_status = result$track_d$status,
  replication_intercept = result$track_d$intercept,
  replication_slope = result$track_d$slope,
  reason = result$frozen$reason %||% result$track_a$reason,
  workers = 4L,
  code_commit = system("git rev-parse HEAD", intern = TRUE),
  null_significant = as.integer(sum(sig_training$truth_class == "null")),
  clear_significant = as.integer(sum(sig_training$truth_class == "clear"))
)
jsonlite::write_json(
  diag, file.path(sum_dir, "candidate-diagnostics.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null"
)
utils::write.csv(acct$by_scenario, file.path(sum_dir, "training-occupancy.csv"),
                 row.names = FALSE)

manifest <- list(
  phase = "training",
  workers = 4L,
  code_commit = diag$code_commit,
  scenario_manifest_hash = scenario_hash,
  training_artifact_hash = training_hash,
  n_core_significant = nrow(sig_training),
  n_audit = nrow(all_reps),
  n_replication_draws = nrow(replication_data),
  candidate_status = result$frozen$status,
  candidate_hash = result$frozen$candidate_hash,
  cutoff = result$frozen$cutoff,
  validation_accessed = FALSE
)
saveRDS(manifest, file.path(sum_dir, "training-manifest.rds"), version = 2)
jsonlite::write_json(
  manifest, file.path(sum_dir, "training-manifest.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null"
)

message("DONE")
print(diag[c("status", "frozen_status", "cutoff", "candidate_hash",
             "null_significant", "clear_significant", "reason")])
