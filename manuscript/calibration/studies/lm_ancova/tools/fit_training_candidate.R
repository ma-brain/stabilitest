#!/usr/bin/env Rscript

# Assemble completed training checkpoints and fit/freeze the ANCOVA candidate.
# Intentionally avoids loading the monolithic run-results.rds.

`%||%` <- function(left, right) if (is.null(left)) right else left

study <- normalizePath("manuscript/calibration/studies/lm_ancova", mustWork = TRUE)
project <- normalizePath(".", mustWork = TRUE)
env <- new.env(parent = globalenv())
sys.source(file.path(study, "R", "load_study.R"), envir = env)
env$load_lm_ancova_study(project_root = project, envir = env)
sys.source(file.path(study, "analyse_calibration.R"), envir = env)
sys.source(file.path(study, "tools", "assemble_replicates.R"), envir = env)

root <- file.path(study, "artifacts/raw/training/checkpoints/full/checkpoints")
dirs <- list.dirs(root, recursive = FALSE)
parts <- lapply(dirs, function(d) {
  obj <- readRDS(file.path(d, "full.rds"))
  if (is.data.frame(obj)) obj else obj$payload$replicates
})
all_reps <- dplyr::bind_rows(parts)
training <- all_reps[
  all_reps$design_layer == "core" & all_reps$status == "completed",
  ,
  drop = FALSE
]
message(sprintf("all rows: %d; core completed: %d", nrow(all_reps), nrow(training)))
print(table(training$truth_class))

core_ids <- unique(as.character(training$scenario_id))
env$lm_ancova_assert_publication_ready(
  all_reps[all_reps$design_layer == "core", , drop = FALSE],
  min_quota = 100L,
  max_failure_rate = 0.05,
  required_scenarios = core_ids
)
acct <- env$lm_ancova_publication_accounting(all_reps, min_quota = 100L)
message("core publication accounting OK")

scenarios <- env$lm_ancova_scenarios()
scenario_hash <- env$calibration_scenario_hash(scenarios)
training_hash <- env$calibration_hash_object(training)

result <- env$analyse_lm_ancova_calibration(
  training = training,
  validation = NULL,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash
)

message(sprintf("status=%s frozen=%s", result$status, result$frozen$status))
if (!is.null(result$frozen$cutoffs)) {
  message(sprintf("cutoffs=%s", paste(result$frozen$cutoffs, collapse = "/")))
}
message(sprintf("candidate_hash=%s", result$frozen$candidate_hash %||% NA_character_))
message(sprintf("reason=%s", paste(result$frozen$reason %||% result$fit$reason %||% NA, collapse = ",")))

sum_dir <- file.path(study, "artifacts/summaries")
art_dir <- file.path(study, "artifacts/raw/training")
dir.create(sum_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(training, file.path(art_dir, "completed_training_core.rds"), version = 2)
saveRDS(all_reps, file.path(art_dir, "audit_training.rds"), version = 2)
saveRDS(result, file.path(sum_dir, "training-analysis.rds"), version = 2)
saveRDS(result$frozen, file.path(sum_dir, "candidate.rds"), version = 2)
saveRDS(result$fit, file.path(sum_dir, "training-fit.rds"), version = 2)

diag <- list(
  status = result$status,
  frozen_status = result$frozen$status,
  cutoffs = result$frozen$cutoffs,
  candidate_hash = result$frozen$candidate_hash,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  n_training = nrow(training),
  n_audit = nrow(all_reps),
  reason = result$frozen$reason %||% result$fit$reason,
  workers = 4L,
  code_commit = system("git rev-parse HEAD", intern = TRUE)
)
if (is.list(result$fit)) {
  keep <- intersect(names(result$fit), c(
    "status", "reason", "cutoffs", "metrics", "feasible", "n_candidates", "selected"
  ))
  diag$fit <- result$fit[keep]
}
if (is.list(result$frozen)) {
  keep <- intersect(names(result$frozen), c(
    "status", "reason", "cutoffs", "candidate_hash", "metrics",
    "scenario_manifest_hash", "training_manifest_hash"
  ))
  diag$frozen <- result$frozen[keep]
}
jsonlite::write_json(
  diag,
  file.path(sum_dir, "candidate-diagnostics.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
utils::write.csv(acct$by_scenario, file.path(sum_dir, "training-failures.csv"),
                 row.names = FALSE)

manifest <- list(
  phase = "training",
  workers = 4L,
  code_commit = diag$code_commit,
  scenario_manifest_hash = scenario_hash,
  training_artifact_hash = training_hash,
  n_core_completed = nrow(training),
  n_audit = nrow(all_reps),
  candidate_status = result$frozen$status,
  candidate_hash = result$frozen$candidate_hash,
  cutoffs = result$frozen$cutoffs,
  validation_accessed = FALSE,
  assembled_from = "checkpoints/full",
  note = paste(
    "Parent process died after writing run-results.rds;",
    "recovery assembled from complete scenario checkpoints."
  )
)
saveRDS(manifest, file.path(sum_dir, "training-manifest.rds"), version = 2)
jsonlite::write_json(
  manifest,
  file.path(sum_dir, "training-manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)

message("DONE")
print(diag[c("status", "frozen_status", "cutoffs", "candidate_hash", "reason")])
