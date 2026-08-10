#!/usr/bin/env Rscript

# Freeze the production training decision and all inputs before validation.

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "welch_unpaired"),
    mustWork = TRUE
  )
}

.sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

study_root <- .study_root()
project_root <- normalizePath(file.path(study_root, "..", "..", "..", ".."),
                              mustWork = TRUE)
summary_dir <- file.path(study_root, "artifacts", "summaries")
training_dir <- file.path(study_root, "artifacts", "training")
validation_dir <- file.path(study_root, "artifacts", "validation")
if (dir.exists(validation_dir)) {
  stop("candidate freeze requires an unopened validation directory",
       call. = FALSE)
}

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_welch_study(project_root = project_root, envir = env)

analysis_path <- file.path(summary_dir, "training-analysis.rds")
if (!file.exists(analysis_path)) {
  stop("training analysis is missing; run fit_training_candidate.R first",
       call. = FALSE)
}
fit <- readRDS(analysis_path)

input_paths <- c(
  sap = file.path(study_root, "CALIBRATION_SAP.md"),
  resolved_scenarios = file.path(
    study_root, "artifacts", "design", "resolved-scenarios.csv"
  ),
  scenario_manifest = file.path(
    study_root, "artifacts", "design", "design-manifest.json"
  ),
  generator = file.path(study_root, "R", "generator.R"),
  adapter = file.path(study_root, "R", "adapter.R"),
  thresholds = file.path(study_root, "R", "thresholds.R"),
  training_completed = file.path(training_dir, "completed.rds"),
  training_ledger = file.path(training_dir, "screening-ledger.rds"),
  training_runner_manifest = file.path(training_dir, "manifest.json")
)
missing <- input_paths[!file.exists(input_paths)]
if (length(missing)) {
  stop(sprintf("candidate hash input missing: %s",
               paste(names(missing), collapse = ", ")), call. = FALSE)
}
input_hashes <- vapply(input_paths, .sha256_file, character(1))
package_version <- unname(read.dcf(file.path(project_root, "DESCRIPTION"))[
  1L, "Version"
])
context <- list(
  study_version = "prospective Welch study 2026-2",
  gates = env$WELCH_GATE_CONSTANTS,
  scenario_manifest_hash = input_hashes[["scenario_manifest"]],
  weights = env$WELCH_FROZEN_WEIGHTS,
  sap_hash = input_hashes[["sap"]],
  resolved_scenarios_hash = input_hashes[["resolved_scenarios"]],
  generator_hash = input_hashes[["generator"]],
  adapter_hash = input_hashes[["adapter"]],
  thresholds_hash = input_hashes[["thresholds"]],
  training_data_hash = digest::digest(
    input_hashes[c(
      "training_completed", "training_ledger", "training_runner_manifest"
    )],
    algo = "sha256", serialize = TRUE
  ),
  package_version = package_version
)
frozen <- env$freeze_welch_candidate(fit, context)
if (!identical(
  frozen$candidate_hash,
  env$recompute_welch_candidate_hash(frozen)
)) {
  stop("fresh candidate hash calculation does not match", call. = FALSE)
}

dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(frozen, file.path(summary_dir, "candidate.rds"), version = 3)
writeLines(frozen$candidate_hash,
           file.path(summary_dir, "candidate-hash.txt"), useBytes = TRUE)

compact_metrics <- function(metrics) {
  if (is.null(metrics)) return(NULL)
  metrics[setdiff(
    names(metrics),
    c("bands", "archetype_ordering", "false_reassurance_cluster_draws",
      "clear_identification_cluster_draws")
  )]
}
diagnostics <- list(
  status = frozen$status,
  reason = frozen$reason,
  mapping_type = frozen$mapping_type,
  cutoffs = frozen$cutoffs,
  candidate_hash = frozen$candidate_hash,
  metrics = compact_metrics(frozen$metrics),
  historical_55_70 = compact_metrics(frozen$historical_55_70),
  fit_runtime_seconds = frozen$fit_runtime_seconds,
  training_n = frozen$training_n,
  training_truth_counts = frozen$training_truth_counts,
  context = context,
  held_out_opened = FALSE,
  validation_refit = FALSE
)
jsonlite::write_json(
  diagnostics, file.path(summary_dir, "candidate-diagnostics.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15
)

occupancy <- utils::read.csv(file.path(summary_dir, "training-occupancy.csv"),
                             stringsAsFactors = FALSE)
failures <- utils::read.csv(file.path(summary_dir, "training-failures.csv"),
                            stringsAsFactors = FALSE)
runner_manifest <- jsonlite::read_json(
  file.path(training_dir, "manifest.json"), simplifyVector = TRUE
)
manifest <- list(
  phase = "production_training_freeze",
  study_version = context$study_version,
  package_version = package_version,
  code_commit_before_freeze = system2(
    "git", c("-C", project_root, "rev-parse", "HEAD"), stdout = TRUE
  ),
  candidate_status = frozen$status,
  reason = frozen$reason,
  mapping_type = frozen$mapping_type,
  cutoffs = frozen$cutoffs,
  candidate_hash = frozen$candidate_hash,
  training_completed = sum(occupancy$completed),
  training_attempted = sum(occupancy$attempted),
  training_failures = nrow(failures),
  required_cells = nrow(occupancy),
  occupancy_pass = all(occupancy$occupancy_pass),
  runner_runtime_seconds = runner_manifest$runtime_seconds,
  fit_runtime_seconds = frozen$fit_runtime_seconds,
  validation_accessed = FALSE,
  held_out_opened = FALSE,
  validation_refit = FALSE,
  input_sha256 = as.list(input_hashes)
)
jsonlite::write_json(
  manifest, file.path(summary_dir, "training-manifest.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15
)

message(sprintf(
  "Frozen training decision status=%s mapping=%s cutoffs=%s hash=%s",
  frozen$status, frozen$mapping_type, paste(frozen$cutoffs, collapse = "/"),
  frozen$candidate_hash
))
