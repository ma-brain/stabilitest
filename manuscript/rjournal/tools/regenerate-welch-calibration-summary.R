#!/usr/bin/env Rscript

# Build compact R Journal evidence directly from the immutable prospective
# Welch publication. No thresholds or study results are recomputed here.

suppressPackageStartupMessages(library(stabilitest))

.script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE),
                             value = TRUE)[[1L]]),
  mustWork = TRUE
)
.project_root <- normalizePath(file.path(dirname(.script_path), "..", "..", ".."),
                               mustWork = TRUE)
.published_dir <- file.path(
  .project_root, "manuscript", "calibration", "studies", "welch_unpaired",
  "published"
)
.verdict_path <- file.path(.published_dir, "VERDICT.json")
.output_dir <- file.path(.project_root, "manuscript", "rjournal", "artifacts",
                         "welch-calibration")

t_start <- Sys.time()
verdict <- readRDS(file.path(.published_dir, "VERDICT.rds"))
occupancy <- utils::read.csv(file.path(.published_dir, "training-occupancy.csv"),
                             stringsAsFactors = FALSE)
failures <- utils::read.csv(file.path(.published_dir, "training-failures.csv"),
                            stringsAsFactors = FALSE)
historical <- verdict$historical_55_70

summary <- data.frame(
  study_version = verdict$study_version,
  registry_version = verdict$registry_version,
  status = verdict$status,
  reason = verdict$reason,
  mapping_type = verdict$mapping_type,
  cutoff_fragile = verdict$cutoffs[[1L]],
  cutoff_robust = verdict$cutoffs[[2L]],
  candidate_hash = verdict$candidate_hash,
  held_out_opened = verdict$held_out_opened,
  validation_refit = verdict$validation_refit,
  training_n = verdict$training_n,
  required_cells = nrow(occupancy),
  completed = sum(occupancy$completed),
  failures = nrow(failures),
  historical_false_reassurance = historical$false_reassurance,
  historical_false_reassurance_wilson_upper =
    historical$false_reassurance_wilson_upper,
  historical_false_reassurance_cluster_upper =
    historical$false_reassurance_cluster_upper,
  historical_false_reassurance_conservative_upper =
    historical$false_reassurance_conservative_upper,
  historical_clear_identification = historical$clear_identification,
  historical_clear_identification_wilson_lower =
    historical$clear_identification_wilson_lower,
  historical_clear_identification_cluster_lower =
    historical$clear_identification_cluster_lower,
  historical_clear_identification_conservative_lower =
    historical$clear_identification_conservative_lower,
  historical_balanced_ordinal_accuracy = historical$balanced_ordinal_accuracy,
  historical_minimum_band_occupancy = historical$minimum_band_occupancy,
  historical_pooled_ordering_ok = historical$pooled_ordering_ok,
  historical_material_reversal = historical$material_reversal,
  stringsAsFactors = FALSE
)

dir.create(.output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(summary, file.path(.output_dir, "welch-calibration-summary.rds"), version = 3)
utils::write.csv(summary,
                 file.path(.output_dir, "welch-calibration-summary.csv"),
                 row.names = FALSE, na = "")

registry <- stabilitest:::load_calibration_registry()
welch <- registry[registry$calibration_unit == "welch_unpaired", , drop = FALSE]
t_end <- Sys.time()
manifest <- list(
  artifact_role = "prospective_welch_calibration_summary",
  generated_at = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
  runtime_seconds = as.numeric(difftime(t_end, t_start, units = "secs")),
  package_version = as.character(utils::packageVersion("stabilitest")),
  git_commit = system2("git", c("-C", .project_root, "rev-parse", "HEAD"),
                       stdout = TRUE),
  r_version = R.version.string,
  script_path = file.path("manuscript", "rjournal", "tools",
                          basename(.script_path)),
  script_sha256 = digest::digest(file = .script_path, algo = "sha256"),
  seed = "not-applicable-frozen-summary",
  source_study_verdict_hash = digest::digest(file = .verdict_path,
                                             algo = "sha256"),
  welch_calibration_status = welch$status,
  welch_label_emitted = identical(welch$status, "validated_method_specific"),
  candidate_hash = verdict$candidate_hash,
  held_out_opened = verdict$held_out_opened,
  validation_refit = verdict$validation_refit
)
jsonlite::write_json(
  manifest, file.path(.output_dir, "manifest.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15
)

print(summary)
message("Welch calibration summary written to ", .output_dir)
