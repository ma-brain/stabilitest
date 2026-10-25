#!/usr/bin/env Rscript

# Build compact R Journal evidence directly from the immutable relaxed-gates
# Welch publication (study 2026-3). No thresholds or study results are
# recomputed here; the p-value comparator row is the one the frozen SAP
# defines (closest training false reassurance to the score candidate).

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
  "published", "2026-3"
)
.verdict_path <- file.path(.published_dir, "VERDICT.json")
.output_dir <- file.path(.project_root, "manuscript", "rjournal", "artifacts",
                         "welch-calibration-2026-3")

t_start <- Sys.time()
verdict <- readRDS(file.path(.published_dir, "VERDICT.rds"))
candidate <- readRDS(file.path(.published_dir, "candidate.rds"))
occupancy <- utils::read.csv(file.path(.published_dir, "training-occupancy.csv"),
                             stringsAsFactors = FALSE)
failures <- utils::read.csv(file.path(.published_dir, "training-failures.csv"),
                            stringsAsFactors = FALSE)
stopifnot(identical(candidate$candidate_hash, verdict$candidate_hash))

m <- candidate$metrics
grid <- candidate$p_comparator$grid
nearest <- which.min(abs(grid$false_reassurance - m$false_reassurance))
p_only <- grid[nearest, , drop = FALSE]
archetypes <- names(m$archetype_false_reassurance)

summary <- data.frame(
  study_version = verdict$study_version,
  registry_version = verdict$registry_version,
  status = verdict$status,
  reason = verdict$reason,
  mapping_type = candidate$mapping_type,
  cutoff_stable = candidate$cutoffs[[1L]],
  candidate_hash = verdict$candidate_hash,
  held_out_opened = verdict$held_out_opened,
  validation_refit = verdict$validation_refit,
  training_n = verdict$training_n,
  required_cells = nrow(occupancy),
  completed = sum(occupancy$completed),
  failures = nrow(failures),
  false_reassurance = m$false_reassurance,
  false_reassurance_wilson_upper = m$false_reassurance_wilson_upper,
  false_reassurance_cluster_upper = m$false_reassurance_cluster_upper,
  false_reassurance_conservative_upper = m$false_reassurance_conservative_upper,
  clear_identification = m$clear_identification,
  clear_identification_wilson_lower = m$clear_identification_wilson_lower,
  clear_identification_cluster_lower = m$clear_identification_cluster_lower,
  clear_identification_conservative_lower =
    m$clear_identification_conservative_lower,
  band_occupancy = m$band_occupancy,
  worst_archetype_false_reassurance = max(m$archetype_false_reassurance),
  worst_archetype = archetypes[which.max(m$archetype_false_reassurance)],
  comparator_status = candidate$p_comparator$status,
  p_only_q = p_only$label,
  p_only_false_reassurance = p_only$false_reassurance,
  p_only_clear_identification = p_only$clear_identification,
  p_only_feasible_grid_points = sum(grid$feasible),
  p_only_grid_points = nrow(grid),
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
  artifact_role = "prospective_welch_2026_3_calibration_summary",
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

print(t(summary))
message("Welch 2026-3 calibration summary written to ", .output_dir)
