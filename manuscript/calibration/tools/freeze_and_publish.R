#!/usr/bin/env Rscript

# Assemble (if needed) → fit → freeze → held-out evaluate once → write published/.
#
# Usage (from repo root):
#   Rscript manuscript/calibration/tools/freeze_and_publish.R
#   Rscript manuscript/calibration/tools/freeze_and_publish.R --reassemble
#
# Does not commit. Raw checkpoints stay gitignored; only compact files under
# manuscript/calibration/published/ are intended for git.

args <- commandArgs(trailingOnly = TRUE)
reassemble <- "--reassemble" %in% args

root <- normalizePath(".", mustWork = TRUE)
loader <- file.path(root, "manuscript", "calibration", "R", "load_calibration.R")
env <- new.env(parent = globalenv())
sys.source(loader, envir = env)
env$load_calibration(project_root = root, envir = env)
sys.source(file.path(root, "manuscript", "calibration", "analyse_calibration.R"), envir = env)

assembled_dir <- file.path(root, "manuscript", "calibration", "artifacts", "raw", "assembled")
training_path <- file.path(assembled_dir, "training-replicates.rds")
validation_path <- file.path(assembled_dir, "validation-replicates.rds")

if (reassemble || !file.exists(training_path) || !file.exists(validation_path)) {
  cat("Assembling checkpoints...\n")
  system2("Rscript", c(file.path(root, "manuscript/calibration/tools/assemble_replicates.R"),
                       training_path, validation_path), stdout = "", stderr = "")
}

training <- readRDS(training_path)
validation <- readRDS(validation_path)
sc <- env$calibration_scenarios()
scenario_hash <- env$calibration_scenario_hash(sc)

mk_manifest <- function(split, validation_only, replicates) {
  list(
    artifact_kind = "calibration-publication-manifest",
    artifact_version = "calibration-publication-1",
    manifest_version = "calibration-1",
    status = "publication_run",
    mode = "full",
    split = split,
    options = list(
      mode = "full", phase = "all", engine = "all",
      validation_only = validation_only, split = split, workers = 6L, resume = TRUE
    ),
    scenario_manifest_hash = scenario_hash,
    scenario_count = nrow(sc),
    scenario_ids = sort(unique(as.character(replicates$scenario_id))),
    n_boot = 1000L,
    target_replicates = 500L,
    minimum_heldout_stratum = 100L,
    attempted_replicates = nrow(replicates),
    completed_replicates = sum(replicates$status == "completed", na.rm = TRUE),
    failed_replicates = sum(replicates$status != "completed", na.rm = TRUE),
    reduced_fixture = FALSE,
    unsupported = c(
      "binomial_stress_separation: complete separation forces universal screening failure (recorded unsupported)."
    )
  )
}

train_manifest <- mk_manifest("training", FALSE, training)
valid_manifest <- mk_manifest("validation", TRUE, validation)

out <- tempfile("calibration-publish-")
dir.create(out)
result <- env$calibration_analysis_from_files(
  training, validation, train_manifest, valid_manifest,
  output = out, minimum_stratum_n = 100L
)

train_manifest$candidate_hash <- result$candidate_hash
train_manifest$registry_hash <- result$registry_hash
train_manifest$validation_refit <- isTRUE(result$validation$refit)
valid_manifest$candidate_hash <- result$candidate_hash
valid_manifest$registry_hash <- result$registry_hash
valid_manifest$validation_refit <- isTRUE(result$validation$refit)

published <- file.path(root, "manuscript", "calibration", "published")
file.copy(file.path(out, "calibration-registry.csv"),
          file.path(published, "calibration-registry.csv"), overwrite = TRUE)
file.copy(file.path(out, "non-significant-registry.csv"),
          file.path(published, "non-significant-registry.csv"), overwrite = TRUE)
file.copy(file.path(out, "calibration-registry.rds"),
          file.path(published, "calibration-registry.rds"), overwrite = TRUE)
dput(train_manifest, file.path(published, "training-manifest.dput"))
dput(valid_manifest, file.path(published, "validation-manifest.dput"))

hash_targets <- c(
  "training-manifest.dput",
  "validation-manifest.dput",
  "calibration-registry.csv",
  "non-significant-registry.csv",
  "pilot-runtime-summary.csv",
  "pilot-failure-summary.csv"
)
hash_targets <- hash_targets[file.exists(file.path(published, hash_targets))]
lines <- vapply(hash_targets, function(f) {
  sprintf("%s  manuscript/calibration/published/%s",
          unname(tools::md5sum(file.path(published, f))), f)
}, character(1))
writeLines(lines, file.path(published, "output-hashes.txt"))

cat("candidate_hash:", result$candidate_hash, "\n")
cat("registry_hash:", result$registry_hash, "\n")
cat("refit:", result$validation$refit, "\n")
print(result$registry[, intersect(names(result$registry),
                                  c("analysis_family", "lower_cutoff", "upper_cutoff",
                                    "status", "status_public", "reason",
                                    "heldout_improvement", "material_difference"))])
cat("Wrote manuscript/calibration/published/\n")
