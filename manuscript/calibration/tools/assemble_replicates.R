#!/usr/bin/env Rscript

# Assemble completed calibration replicates from per-scenario checkpoints into
# flat training/validation tables for analyse_calibration() / freeze+publish.
#
# Per-family Task 15 runs overwrite run-results.rds, but atomic checkpoints
# under artifacts/raw/{training,validation}/checkpoints/full/checkpoints/
# survive.  This tool stacks those checkpoints, clamps unit-interval p-values,
# drops failed rows, and writes compact replicate RDS files.
#
# Usage (from repo root):
#   Rscript manuscript/calibration/tools/assemble_replicates.R \
#     [training_out.rds] [validation_out.rds]

args <- commandArgs(trailingOnly = TRUE)
training_out <- if (length(args) >= 1L && nzchar(args[[1]])) args[[1]] else
  "manuscript/calibration/artifacts/raw/assembled/training-replicates.rds"
validation_out <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else
  "manuscript/calibration/artifacts/raw/assembled/validation-replicates.rds"

root <- normalizePath(".", mustWork = TRUE)
loader <- file.path(root, "manuscript", "calibration", "R", "load_calibration.R")
env <- new.env(parent = globalenv())
sys.source(loader, envir = env)
env$load_calibration(project_root = root, envir = env)

collect_split <- function(split) {
  ck_root <- file.path(
    root, "manuscript", "calibration", "artifacts", "raw", split,
    "checkpoints", "full", "checkpoints"
  )
  if (!dir.exists(ck_root)) {
    stop(sprintf("checkpoint root missing for %s: %s", split, ck_root), call. = FALSE)
  }
  dirs <- list.dirs(ck_root, full.names = TRUE, recursive = FALSE)
  rows <- list()
  for (d in dirs) {
    f <- file.path(d, "full.rds")
    if (!file.exists(f)) next
    payload <- readRDS(f)
    replicates <- payload$payload$replicates
    if (!is.data.frame(replicates) || !nrow(replicates)) next
    completed <- !is.na(replicates$status) & replicates$status == "completed"
    if (!any(completed)) next
    replicates$original_p[completed] <- pmin(1, pmax(0, as.numeric(replicates$original_p[completed])))
    replicates$effective_p[completed] <- pmin(1, pmax(0, as.numeric(replicates$effective_p[completed])))
    env$validate_calibration_replicates(replicates)
    keep <- replicates[completed, , drop = FALSE]
    rows[[length(rows) + 1L]] <- keep
  }
  if (!length(rows)) {
    stop(sprintf("no completed replicates found under %s", ck_root), call. = FALSE)
  }
  out <- dplyr::bind_rows(rows)
  # Stable order for deterministic hashes downstream.
  out <- out[order(out$analysis_family, out$scenario_id, out$replicate_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

training <- collect_split("training")
validation <- collect_split("validation")

dir.create(dirname(training_out), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(validation_out), recursive = TRUE, showWarnings = FALSE)
saveRDS(training, training_out, version = 2)
saveRDS(validation, validation_out, version = 2)

summarise_split <- function(x, label) {
  cat(sprintf(
    "%s: %d completed replicates across %d scenarios / %d families\n",
    label, nrow(x), length(unique(x$scenario_id)), length(unique(x$analysis_family))
  ))
  print(table(x$analysis_family, x$design_layer))
}

cat("Wrote:\n  ", training_out, "\n  ", validation_out, "\n", sep = "")
summarise_split(training, "training")
summarise_split(validation, "validation")
