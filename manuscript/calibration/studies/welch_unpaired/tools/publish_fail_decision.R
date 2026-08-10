#!/usr/bin/env Rscript

# Publish the final no-candidate Welch verdict without opening held-out data.

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
validation_dir <- file.path(study_root, "artifacts", "validation")
published_dir <- file.path(study_root, "published")

if (dir.exists(validation_dir)) {
  stop("fail publication requires validation to remain unopened", call. = FALSE)
}
if (dir.exists(published_dir) && length(list.files(published_dir, all.files = TRUE,
                                                   no.. = TRUE))) {
  stop("published Welch decision already exists", call. = FALSE)
}

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_welch_study(project_root = project_root, envir = env)

candidate_path <- file.path(summary_dir, "candidate.rds")
candidate_hash_path <- file.path(summary_dir, "candidate-hash.txt")
if (!file.exists(candidate_path) || !file.exists(candidate_hash_path)) {
  stop("committed training freeze is missing", call. = FALSE)
}
candidate <- readRDS(candidate_path)
recorded_hash <- trimws(readLines(candidate_hash_path, warn = FALSE)[[1L]])
fresh_hash <- env$recompute_welch_candidate_hash(candidate)
if (!identical(candidate$candidate_hash, recorded_hash) ||
    !identical(candidate$candidate_hash, fresh_hash)) {
  stop("frozen candidate hash mismatch", call. = FALSE)
}
if (!identical(candidate$status, "no_feasible_thresholds")) {
  stop("fail-only publication requires no_feasible_thresholds", call. = FALSE)
}
if (isTRUE(candidate$held_out_opened) || isTRUE(candidate$validation_refit)) {
  stop("frozen no-candidate decision records held-out access", call. = FALSE)
}

registry_path <- file.path(project_root, "inst", "extdata",
                           "calibration-registry.csv")
registry <- utils::read.csv(
  registry_path, stringsAsFactors = FALSE, check.names = FALSE,
  na.strings = c("", "NA")
)
welch <- registry$calibration_unit == "welch_unpaired"
if (sum(welch) != 1L) stop("active registry has no unique Welch row", call. = FALSE)
registry$status[welch] <- "uncalibrated"
registry$cutoff_fragile[welch] <- NA_real_
registry$cutoff_robust[welch] <- NA_real_
registry$version[welch] <- "welch-2026-2"
registry$source[welch] <-
  "manuscript/calibration/studies/welch_unpaired/published"
registry$supported_conditions[welch] <- paste0(
  "no_feasible_thresholds; candidate_hash:", candidate$candidate_hash,
  "; held-out not opened; categorical bands suppressed; ",
  "numeric scores and component metrics only"
)

verdict <- list(
  status = "uncalibrated",
  reason = "no_feasible_thresholds",
  study_version = "prospective Welch study 2026-2",
  registry_version = "welch-2026-2",
  mapping_type = "none",
  cutoffs = c(NA_real_, NA_real_),
  candidate_hash = candidate$candidate_hash,
  training_n = candidate$training_n,
  training_truth_counts = candidate$training_truth_counts,
  historical_55_70 = env$compact_welch_metrics(candidate$historical_55_70),
  held_out_opened = FALSE,
  validation_refit = FALSE,
  training_freeze_commit = system2(
    "git", c("-C", project_root, "rev-parse", "HEAD"), stdout = TRUE
  )
)

dir.create(published_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(verdict, file.path(published_dir, "VERDICT.rds"), version = 3)
jsonlite::write_json(
  verdict, file.path(published_dir, "VERDICT.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = 15
)
utils::write.csv(registry, file.path(published_dir, "registry.csv"),
                 row.names = FALSE, na = "")
saveRDS(registry, file.path(published_dir, "registry.rds"), version = 3)

evidence <- c(
  "candidate.rds", "candidate-diagnostics.json", "training-occupancy.csv",
  "training-failures.csv", "training-manifest.json"
)
for (name in evidence) {
  source <- file.path(summary_dir, name)
  if (!file.copy(source, file.path(published_dir, name), overwrite = FALSE)) {
    stop(sprintf("failed to publish frozen evidence: %s", name), call. = FALSE)
  }
}

hash_names <- c("VERDICT.json", "VERDICT.rds", "registry.csv", "registry.rds",
                evidence)
hashes <- vapply(file.path(published_dir, hash_names), .sha256_file, character(1))
writeLines(
  sprintf("%s  %s", unname(hashes), hash_names),
  file.path(published_dir, "output-hashes.txt"), useBytes = TRUE
)

message(sprintf(
  "Published fail-closed Welch verdict hash=%s held_out_opened=FALSE",
  candidate$candidate_hash
))
