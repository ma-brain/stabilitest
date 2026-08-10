#!/usr/bin/env Rscript

# Fit the frozen hierarchy using production training scores only.

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

study_root <- .study_root()
project_root <- normalizePath(file.path(study_root, "..", "..", "..", ".."),
                              mustWork = TRUE)
validation_dir <- file.path(study_root, "artifacts", "validation")
if (dir.exists(validation_dir)) {
  stop("training fitter refuses to run after validation output exists",
       call. = FALSE)
}

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_welch_study(project_root = project_root, envir = env)

training_dir <- file.path(study_root, "artifacts", "training")
summary_dir <- file.path(study_root, "artifacts", "summaries")
required <- file.path(
  training_dir,
  c("completed.rds", "failures.rds", "screening-ledger.rds", "occupancy.csv",
    "manifest.json")
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop(sprintf("production training artifact missing: %s",
               paste(basename(missing), collapse = ", ")), call. = FALSE)
}

completed <- readRDS(file.path(training_dir, "completed.rds"))
failures <- readRDS(file.path(training_dir, "failures.rds"))
ledger <- readRDS(file.path(training_dir, "screening-ledger.rds"))
occupancy <- utils::read.csv(file.path(training_dir, "occupancy.csv"),
                             stringsAsFactors = FALSE)
runner_manifest <- jsonlite::read_json(
  file.path(training_dir, "manifest.json"), simplifyVector = TRUE
)

if (anyDuplicated(completed[c("scenario_id", "replicate_id")])) {
  stop("training completed scores contain duplicate replicate IDs", call. = FALSE)
}
if (anyDuplicated(ledger[c("scenario_id", "replicate_id")])) {
  stop("training screening ledger contains duplicate replicate IDs", call. = FALSE)
}
if (nrow(occupancy) != 18L || !all(occupancy$quota_met) ||
    !all(occupancy$occupancy_pass) || any(occupancy$pending != 0L)) {
  stop("production training occupancy gate failed", call. = FALSE)
}
if (any(occupancy$attempted != occupancy$screened + occupancy$screening_failed) ||
    any(occupancy$screened != occupancy$screened_significant + occupancy$excluded) ||
    any(occupancy$screened_significant !=
        occupancy$completed + occupancy$analysis_failed)) {
  stop("production training accounting does not reconcile", call. = FALSE)
}
if (!all(completed$status == "completed") ||
    !all(completed$original_significant) ||
    !all(completed$design_layer == "training")) {
  stop("training fitter received ineligible score rows", call. = FALSE)
}
if (!identical(runner_manifest$layer, "training") ||
    !identical(runner_manifest$mode, "production") ||
    !identical(runner_manifest$silent_drops, 0L)) {
  stop("production training runner manifest failed authority checks",
       call. = FALSE)
}

message(sprintf(
  "Fitting %d completed training scores across %d required cells",
  nrow(completed), nrow(occupancy)
))
started <- Sys.time()
fit <- env$fit_welch_candidate(
  completed,
  cluster_B = env$WELCH_GATE_CONSTANTS$cluster_bootstrap_B,
  cluster_seed = env$WELCH_GATE_CONSTANTS$cluster_bootstrap_seed
)
fit$fit_runtime_seconds <- unname(as.numeric(difftime(Sys.time(), started,
                                                       units = "secs")))
fit$training_n <- nrow(completed)
fit$training_truth_counts <- as.list(table(completed$truth))
fit$validation_accessed <- FALSE

dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(fit, file.path(summary_dir, "training-analysis.rds"), version = 3)
utils::write.csv(occupancy, file.path(summary_dir, "training-occupancy.csv"),
                 row.names = FALSE)
if (nrow(failures)) {
  utils::write.csv(failures, file.path(summary_dir, "training-failures.csv"),
                   row.names = FALSE, na = "")
} else {
  utils::write.csv(
    data.frame(
      scenario_id = character(), replicate_id = integer(),
      failure_stage = character(), failure_class = character(),
      failure_message = character(), stringsAsFactors = FALSE
    ),
    file.path(summary_dir, "training-failures.csv"), row.names = FALSE
  )
}

message(sprintf(
  "Training fit status=%s mapping=%s cutoffs=%s runtime=%.1fs",
  fit$status, fit$mapping_type, paste(fit$cutoffs, collapse = "/"),
  fit$fit_runtime_seconds
))
