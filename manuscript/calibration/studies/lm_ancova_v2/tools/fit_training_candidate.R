#!/usr/bin/env Rscript

# Assemble completed training checkpoints and fit/freeze the ANCOVA v2
# two-band candidate (single integer L). Training only — held-out stays closed.
# Prefer per-scenario checkpoints over monolithic run-results.rds.

`%||%` <- function(left, right) if (is.null(left)) right else left

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "lm_ancova_v2"),
    mustWork = TRUE
  )
}

study <- .study_root()
project <- normalizePath(file.path(study, "..", "..", "..", ".."), mustWork = TRUE)

env <- new.env(parent = globalenv())
sys.source(file.path(study, "R", "load_study.R"), envir = env)
env$load_lm_ancova_v2_study(project_root = project, envir = env)
sys.source(file.path(study, "analyse_calibration.R"), envir = env)
sys.source(
  file.path(
    project, "manuscript", "calibration", "studies", "lm_ancova",
    "tools", "assemble_replicates.R"
  ),
  envir = env
)

checkpoint_root <- file.path(
  study, "artifacts", "raw", "training", "checkpoints", "full", "checkpoints"
)
if (!dir.exists(checkpoint_root)) {
  stop(sprintf("checkpoint root missing: %s", checkpoint_root), call. = FALSE)
}
dirs <- list.dirs(checkpoint_root, recursive = FALSE)
if (!length(dirs)) {
  stop(sprintf("no scenario checkpoints under %s", checkpoint_root), call. = FALSE)
}

parts <- lapply(dirs, function(d) {
  path <- file.path(d, "full.rds")
  if (!file.exists(path)) {
    stop(sprintf("missing checkpoint: %s", path), call. = FALSE)
  }
  obj <- readRDS(path)
  if (is.data.frame(obj)) {
    obj
  } else if (is.list(obj) && !is.null(obj$payload$replicates)) {
    obj$payload$replicates
  } else if (is.list(obj) && is.data.frame(obj$replicates)) {
    obj$replicates
  } else {
    stop(sprintf("unrecognized checkpoint payload: %s", path), call. = FALSE)
  }
})
all_reps <- dplyr::bind_rows(parts)

# Fit strata: core null+clear completed only (borderline / stress excluded).
# Significant filter is applied inside .ancova_v2_eligible_rows().
truth <- as.character(all_reps$truth_class)
training <- all_reps[
  all_reps$design_layer == "core" &
    all_reps$status == "completed" &
    truth %in% c("null", "clear"),
  ,
  drop = FALSE
]
message(sprintf(
  "all rows: %d; core null+clear completed: %d",
  nrow(all_reps), nrow(training)
))
print(table(training$truth_class))

if (any(as.character(training$design_layer) == "validation", na.rm = TRUE) ||
    any(grepl("validation", as.character(training$scenario_id), fixed = TRUE))) {
  stop("held-out validation rows present in training fit input", call. = FALSE)
}

core_fit_ids <- unique(as.character(training$scenario_id))
env$lm_ancova_assert_publication_ready(
  training,
  min_quota = 100L,
  max_failure_rate = 0.05,
  required_scenarios = core_fit_ids
)
acct <- env$lm_ancova_publication_accounting(all_reps, min_quota = 100L)
message("core publication accounting OK")

scenarios <- env$lm_ancova_v2_scenarios(clear_target_power = 0.90)
scenario_hash <- env$calibration_scenario_hash(scenarios)
training_hash <- env$calibration_hash_object(training)

result <- env$analyse_lm_ancova_v2_calibration(
  training = training,
  validation = NULL,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash
)

message(sprintf("status=%s frozen=%s", result$status, result$frozen$status))
if (!is.null(result$frozen$cutoff) && is.finite(result$frozen$cutoff)) {
  message(sprintf("cutoff L=%s", result$frozen$cutoff))
}
message(sprintf("candidate_hash=%s", result$frozen$candidate_hash %||% NA_character_))
message(sprintf(
  "reason=%s",
  paste(result$frozen$reason %||% result$fit$reason %||% NA, collapse = ",")
))

sum_dir <- file.path(study, "artifacts", "summaries")
art_dir <- file.path(study, "artifacts", "raw", "training")
dir.create(sum_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(art_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(training, file.path(art_dir, "completed_training_core.rds"), version = 2)
saveRDS(all_reps, file.path(art_dir, "audit_training.rds"), version = 2)
saveRDS(result, file.path(sum_dir, "training-analysis.rds"), version = 2)
saveRDS(result$frozen, file.path(sum_dir, "candidate.rds"), version = 2)
saveRDS(result$fit, file.path(sum_dir, "training-fit.rds"), version = 2)

# Compact grid diagnostics (FR/RI feasibility landscape).
if (is.data.frame(result$fit$grid) && nrow(result$fit$grid)) {
  grid <- result$fit$grid
  utils::write.csv(
    grid,
    file.path(sum_dir, "training-cutoff-grid.csv"),
    row.names = FALSE
  )
  # Count which training constraints fail across the L grid.
  constraint_rows <- list(
    data.frame(
      constraint = "false_reassurance_point",
      n_fail = sum(!(is.finite(grid$false_reassurance) &
                       grid$false_reassurance <= 0.05), na.rm = TRUE),
      stringsAsFactors = FALSE
    ),
    data.frame(
      constraint = "false_reassurance_upper",
      n_fail = sum(!(is.finite(grid$false_reassurance_upper) &
                       grid$false_reassurance_upper <= 0.10), na.rm = TRUE),
      stringsAsFactors = FALSE
    ),
    data.frame(
      constraint = "not_fragile_identification_point",
      n_fail = sum(!(is.finite(grid$not_fragile_identification) &
                       grid$not_fragile_identification >= 0.70), na.rm = TRUE),
      stringsAsFactors = FALSE
    ),
    data.frame(
      constraint = "not_fragile_identification_lower",
      n_fail = sum(!(is.finite(grid$not_fragile_identification_lower) &
                       grid$not_fragile_identification_lower >= 0.60), na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  )
  utils::write.csv(
    do.call(rbind, constraint_rows),
    file.path(sum_dir, "training-constraint-failures.csv"),
    row.names = FALSE
  )
  # Top infeasible rows by clear identification (diagnostic only).
  top <- grid[order(-grid$not_fragile_identification, grid$cutoff), , drop = FALSE]
  utils::write.csv(
    utils::head(top, 10L),
    file.path(sum_dir, "training-top-infeasible.csv"),
    row.names = FALSE
  )
}

diag <- list(
  status = result$status,
  frozen_status = result$frozen$status,
  cutoff = result$frozen$cutoff %||% result$fit$cutoff %||% NA_integer_,
  candidate_hash = result$frozen$candidate_hash,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  n_training = nrow(training),
  n_audit = nrow(all_reps),
  reason = result$frozen$reason %||% result$fit$reason,
  workers = 4L,
  validation_accessed = FALSE,
  code_commit = system("git rev-parse HEAD", intern = TRUE)
)
if (is.list(result$fit)) {
  keep <- intersect(names(result$fit), c(
    "status", "reason", "cutoff", "metrics", "selected_row"
  ))
  diag$fit <- result$fit[keep]
}
if (is.list(result$frozen)) {
  keep <- intersect(names(result$frozen), c(
    "status", "reason", "cutoff", "candidate_hash", "metrics",
    "scenario_manifest_hash", "training_manifest_hash",
    "held_out_opened", "validation_refit"
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
utils::write.csv(
  acct$by_scenario,
  file.path(sum_dir, "training-failures.csv"),
  row.names = FALSE
)

manifest <- list(
  phase = "training",
  calibration_unit = "lm_ancova_v2",
  workers = 4L,
  code_commit = diag$code_commit,
  scenario_manifest_hash = scenario_hash,
  training_manifest_hash = training_hash,
  training_artifact_hash = training_hash,
  n_core_null_clear_completed = nrow(training),
  n_audit = nrow(all_reps),
  candidate_status = result$frozen$status,
  candidate_hash = result$frozen$candidate_hash,
  cutoff = result$frozen$cutoff %||% result$fit$cutoff %||% NA_integer_,
  reason = result$frozen$reason %||% result$fit$reason,
  validation_accessed = FALSE,
  assembled_from = "checkpoints/full",
  cutoff_fit = TRUE,
  note = paste(
    "Task 8 training-only cutoff fit from per-scenario checkpoints;",
    "held-out not opened."
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
print(diag[c("status", "frozen_status", "cutoff", "candidate_hash", "reason")])
if (is.list(result$fit$metrics)) {
  m <- result$fit$metrics
  message(sprintf(
    "FR=%.4f (n=%s) upper=%.4f; RI=%.4f (n=%s) lower=%.4f",
    m$false_reassurance %||% NA_real_,
    m$false_reassurance_n %||% NA_integer_,
    m$false_reassurance_upper %||% NA_real_,
    m$not_fragile_identification %||% NA_real_,
    m$not_fragile_identification_n %||% NA_integer_,
    m$not_fragile_identification_lower %||% NA_real_
  ))
}
