#!/usr/bin/env Rscript

# Assemble training occupancy from per-scenario checkpoints without fitting L.
# Prefer checkpoints over monolithic run-results.rds (parent may die after write).

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

study_root <- .study_root()
project_root <- normalizePath(
  file.path(study_root, "..", "..", "..", ".."),
  mustWork = TRUE
)

args <- commandArgs(trailingOnly = TRUE)
checkpoint_root <- file.path(
  study_root, "artifacts", "raw", "training", "checkpoints", "full", "checkpoints"
)
out_csv <- file.path(study_root, "artifacts", "summaries", "training-occupancy.csv")
out_fail <- file.path(study_root, "artifacts", "summaries", "training-failures.csv")
out_manifest <- file.path(
  study_root, "artifacts", "summaries", "training-manifest.json"
)

if ("--checkpoints" %in% args) {
  checkpoint_root <- args[[which(args == "--checkpoints") + 1L]]
}
if ("--output" %in% args) {
  out_csv <- args[[which(args == "--output") + 1L]]
}

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_lm_ancova_v2_study(project_root = project_root, envir = env)

# Lightweight v1 accounting helpers (no cutoff fit).
sys.source(
  file.path(
    project_root, "manuscript", "calibration", "studies", "lm_ancova",
    "tools", "assemble_replicates.R"
  ),
  envir = env
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

acct <- env$lm_ancova_publication_accounting(all_reps, min_quota = 100L)
by_scenario <- acct$by_scenario

# Enrich with design_layer / truth_class / runtime from replicates.
meta_cols <- lapply(split(all_reps, all_reps$scenario_id), function(rows) {
  data.frame(
    scenario_id = as.character(rows$scenario_id[[1L]]),
    design_layer = as.character(rows$design_layer[[1L]] %||% NA_character_),
    truth_class = as.character(rows$truth_class[[1L]] %||% NA_character_),
    n = as.integer(rows$n[[1L]] %||% rows$sample_size[[1L]] %||% NA_integer_),
    runtime_sec = if ("runtime_seconds" %in% names(rows)) {
      sum(as.numeric(rows$runtime_seconds), na.rm = TRUE)
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
})
meta <- do.call(rbind, meta_cols)
occupancy <- merge(by_scenario, meta, by = "scenario_id", all.x = TRUE, sort = FALSE)
occupancy$target_n <- 100L
occupancy$complete_flag <- occupancy$completed >= occupancy$target_n &
  (is.na(occupancy$failure_rate) | occupancy$failure_rate <= 0.05)
occupancy <- occupancy[
  order(occupancy$design_layer, occupancy$scenario_id),
  c(
    "scenario_id", "design_layer", "truth_class", "complete_flag", "target_n",
    "completed", "failed", "excluded", "n", "failure_rate", "runtime_sec"
  ),
  drop = FALSE
]

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(occupancy, out_csv, row.names = FALSE)
utils::write.csv(by_scenario, out_fail, row.names = FALSE)

scenarios <- env$lm_ancova_v2_scenarios(clear_target_power = 0.90)
scenario_hash <- env$calibration_scenario_hash(scenarios)
core_completed <- sum(
  all_reps$design_layer == "core" & all_reps$status == "completed",
  na.rm = TRUE
)

manifest <- list(
  phase = "training",
  calibration_unit = "lm_ancova_v2",
  workers = 4L,
  code_commit = system("git rev-parse HEAD", intern = TRUE),
  scenario_manifest_hash = scenario_hash,
  n_core_completed = as.integer(core_completed),
  n_audit = nrow(all_reps),
  n_scenarios = nrow(occupancy),
  n_complete_scenarios = sum(occupancy$complete_flag, na.rm = TRUE),
  validation_accessed = any(
    all_reps$design_layer == "validation",
    na.rm = TRUE
  ),
  assembled_from = "checkpoints/full",
  cutoff_fit = FALSE,
  note = paste(
    "Occupancy assembled from per-scenario checkpoints;",
    "cutoff search (Task 8) not run."
  )
)
jsonlite::write_json(
  manifest,
  out_manifest,
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
saveRDS(
  manifest,
  sub("\\.json$", ".rds", out_manifest),
  version = 2
)

message(sprintf(
  "Wrote %s (%d scenarios, %d complete, core_completed=%d, validation_accessed=%s)",
  out_csv,
  nrow(occupancy),
  sum(occupancy$complete_flag, na.rm = TRUE),
  core_completed,
  manifest$validation_accessed
))
print(occupancy[, c("scenario_id", "completed", "failed", "complete_flag")])
