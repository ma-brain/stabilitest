#!/usr/bin/env Rscript

# Compact occupancy summary for a lm_ancova_v3 runner output directory.

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "lm_ancova_v3"),
    mustWork = TRUE
  )
}

`%||%` <- function(left, right) if (is.null(left)) right else left

.args_value <- function(args, flag) {
  if (!flag %in% args) return(NULL)
  args[[which(args == flag) + 1L]]
}

args <- commandArgs(trailingOnly = TRUE)
input <- .args_value(args, "--input")
if (is.null(input)) stop("--input <runner-output-dir> is required", call. = FALSE)
label <- .args_value(args, "--label") %||% "pilot"
out <- .args_value(args, "--output") %||% file.path(
  .study_root(), "artifacts", "summaries", sprintf("%s-occupancy.csv", label)
)

results_path <- file.path(input, "run-results.rds")
if (!file.exists(results_path)) {
  stop(sprintf("missing run-results.rds under %s", input), call. = FALSE)
}
raw <- readRDS(results_path)

# Runner stores list(screen=..., analyse=...); prefer analyse replicates.
if (is.list(raw) && !is.data.frame(raw) && "analyse" %in% names(raw)) {
  parts <- raw$analyse
  if (is.list(parts) && !is.data.frame(parts)) {
    parts <- parts[vapply(parts, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]
    results <- if (length(parts)) dplyr::bind_rows(parts) else data.frame()
  } else if (is.data.frame(parts)) {
    results <- parts
  } else {
    results <- data.frame()
  }
} else if (is.data.frame(raw)) {
  results <- raw
} else {
  stop("unrecognized run-results.rds structure", call. = FALSE)
}

if (!nrow(results)) stop("run-results.rds has no analyse rows", call. = FALSE)

status <- if ("status" %in% names(results)) as.character(results$status) else rep("completed", nrow(results))
completed <- results[is.na(status) | status == "completed", , drop = FALSE]
failed <- results[!is.na(status) & status != "completed", , drop = FALSE]

by_id <- split(completed, completed$scenario_id)
rows <- lapply(names(by_id), function(id) {
  part <- by_id[[id]]
  fail_n <- sum(failed$scenario_id == id, na.rm = TRUE)
  runtime <- if ("runtime_seconds" %in% names(part)) {
    sum(as.numeric(part$runtime_seconds), na.rm = TRUE)
  } else {
    NA_real_
  }
  data.frame(
    scenario_id = id,
    design_layer = as.character(part$design_layer[[1L]] %||% NA_character_),
    truth_class = as.character(part$truth_class[[1L]] %||% NA_character_),
    completed = nrow(part),
    failed = as.integer(fail_n),
    failure_rate = if ((nrow(part) + fail_n) > 0) {
      fail_n / (nrow(part) + fail_n)
    } else {
      NA_real_
    },
    runtime_sec = runtime,
    stringsAsFactors = FALSE
  )
})

occ <- do.call(rbind, rows)
occ <- occ[order(occ$scenario_id), , drop = FALSE]
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(occ, out, row.names = FALSE)

summary_path <- sub("occupancy\\.csv$", "occupancy-summary.txt", out)
sink(summary_path)
cat(sprintf("label=%s\n", label))
cat(sprintf("input=%s\n", normalizePath(input, mustWork = TRUE)))
cat(sprintf("n_scenarios=%d\n", nrow(occ)))
cat(sprintf("completed_total=%d\n", sum(occ$completed)))
cat(sprintf("failed_total=%d\n", sum(occ$failed)))
cat(sprintf(
  "max_failure_rate=%.4f\n",
  if (nrow(occ)) max(occ$failure_rate, na.rm = TRUE) else NA_real_
))
cat(sprintf("runtime_sec_total=%.1f\n", sum(occ$runtime_sec, na.rm = TRUE)))
sink()

message("Wrote ", out)
message("Wrote ", summary_path)
print(occ)
