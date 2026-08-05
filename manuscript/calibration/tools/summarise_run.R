#!/usr/bin/env Rscript

# Compact, committed summary of a per-family calibration run (resumable chunks).
#
# Reads the run-results.rds written by run_calibration.R for a training run and
# (optionally) a held-out validation run, and writes one compact CSV row per
# scenario x screening-conclusion stratum: occupancy, score distribution, and
# shared-band (55/70) label counts.  These summaries are intentionally tracked
# so partial progress survives a fresh environment; they are diagnostic
# snapshots and do NOT replace the locked training -> freeze -> held-out
# analysis, which still needs the full replicate compute.
#
# Usage:
#   Rscript manuscript/calibration/tools/summarise_run.R <engine> \
#     [training_run_results.rds] [validation_run_results.rds] [out_csv]
#
# Defaults (relative to the repo root):
#   training   = manuscript/calibration/artifacts/raw/training/run-results.rds
#   validation = manuscript/calibration/artifacts/raw/validation/run-results.rds
#   out_csv    = manuscript/calibration/artifacts/summaries/<engine>-run-summary.csv

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || !nzchar(args[[1]])) {
  stop("usage: summarise_run.R <engine> [training.rds] [validation.rds] [out_csv]", call. = FALSE)
}
engine <- args[[1]]
training_path <- if (length(args) >= 2L && nzchar(args[[2]])) args[[2]] else
  "manuscript/calibration/artifacts/raw/training/run-results.rds"
validation_path <- if (length(args) >= 3L && nzchar(args[[3]])) args[[3]] else
  "manuscript/calibration/artifacts/raw/validation/run-results.rds"
out_csv <- if (length(args) >= 4L && nzchar(args[[4]])) args[[4]] else
  file.path("manuscript/calibration/artifacts/summaries", paste0(engine, "-run-summary.csv"))

collect <- function(path, split) {
  if (!file.exists(path)) {
    message(sprintf("skip %s (missing: %s)", split, path))
    return(NULL)
  }
  r <- readRDS(path)
  # n_boot is recorded in the sibling run-plan.rds, not on each replicate row.
  plan_path <- file.path(dirname(path), "run-plan.rds")
  plan_n_boot <- if (file.exists(plan_path)) {
    tryCatch(as.integer(readRDS(plan_path)$n_boot), error = function(e) NA_integer_)
  } else NA_integer_
  rows <- list()
  for (i in seq_along(r$analyse)) {
    a <- r$analyse[[i]]; s <- r$screen[[i]]
    if (!is.data.frame(a) || !nrow(a)) next
    sc_status <- attr(s$selected, "status")
    denom <- if (!is.null(s$denominator)) s$denominator else NA_integer_
    scenario_failed_total <- sum(a$status != "completed", na.rm = TRUE) + sum(is.na(a$status))
    # which() avoids NA logical-indexing phantom rows for failed replicates
    # whose screening_conclusion is NA.
    conclusions <- sort(unique(a$screening_conclusion[!is.na(a$screening_conclusion)]))
    for (concl in conclusions) {
      sub <- a[which(a$screening_conclusion == concl), , drop = FALSE]
      done <- sub[which(sub$status == "completed"), , drop = FALSE]
      sc <- suppressWarnings(as.numeric(done$overall_score))
      sc <- sc[is.finite(sc)]
      band <- function(lo, hi) sum(sc > lo & sc <= hi)
      rows[[length(rows) + 1L]] <- data.frame(
        split = split,
        design_layer = sub$design_layer[1],
        scenario_id = sub$scenario_id[1],
        analysis_family = sub$analysis_family[1],
        truth_class = sub$truth_class[1],
        screening_conclusion = concl,
        n_boot = if ("n_boot" %in% names(sub) && !is.na(sub$n_boot[1])) sub$n_boot[1] else plan_n_boot,
        screened = denom,
        selected = nrow(sub),
        completed = nrow(done),
        failed = nrow(sub) - nrow(done),
        score_mean = if (length(sc)) round(mean(sc), 2) else NA_real_,
        score_median = if (length(sc)) round(stats::median(sc), 2) else NA_real_,
        score_sd = if (length(sc)) round(stats::sd(sc), 2) else NA_real_,
        score_min = if (length(sc)) round(min(sc), 2) else NA_real_,
        score_max = if (length(sc)) round(max(sc), 2) else NA_real_,
        n_fragile_le55 = band(-Inf, 55),
        n_moderate_55_70 = band(55, 70),
        n_robust_gt70 = band(70, Inf),
        scenario_failed_total = scenario_failed_total,
        scenario_status = if (is.null(sc_status)) NA_character_ else sc_status,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

all <- do.call(rbind, Filter(Negate(is.null), list(
  collect(training_path, "training"),
  collect(validation_path, "validation")
)))
if (is.null(all) || !nrow(all)) stop("no replicate results found to summarise", call. = FALSE)

dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(all, out_csv, row.names = FALSE)
cat(sprintf("Wrote %s (%d stratum rows, %d completed analyses, %d scenario failures)\n",
            out_csv, nrow(all), sum(all$completed), sum(unique(all[c("scenario_id","scenario_failed_total")])$scenario_failed_total)))
print(all[, c("split", "scenario_id", "screening_conclusion", "completed",
              "score_median", "n_robust_gt70", "scenario_status")], row.names = FALSE)
