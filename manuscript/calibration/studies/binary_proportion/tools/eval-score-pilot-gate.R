#!/usr/bin/env Rscript

# Sealed score-only pilot gate for the binary-proportion (Phase 1: fisher_exact)
# calibration.  Computes the feasibility-projection metric from pilot scores:
#   - the smallest FR-safe integer L (FR <= 0.05, Wilson upper <= 0.10)
#   - projected RI = P(score > L | clear, significant)
# Gate: go >= 0.72; hard no-go < 0.70; marginal [0.70, 0.72) => human decides.
# Location / overlap / AUC are archived as diagnostics only.

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", arguments, value = TRUE)
script_path <- if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1]])
} else {
  file.path(getwd(), "manuscript", "calibration", "studies",
            "binary_proportion", "tools", "eval-score-pilot-gate.R")
}
study_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
project_root <- normalizePath(file.path(study_root, "..", "..", "..", ".."),
                              mustWork = TRUE)

# Load the study environment (shared helpers + study modules).
env <- new.env(parent = globalenv())
loader <- file.path(study_root, "R", "load_study.R")
sys.source(loader, envir = env)
env$load_binary_proportion_study(project_root = project_root, envir = env)

args <- commandArgs(trailingOnly = TRUE)
pilot_dir <- if ("--pilot-dir" %in% args) {
  args[which(args == "--pilot-dir") + 1L]
} else {
  file.path(study_root, "outputs", "score-pilot")
}
out_path <- if ("--out" %in% args) {
  args[which(args == "--out") + 1L]
} else {
  file.path(study_root, "artifacts", "summaries", "SCORE_PILOT_GATE.json")
}

# Read the pilot run-results and assemble the score table.
run_results <- readRDS(file.path(pilot_dir, "run-results.rds"))
analyse <- run_results$analyse
replicates <- do.call(dplyr::bind_rows, lapply(analyse, function(x) {
  if (is.data.frame(x) && nrow(x)) x else NULL
}))
if (!nrow(replicates)) {
  env$.calibration_manifest_abort("no completed pilot replicates found")
}

# Restrict to completed significant rows with a finite score.
replicates <- replicates[
  !is.na(replicates$status) & replicates$status == "completed" &
    !is.na(replicates$screening_conclusion) &
    replicates$screening_conclusion == "significant" &
    is.finite(replicates$overall_score), , drop = FALSE
]

null_scores <- replicates$overall_score[replicates$truth_class == "null"]
clear_scores <- replicates$overall_score[replicates$truth_class == "clear"]

# Wilson interval bound (reuse the study helper).
wilson <- function(x, n, side, conf_level = 0.95) {
  if (!is.finite(n) || n <= 0) return(NA_real_)
  z <- stats::qnorm(conf_level)
  centre <- (x + z^2 / 2) / (n + z^2)
  radius <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
  if (identical(side, "upper")) min(1, centre + radius) else max(0, centre - radius)
}

# Smallest FR-safe L: FR <= 0.05 with Wilson upper <= 0.10.
fr_safe_L <- NA_integer_
fr_at_L <- NA_real_
fr_upper_at_L <- NA_real_
ri_at_L <- NA_real_
ri_lower_at_L <- NA_real_
if (length(null_scores) && length(clear_scores)) {
  for (L in 0:100) {
    fr_count <- sum(null_scores > L)
    fr_n <- length(null_scores)
    fr_point <- fr_count / fr_n
    fr_upper <- wilson(fr_count, fr_n, "upper")
    if (fr_point <= 0.05 && fr_upper <= 0.10) {
      fr_safe_L <- L
      fr_at_L <- fr_point
      fr_upper_at_L <- fr_upper
      ri_count <- sum(clear_scores > L)
      ri_n <- length(clear_scores)
      ri_at_L <- ri_count / ri_n
      ri_lower_at_L <- wilson(ri_count, ri_n, "lower")
      break
    }
  }
}

verdict <- if (is.na(ri_at_L)) {
  "no_fr_safe_cutoff"
} else if (ri_at_L >= 0.72) {
  "go"
} else if (ri_at_L < 0.70) {
  "hard_no_go"
} else {
  "marginal"
}

# Diagnostic: pooled AUC (clear vs null) and score-location summary.
auc <- if (length(null_scores) && length(clear_scores)) {
  r <- rank(c(clear_scores, null_scores))
  n1 <- length(clear_scores); n2 <- length(null_scores)
  (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * n2)
} else NA_real_

gate <- list(
  metric = "projected_ri_at_fr_safe_L",
  fr_safe_L = fr_safe_L,
  projected_ri = ri_at_L,
  projected_ri_lower = ri_lower_at_L,
  false_reassurance = fr_at_L,
  false_reassurance_upper = fr_upper_at_L,
  verdict = verdict,
  gate_thresholds = list(go = 0.72, marginal_low = 0.70),
  n_null_significant = length(null_scores),
  n_clear_significant = length(clear_scores),
  diagnostics = list(
    null_score_summary = if (length(null_scores)) {
      as.list(round(summary(null_scores), 3))
    } else NULL,
    clear_score_summary = if (length(clear_scores)) {
      as.list(round(summary(clear_scores), 3))
    } else NULL,
    pooled_auc_clear_vs_null = round(auc, 4)
  )
)

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
jsonlite_available <- requireNamespace("jsonlite", quietly = TRUE)
if (jsonlite_available) {
  jsonlite::write_json(gate, out_path, auto_unbox = TRUE, pretty = TRUE)
} else {
  # Fallback: dput-style if jsonlite is absent.
  con <- file(out_path, "w"); on.exit(close(con))
  writeLines(c("# SCORE_PILOT_GATE (jsonlite unavailable; R structure follows)", deparse(gate)), con)
}

cat("Sealed pilot gate written to", out_path, "\n")
cat(sprintf("  projected RI at FR-safe L=%s: %.4f (lower %.4f)\n",
            fr_safe_L, ri_at_L, ri_lower_at_L))
cat(sprintf("  verdict: %s\n", verdict))
invisible(gate)
