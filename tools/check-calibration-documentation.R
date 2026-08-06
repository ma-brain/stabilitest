#!/usr/bin/env Rscript

# Source-tree audit for the method-specific calibration policy. This is kept
# outside tests/testthat because the files it scans are intentionally excluded
# from, or transformed by, an installed R package.

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", arguments, value = TRUE)
script_path <- if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1]])
} else {
  file.path(getwd(), "tools", "check-calibration-documentation.R")
}
root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

policy_files <- file.path(root, c(
  "README.md", "NEWS.md", "R/robustness_analysis.R",
  "R/robustness_models.R", "R/robustness_tost.R",
  "vignettes/pain-case-study.Rmd",
  "manuscript/robustness_analysis_manuscript.md",
  "manuscript/methodological_review.md",
  "manuscript/calibration/CALIBRATION_SAP.md",
  "manuscript/calibration/studies/binary_proportion/CALIBRATION_SAP.md"))
existing_policy_files <- policy_files[file.exists(policy_files)]
if (!length(existing_policy_files)) {
  stop("No source policy files were found under ", root, call. = FALSE)
}

text <- paste(unlist(lapply(existing_policy_files, readLines, warn = FALSE)),
              collapse = "\n")
violations <- character()
assert_match <- function(pattern, description, ignore.case = TRUE) {
  if (!grepl(pattern, text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}

assert_match("numeric scores and (all )?component metrics",
             "active policy does not retain numeric scores and components")
assert_match("labels are suppressed|categorical labels are suppressed",
             "active policy does not suppress uncalibrated labels")
assert_match("welch_unpaired",
             "active policy does not name welch_unpaired",
             ignore.case = FALSE)
assert_match("public `?robustness_analysis[(][)]`? dispatcher",
             "active policy does not preserve the public dispatcher")
assert_match("lm_ancova", "active policy does not name lm_ancova",
             ignore.case = FALSE)

# Proportions Phase 1 SAP key sentences.  These pin the frozen policy so a
# silent change to the weights, gates, truth target, or freeze discipline is
# caught by the audit.
assert_match("fisher_exact", "proportions policy does not name fisher_exact",
             ignore.case = FALSE)
assert_match("fragility = 0\\.5.*bootstrap = 0\\.5.*jackknife = 0",
             "proportions policy does not freeze the jackknife-light weights")
assert_match("clear exact power 0\\.95",
             "proportions policy does not freeze the clear-power 0.95 target")
assert_match("borderline exact power 0\\.60 \\(diagnostic only\\)",
             "proportions policy does not mark borderline diagnostic-only")
assert_match("training 61001\\+, validation 62001\\+, stress 63001\\+",
             "proportions policy does not freeze the disjoint seed blocks")
assert_match("projected RI >= 0\\.72",
             "proportions policy does not freeze the pilot go threshold")
assert_match("FR <= 0\\.05 with Wilson upper <= 0\\.10",
             "proportions policy does not freeze the Track A FR gate")
assert_match("RI >= 0\\.70 with Wilson lower >= 0\\.60",
             "proportions policy does not freeze the Track A RI gate")
assert_match("20260809L",
             "proportions policy does not freeze the case-study seed")
assert_match("appear in no[[:space:]]+calibration ledger",
             "proportions policy does not isolate the case-study seed from ledgers")
assert_match("per-arm n within \\[25, 200\\]",
             "proportions policy does not freeze the runtime n bounds")
assert_match("chi_square_2x2",
             "proportions policy does not sequence the Phase 2 unit",
             ignore.case = FALSE)

scan_files <- file.path(root, c(
  "README.md", "NEWS.md", "R", "man", "vignettes",
  "manuscript/robustness_analysis_manuscript.md",
  "manuscript/methodological_review.md",
  "manuscript/calibration/CALIBRATION_SAP.md",
  "manuscript/calibration/studies/binary_proportion"))
files <- unique(unlist(lapply(scan_files, function(path) {
  if (dir.exists(path)) {
    return(list.files(path, pattern = "\\.(R|Rd|md|Rmd)$",
                      recursive = TRUE, full.names = TRUE))
  }
  if (file.exists(path)) path else character()
})))

# These phrases previously asserted that the 55/70 mapping transferred to
# every engine or that the generic two_sample family was an active key.
stale_patterns <- c(
  "assumed transferable",
  "score bands are shared",
  "bands were calibrated by simulation",
  "calibrated interpretation bands.*(all|every|shared)",
  "anchors (its )?summary score to an empirical calibration",
  "For a significant primary result: score > 70",
  "empirical calibration that distinguishes chance",
  "calibration (family|unit).{0,40}two_sample",
  "two_sample.{0,40}calibration (family|unit)"
)

historical_context <- function(lines, index, path) {
  if (grepl("/published/", path, fixed = TRUE)) return(TRUE)
  lo <- max(1L, index - 40L)
  hi <- min(length(lines), index + 3L)
  grepl("Task 15|historical|inactive|archive|archived",
        paste(lines[lo:hi], collapse = " "), ignore.case = TRUE)
}

for (path in files) {
  lines <- readLines(path, warn = FALSE)
  for (pattern in stale_patterns) {
    hits <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE)
    if (!length(hits)) next
    active_hits <- hits[!vapply(hits, historical_context, logical(1),
                                lines = lines, path = path)]
    if (length(active_hits)) {
      relative_path <- substring(path, nchar(root) + 2L)
      violations <- c(
        violations,
        sprintf("%s:%s: %s", relative_path, active_hits,
                trimws(lines[active_hits])))
    }
  }
}

if (length(violations)) {
  cat("Calibration documentation audit failed:\n", sep = "")
  cat(paste0("- ", violations, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}

cat("Calibration documentation audit passed (", length(existing_policy_files),
    " policy files scanned).\n", sep = "")
