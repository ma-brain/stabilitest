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
  "manuscript/calibration/CALIBRATION_SAP.md"))
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

# Gate A ANCOVA study policy (independent method-specific calibration).
ancova_policy_files <- file.path(root, c(
  "README.md", "NEWS.md",
  "manuscript/calibration/README.md",
  "manuscript/calibration/studies/lm_ancova/README.md",
  "manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md",
  "manuscript/calibration/studies/lm_ancova/manuscript.md",
  "vignettes/ancova-case-study.Rmd"
))
ancova_required <- file.path(
  root, "manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md"
)
if (!file.exists(ancova_required)) {
  violations <- c(violations, "missing ANCOVA CALIBRATION_SAP.md")
}
ancova_existing <- ancova_policy_files[file.exists(ancova_policy_files)]
ancova_text <- if (length(ancova_existing)) {
  paste(unlist(lapply(ancova_existing, readLines, warn = FALSE)), collapse = "\n")
} else {
  ""
}
assert_ancova <- function(pattern, description, ignore.case = TRUE) {
  if (!grepl(pattern, ancova_text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}
assert_ancova("canonical significant 1-df treatment|canonical 1-df ANCOVA treatment",
              "ANCOVA docs omit canonical significant 1-df treatment scope")
assert_ancova("60%.{0,20}90%|0\\.60.{0,20}0\\.90|power-defined",
              "ANCOVA docs omit 60%/90% power-defined truth strata")
assert_ancova("multi-df",
              "ANCOVA docs omit multi-df label suppression")
assert_ancova("weights remain frozen|score weights remain frozen|frozen.*weights",
              "ANCOVA docs omit frozen score weights")
assert_ancova("Welch.{0,40}comparator|55/70.{0,40}comparator|comparator.{0,40}Welch|not an ANCOVA (default|fallback)",
              "ANCOVA docs omit that 55/70 is a Welch comparator, not an ANCOVA fallback")
assert_ancova("no_feasible_thresholds",
              "ANCOVA docs omit no_feasible_thresholds fail-closed reason",
              ignore.case = FALSE)
assert_ancova("held-out not opened|held.out not opened|validation.{0,40}not (opened|accessed)",
              "ANCOVA docs omit that held-out validation was not opened")
assert_ancova("Gate B.{0,120}(fail-closed|uncalibrated)|(fail-closed|uncalibrated).{0,120}Gate B|Gate B closed",
              "ANCOVA docs omit Gate B fail-closed uncalibrated closeout")
assert_ancova("pain_ancova_trial",
              "ANCOVA docs omit pain_ancova_trial")
assert_ancova("prospectively frozen|never enters training|excluded from.*calibration",
              "ANCOVA docs omit that pain_ancova_trial is a non-calibrating frozen illustration")
assert_ancova("Illustrative synthetic case study|case study follows|after the calibration results",
              "ANCOVA docs omit that the manuscript case study follows calibration results")

scan_files <- file.path(root, c(
  "README.md", "NEWS.md", "R", "man", "vignettes",
  "manuscript/robustness_analysis_manuscript.md",
  "manuscript/methodological_review.md",
  "manuscript/calibration/CALIBRATION_SAP.md"))
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
