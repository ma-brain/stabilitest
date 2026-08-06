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

# Gate A ANCOVA v2 Track A SAP (two-band jackknife-light protocol).
ancova_v2_policy_files <- file.path(root, c(
  "README.md", "NEWS.md",
  "manuscript/calibration/README.md",
  "manuscript/calibration/studies/lm_ancova_v2/README.md",
  "manuscript/calibration/studies/lm_ancova_v2/CALIBRATION_SAP.md"
))
ancova_v2_required <- file.path(
  root, "manuscript/calibration/studies/lm_ancova_v2/CALIBRATION_SAP.md"
)
if (!file.exists(ancova_v2_required)) {
  violations <- c(violations, "missing ANCOVA v2 CALIBRATION_SAP.md")
}
ancova_v2_existing <- ancova_v2_policy_files[file.exists(ancova_v2_policy_files)]
ancova_v2_text <- if (length(ancova_v2_existing)) {
  paste(unlist(lapply(ancova_v2_existing, readLines, warn = FALSE)), collapse = "\n")
} else {
  ""
}
assert_ancova_v2 <- function(pattern, description, ignore.case = TRUE) {
  if (!grepl(pattern, ancova_v2_text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}
assert_ancova_v2("lm_ancova_v2",
                 "ANCOVA v2 docs omit lm_ancova_v2",
                 ignore.case = FALSE)
assert_ancova_v2("fragility\\s*=\\s*0\\.5",
                 "ANCOVA v2 docs omit fragility = 0.5")
assert_ancova_v2("bootstrap\\s*=\\s*0\\.5",
                 "ANCOVA v2 docs omit bootstrap = 0.5")
assert_ancova_v2("jackknife\\s*=\\s*0(?!\\.\\d)",
                 "ANCOVA v2 docs omit jackknife = 0")
assert_ancova_v2(
  "Fragile if score\\s*[≤<=]\\s*L|score\\s*[≤<=]\\s*L.{0,60}Fragile",
  "ANCOVA v2 docs omit Fragile if score ≤ L band rule"
)
assert_ancova_v2("Not fragile",
                 "ANCOVA v2 docs omit Not fragile band")
assert_ancova_v2("null.{0,40}clear only|fitting strata.{0,40}null.{0,20}clear",
                 "ANCOVA v2 docs omit null + clear fitting strata")
assert_ancova_v2("borderline.{0,40}diagnostic|diagnostic.?only",
                 "ANCOVA v2 docs omit borderline diagnostic-only policy")
assert_ancova_v2("median\\(clear\\).{0,30}median\\(null\\)",
                 "ANCOVA v2 docs omit pilot median(clear) − median(null) formula")
assert_ancova_v2("P\\(null\\s*>\\s*median\\(clear\\)\\)",
                 "ANCOVA v2 docs omit pilot P(null > median(clear)) formula")
assert_ancova_v2(
  "[≥>=]\\s*20.{0,200}15\\s*[≤<=].{0,40}<\\s*20.{0,80}<\\s*15|[≥>=]\\s*20.{0,40}15\\s*[≤<=]\\s*Δ\\s*<\\s*20.{0,40}Δ\\s*<\\s*15",
  "ANCOVA v2 docs omit locked Δ go ≥ 20 / marginal 15–20 / hard < 15"
)
assert_ancova_v2(
  "[≤<=]\\s*0\\.10.{0,200}0\\.10\\s*<.{0,40}[≤<=]\\s*0\\.20.{0,80}>\\s*0\\.20",
  "ANCOVA v2 docs omit locked O go ≤ 0.10 / marginal ≤ 0.20 / hard > 0.20"
)
assert_ancova_v2(
  "[≥>=]\\s*0\\.75.{0,200}0\\.70\\s*[≤<=].{0,40}<\\s*0\\.75.{0,80}<\\s*0\\.70",
  "ANCOVA v2 docs omit locked AUC go ≥ 0.75 / marginal 0.70–0.75 / hard < 0.70"
)
assert_ancova_v2(
  "0\\.90.{0,80}0\\.95|clear.{0,40}0\\.90.{0,40}0\\.95|escalate.{0,40}0\\.95",
  "ANCOVA v2 docs omit clear power 0.90 default / 0.95 escalation rule"
)
assert_ancova_v2("workers.{0,30}`?4`?|Workers.+`4`",
                 "ANCOVA v2 docs omit workers default 4")
assert_ancova_v2("n_boot.{0,30}`?1000`?|`1000`",
                 "ANCOVA v2 docs omit n_boot = 1000")
assert_ancova_v2("max_screen_draws.{0,30}`?10000`?|`10000`",
                 "ANCOVA v2 docs omit max_screen_draws = 10000")
assert_ancova_v2("100.{0,40}significant|significant.{0,40}100",
                 "ANCOVA v2 docs omit ≥100 significant quota")
assert_ancova_v2(
  "false reassurance.{0,40}0\\.05|FR.{0,40}0\\.05",
  "ANCOVA v2 docs omit FR ≤ 0.05 acceptance gate"
)
assert_ancova_v2(
  "Wilson upper\\s*[≤<=]\\s*0\\.10|one-sided 95% Wilson upper\\s*[≤<=]\\s*0\\.10",
  "ANCOVA v2 docs omit Wilson FR upper ≤ 0.10"
)
assert_ancova_v2(
  "Not.?fragile identification.{0,40}0\\.70|identification on clear.{0,40}0\\.70",
  "ANCOVA v2 docs omit Not-fragile identification ≥ 0.70 gate"
)
assert_ancova_v2(
  "Wilson lower\\s*[≥>=]\\s*0\\.60|one-sided 95% Wilson lower\\s*[≥>=]\\s*0\\.60",
  "ANCOVA v2 docs omit Wilson RI lower ≥ 0.60"
)
assert_ancova_v2("no_feasible_thresholds",
                 "ANCOVA v2 docs omit no_feasible_thresholds fail-closed reason",
                 ignore.case = FALSE)
if (grepl("no_feasible_threshold(?!s)", ancova_v2_text, perl = TRUE)) {
  violations <- c(violations,
                  "ANCOVA v2 docs use singular no_feasible_threshold (must be plural)")
}
if (grepl("no_feasible_thresholds.{0,60}or equivalent|or equivalent.{0,60}no_feasible",
          ancova_v2_text, ignore.case = TRUE, perl = TRUE)) {
  violations <- c(violations,
                  "ANCOVA v2 docs soften no_feasible_thresholds with 'or equivalent'")
}
assert_ancova_v2(
  "v1 validation.{0,100}(absent|not|never)|absent from v2|no v1 validation",
  "ANCOVA v2 docs omit assertion that v1 validation seeds/IDs are absent from v2"
)
assert_ancova_v2(
  "Welch.{0,40}comparator|55/70.{0,40}comparator|not an ANCOVA (default|fallback)",
  "ANCOVA v2 docs omit that Welch 55/70 is not an ANCOVA fallback"
)
assert_ancova_v2(
  "Track A|two-band|Fragile / Not fragile|Fragile vs Not fragile",
  "ANCOVA v2 docs omit Track A two-band claim"
)
assert_ancova_v2(
  "Gate B.{0,120}(fail-closed|uncalibrated)|(fail-closed|uncalibrated).{0,120}Gate B|Gate B closed",
  "ANCOVA v2 docs omit Gate B fail-closed uncalibrated closeout"
)
assert_ancova_v2(
  "held-out not opened|held.out not opened|validation.{0,40}not (opened|accessed)",
  "ANCOVA v2 docs omit that held-out validation was not opened"
)
assert_ancova_v2(
  "3dc2a1f840b3eb725bea629dc130f070",
  "ANCOVA v2 docs omit published candidate hash",
  ignore.case = FALSE
)
assert_ancova_v2(
  "lm-ancova-v2-2026-1|published/",
  "ANCOVA v2 docs omit published version or published/ path"
)
assert_ancova_v2(
  "labels?.{0,40}suppressed|categorical labels stay suppressed|labels remain suppressed",
  "ANCOVA v2 docs omit that labels remain suppressed"
)

# Phase 1 v3-lite: empirical fail-closed narrative (NEWS + calibration README).
# Do not require edits under studies/lm_ancova_v2/ (immutable publication).
ancova_v2_outcome_files <- file.path(root, c(
  "NEWS.md",
  "manuscript/calibration/README.md"
))
ancova_v2_outcome_existing <- ancova_v2_outcome_files[file.exists(ancova_v2_outcome_files)]
ancova_v2_outcome_text <- if (length(ancova_v2_outcome_existing)) {
  paste(unlist(lapply(ancova_v2_outcome_existing, readLines, warn = FALSE)),
        collapse = "\n")
} else {
  ""
}
assert_ancova_v2_outcome <- function(pattern, description, ignore.case = TRUE) {
  if (!grepl(pattern, ancova_v2_outcome_text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}
assert_ancova_v2_outcome(
  "executed fail-closed|fail-closed after.{0,80}(pilot|training)",
  "ANCOVA calibration docs omit that v2 was executed fail-closed"
)
assert_ancova_v2_outcome("24\\.4", "ANCOVA calibration docs omit v2 pilot Δ = 24.4")
assert_ancova_v2_outcome("0\\.034", "ANCOVA calibration docs omit v2 pilot overlap = 0.034")
assert_ancova_v2_outcome("0\\.892", "ANCOVA calibration docs omit v2 pilot AUC = 0.892")
assert_ancova_v2_outcome(
  "0\\.554",
  "ANCOVA calibration docs omit best RI at FR-safe L = 0.554"
)
assert_ancova_v2_outcome(
  "Finding 4|2026-08-06-lm-ancova-v3-design",
  "ANCOVA calibration docs omit cross-reference to v3 design Finding 4"
)
assert_ancova_v2_outcome(
  "false.?GO|pilot GO|location",
  "ANCOVA calibration docs omit false-GO / pilot location-metric narrative"
)

# Phase 1 v3-lite Track E SAP (violation detection; Track D parked).
ancova_v3_policy_files <- file.path(root, c(
  "manuscript/calibration/studies/lm_ancova_v3/README.md",
  "manuscript/calibration/studies/lm_ancova_v3/CALIBRATION_SAP.md"
))
ancova_v3_required <- file.path(
  root, "manuscript/calibration/studies/lm_ancova_v3/CALIBRATION_SAP.md"
)
if (!file.exists(ancova_v3_required)) {
  violations <- c(violations, "missing ANCOVA v3 CALIBRATION_SAP.md")
}
ancova_v3_existing <- ancova_v3_policy_files[file.exists(ancova_v3_policy_files)]
ancova_v3_text <- if (length(ancova_v3_existing)) {
  paste(unlist(lapply(ancova_v3_existing, readLines, warn = FALSE)), collapse = "\n")
} else {
  ""
}
assert_ancova_v3 <- function(pattern, description, ignore.case = TRUE) {
  if (!grepl(pattern, ancova_v3_text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}
assert_ancova_v3("lm_ancova_v3", "ANCOVA v3 docs omit lm_ancova_v3", ignore.case = FALSE)
assert_ancova_v3("Track E|violation.?detection",
                 "ANCOVA v3 docs omit Track E / violation-detection scope")
assert_ancova_v3("fragility\\s*=\\s*0\\.5",
                 "ANCOVA v3 docs omit fragility = 0.5")
assert_ancova_v3("bootstrap\\s*=\\s*0\\.5",
                 "ANCOVA v3 docs omit bootstrap = 0.5")
assert_ancova_v3("jackknife\\s*=\\s*0(?!\\.\\d)",
                 "ANCOVA v3 docs omit jackknife = 0")
assert_ancova_v3(
  "0\\.4.{0,20}0\\.4.{0,20}0\\.2|jackknife\\s*=\\s*0\\.4",
  "ANCOVA v3 docs omit v1 composite archived as comparator"
)
assert_ancova_v3(
  "n\\s*∈\\s*\\{40,\\s*80,\\s*160\\}|n ∈ \\{40, 80, 160\\}",
  "ANCOVA v3 docs omit clean-cell n ∈ {40, 80, 160}"
)
assert_ancova_v3("0\\.40|R²\\s*=\\s*0\\.40|R\\^?2\\s*=\\s*0\\.40",
                 "ANCOVA v3 docs omit baseline R² = 0.40")
assert_ancova_v3("0\\.90", "ANCOVA v3 docs omit clear power 0.90")
assert_ancova_v3(
  "allocation|2:1|heteroscedastic|heavy tails|missing baseline|interaction",
  "ANCOVA v3 docs omit the five frozen violation types"
)
assert_ancova_v3(
  "diagnostic.{0,40}null|null.{0,40}diagnostic|n\\s*=\\s*80",
  "ANCOVA v3 docs omit diagnostic null pairs at n = 80"
)
assert_ancova_v3(
  "no gate|not gated|secondary.{0,20}no gate",
  "ANCOVA v3 docs omit that null pairs carry no gate"
)
assert_ancova_v3("54001", "ANCOVA v3 docs omit Track E seed range 54001+")
assert_ancova_v3("20260807", "ANCOVA v3 docs omit master seed 20260807")
assert_ancova_v3(
  "51001|52001|53001",
  "ANCOVA v3 docs omit reserved Track D seed ranges"
)
assert_ancova_v3(
  "Track D.{0,80}parked|parked.{0,80}Track D",
  "ANCOVA v3 docs omit that Track D is parked"
)
assert_ancova_v3(
  "binary.?proportion|explicit human",
  "ANCOVA v3 docs omit Track D un-parking condition"
)
assert_ancova_v3(
  "ΔAUC|delta.?AUC|AUC_score\\s*[−-]\\s*AUC_p",
  "ANCOVA v3 docs omit ΔAUC = AUC_score − AUC_p"
)
assert_ancova_v3(
  "clean\\s*>\\s*violated|score orientation.{0,40}clean",
  "ANCOVA v3 docs omit pre-specified score orientation clean > violated"
)
assert_ancova_v3(
  "favors p|p-favoring|whichever favors p|conservative for the claim",
  "ANCOVA v3 docs omit empirical p-favoring orientation rule"
)
assert_ancova_v3(
  "cluster.?bootstrap|scenario.?cluster",
  "ANCOVA v3 docs omit scenario-cluster bootstrap"
)
assert_ancova_v3(
  "B\\s*=\\s*1000|B = 1000",
  "ANCOVA v3 docs omit bootstrap B = 1000"
)
assert_ancova_v3(
  "0\\.10|≥\\s*0\\.10",
  "ANCOVA v3 docs omit pooled ΔAUC ≥ 0.10 gate"
)
assert_ancova_v3(
  "lower bound\\s*>\\s*0|CI lower.{0,20}>\\s*0",
  "ANCOVA v3 docs omit CI lower bound > 0 gate"
)
assert_ancova_v3(
  "workers.{0,30}`?4`?|Workers.+`4`|maximum.{0,20}4",
  "ANCOVA v3 docs omit workers maximum 4"
)
assert_ancova_v3("n_boot.{0,30}`?1000`?|`1000`",
                 "ANCOVA v3 docs omit n_boot = 1000")
assert_ancova_v3(
  "100.{0,40}significant|significant.{0,40}100",
  "ANCOVA v3 docs omit ≥100 significant primary quota"
)
assert_ancova_v3(
  "5%|≤\\s*0\\.05|failures.{0,20}5",
  "ANCOVA v3 docs omit ≤5% failure limit"
)
assert_ancova_v3(
  "10000|draws.{0,20}10000",
  "ANCOVA v3 docs omit power-check draws 10000"
)
assert_ancova_v3(
  "0\\.02|tolerance.{0,20}0\\.02",
  "ANCOVA v3 docs omit power-check tolerance 0.02"
)
assert_ancova_v3(
  "v1.{0,40}validation|validation.{0,40}(seeds|IDs).{0,40}(absent|no v3)",
  "ANCOVA v3 docs omit that v1/v2 validation seeds/IDs are absent from v3"
)
assert_ancova_v3(
  "Welch 55/70.{0,40}not.{0,40}ANCOVA|not an ANCOVA fallback",
  "ANCOVA v3 docs omit that Welch 55/70 is not an ANCOVA fallback"
)

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
