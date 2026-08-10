# ==============================================================================
# R Journal evidence artifact: Welch case study recomputed with the released
# package and PACKAGE DEFAULTS (seed = 123), per
# docs/plans/2026-08-06-r-journal-submission-plan.md Task 3.2.
#
# The original manuscript case study used seed = 14 with an apologetic RNG
# caveat ("+/-~1 point across RNG streams"); this artifact removes that
# caveat by using the documented default seed and a single fixed n_boot
# throughout the article (1000, the package default; the shipped vignette uses
# 500 only to keep vignette runtime short).
# ==============================================================================

suppressPackageStartupMessages(library(stabilitest))

N_BOOT <- 1000L
OUTPUT_DIR <- file.path("manuscript", "rjournal", "artifacts", "case-study")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

t_start <- Sys.time()

res <- robustness_analysis(
  pain_treatment, pain_placebo,
  test_type = "t.test",
  n_boot = N_BOOT,
  seed = 123,       # package default; no longer seed = 14
  interpret = TRUE
)

t_end <- Sys.time()

saveRDS(res, file.path(OUTPUT_DIR, "welch-case-study.rds"))

.script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE),
                             value = TRUE)[[1L]]),
  mustWork = TRUE
)
.project_root <- normalizePath(file.path(dirname(.script_path), "..", "..", ".."),
                               mustWork = TRUE)
.verdict_path <- file.path(
  .project_root, "manuscript", "calibration", "studies", "welch_unpaired",
  "published", "VERDICT.json"
)
.registry <- stabilitest:::load_calibration_registry()
.welch <- .registry[.registry$calibration_unit == "welch_unpaired", , drop = FALSE]

manifest <- list(
  artifact_role = "numeric_welch_case_study",
  generated_at = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
  runtime_seconds = as.numeric(difftime(t_end, t_start, units = "secs")),
  package_version = as.character(utils::packageVersion("stabilitest")),
  git_commit = system2("git", c("-C", .project_root, "rev-parse", "HEAD"),
                       stdout = TRUE),
  r_version = R.version.string,
  script_path = file.path("manuscript", "rjournal", "tools",
                          basename(.script_path)),
  script_sha256 = digest::digest(file = .script_path, algo = "sha256"),
  source_study_verdict_hash = digest::digest(file = .verdict_path,
                                             algo = "sha256"),
  welch_calibration_status = .welch$status,
  welch_label_emitted = !is.na(res$robustness_interpretation),
  n_boot = N_BOOT,
  seed = 123L,
  test_type = "t.test",
  dataset = "pain_treatment / pain_placebo (packaged data)",
  overall_robustness = res$robustness_metrics$overall_robustness,
  original_p = res$original_p,
  original_significant = res$original_significant,
  worstcase_fragility_k = res$robustness_metrics$worstcase_fragility_k,
  bootstrap_reproducibility = res$robustness_metrics$bootstrap_reproducibility,
  jackknife_conclusion_stability = res$robustness_metrics$jackknife_conclusion_stability
)
jsonlite::write_json(
  manifest, file.path(OUTPUT_DIR, "manifest.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15
)

print(res)
message("Case study artifact written to ", OUTPUT_DIR)
