# ==============================================================================
# R Journal evidence artifact: Welch case study recomputed with the released
# package and PACKAGE DEFAULTS (seed = 123), per
# docs/plans/2026-08-06-r-journal-submission-plan.md Task 3.2.
#
# The original manuscript case study used seed = 14 with an apologetic RNG
# caveat ("+/-~1 point across RNG streams"); this artifact removes that
# caveat by using the documented default seed and a single fixed n_boot
# throughout the article (2000, matching the historical manuscript's
# precision; the shipped vignette uses 500 only to keep vignette runtime
# short).
# ==============================================================================

suppressPackageStartupMessages(library(stabilitest))

N_BOOT <- 2000L
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

manifest <- list(
  generated_at = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
  runtime_seconds = as.numeric(difftime(t_end, t_start, units = "secs")),
  package_version = as.character(utils::packageVersion("stabilitest")),
  r_version = R.version.string,
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
manifest_lines <- vapply(names(manifest), function(nm) {
  v <- manifest[[nm]]
  sprintf("%s: %s", nm, if (is.na(v)) "NA" else as.character(v))
}, character(1))
writeLines(manifest_lines, file.path(OUTPUT_DIR, "manifest.txt"))

print(res)
message("Case study artifact written to ", OUTPUT_DIR)
