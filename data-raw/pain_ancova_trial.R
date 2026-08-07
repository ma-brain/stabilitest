# Prospectively frozen synthetic ANCOVA pain-trial illustration.
#
# This dataset is generated once from the constants below and is excluded from
# every calibration training, candidate-selection, and held-out validation step.
# Do not regenerate or select it by p-value, score, or categorical band.

PAIN_ANCOVA_CASE_SEED <- 20260806L
PAIN_ANCOVA_CASE_N <- 80L

generate_pain_ancova_trial <- function(seed = PAIN_ANCOVA_CASE_SEED) {
  set.seed(seed)
  arm <- sample(rep(c("Placebo", "Active"), each = PAIN_ANCOVA_CASE_N / 2L))
  baseline <- round(
    pmin(90, pmax(35, stats::rnorm(PAIN_ANCOVA_CASE_N, 65, 12))), 1
  )
  week12 <- round(
    20 + 0.60 * baseline - 7.5 * (arm == "Active") +
      stats::rnorm(PAIN_ANCOVA_CASE_N, 0, 10),
    1
  )
  data.frame(
    subject_id = sprintf("PAIN-A%03d", seq_len(PAIN_ANCOVA_CASE_N)),
    arm = factor(arm, levels = c("Placebo", "Active")),
    baseline_pain = baseline,
    week12_pain = week12,
    change = round(week12 - baseline, 1),
    stringsAsFactors = FALSE
  )
}

pain_ancova_trial <- generate_pain_ancova_trial()

# Plausibility gate before packaging. Revise the DGP/seed only if this fails,
# and never after inspecting the worked robustness analysis.
stopifnot(
  nrow(pain_ancova_trial) == PAIN_ANCOVA_CASE_N,
  all(pain_ancova_trial$baseline_pain >= 0 & pain_ancova_trial$baseline_pain <= 100),
  all(pain_ancova_trial$week12_pain >= 0 & pain_ancova_trial$week12_pain <= 100),
  !anyNA(pain_ancova_trial)
)

# Persist only when this script is executed directly.
if (sys.nframe() == 0L) {
  dir.create("data", showWarnings = FALSE)
  save(pain_ancova_trial, file = "data/pain_ancova_trial.rda", version = 2,
       compress = "xz")
}
