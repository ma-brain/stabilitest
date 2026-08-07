# Prospectively frozen synthetic oncology responder-trial illustration.
#
# This dataset is generated once from the constants below and is excluded from
# every calibration training, candidate-selection, and held-out validation step.
# The generator rates (control 0.20, active 0.45) are a clinical-plausibility
# choice frozen BEFORE any p-value, score, or categorical band was inspected.
# Do not regenerate or select it by p-value, score, or band.

ONC_RESPONSE_CASE_SEED <- 20260809L
ONC_RESPONSE_CASE_N <- 120L
ONC_RESPONSE_CONTROL_RATE <- 0.20
ONC_RESPONSE_ACTIVE_RATE <- 0.45

generate_onc_response_trial <- function(seed = ONC_RESPONSE_CASE_SEED) {
  set.seed(seed)
  # 60 per arm; assignment is deterministic (first 60 Active, next 60 Placebo)
  # so the frozen data has a stable, inspectable arm ordering independent of
  # the RNG stream.  Response draws use the frozen seed.
  n_per_arm <- ONC_RESPONSE_CASE_N / 2L
  active <- stats::rbinom(n_per_arm, 1L, ONC_RESPONSE_ACTIVE_RATE)
  placebo <- stats::rbinom(n_per_arm, 1L, ONC_RESPONSE_CONTROL_RATE)
  data.frame(
    subject_id = sprintf("ONC-%03d", seq_len(ONC_RESPONSE_CASE_N)),
    arm = factor(
      c(rep("Active", n_per_arm), rep("Placebo", n_per_arm)),
      levels = c("Placebo", "Active")
    ),
    response = as.integer(c(active, placebo)),
    stringsAsFactors = FALSE
  )
}

onc_response_trial <- generate_onc_response_trial()

# Plausibility gate before packaging. These check structural plausibility only
# (arm sizes, binary responses, no missingness, unique IDs) - never the p-value,
# score, or band. Revise the DGP/seed only if this fails, and never after
# inspecting the worked robustness analysis.
stopifnot(
  nrow(onc_response_trial) == ONC_RESPONSE_CASE_N,
  length(unique(onc_response_trial$subject_id)) == ONC_RESPONSE_CASE_N,
  all(onc_response_trial$response %in% c(0L, 1L)),
  !anyNA(onc_response_trial),
  as.integer(table(onc_response_trial$arm))[[1]] == 60L,
  as.integer(table(onc_response_trial$arm))[[2]] == 60L
)

# Persist only when this script is executed directly.
if (sys.nframe() == 0L) {
  dir.create("data", showWarnings = FALSE)
  save(onc_response_trial, file = "data/onc_response_trial.rda", version = 2,
       compress = "xz")
}
