# Prospective Welch calibration scenario contract.
#
# This file is frozen before any robustness score is produced. Training and
# validation rows use disjoint scenario identities and seed ranges. Stress
# rows are diagnostic only and never enter fitting or acceptance decisions.

.welch_target_power <- function(truth) {
  switch(
    as.character(truth),
    null = 0,
    borderline = 0.60,
    clear = 0.95,
    stop(sprintf("unknown Welch truth stratum: %s", truth), call. = FALSE)
  )
}

.welch_scenario_row <- function(scenario_id, archetype_id, design_layer,
                                study_role, truth, distribution,
                                n_group1, n_group2, sd_group1, sd_group2,
                                seed, contamination_fraction = 0,
                                contamination_offset = 0) {
  data.frame(
    scenario_id = scenario_id,
    archetype_id = archetype_id,
    design_layer = design_layer,
    study_role = study_role,
    required_for_fit = identical(study_role, "candidate_fitting"),
    required_for_acceptance = identical(study_role, "confirmation"),
    truth = truth,
    distribution = distribution,
    n_group1 = as.integer(n_group1),
    n_group2 = as.integer(n_group2),
    sd_group1 = as.numeric(sd_group1),
    sd_group2 = as.numeric(sd_group2),
    delta = if (identical(truth, "null")) 0 else NA_real_,
    target_power = .welch_target_power(truth),
    alpha = 0.05,
    n_boot = 1000L,
    max_removal_pct = 0.30,
    replicate_quota = 250L,
    draw_cap = 20000L,
    seed = as.integer(seed),
    contamination_fraction = as.numeric(contamination_fraction),
    contamination_offset = as.numeric(contamination_offset),
    stringsAsFactors = FALSE
  )
}

#' Return the frozen prospective Welch calibration scenarios.
#'
#' Core rows are listed explicitly so their identities, order, and seeds do
#' not depend on expansion or sorting behavior. Non-null deltas are resolved
#' by the frozen power solver and written to a design artifact; scenario IDs
#' and seeds never change.
welch_scenarios <- function() {
  rows <- list(
    .welch_scenario_row("WTR-W01-null", "W01", "training", "candidate_fitting", "null", "normal", 25, 25, 1, 1, 202610001),
    .welch_scenario_row("WTR-W01-borderline", "W01", "training", "candidate_fitting", "borderline", "normal", 25, 25, 1, 1, 202610002),
    .welch_scenario_row("WTR-W01-clear", "W01", "training", "candidate_fitting", "clear", "normal", 25, 25, 1, 1, 202610003),
    .welch_scenario_row("WTR-W02-null", "W02", "training", "candidate_fitting", "null", "normal", 50, 50, 1, 2, 202610004),
    .welch_scenario_row("WTR-W02-borderline", "W02", "training", "candidate_fitting", "borderline", "normal", 50, 50, 1, 2, 202610005),
    .welch_scenario_row("WTR-W02-clear", "W02", "training", "candidate_fitting", "clear", "normal", 50, 50, 1, 2, 202610006),
    .welch_scenario_row("WTR-W03-null", "W03", "training", "candidate_fitting", "null", "normal", 100, 100, 2, 1, 202610007),
    .welch_scenario_row("WTR-W03-borderline", "W03", "training", "candidate_fitting", "borderline", "normal", 100, 100, 2, 1, 202610008),
    .welch_scenario_row("WTR-W03-clear", "W03", "training", "candidate_fitting", "clear", "normal", 100, 100, 2, 1, 202610009),
    .welch_scenario_row("WTR-W04-null", "W04", "training", "candidate_fitting", "null", "normal", 200, 200, 1, 1, 202610010),
    .welch_scenario_row("WTR-W04-borderline", "W04", "training", "candidate_fitting", "borderline", "normal", 200, 200, 1, 1, 202610011),
    .welch_scenario_row("WTR-W04-clear", "W04", "training", "candidate_fitting", "clear", "normal", 200, 200, 1, 1, 202610012),
    .welch_scenario_row("WTR-W05-null", "W05", "training", "candidate_fitting", "null", "normal", 40, 60, 1, 2, 202610013),
    .welch_scenario_row("WTR-W05-borderline", "W05", "training", "candidate_fitting", "borderline", "normal", 40, 60, 1, 2, 202610014),
    .welch_scenario_row("WTR-W05-clear", "W05", "training", "candidate_fitting", "clear", "normal", 40, 60, 1, 2, 202610015),
    .welch_scenario_row("WTR-W06-null", "W06", "training", "candidate_fitting", "null", "normal", 60, 40, 2, 1, 202610016),
    .welch_scenario_row("WTR-W06-borderline", "W06", "training", "candidate_fitting", "borderline", "normal", 60, 40, 2, 1, 202610017),
    .welch_scenario_row("WTR-W06-clear", "W06", "training", "candidate_fitting", "clear", "normal", 60, 40, 2, 1, 202610018),

    .welch_scenario_row("WVA-W01-null", "W01", "validation", "confirmation", "null", "normal", 25, 25, 1, 1, 202620001),
    .welch_scenario_row("WVA-W01-borderline", "W01", "validation", "confirmation", "borderline", "normal", 25, 25, 1, 1, 202620002),
    .welch_scenario_row("WVA-W01-clear", "W01", "validation", "confirmation", "clear", "normal", 25, 25, 1, 1, 202620003),
    .welch_scenario_row("WVA-W02-null", "W02", "validation", "confirmation", "null", "normal", 50, 50, 1, 2, 202620004),
    .welch_scenario_row("WVA-W02-borderline", "W02", "validation", "confirmation", "borderline", "normal", 50, 50, 1, 2, 202620005),
    .welch_scenario_row("WVA-W02-clear", "W02", "validation", "confirmation", "clear", "normal", 50, 50, 1, 2, 202620006),
    .welch_scenario_row("WVA-W03-null", "W03", "validation", "confirmation", "null", "normal", 100, 100, 2, 1, 202620007),
    .welch_scenario_row("WVA-W03-borderline", "W03", "validation", "confirmation", "borderline", "normal", 100, 100, 2, 1, 202620008),
    .welch_scenario_row("WVA-W03-clear", "W03", "validation", "confirmation", "clear", "normal", 100, 100, 2, 1, 202620009),
    .welch_scenario_row("WVA-W04-null", "W04", "validation", "confirmation", "null", "normal", 200, 200, 1, 1, 202620010),
    .welch_scenario_row("WVA-W04-borderline", "W04", "validation", "confirmation", "borderline", "normal", 200, 200, 1, 1, 202620011),
    .welch_scenario_row("WVA-W04-clear", "W04", "validation", "confirmation", "clear", "normal", 200, 200, 1, 1, 202620012),
    .welch_scenario_row("WVA-W05-null", "W05", "validation", "confirmation", "null", "normal", 40, 60, 1, 2, 202620013),
    .welch_scenario_row("WVA-W05-borderline", "W05", "validation", "confirmation", "borderline", "normal", 40, 60, 1, 2, 202620014),
    .welch_scenario_row("WVA-W05-clear", "W05", "validation", "confirmation", "clear", "normal", 40, 60, 1, 2, 202620015),
    .welch_scenario_row("WVA-W06-null", "W06", "validation", "confirmation", "null", "normal", 60, 40, 2, 1, 202620016),
    .welch_scenario_row("WVA-W06-borderline", "W06", "validation", "confirmation", "borderline", "normal", 60, 40, 2, 1, 202620017),
    .welch_scenario_row("WVA-W06-clear", "W06", "validation", "confirmation", "clear", "normal", 60, 40, 2, 1, 202620018),

    .welch_scenario_row("WST-T5-null", "ST-T5", "stress", "stress_reporting", "null", "student_t5", 50, 50, 1, 1, 202630001),
    .welch_scenario_row("WST-T5-clear", "ST-T5", "stress", "stress_reporting", "clear", "student_t5", 50, 50, 1, 1, 202630002),
    .welch_scenario_row("WST-LN-null", "ST-LN", "stress", "stress_reporting", "null", "centered_lognormal", 100, 100, 1, 1, 202630003),
    .welch_scenario_row("WST-LN-clear", "ST-LN", "stress", "stress_reporting", "clear", "centered_lognormal", 100, 100, 1, 1, 202630004),
    .welch_scenario_row("WST-GE-null", "ST-GE", "stress", "stress_reporting", "null", "gross_error_treatment", 50, 50, 1, 1, 202630005, 0.05, 8),
    .welch_scenario_row("WST-GE-clear", "ST-GE", "stress", "stress_reporting", "clear", "gross_error_treatment", 50, 50, 1, 1, 202630006, 0.05, 8)
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
