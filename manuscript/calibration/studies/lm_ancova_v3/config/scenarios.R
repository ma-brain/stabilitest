# Isolated ANCOVA v3 Track E (violation-detection) scenario contract.
# Phase 1 only: clean clear cells + matched violations + diagnostic null pairs.
# Track D seed ranges 51001+/52001+/53001+ remain reserved and unused.

.LM_ANCOVA_V3_VIOLATIONS <- c(
  "allocation_2to1",
  "heteroscedastic",
  "heavy_tails",
  "missing_baseline",
  "interaction"
)

.lm_ancova_v3_clean_id <- function(n, truth_class) {
  sprintf(
    "lm_ancova_v3_clean_n%d_r2_40_%s",
    as.integer(n),
    as.character(truth_class)
  )
}

.lm_ancova_v3_viol_id <- function(n, truth_class, violation_type) {
  sprintf(
    "lm_ancova_v3_viol_n%d_r2_40_%s_%s",
    as.integer(n),
    as.character(truth_class),
    as.character(violation_type)
  )
}

.lm_ancova_v3_generator <- function(n, truth_class, violation_type = NULL) {
  target_power <- if (identical(as.character(truth_class), "null")) {
    0
  } else if (identical(as.character(truth_class), "clear")) {
    0.90
  } else {
    stop(sprintf("unsupported Track E truth class: %s", truth_class), call. = FALSE)
  }
  generator <- list(
    n = as.integer(n),
    baseline_r2 = 0.40,
    target_power = target_power,
    allocation = 0.5,
    residual_sd = 1,
    effect_direction = 1
  )
  if (!is.null(violation_type) && nzchar(as.character(violation_type))) {
    vt <- as.character(violation_type)
    generator$stress <- vt
    # Nominal allocation stays 0.5 for all cells (identical clean-solved effect).
    # The adapter applies 2:1 sampling for allocation_2to1 at generate time.
    if (identical(vt, "missing_baseline")) {
      generator$missing_rate <- 0.10
    }
  }
  generator
}

.lm_ancova_v3_scenario_row <- function(scenario_id, design_layer, truth_class,
                                       sample_size, scenario_seed, generator,
                                       screening_target = 100L,
                                       diagnostic_only = FALSE,
                                       violation_type = NULL,
                                       matched_clean_id = NULL) {
  params <- list(
    generator = generator,
    analysis = list(
      formula = "outcome ~ treatment + baseline",
      term = "treatmentB",
      alpha = 0.05
    ),
    screening = list(
      conclusions = "significant",
      target_n = as.integer(screening_target)
    ),
    diagnostic_only = isTRUE(diagnostic_only)
  )
  if (!is.null(violation_type) && nzchar(as.character(violation_type))) {
    params$violation_type <- as.character(violation_type)
  }
  if (!is.null(matched_clean_id) && nzchar(as.character(matched_clean_id))) {
    params$matched_clean_id <- as.character(matched_clean_id)
  }

  tibble::tibble(
    scenario_id = scenario_id,
    analysis_engine = "lm",
    calibration_family = "linear_model",
    calibration_unit = "lm_ancova_v3",
    endpoint = "coefficient",
    design_layer = design_layer,
    data_generator = "generate_lm_ancova",
    primary_adapter = "ancova_primary_decision",
    robustness_adapter = "robustness_lm",
    truth_class = truth_class,
    target_conclusion = "significant",
    sample_size = as.integer(sample_size),
    n_boot = 1000L,
    max_removal_pct = 0.30,
    training_split = 0.70,
    scenario_seed = as.integer(scenario_seed),
    parameters = list(params)
  )
}

.lm_ancova_v3_track_e_rows <- function(seed_start = 54001L) {
  seed <- as.integer(seed_start)
  rows <- list()

  # Primary: 3 clean clear + 15 matched violated clear (design_layer = core).
  for (n in c(40L, 80L, 160L)) {
    clean_id <- .lm_ancova_v3_clean_id(n, "clear")
    rows[[length(rows) + 1L]] <- .lm_ancova_v3_scenario_row(
      scenario_id = clean_id,
      design_layer = "core",
      truth_class = "clear",
      sample_size = n,
      scenario_seed = seed,
      generator = .lm_ancova_v3_generator(n, "clear"),
      screening_target = 100L,
      diagnostic_only = FALSE
    )
    seed <- seed + 1L

    for (vt in .LM_ANCOVA_V3_VIOLATIONS) {
      rows[[length(rows) + 1L]] <- .lm_ancova_v3_scenario_row(
        scenario_id = .lm_ancova_v3_viol_id(n, "clear", vt),
        design_layer = "core",
        truth_class = "clear",
        sample_size = n,
        scenario_seed = seed,
        generator = .lm_ancova_v3_generator(n, "clear", vt),
        screening_target = 100L,
        diagnostic_only = FALSE,
        violation_type = vt,
        matched_clean_id = clean_id
      )
      seed <- seed + 1L
    }
  }

  # Diagnostic null pairs at n = 80 only (design_layer = stress; no gate).
  n_diag <- 80L
  clean_null_id <- .lm_ancova_v3_clean_id(n_diag, "null")
  rows[[length(rows) + 1L]] <- .lm_ancova_v3_scenario_row(
    scenario_id = clean_null_id,
    design_layer = "stress",
    truth_class = "null",
    sample_size = n_diag,
    scenario_seed = seed,
    generator = .lm_ancova_v3_generator(n_diag, "null"),
    screening_target = 50L,
    diagnostic_only = TRUE
  )
  seed <- seed + 1L

  for (vt in .LM_ANCOVA_V3_VIOLATIONS) {
    rows[[length(rows) + 1L]] <- .lm_ancova_v3_scenario_row(
      scenario_id = .lm_ancova_v3_viol_id(n_diag, "null", vt),
      design_layer = "stress",
      truth_class = "null",
      sample_size = n_diag,
      scenario_seed = seed,
      generator = .lm_ancova_v3_generator(n_diag, "null", vt),
      screening_target = 50L,
      diagnostic_only = TRUE,
      violation_type = vt,
      matched_clean_id = clean_null_id
    )
    seed <- seed + 1L
  }

  dplyr::bind_rows(rows)
}

#' Return the isolated ANCOVA v3 Track E scenario contract.
#'
#' Primary cells are clean clear + five matched violations at each n in
#' {40, 80, 160} with baseline R^2 = 0.40 and clear power 0.90. Diagnostic
#' null pairs at n = 80 are stress-layer only and carry no gate.
lm_ancova_v3_scenarios <- function() {
  .lm_ancova_v3_track_e_rows(seed_start = 54001L)
}
