# Isolated ANCOVA v2 calibration scenario contract.
# Fitting strata are null + clear; borderline is diagnostic-only.
# Clear default target power is 0.90; SAP may freeze 0.95 after pilot.

.lm_ancova_v2_truth_power <- function(truth_class, clear_target_power = 0.90) {
  clear_target_power <- as.numeric(clear_target_power)
  if (!clear_target_power %in% c(0.90, 0.95)) {
    stop(
      "clear_target_power must be 0.90 (default) or 0.95 (SAP freeze)",
      call. = FALSE
    )
  }
  switch(
    as.character(truth_class),
    null = 0,
    borderline = 0.60,
    clear = clear_target_power,
    stop(sprintf("unknown ANCOVA v2 truth class: %s", truth_class), call. = FALSE)
  )
}

.lm_ancova_v2_scenario_row <- function(scenario_id, design_layer, truth_class,
                                       sample_size, scenario_seed, generator,
                                       screening_target = 100L,
                                       diagnostic_only = FALSE) {
  tibble::tibble(
    scenario_id = scenario_id,
    analysis_engine = "lm",
    calibration_family = "linear_model",
    calibration_unit = "lm_ancova_v2",
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
    parameters = list(list(
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
    ))
  )
}

.lm_ancova_v2_grid_rows <- function(grid, design_layer, seed_start,
                                    clear_target_power = 0.90) {
  rows <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    n <- as.integer(grid$n[[i]])
    baseline_r2 <- as.numeric(grid$baseline_r2[[i]])
    truth_class <- as.character(grid$truth_class[[i]])
    target_power <- .lm_ancova_v2_truth_power(
      truth_class,
      clear_target_power = clear_target_power
    )
    scenario_id <- sprintf(
      "lm_ancova_v2_%s_n%d_r2_%02d_%s",
      design_layer,
      n,
      as.integer(round(100 * baseline_r2)),
      truth_class
    )
    rows[[i]] <- .lm_ancova_v2_scenario_row(
      scenario_id = scenario_id,
      design_layer = design_layer,
      truth_class = truth_class,
      sample_size = n,
      scenario_seed = seed_start + i - 1L,
      generator = list(
        n = n,
        baseline_r2 = baseline_r2,
        target_power = target_power,
        allocation = 0.5,
        residual_sd = 1,
        effect_direction = 1
      ),
      diagnostic_only = identical(truth_class, "borderline")
    )
  }
  dplyr::bind_rows(rows)
}

.lm_ancova_v2_stress_rows <- function(seed_start, clear_target_power = 0.90) {
  # clear_target_power unused for stress (borderline only) but kept for API symmetry.
  force(clear_target_power)
  specs <- list(
    list(
      id = "lm_ancova_v2_stress_allocation_2to1",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 2 / 3, residual_sd = 1, effect_direction = 1,
        stress = "allocation_2to1"
      )
    ),
    list(
      id = "lm_ancova_v2_stress_heteroscedastic",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "heteroscedastic"
      )
    ),
    list(
      id = "lm_ancova_v2_stress_heavy_tails",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "heavy_tails"
      )
    ),
    list(
      id = "lm_ancova_v2_stress_missing_baseline",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "missing_baseline", missing_rate = 0.10
      )
    ),
    list(
      id = "lm_ancova_v2_stress_nonlinear_baseline",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "nonlinear_baseline"
      )
    ),
    list(
      id = "lm_ancova_v2_stress_interaction",
      truth = "borderline",
      n = 120L,
      generator = list(
        n = 120L, baseline_r2 = 0.40, target_power = 0.60,
        allocation = 0.5, residual_sd = 1, effect_direction = 1,
        stress = "interaction"
      )
    )
  )

  rows <- vector("list", length(specs))
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    rows[[i]] <- .lm_ancova_v2_scenario_row(
      scenario_id = spec$id,
      design_layer = "stress",
      truth_class = spec$truth,
      sample_size = spec$n,
      scenario_seed = seed_start + i - 1L,
      generator = spec$generator,
      diagnostic_only = TRUE
    )
  }
  dplyr::bind_rows(rows)
}

#' Return the isolated ANCOVA v2 calibration scenario contract.
#'
#' Training uses the core layer; held-out evaluation uses validation only.
#' Borderline rows are diagnostic-only and never enter cutoff fitting.
#' Clear target power defaults to 0.90; pass 0.95 only after a SAP freeze.
lm_ancova_v2_scenarios <- function(clear_target_power = 0.90) {
  training <- expand.grid(
    n = c(40L, 80L, 160L),
    baseline_r2 = c(0.10, 0.40, 0.70),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  validation <- expand.grid(
    n = c(60L, 120L, 240L),
    baseline_r2 = c(0.25, 0.55),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(
    .lm_ancova_v2_grid_rows(
      training, "core", seed_start = 41001L,
      clear_target_power = clear_target_power
    ),
    .lm_ancova_v2_grid_rows(
      validation, "validation", seed_start = 42001L,
      clear_target_power = clear_target_power
    ),
    .lm_ancova_v2_stress_rows(
      seed_start = 43001L,
      clear_target_power = clear_target_power
    )
  )
}
