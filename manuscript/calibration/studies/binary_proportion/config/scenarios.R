# Isolated binary-proportion calibration scenario contract (Phase 1: fisher_exact).
#
# Frozen constants (do not alter):
#   - Unit: fisher_exact only in Phase 1; chi_square_2x2 / two_sample_prop are
#     deferred to their own phases and never appear here.
#   - Training grid: n/arm {25, 50, 100, 200} x p0 {0.10, 0.25, 0.50} x truth
#     {null, borderline, clear} = 36 scenarios.
#   - Held-out grid: n/arm {35, 75, 150} x p0 {0.15, 0.40} x same truth = 18.
#   - Truth targets: null exact; borderline exact power 0.60 (diagnostic only);
#     clear exact power 0.95, solved against the enumerated exact power of
#     fisher.test (not a normal approximation).
#   - Seeds: training 61001+, validation 62001+, stress 63001+. Disjoint from
#     Welch, lm_ancova (31001+/32001+/33001+), and the replication/cluster
#     bootstrap masters (20260808).

.binary_proportion_truth_power <- function(truth_class) {
  switch(
    as.character(truth_class),
    null = 0,
    borderline = 0.60,
    clear = 0.95,
    stop(sprintf("unknown binary_proportion truth class: %s", truth_class),
         call. = FALSE)
  )
}

.binary_proportion_scenario_row <- function(scenario_id, design_layer,
                                             truth_class, n_per_arm, p0,
                                             scenario_seed, generator,
                                             screening_target = 100L) {
  target_power <- .binary_proportion_truth_power(truth_class)
  diagnostic_only <- identical(truth_class, "borderline")
  tibble::tibble(
    scenario_id = scenario_id,
    analysis_engine = "proportion",
    calibration_family = "binary_proportion",
    calibration_unit = "fisher_exact",
    endpoint = "risk_difference",
    design_layer = design_layer,
    data_generator = "generate_binary_proportion",
    primary_adapter = "fisher_exact_primary_decision",
    robustness_adapter = "robustness_analysis",
    truth_class = truth_class,
    target_conclusion = "significant",
    sample_size = as.integer(2L * n_per_arm),
    n_boot = 1000L,
    max_removal_pct = 0.30,
    training_split = 0.70,
    scenario_seed = as.integer(scenario_seed),
    parameters = list(list(
      generator = generator,
      analysis = list(
        test_type = "fisher",
        alpha = 0.05,
        weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
      ),
      screening = list(
        conclusions = "significant",
        target_n = as.integer(screening_target)
      ),
      # Carried on the row so the SAP freeze and downstream modules can read
      # the frozen truth target and diagnostic flag without recomputation.
      truth = list(
        target_power = target_power,
        solve_exact_power = TRUE,
        diagnostic_only = diagnostic_only
      )
    ))
  )
}

.binary_proportion_grid_rows <- function(grid, design_layer, seed_start) {
  rows <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    n_per_arm <- as.integer(grid$n[[i]])
    p0 <- as.numeric(grid$p0[[i]])
    truth_class <- as.character(grid$truth_class[[i]])
    target_power <- .binary_proportion_truth_power(truth_class)
    p0_tag <- sub("^0\\.", "", sprintf("%.2f", p0))
    scenario_id <- sprintf(
      "fisher_exact_%s_n%d_p0_%s_%s",
      design_layer, n_per_arm, p0_tag, truth_class
    )
    rows[[i]] <- .binary_proportion_scenario_row(
      scenario_id = scenario_id,
      design_layer = design_layer,
      truth_class = truth_class,
      n_per_arm = n_per_arm,
      p0 = p0,
      scenario_seed = seed_start + i - 1L,
      generator = list(
        n_per_arm = n_per_arm,
        p0 = p0,
        target_power = target_power,
        solve_exact_power = TRUE,
        diagnostic_only = identical(truth_class, "borderline"),
        allocation = 0.5,
        effect_direction = 1
      )
    )
  }
  dplyr::bind_rows(rows)
}

# Stress rows are diagnostic only: never fit, never accept.  Each carries a
# distinct generator switch (Task 3 implements the switches); the row contract
# is frozen here so the SAP can cite the exact set.
.binary_proportion_stress_rows <- function(seed_start) {
  specs <- list(
    list(
      id = "fisher_exact_stress_allocation_2to1",
      truth = "borderline",
      n_per_arm = 100L,
      p0 = 0.25,
      generator = list(
        n_per_arm = 100L, p0 = 0.25, target_power = 0.60,
        solve_exact_power = TRUE, diagnostic_only = TRUE,
        allocation = 2 / 3, effect_direction = 1,
        stress = "allocation_2to1"
      )
    ),
    list(
      id = "fisher_exact_stress_rare_events",
      truth = "borderline",
      n_per_arm = 100L,
      p0 = 0.03,
      generator = list(
        n_per_arm = 100L, p0 = 0.03, target_power = 0.60,
        solve_exact_power = TRUE, diagnostic_only = TRUE,
        allocation = 0.5, effect_direction = 1,
        stress = "rare_events"
      )
    ),
    list(
      id = "fisher_exact_stress_misclassification",
      truth = "borderline",
      n_per_arm = 100L,
      p0 = 0.25,
      generator = list(
        n_per_arm = 100L, p0 = 0.25, target_power = 0.60,
        solve_exact_power = TRUE, diagnostic_only = TRUE,
        allocation = 0.5, effect_direction = 1,
        stress = "misclassification", misclassification_rate = 0.05
      )
    ),
    list(
      id = "fisher_exact_stress_overdispersion",
      truth = "borderline",
      n_per_arm = 100L,
      p0 = 0.25,
      generator = list(
        n_per_arm = 100L, p0 = 0.25, target_power = 0.60,
        solve_exact_power = TRUE, diagnostic_only = TRUE,
        allocation = 0.5, effect_direction = 1,
        stress = "overdispersion", overdispersion_rho = 0.10
      )
    ),
    list(
      id = "fisher_exact_stress_missing_outcomes",
      truth = "borderline",
      n_per_arm = 100L,
      p0 = 0.25,
      generator = list(
        n_per_arm = 100L, p0 = 0.25, target_power = 0.60,
        solve_exact_power = TRUE, diagnostic_only = TRUE,
        allocation = 0.5, effect_direction = 1,
        stress = "missing_outcomes", missing_rate = 0.10
      )
    )
  )

  rows <- vector("list", length(specs))
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    rows[[i]] <- .binary_proportion_scenario_row(
      scenario_id = spec$id,
      design_layer = "stress",
      truth_class = spec$truth,
      n_per_arm = spec$n_per_arm,
      p0 = spec$p0,
      scenario_seed = seed_start + i - 1L,
      generator = spec$generator
    )
  }
  dplyr::bind_rows(rows)
}

#' Return the isolated binary-proportion (Phase 1: fisher_exact) scenario contract.
#'
#' Training uses the core layer; held-out evaluation uses validation only.
#' Stress rows are diagnostic and never enter cutoff fitting or acceptance.
#' Phase 2/3 units (chi_square_2x2, two_sample_prop) are deferred to their own
#' studies; only fisher_exact appears here.
binary_proportion_scenarios <- function() {
  training <- expand.grid(
    n = c(25L, 50L, 100L, 200L),
    p0 = c(0.10, 0.25, 0.50),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  validation <- expand.grid(
    n = c(35L, 75L, 150L),
    p0 = c(0.15, 0.40),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  dplyr::bind_rows(
    .binary_proportion_grid_rows(training, "core", seed_start = 61001L),
    .binary_proportion_grid_rows(validation, "validation", seed_start = 62001L),
    .binary_proportion_stress_rows(seed_start = 63001L)
  )
}
