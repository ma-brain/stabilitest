# Thin ANCOVA v3 screening/robustness adapter for Track E.
# Score uses jackknife-light weights; the locked v1 composite is archived
# as a comparator and is never gated on.

.lm_ancova_v3_score_weights <- c(
  jackknife = 0, fragility = 0.5, bootstrap = 0.5
)

.lm_ancova_v1_comparator_weights <- c(
  jackknife = 0.4, fragility = 0.4, bootstrap = 0.2
)

.lm_ancova_v3_assemble_score <- function(s_jack, frag_comp, s_boot, weights) {
  weights[["jackknife"]] * s_jack +
    weights[["fragility"]] * frag_comp +
    weights[["bootstrap"]] * s_boot
}

.lm_ancova_v3_archive_v1_comparator <- function(result) {
  metrics <- result$metrics %||% result$robustness_metrics
  if (is.null(metrics)) {
    stop("robustness result is missing metrics for v1 comparator", call. = FALSE)
  }
  result$v1_comparator_score <- .lm_ancova_v3_assemble_score(
    metrics$jackknife_conclusion_stability,
    metrics$worstcase_fragility_component,
    metrics$bootstrap_reproducibility,
    .lm_ancova_v1_comparator_weights
  )
  result
}

.lm_ancova_v3_scenario_params <- function(scenario) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) {
      stop("ANCOVA v3 scenario must contain one row", call. = FALSE)
    }
    scenario <- as.list(scenario[1L, , drop = FALSE])
  }
  params <- scenario$parameters
  if (is.list(params) && length(params) == 1L && is.list(params[[1L]])) {
    params <- params[[1L]]
  }
  if (!is.list(params)) params <- list()
  params
}

.lm_ancova_v3_generator_params <- function(scenario) {
  params <- .lm_ancova_v3_scenario_params(scenario)
  generator <- params$generator %||% list()
  if (!is.list(generator)) generator <- list()
  generator
}

#' Generate ANCOVA data for Track E, preserving clean-solved effects under
#' allocation_2to1 (violation applied on top of identical nominal parameters).
generate_lm_ancova_v3 <- function(scenario = list(), seed = NULL, ...) {
  generator <- .lm_ancova_v3_generator_params(scenario)
  stress <- as.character(generator$stress %||% "")
  if (!identical(stress, "allocation_2to1")) {
    return(generate_lm_ancova(scenario, seed = seed, ...))
  }

  # Solve effect under clean nominal allocation = 0.5, then sample 2:1.
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- as.integer(generator$n %||% 80L)
  baseline_r2 <- as.numeric(generator$baseline_r2 %||% 0.40)
  target_power <- as.numeric(generator$target_power %||% 0)
  residual_sd <- as.numeric(generator$residual_sd %||% 1)
  effect_direction <- as.numeric(generator$effect_direction %||% 1)
  alloc_viol <- 2 / 3

  gamma <- sqrt(baseline_r2 / max(1e-12, 1 - baseline_r2))
  beta_abs <- solve_ancova_effect(
    n = n,
    target_power = target_power,
    alpha = 0.05,
    residual_sd = residual_sd,
    allocation = 0.5
  )
  beta <- effect_direction * beta_abs

  treatment <- .lm_ancova_balanced_treatment(n, allocation = alloc_viol)
  baseline <- stats::rnorm(n)
  treated <- as.integer(treatment == "B")
  mean_outcome <- beta * treated + gamma * baseline
  outcome <- mean_outcome + .lm_ancova_residual(
    n, residual_sd, stress = NULL, baseline = baseline, treatment = treatment
  )
  data <- data.frame(
    .row_id = seq_len(n),
    outcome = outcome,
    treatment = treatment,
    baseline = baseline,
    stringsAsFactors = FALSE
  )
  truth <- list(
    family = "lm_ancova",
    term = "treatmentB",
    effect = beta,
    beta = beta,
    gamma = gamma,
    baseline_r2 = baseline_r2,
    target_power = target_power,
    allocation = alloc_viol,
    residual_sd = residual_sd,
    effect_direction = effect_direction,
    stress = "allocation_2to1",
    missing_rate = 0,
    missing_row_ids = integer(),
    row_id = data$.row_id,
    target = if (isTRUE(all.equal(beta, 0))) "null" else "effect",
    nominal_allocation = 0.5
  )
  .truth_row(data, truth)
}

ancova_v3_primary_decision <- function(data, scenario = list(), term = NULL,
                                       formula = NULL, alpha = NULL) {
  primary_decision_lm(
    data = data,
    scenario = scenario,
    term = term,
    formula = formula,
    alpha = alpha
  )
}

.run_ancova_v3_robustness <- function(data, scenario = list(), n_boot = NULL,
                                      seed = NULL, ...) {
  data <- .model_input(data)$data
  a <- .model_analysis(scenario)
  formula <- .model_formula(data, scenario, a$formula, "lm")
  args <- list(
    formula = formula,
    data = data,
    term = a$term,
    alpha = a$alpha,
    n_boot = n_boot %||% 1000L,
    max_removal_pct = 0.30,
    weights = .lm_ancova_v3_score_weights
  )
  if (!is.null(seed)) args$seed <- seed
  extra <- list(...)
  if (length(extra)) args <- utils::modifyList(args, extra)
  result <- do.call(stabilitest::robustness_lm, args)
  .lm_ancova_v3_archive_v1_comparator(result)
}

lm_ancova_v3_adapter <- function() {
  list(
    generate = generate_lm_ancova_v3,
    generate_data = generate_lm_ancova_v3,
    primary_decision = ancova_v3_primary_decision,
    run_robustness = .run_ancova_v3_robustness,
    robustness_analysis = .run_ancova_v3_robustness
  )
}
