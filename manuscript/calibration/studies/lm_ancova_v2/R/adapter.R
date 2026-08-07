# Thin ANCOVA v2 screening/robustness adapter for the isolated study.
# Track A score uses jackknife-light weights; the locked v1 composite is
# archived as a comparator and is never the fitting score.

.lm_ancova_v2_score_weights <- c(
  jackknife = 0, fragility = 0.5, bootstrap = 0.5
)

.lm_ancova_v1_comparator_weights <- c(
  jackknife = 0.4, fragility = 0.4, bootstrap = 0.2
)

.lm_ancova_v2_assemble_score <- function(s_jack, frag_comp, s_boot, weights) {
  weights[["jackknife"]] * s_jack +
    weights[["fragility"]] * frag_comp +
    weights[["bootstrap"]] * s_boot
}

.lm_ancova_v2_archive_v1_comparator <- function(result) {
  metrics <- result$metrics %||% result$robustness_metrics
  if (is.null(metrics)) {
    stop("robustness result is missing metrics for v1 comparator", call. = FALSE)
  }
  result$v1_comparator_score <- .lm_ancova_v2_assemble_score(
    metrics$jackknife_conclusion_stability,
    metrics$worstcase_fragility_component,
    metrics$bootstrap_reproducibility,
    .lm_ancova_v1_comparator_weights
  )
  result
}

ancova_v2_primary_decision <- function(data, scenario = list(), term = NULL,
                                       formula = NULL, alpha = NULL) {
  primary_decision_lm(
    data = data,
    scenario = scenario,
    term = term,
    formula = formula,
    alpha = alpha
  )
}

.run_ancova_v2_robustness <- function(data, scenario = list(), n_boot = NULL,
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
    weights = .lm_ancova_v2_score_weights
  )
  if (!is.null(seed)) args$seed <- seed
  extra <- list(...)
  if (length(extra)) args <- utils::modifyList(args, extra)
  result <- do.call(stabilitest::robustness_lm, args)
  .lm_ancova_v2_archive_v1_comparator(result)
}

lm_ancova_v2_adapter <- function() {
  list(
    generate = generate_lm_ancova,
    generate_data = generate_lm_ancova,
    primary_decision = ancova_v2_primary_decision,
    run_robustness = .run_ancova_v2_robustness,
    robustness_analysis = .run_ancova_v2_robustness
  )
}
