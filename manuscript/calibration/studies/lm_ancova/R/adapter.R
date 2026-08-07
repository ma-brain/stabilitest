# Thin ANCOVA screening/robustness adapter for the isolated study.

ancova_primary_decision <- function(data, scenario = list(), term = NULL,
                                    formula = NULL, alpha = NULL) {
  primary_decision_lm(
    data = data,
    scenario = scenario,
    term = term,
    formula = formula,
    alpha = alpha
  )
}

.run_ancova_robustness <- function(data, scenario = list(), n_boot = NULL,
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
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)
  )
  if (!is.null(seed)) args$seed <- seed
  extra <- list(...)
  if (length(extra)) args <- utils::modifyList(args, extra)
  do.call(stabilitest::robustness_lm, args)
}

lm_ancova_adapter <- function() {
  list(
    generate = generate_lm_ancova,
    generate_data = generate_lm_ancova,
    primary_decision = ancova_primary_decision,
    run_robustness = .run_ancova_robustness,
    robustness_analysis = .run_ancova_robustness
  )
}
