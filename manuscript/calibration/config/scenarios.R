# Frozen analysis-specific calibration scenarios.

#' Return the calibration scenario contract.
#'
#' Each row is a reproducible smoke scenario.  Generator-specific settings are
#' kept in the `parameters` list-column so that the tabular contract remains
#' stable as scenario families grow.
#'
#' @return A tibble with the frozen calibration schema.
#' @export
calibration_scenarios <- function() {
  scenarios <- tibble::tibble(
    scenario_id = c(
      "two_sample_smoke", "proportion_smoke", "lm_smoke", "binomial_smoke",
      "poisson_smoke", "cox_smoke", "tost_smoke"
    ),
    analysis_family = c(
      "two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost"
    ),
    endpoint = c(
      "mean_difference", "risk_difference", "coefficient", "odds_ratio",
      "rate_ratio", "hazard_ratio", "equivalence"
    ),
    design_layer = c("core", "core", "stress", "stress", "validation", "validation", "core"),
    data_generator = c(
      "generate_two_sample", "generate_proportion", "generate_lm",
      "generate_binomial", "generate_poisson", "generate_cox", "generate_tost"
    ),
    primary_adapter = c(
      "robustness_analysis", "robustness_analysis", "robustness_lm",
      "robustness_glm", "robustness_glm", "robustness_surv", "robustness_tost"
    ),
    robustness_adapter = c(
      "t.test", "prop.test", "term_test", "term_test", "term_test",
      "coxph", "tost_mean"
    ),
    truth_class = c("null", "clear", "clear", "borderline", "clear", "borderline", "clear"),
    target_conclusion = c(
      "non_significant", "significant", "significant", "significant",
      "significant", "significant", "equivalent"
    ),
    sample_size = c(50L, 80L, 80L, 120L, 120L, 160L, 80L),
    n_boot = rep(1000L, 7L),
    max_removal_pct = rep(0.30, 7L),
    training_split = rep(0.7, 7L),
    scenario_seed = c(1101L, 1201L, 1301L, 1401L, 1501L, 1601L, 1701L),
    parameters = list(
      list(
        generator = list(n_per_group = 25L, effect_size = 0, sd = 1),
        analysis = list(test_type = "t.test", alpha = 0.05)
      ),
      list(
        generator = list(n_per_group = 40L, probability_control = 0.4, probability_treatment = 0.65),
        analysis = list(test_type = "prop", alpha = 0.05)
      ),
      list(
        generator = list(n = 80L, effect_size = 0.5, noise_sd = 1, covariate_effect = 0.3),
        analysis = list(term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 120L, baseline_log_odds = -1, treatment_log_odds = 0.8),
        analysis = list(family = "binomial", link = "logit", term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 120L, baseline_log_rate = 0.2, treatment_log_rate = 0.35),
        analysis = list(family = "poisson", link = "log", term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 160L, hazard_ratio = 1.5, censoring_rate = 0.2),
        analysis = list(term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n_per_group = 40L, mean_difference = 0, equivalence_margin = 0.5),
        analysis = list(endpoint = "mean", margin = 0.5, alpha = 0.05)
      )
    )
  ) |>
    tibble::add_row(
      scenario_id = "tost_prop_equivalence", analysis_family = "tost", endpoint = "risk_difference",
      design_layer = "validation", data_generator = "generate_tost", primary_adapter = "robustness_tost",
      robustness_adapter = "tost_prop", truth_class = "clear", target_conclusion = "equivalent",
      sample_size = 80L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
      scenario_seed = 1901L,
      parameters = list(list(generator = list(n_per_group = 40L, endpoint = "prop",
                                              type = "equivalence", delta_L = -.15, delta_U = .15,
                                              probability_control = .5, probability_treatment = .5),
                              analysis = list(endpoint = "prop", type = "equivalence",
                                              delta_L = -.15, delta_U = .15, alpha = .05))),
      .before = Inf
    ) |>
    tibble::add_row(
      scenario_id = "tost_or_noninferiority", analysis_family = "tost", endpoint = "odds_ratio",
      design_layer = "stress", data_generator = "generate_tost", primary_adapter = "robustness_tost",
      robustness_adapter = "tost_or", truth_class = "clear", target_conclusion = "noninferior",
      sample_size = 80L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
      scenario_seed = 2001L,
      parameters = list(list(generator = list(n_per_group = 40L, endpoint = "or",
                                              type = "noninferiority", margin = 1.5,
                                              odds_ratio = 1.2),
                              analysis = list(endpoint = "or", type = "noninferiority",
                                              margin = 1.5, higher_is_better = TRUE, alpha = .05))),
      .before = Inf
    )
  # Endpoint and stress coverage for the model-specific adapters.  The original
  # seven smoke IDs remain frozen; these rows add Weibull/non-PH Cox and binary
  # TOST/NI checks without changing the common artifact schema.
  tibble::add_row(
    scenarios,
    scenario_id = "cox_weibull_stress", analysis_family = "cox", endpoint = "hazard_ratio",
    design_layer = "stress", data_generator = "generate_cox", primary_adapter = "robustness_surv",
    robustness_adapter = "coxph", truth_class = "borderline", target_conclusion = "significant",
    sample_size = 160L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
    scenario_seed = 1801L,
    parameters = list(list(generator = list(n = 160L, hazard_ratio = 1.5,
                                            distribution = "weibull", shape = 1.3,
                                            censoring_rate = .2, time_varying_effect = TRUE),
                            analysis = list(term = "treatment", alpha = .05))),
    .before = Inf
  )
  
}
