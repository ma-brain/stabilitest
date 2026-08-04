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
  tibble::tibble(
    scenario_id = c(
      "two_sample_smoke", "proportion_smoke", "lm_smoke", "binomial_smoke",
      "poisson_smoke", "cox_smoke", "tost_smoke",
      "lm_core_ancova", "lm_stress_heteroscedastic_missing", "lm_validation_multidf",
      "binomial_core_logit", "binomial_stress_separation", "binomial_validation_multidf",
      "poisson_core_offset", "poisson_stress_overdispersion", "poisson_validation_multidf"
    ),
    analysis_family = c(
      "two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost",
      "lm", "lm", "lm", "binomial", "binomial", "binomial",
      "poisson", "poisson", "poisson"
    ),
    endpoint = c(
      "mean_difference", "risk_difference", "coefficient", "odds_ratio",
      "rate_ratio", "hazard_ratio", "equivalence", "coefficient", "coefficient",
      "coefficient", "odds_ratio", "odds_ratio", "odds_ratio", "rate_ratio",
      "rate_ratio", "rate_ratio"
    ),
    design_layer = c("core", "core", "stress", "stress", "validation", "validation", "core",
                     "core", "stress", "validation", "core", "stress", "validation",
                     "core", "stress", "validation"),
    data_generator = c(
      "generate_two_sample", "generate_proportion", "generate_lm",
      "generate_binomial", "generate_poisson", "generate_cox", "generate_tost",
      "generate_lm", "generate_lm", "generate_lm", "generate_binomial",
      "generate_binomial", "generate_binomial", "generate_poisson", "generate_poisson",
      "generate_poisson"
    ),
    primary_adapter = c(
      "robustness_analysis", "robustness_analysis", "robustness_lm",
      "robustness_glm", "robustness_glm", "robustness_surv", "robustness_tost",
      "robustness_lm", "robustness_lm", "robustness_lm", "robustness_glm",
      "robustness_glm", "robustness_glm", "robustness_glm", "robustness_glm",
      "robustness_glm"
    ),
    robustness_adapter = c(
      "t.test", "prop.test", "term_test", "term_test", "term_test",
      "coxph", "tost_mean", "term_test", "term_test", "term_test", "term_test",
      "term_test", "term_test", "term_test", "term_test", "term_test"
    ),
    truth_class = c("null", "clear", "clear", "borderline", "clear", "borderline", "clear",
                    "clear", "borderline", "clear", "clear", "borderline", "clear",
                    "clear", "borderline", "clear"),
    target_conclusion = c(
      "non_significant", "significant", "significant", "significant",
      "significant", "significant", "equivalent", "significant", "significant",
      "significant", "significant", "significant", "significant", "significant",
      "significant", "significant"
    ),
    sample_size = c(50L, 80L, 80L, 120L, 120L, 160L, 80L,
                    90L, 100L, 110L, 100L, 120L, 130L, 100L, 120L, 140L),
    n_boot = rep(1000L, 16L),
    max_removal_pct = rep(0.30, 16L),
    training_split = rep(0.7, 16L),
    scenario_seed = c(1101L, 1201L, 1301L, 1401L, 1501L, 1601L, 1701L,
                      1311L, 1312L, 1313L, 1411L, 1412L, 1413L, 1511L, 1512L, 1513L),
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
      ),
      list(
        generator = list(n = 90L, effect = 0.6, covariate_effect = 0.6,
                         prognostic = TRUE, imbalance = 0.15),
        analysis = list(term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 100L, effect = 0.5, covariate_effect = 0.4,
                         imbalance = 0.35, heteroscedastic = TRUE,
                         missing_rate = 0.10),
        analysis = list(term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 110L, effect = 0.5, covariate_effect = 0.5,
                         factor_levels = 3L, imbalance = 0.20),
        analysis = list(term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 100L, prevalence = 0.35, effect = log(2),
                         covariate_effect = 0.35, imbalance = 0.15),
        analysis = list(family = "binomial", link = "logit", term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 120L, prevalence = 0.10, effect = log(3),
                         separation = TRUE, imbalance = 0.30),
        analysis = list(family = "binomial", link = "logit", term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 130L, prevalence = 0.40, effect = log(1.5),
                         factor_levels = 3L, covariate_effect = 0.25),
        analysis = list(family = "binomial", link = "logit", term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 100L, rate = 0.8, effect = log(1.5),
                         exposure = TRUE, covariate_effect = 0.2),
        analysis = list(family = "poisson", link = "log", term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 120L, rate = 0.5, effect = log(1.4),
                         exposure = TRUE, overdispersion = TRUE, imbalance = 0.25),
        analysis = list(family = "poisson", link = "log", term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 140L, rate = 0.8, effect = log(1.8), exposure = TRUE,
                         factor_levels = 3L, covariate_effect = 0.2),
        analysis = list(family = "poisson", link = "log", term = "treatment", alpha = 0.05)
      )
    )
  )
}
