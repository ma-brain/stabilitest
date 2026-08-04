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
  base <- tibble::tibble(
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
  )

  # Two-sample calibration is stratified by truth class and design layer.  The
  # validation rows deliberately change generator parameters (distribution,
  # variance, and effect) rather than merely reserving a new random seed.
  expanded <- tibble::tibble(
    scenario_id = c(
      "two_sample_core_null_n20", "two_sample_core_moderate_n40",
      "two_sample_core_large_n80", "two_sample_stress_null_n20",
      "two_sample_stress_moderate_n40", "two_sample_stress_large_n80",
      "two_sample_validation_null_n30", "two_sample_validation_moderate_n60",
      "two_sample_validation_large_n100"
    ),
    analysis_family = rep("two_sample", 9L),
    endpoint = rep("mean_difference", 9L),
    design_layer = c(rep("core", 3L), rep("stress", 3L), rep("validation", 3L)),
    data_generator = rep("generate_two_sample", 9L),
    primary_adapter = rep("two_sample_primary_decision", 9L),
    robustness_adapter = c(
      "t.test", "t.test", "t.test", "brunner_munzel", "wilcoxon", "t.test",
      "paired.t.test", "brunner_munzel", "wilcoxon"
    ),
    truth_class = rep(c("null", "moderate", "large"), 3L),
    target_conclusion = rep(c("non_significant", "significant", "significant"), 3L),
    sample_size = c(20L, 40L, 80L, 20L, 40L, 80L, 30L, 60L, 100L),
    n_boot = rep(1000L, 9L),
    max_removal_pct = rep(0.30, 9L),
    training_split = rep(0.70, 9L),
    scenario_seed = c(2101L, 2102L, 2103L, 2201L, 2202L, 2203L, 2301L, 2302L, 2303L),
    parameters = list(
      list(generator = list(n_per_group = 10L, effect_size = 0, distribution = "normal"),
           analysis = list(test_type = "t.test", alpha = 0.05, correct = TRUE)),
      list(generator = list(n_per_group = 20L, effect_size = 0.45, distribution = "normal"),
           analysis = list(test_type = "t.test", alpha = 0.05, correct = TRUE)),
      list(generator = list(n_per_group = 40L, effect_size = 0.9, distribution = "normal"),
           analysis = list(test_type = "t.test", alpha = 0.05, correct = TRUE)),
      list(generator = list(n_per_group = 10L, effect_size = 0, distribution = "normal",
                            sd_control = 1, sd_treatment = 3),
           analysis = list(test_type = "brunner_munzel", alpha = 0.05)),
      list(generator = list(n_per_group = 20L, effect_size = 0.45, distribution = "heavy_tailed"),
           analysis = list(test_type = "wilcoxon", alpha = 0.05)),
      list(generator = list(n_per_group = 40L, effect_size = 0.9, distribution = "normal",
                            contamination = 0.15),
           analysis = list(test_type = "t.test", alpha = 0.05)),
      list(generator = list(n_per_group = 15L, effect_size = 0, distribution = "heavy_tailed",
                            paired = TRUE),
           analysis = list(test_type = "paired.t.test", alpha = 0.05)),
      list(generator = list(n_per_group = 30L, effect_size = 0.6, distribution = "normal",
                            sd_control = 1, sd_treatment = 2.5),
           analysis = list(test_type = "brunner_munzel", alpha = 0.05)),
      list(generator = list(n_per_group = 50L, effect_size = 1.2, distribution = "heavy_tailed",
                            contamination = 0.10),
           analysis = list(test_type = "wilcoxon", alpha = 0.05))
    )
  )

  tibble::as_tibble(rbind(base, expanded), .name_repair = "check_unique")
}
