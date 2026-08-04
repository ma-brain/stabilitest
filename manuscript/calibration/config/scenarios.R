# Frozen analysis-specific calibration scenarios.

#' Return the calibration scenario contract.
#'
#' Each row is a reproducible smoke scenario.  Generator-specific settings are
#' kept in the `parameters` list-column so that the tabular contract remains
#' stable as scenario families grow.
#'
#' Adapter metadata is documentary: the runner dispatches by `analysis_family`.
#' `primary_adapter` names the screening helper; `robustness_adapter` names the
#' public `stabilitest::*` entry point the adapters call.
#'
#' @return A tibble with the frozen calibration schema.
#' @export
calibration_scenarios <- function() {
  base <- tibble::tibble(
    scenario_id = c(
      "two_sample_smoke", "proportion_smoke", "lm_smoke", "binomial_smoke",
      "poisson_smoke", "cox_smoke", "tost_smoke",
      "lm_core_ancova", "lm_stress_heteroscedastic_missing", "lm_validation_multidf",
      "binomial_core_logit", "binomial_stress_separation", "binomial_validation_multidf",
      "poisson_core_offset", "poisson_stress_overdispersion", "poisson_validation_multidf",
      "lm_core_null", "binomial_core_null", "poisson_core_null"
    ),
    analysis_family = c(
      "two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost",
      "lm", "lm", "lm", "binomial", "binomial", "binomial",
      "poisson", "poisson", "poisson", "lm", "binomial", "poisson"
    ),
    endpoint = c(
      "mean_difference", "risk_difference", "coefficient", "odds_ratio",
      "rate_ratio", "hazard_ratio", "equivalence", "coefficient", "coefficient",
      "coefficient", "odds_ratio", "odds_ratio", "odds_ratio", "rate_ratio",
      "rate_ratio", "rate_ratio", "coefficient", "odds_ratio", "rate_ratio"
    ),
    design_layer = c("core", "core", "stress", "stress", "validation", "validation", "core",
                     "core", "stress", "validation", "core", "stress", "validation",
                     "core", "stress", "validation", "core", "core", "core"),
    data_generator = c(
      "generate_two_sample", "generate_proportion", "generate_lm",
      "generate_binomial", "generate_poisson", "generate_cox", "generate_tost",
      "generate_lm", "generate_lm", "generate_lm", "generate_binomial",
      "generate_binomial", "generate_binomial", "generate_poisson", "generate_poisson",
      "generate_poisson", "generate_lm", "generate_binomial", "generate_poisson"
    ),
    primary_adapter = c(
      "two_sample_primary_decision", "two_sample_primary_decision",
      "primary_decision_lm", "primary_decision_glm", "primary_decision_glm",
      "screen_cox", "screen_tost",
      "primary_decision_lm", "primary_decision_lm", "primary_decision_lm",
      "primary_decision_glm", "primary_decision_glm", "primary_decision_glm",
      "primary_decision_glm", "primary_decision_glm", "primary_decision_glm",
      "primary_decision_lm", "primary_decision_glm", "primary_decision_glm"
    ),
    robustness_adapter = c(
      "robustness_analysis", "robustness_analysis", "robustness_lm",
      "robustness_glm", "robustness_glm", "robustness_surv", "robustness_tost",
      "robustness_lm", "robustness_lm", "robustness_lm", "robustness_glm",
      "robustness_glm", "robustness_glm", "robustness_glm", "robustness_glm",
      "robustness_glm", "robustness_lm", "robustness_glm", "robustness_glm"
    ),
    truth_class = c("null", "clear", "clear", "borderline", "clear", "borderline", "clear",
                    "clear", "borderline", "clear", "clear", "borderline", "clear",
                    "clear", "borderline", "clear", "null", "null", "null"),
    target_conclusion = c(
      "non_significant", "significant", "significant", "significant",
      "significant", "significant", "equivalent", "significant", "significant",
      "significant", "significant", "significant", "significant", "significant",
      "significant", "significant", "non_significant", "non_significant",
      "non_significant"
    ),
    sample_size = c(50L, 80L, 80L, 120L, 120L, 160L, 80L,
                    90L, 100L, 110L, 100L, 120L, 130L, 100L, 120L, 140L,
                    90L, 100L, 110L),
    n_boot = rep(1000L, 19L),
    max_removal_pct = rep(0.30, 19L),
    training_split = rep(0.7, 19L),
    scenario_seed = c(1101L, 1201L, 1301L, 1401L, 1501L, 1601L, 1701L,
                      1311L, 1312L, 1313L, 1411L, 1412L, 1413L, 1511L, 1512L, 1513L,
                      1321L, 1421L, 1521L),
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
        generator = list(
          n_per_group = 40L, endpoint = "mean", type = "equivalence",
          mean_difference = 0, equivalence_margin = 0.5
        ),
        analysis = list(
          endpoint = "mean", type = "equivalence", margin = 0.5, alpha = 0.05
        )
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
      ),
      list(
        generator = list(n = 90L, effect = 0, covariate_effect = 0.6),
        analysis = list(term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 100L, prevalence = 0.35, effect = 0, covariate_effect = 0.25),
        analysis = list(family = "binomial", link = "logit", term = "treatmentB", alpha = 0.05)
      ),
      list(
        generator = list(n = 110L, rate = 0.8, effect = 0, exposure = TRUE, covariate_effect = 0.15),
        analysis = list(family = "poisson", link = "log", term = "treatmentB", alpha = 0.05)
      )
    )
  )

  # Two-sample calibration is stratified by truth class and design layer.  The
  # validation rows deliberately change generator parameters (distribution,
  # variance, and effect) rather than merely reserving a new random seed.
  expanded <- tibble::tibble(
    scenario_id = c(
      "two_sample_core_null_n20", "two_sample_core_borderline_n40",
      "two_sample_core_clear_n80", "two_sample_stress_null_n20",
      "two_sample_stress_borderline_n40", "two_sample_stress_clear_n80",
      "two_sample_validation_null_n30", "two_sample_validation_borderline_n60",
      "two_sample_validation_clear_n100"
    ),
    analysis_family = rep("two_sample", 9L),
    endpoint = rep("mean_difference", 9L),
    design_layer = c(rep("core", 3L), rep("stress", 3L), rep("validation", 3L)),
    data_generator = rep("generate_two_sample", 9L),
    primary_adapter = rep("two_sample_primary_decision", 9L),
    robustness_adapter = rep("robustness_analysis", 9L),
    truth_class = rep(c("null", "borderline", "clear"), 3L),
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

  # Imbalance and sparse binary stressors use different group sizes and are
  # retained as explicit parameter combinations in validation.
  imbalance_sparse <- tibble::tibble(
    scenario_id = c(
      "two_sample_core_imbalanced_n28", "two_sample_stress_imbalanced_binary",
      "two_sample_validation_sparse_chisq", "two_sample_validation_sparse_prop"
    ),
    analysis_family = rep("two_sample", 4L),
    endpoint = c("mean_difference", "risk_difference", "risk_difference", "risk_difference"),
    design_layer = c("core", "stress", "validation", "validation"),
    data_generator = rep("generate_two_sample", 4L),
    primary_adapter = rep("two_sample_primary_decision", 4L),
    robustness_adapter = rep("robustness_analysis", 4L),
    truth_class = c("borderline", "null", "null", "borderline"),
    target_conclusion = c("significant", "non_significant", "non_significant", "significant"),
    sample_size = c(28L, 42L, 54L, 60L),
    n_boot = rep(1000L, 4L),
    max_removal_pct = c(0.30, 0.05, 0.05, 0.05),
    training_split = rep(0.70, 4L),
    scenario_seed = c(2401L, 2402L, 2403L, 2404L),
    parameters = list(
      list(generator = list(n_group1 = 8L, n_group2 = 20L, effect_size = 0.5,
                            distribution = "normal"),
           analysis = list(test_type = "t.test", alpha = 0.05)),
      list(generator = list(n_group1 = 12L, n_group2 = 30L,
                            probability_control = 0.02, probability_treatment = 0.08,
                            distribution = "binary"),
           analysis = list(test_type = "fisher", alpha = 0.05, correct = TRUE)),
      list(generator = list(n_group1 = 18L, n_group2 = 36L,
                            probability_control = 0.04, probability_treatment = 0.12,
                            distribution = "binary"),
           analysis = list(test_type = "chisq", alpha = 0.05, correct = TRUE)),
      list(generator = list(n_group1 = 20L, n_group2 = 40L,
                            probability_control = 0.05, probability_treatment = 0.20,
                            distribution = "binary"),
           analysis = list(test_type = "prop", alpha = 0.05, correct = TRUE))
    )
  )

  # Proportion-family held-out depth for Fisher / chi-square / prop beyond the
  # smoke row.  Generators reuse generate_proportion (= generate_two_sample).
  proportion_depth <- tibble::tibble(
    scenario_id = c(
      "proportion_stress_fisher_null",
      "proportion_stress_prop_borderline",
      "proportion_validation_chisq_clear"
    ),
    analysis_family = rep("proportion", 3L),
    endpoint = rep("risk_difference", 3L),
    design_layer = c("stress", "stress", "validation"),
    data_generator = rep("generate_proportion", 3L),
    primary_adapter = rep("two_sample_primary_decision", 3L),
    robustness_adapter = rep("robustness_analysis", 3L),
    truth_class = c("null", "borderline", "clear"),
    target_conclusion = c("non_significant", "significant", "significant"),
    sample_size = c(60L, 100L, 120L),
    n_boot = rep(1000L, 3L),
    max_removal_pct = c(0.10, 0.15, 0.20),
    training_split = rep(0.70, 3L),
    scenario_seed = c(1211L, 1212L, 1213L),
    parameters = list(
      list(
        generator = list(
          n_group1 = 20L, n_group2 = 40L,
          probability_control = 0.25, probability_treatment = 0.25,
          distribution = "binary"
        ),
        analysis = list(test_type = "fisher", alpha = 0.05, correct = TRUE)
      ),
      list(
        generator = list(
          n_per_group = 50L,
          probability_control = 0.35, probability_treatment = 0.50,
          distribution = "binary"
        ),
        analysis = list(test_type = "prop", alpha = 0.05, correct = TRUE)
      ),
      list(
        generator = list(
          n_per_group = 60L,
          probability_control = 0.30, probability_treatment = 0.55,
          distribution = "binary"
        ),
        analysis = list(test_type = "chisq", alpha = 0.05, correct = TRUE)
      )
    )
  )

  # Paired Wilcoxon is supported by the two-sample adapter but needs dedicated
  # registry rows so coverage tests can catch regressions.
  wilcoxon_paired <- tibble::tibble(
    scenario_id = c(
      "two_sample_core_wilcoxon_paired_null",
      "two_sample_stress_wilcoxon_paired_clear",
      "two_sample_validation_wilcoxon_paired_borderline"
    ),
    analysis_family = rep("two_sample", 3L),
    endpoint = rep("mean_difference", 3L),
    design_layer = c("core", "stress", "validation"),
    data_generator = rep("generate_two_sample", 3L),
    primary_adapter = rep("two_sample_primary_decision", 3L),
    robustness_adapter = rep("robustness_analysis", 3L),
    truth_class = c("null", "clear", "borderline"),
    target_conclusion = c("non_significant", "significant", "significant"),
    sample_size = c(40L, 60L, 80L),
    n_boot = rep(1000L, 3L),
    max_removal_pct = rep(0.30, 3L),
    training_split = rep(0.70, 3L),
    scenario_seed = c(2501L, 2502L, 2503L),
    parameters = list(
      list(
        generator = list(
          n_per_group = 20L, effect_size = 0, distribution = "normal", paired = TRUE
        ),
        analysis = list(test_type = "wilcoxon.paired", alpha = 0.05)
      ),
      list(
        generator = list(
          n_per_group = 30L, effect_size = 1.0, distribution = "heavy_tailed",
          paired = TRUE, contamination = 0.10
        ),
        analysis = list(test_type = "wilcoxon.paired", alpha = 0.05)
      ),
      list(
        generator = list(
          n_per_group = 40L, effect_size = 0.45, distribution = "normal", paired = TRUE
        ),
        analysis = list(test_type = "wilcoxon.paired", alpha = 0.05)
      )
    )
  )

  # Cox core truth strata (null / borderline / clear).  Smoke remains validation;
  # Weibull/non-PH remains the stress row.
  cox_core <- tibble::tibble(
    scenario_id = c(
      "cox_core_null", "cox_core_borderline", "cox_core_clear"
    ),
    analysis_family = rep("cox", 3L),
    endpoint = rep("hazard_ratio", 3L),
    design_layer = rep("core", 3L),
    data_generator = rep("generate_cox", 3L),
    primary_adapter = rep("screen_cox", 3L),
    robustness_adapter = rep("robustness_surv", 3L),
    truth_class = c("null", "borderline", "clear"),
    target_conclusion = c("non_significant", "significant", "significant"),
    sample_size = c(160L, 180L, 200L),
    n_boot = rep(1000L, 3L),
    max_removal_pct = rep(0.30, 3L),
    training_split = rep(0.70, 3L),
    scenario_seed = c(1611L, 1612L, 1613L),
    parameters = list(
      list(
        generator = list(n = 160L, hazard_ratio = 1.0, censoring_rate = 0.2),
        analysis = list(term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 180L, hazard_ratio = 1.4, censoring_rate = 0.25),
        analysis = list(term = "treatment", alpha = 0.05)
      ),
      list(
        generator = list(n = 200L, hazard_ratio = 2.0, censoring_rate = 0.15),
        analysis = list(term = "treatment", alpha = 0.05)
      )
    )
  )

  # TOST core strata: well inside (smoke), near margin, and outside margin.
  tost_core <- tibble::tibble(
    scenario_id = c(
      "tost_core_borderline_near_margin",
      "tost_core_outside_margin"
    ),
    analysis_family = rep("tost", 2L),
    endpoint = rep("equivalence", 2L),
    design_layer = rep("core", 2L),
    data_generator = rep("generate_tost", 2L),
    primary_adapter = rep("screen_tost", 2L),
    robustness_adapter = rep("robustness_tost", 2L),
    truth_class = c("borderline", "null"),
    target_conclusion = c("equivalent", "not_equivalent"),
    sample_size = c(80L, 80L),
    n_boot = rep(1000L, 2L),
    max_removal_pct = rep(0.30, 2L),
    training_split = rep(0.70, 2L),
    scenario_seed = c(1711L, 1712L),
    parameters = list(
      list(
        generator = list(
          n_per_group = 40L, endpoint = "mean", type = "equivalence",
          mean_difference = 0.40, margin = 0.5
        ),
        analysis = list(
          endpoint = "mean", type = "equivalence", margin = 0.5, alpha = 0.05
        )
      ),
      list(
        generator = list(
          n_per_group = 40L, endpoint = "mean", type = "equivalence",
          mean_difference = 0.80, margin = 0.5
        ),
        analysis = list(
          endpoint = "mean", type = "equivalence", margin = 0.5, alpha = 0.05
        )
      )
    )
  )

  scenarios <- tibble::as_tibble(
    rbind(
      base, expanded, imbalance_sparse, proportion_depth,
      wilcoxon_paired, cox_core, tost_core
    ),
    .name_repair = "check_unique"
  )

  # Endpoint and stress coverage for the Cox and TOST adapters.  The original
  # smoke IDs remain frozen; these rows add Weibull/non-PH Cox and binary
  # TOST/NI checks without changing the common artifact schema.
  scenarios <- tibble::add_row(
    scenarios,
    scenario_id = "tost_prop_equivalence", analysis_family = "tost", endpoint = "risk_difference",
    design_layer = "validation", data_generator = "generate_tost", primary_adapter = "screen_tost",
    robustness_adapter = "robustness_tost", truth_class = "clear", target_conclusion = "equivalent",
    sample_size = 80L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
    scenario_seed = 1901L,
    parameters = list(list(generator = list(n_per_group = 40L, endpoint = "prop",
                                            type = "equivalence", delta_L = -.15, delta_U = .15,
                                            probability_control = .5, probability_treatment = .5),
                            analysis = list(endpoint = "prop", type = "equivalence",
                                             delta_L = -.15, delta_U = .15, alpha = .05))),
    .before = Inf
  )
  scenarios <- tibble::add_row(
    scenarios,
    scenario_id = "tost_or_noninferiority", analysis_family = "tost", endpoint = "odds_ratio",
    design_layer = "stress", data_generator = "generate_tost", primary_adapter = "screen_tost",
    robustness_adapter = "robustness_tost", truth_class = "clear", target_conclusion = "noninferior",
    sample_size = 80L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
    scenario_seed = 2001L,
    parameters = list(list(generator = list(n_per_group = 40L, endpoint = "or",
                                            type = "noninferiority", margin = 1.5,
                                            odds_ratio = 1.2),
                            analysis = list(endpoint = "or", type = "noninferiority",
                                             margin = 1.5, higher_is_better = TRUE, alpha = .05))),
    .before = Inf
  )
  tibble::add_row(
    scenarios,
    scenario_id = "cox_weibull_stress", analysis_family = "cox", endpoint = "hazard_ratio",
    design_layer = "stress", data_generator = "generate_cox", primary_adapter = "screen_cox",
    robustness_adapter = "robustness_surv", truth_class = "borderline", target_conclusion = "significant",
    sample_size = 160L, n_boot = 1000L, max_removal_pct = .30, training_split = .7,
    scenario_seed = 1801L,
    parameters = list(list(generator = list(n = 160L, hazard_ratio = 1.5,
                                            distribution = "weibull", shape = 1.3,
                                            censoring_rate = .2, time_varying_effect = TRUE),
                            analysis = list(term = "treatment", alpha = .05))),
    .before = Inf
  )
}
