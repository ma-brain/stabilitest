# Thin binary-proportion screening/robustness adapter for the isolated study.
#
# Screening parity: the adapter's primary decision must equal a direct
# stats::fisher.test on the generated 2x2 table (p, conclusion to 1e-12).  The
# robustness path uses the frozen jackknife-light weights (fragility 0.5,
# bootstrap 0.5, jackknife 0) via stabilitest::robustness_analysis.  The v1
# comparator weights (0.4/0.4/0.2) are archived, never fitted.

# Frozen score weights for the proportions family (v2/v3 jackknife-light policy).
.prop_frozen_weights <- c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
# Archived comparator only (v1); never fitted, never emitted as a label.
.prop_v1_comparator_weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

fisher_exact_primary_decision <- function(data, scenario = list(),
                                          alpha = NULL) {
  groups <- .two_sample_data(data)
  analysis <- .two_sample_analysis(scenario)
  alpha <- if (is.null(alpha)) .two_sample_alpha(analysis, scenario) else alpha
  tested <- .two_sample_primary_test(
    groups$group1, groups$group2, "fisher", alpha, correct = TRUE
  )
  list(
    p = tested$p,
    p_value = tested$p,
    original_p = tested$p,
    conclusion = tested$significant,
    significant = tested$significant,
    original_significant = tested$significant,
    test_type = "fisher",
    alpha = alpha
  )
}

.run_binary_proportion_robustness <- function(data, scenario = list(),
                                              n_boot = NULL, seed = NULL, ...) {
  groups <- .two_sample_data(data)
  analysis <- .two_sample_analysis(scenario)
  alpha <- .two_sample_alpha(analysis, scenario)
  if (is.null(n_boot)) {
    n_boot <- .two_sample_scenario_scalar(scenario, "n_boot", 1000L)
  }
  if (is.null(seed)) {
    seed <- .two_sample_scenario_scalar(scenario, "scenario_seed", 123L)
  }
  removal <- .two_sample_scenario_scalar(scenario, "max_removal_pct", 0.30)
  args <- list(
    group1 = groups$group1,
    group2 = groups$group2,
    test_type = "fisher",
    alpha = alpha,
    n_boot = n_boot,
    max_removal_pct = removal,
    weights = .prop_frozen_weights,
    seed = seed,
    correct = TRUE
  )
  extra <- list(...)
  if (length(extra)) args <- utils::modifyList(args, extra)
  output <- tryCatch(
    do.call(stabilitest::robustness_analysis, args),
    error = function(error) .two_sample_abort(
      sprintf("robustness fisher failed: %s", conditionMessage(error)), error
    )
  )
  if (!is.numeric(output$original_p) || length(output$original_p) != 1L ||
      !is.finite(output$original_p)) {
    .two_sample_abort("robustness fisher returned non-finite p")
  }
  output
}

# Archived v1-weight comparator: recompute the composite score with the v1
# weights (0.4/0.4/0.2) from a robustness result's component metrics.  The v1
# score is recorded for comparison only; it is never the fitted score and never
# emits a label.  build_robustness_metrics is reused so the formula cannot drift
# from the package's own composite definition.
.prop_v1_weight_score <- function(result) {
  metrics <- result$robustness_metrics %||% result$metrics
  if (is.null(metrics)) return(NA_real_)
  s_jack <- metrics$jackknife_conclusion_stability %||% metrics$s_jack
  s_frag <- metrics$worstcase_fragility_component %||% metrics$fragility_component
  s_boot <- metrics$bootstrap_reproducibility
  if (any(is.na(c(s_jack, s_frag, s_boot)))) return(NA_real_)
  w <- .prop_v1_comparator_weights
  unname(w["jackknife"] * s_jack + w["fragility"] * s_frag + w["bootstrap"] * s_boot)
}

binary_proportion_adapter <- function() {
  list(
    generate = generate_binary_proportion,
    generate_data = generate_binary_proportion,
    primary_decision = fisher_exact_primary_decision,
    run_robustness = .run_binary_proportion_robustness,
    robustness_analysis = .run_binary_proportion_robustness,
    v1_weight_score = .prop_v1_weight_score,
    frozen_weights = function() .prop_frozen_weights
  )
}
