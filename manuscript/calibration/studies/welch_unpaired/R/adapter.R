# Thin adapter from the Welch study harness to the stabilitest public API.

WELCH_FROZEN_WEIGHTS <- c(
  jackknife = 0.4,
  fragility = 0.4,
  bootstrap = 0.2
)

WELCH_METRIC_COLUMNS <- c(
  "jackknife_conclusion_stability",
  "jackknife_n_influential",
  "jackknife_pct_influential",
  "jackknife_p_range_lo",
  "jackknife_p_range_hi",
  "worstcase_fragility_k",
  "worstcase_fragility_pct",
  "worstcase_fragility_component",
  "p_at_fragility",
  "extreme_fragility_k",
  "extreme_fragility_pct",
  "bootstrap_reproducibility",
  "bootstrap_p_mean",
  "bootstrap_p_sd",
  "estimate_range_jackknife_lo",
  "estimate_range_jackknife_hi",
  "overall_robustness"
)

.welch_adapter_scalar <- function(scenario, name) {
  p <- .welch_scenario_list(scenario)
  value <- p[[name]]
  if (is.null(value) || length(value) != 1L) {
    stop(sprintf("Welch scenario requires scalar %s", name), call. = FALSE)
  }
  value
}

.welch_split_groups <- function(data) {
  if (!is.data.frame(data) || !all(c("group", "value") %in% names(data))) {
    stop("Welch data must contain group and value columns", call. = FALSE)
  }
  if (anyNA(data$group) || anyNA(data$value) || any(!is.finite(data$value))) {
    stop("Welch data must contain complete finite values", call. = FALSE)
  }
  group <- as.character(data$group)
  if (!setequal(unique(group), c("group1", "group2"))) {
    stop("Welch data must contain group1 and group2", call. = FALSE)
  }
  list(
    group1 = data$value[group == "group1"],
    group2 = data$value[group == "group2"]
  )
}

welch_bootstrap_seed <- function(scenario_seed, replicate_id) {
  as.integer(
    (as.double(scenario_seed) + 32452843 * as.double(replicate_id)) %%
      2147483646 + 1
  )
}

welch_validate_profile <- function(scenario, n_boot = NULL,
                                   max_removal_pct = NULL,
                                   weights = WELCH_FROZEN_WEIGHTS,
                                   production = TRUE) {
  alpha <- as.numeric(.welch_adapter_scalar(scenario, "alpha"))
  frozen_n_boot <- as.integer(.welch_adapter_scalar(scenario, "n_boot"))
  frozen_removal <- as.numeric(
    .welch_adapter_scalar(scenario, "max_removal_pct")
  )
  if (!isTRUE(all.equal(alpha, 0.05))) {
    stop("Welch profile requires frozen alpha = 0.05", call. = FALSE)
  }
  if (is.null(n_boot)) n_boot <- frozen_n_boot
  n_boot <- as.integer(n_boot)
  if (isTRUE(production) && !identical(n_boot, 1000L)) {
    stop("production Welch profile requires frozen n_boot = 1000", call. = FALSE)
  }
  if (!isTRUE(production) && (is.na(n_boot) || n_boot < 1L)) {
    stop("smoke n_boot must be a positive integer", call. = FALSE)
  }
  if (is.null(max_removal_pct)) max_removal_pct <- frozen_removal
  if (!isTRUE(all.equal(as.numeric(max_removal_pct), 0.30))) {
    stop("Welch profile requires the frozen removal cap of 0.30", call. = FALSE)
  }
  if (!is.numeric(weights) || !identical(names(weights), names(WELCH_FROZEN_WEIGHTS)) ||
      !isTRUE(all.equal(unname(weights), unname(WELCH_FROZEN_WEIGHTS)))) {
    stop("Welch profile requires the frozen weights (0.4, 0.4, 0.2)",
         call. = FALSE)
  }
  list(
    alpha = alpha,
    n_boot = n_boot,
    max_removal_pct = as.numeric(max_removal_pct),
    weights = weights
  )
}

welch_primary_decision <- function(data, scenario) {
  groups <- .welch_split_groups(data)
  alpha <- as.numeric(.welch_adapter_scalar(scenario, "alpha"))
  tested <- stats::t.test(groups$group1, groups$group2, var.equal = FALSE)
  significant <- is.finite(tested$p.value) && tested$p.value < alpha
  list(
    p_value = as.numeric(tested$p.value),
    original_p = as.numeric(tested$p.value),
    significant = significant,
    conclusion = if (significant) "significant" else "non_significant",
    statistic = unname(tested$statistic),
    estimate = unname(diff(rev(tested$estimate))),
    alpha = alpha,
    resolved_method = "welch_unpaired"
  )
}

new_welch_score_row <- function(scenario, replicate_id, n_boot,
                                status = c("completed", "failed"),
                                original_p = NA_real_,
                                original_significant = NA,
                                metrics = NULL,
                                elapsed_seconds = NA_real_,
                                failure_stage = NA_character_,
                                failure_class = NA_character_,
                                failure_message = NA_character_,
                                bootstrap_failures = NA_integer_,
                                profile_json = NA_character_,
                                calibration_version = NA_character_,
                                calibration_status = NA_character_) {
  status <- match.arg(status)
  p <- .welch_scenario_list(scenario)
  replicate_id <- as.integer(replicate_id)
  replicate_seed <- welch_replicate_seed(p$seed, replicate_id)
  bootstrap_seed <- welch_bootstrap_seed(p$seed, replicate_id)
  metric_values <- stats::setNames(rep(NA_real_, length(WELCH_METRIC_COLUMNS)),
                                   WELCH_METRIC_COLUMNS)
  if (!is.null(metrics)) {
    if (is.data.frame(metrics)) metrics <- unlist(metrics[1L, , drop = TRUE])
    metrics <- as.numeric(metrics[WELCH_METRIC_COLUMNS])
    names(metrics) <- WELCH_METRIC_COLUMNS
    metric_values[names(metrics)] <- metrics
  }

  row <- data.frame(
    scenario_id = as.character(p$scenario_id),
    archetype_id = as.character(p$archetype_id),
    design_layer = as.character(p$design_layer),
    truth = as.character(p$truth),
    replicate_id = replicate_id,
    replicate_seed = replicate_seed,
    bootstrap_seed = bootstrap_seed,
    original_p = as.numeric(original_p),
    original_significant = as.logical(original_significant),
    original_conclusion = if (isTRUE(original_significant)) {
      "significant"
    } else if (identical(original_significant, FALSE)) {
      "non_significant"
    } else {
      NA_character_
    },
    resolved_method = "welch_unpaired",
    status = status,
    failure_stage = as.character(failure_stage),
    failure_class = as.character(failure_class),
    failure_message = as.character(failure_message),
    elapsed_seconds = as.numeric(elapsed_seconds),
    bootstrap_failures = as.integer(bootstrap_failures),
    profile_alpha = as.numeric(p$alpha),
    profile_n_boot = as.integer(n_boot),
    profile_max_removal_pct = as.numeric(p$max_removal_pct),
    weight_jackknife = unname(WELCH_FROZEN_WEIGHTS[["jackknife"]]),
    weight_fragility = unname(WELCH_FROZEN_WEIGHTS[["fragility"]]),
    weight_bootstrap = unname(WELCH_FROZEN_WEIGHTS[["bootstrap"]]),
    n_group1 = as.integer(p$n_group1),
    n_group2 = as.integer(p$n_group2),
    allocation_ratio = as.numeric(p$n_group1) / as.numeric(p$n_group2),
    calibration_version = as.character(calibration_version),
    calibration_status = as.character(calibration_status),
    profile_json = as.character(profile_json),
    stringsAsFactors = FALSE
  )
  for (column in WELCH_METRIC_COLUMNS) {
    row[[column]] <- unname(metric_values[[column]])
  }
  row
}

welch_score_analysis <- function(data, scenario, replicate_id,
                                 n_boot = NULL,
                                 max_removal_pct = NULL,
                                 weights = WELCH_FROZEN_WEIGHTS,
                                 production = TRUE) {
  profile <- welch_validate_profile(
    scenario,
    n_boot = n_boot,
    max_removal_pct = max_removal_pct,
    weights = weights,
    production = production
  )
  groups <- .welch_split_groups(data)
  p <- .welch_scenario_list(scenario)
  if (length(groups$group1) != as.integer(p$n_group1) ||
      length(groups$group2) != as.integer(p$n_group2)) {
    stop("generated group sizes do not match the frozen scenario", call. = FALSE)
  }
  bootstrap_seed <- welch_bootstrap_seed(p$seed, replicate_id)
  started <- proc.time()[["elapsed"]]
  output <- tryCatch(
    .with_welch_seed(bootstrap_seed, {
      stabilitest::robustness_analysis(
        group1 = groups$group1,
        group2 = groups$group2,
        test_type = "t.test",
        alpha = profile$alpha,
        n_boot = profile$n_boot,
        max_removal_pct = profile$max_removal_pct,
        weights = profile$weights,
        seed = bootstrap_seed,
        interpret = FALSE
      )
    }),
    error = function(error) error
  )
  elapsed <- unname(proc.time()[["elapsed"]] - started)

  if (inherits(output, "error")) {
    return(new_welch_score_row(
      scenario, replicate_id, n_boot = profile$n_boot, status = "failed",
      elapsed_seconds = elapsed,
      failure_stage = "robustness_analysis",
      failure_class = class(output)[[1L]],
      failure_message = conditionMessage(output)
    ))
  }
  if (!identical(output$calibration$calibration_unit, "welch_unpaired")) {
    stop("stabilitest did not resolve the analysis to welch_unpaired",
         call. = FALSE)
  }

  runtime_profile <- list(
    method = "welch_unpaired",
    complete_cases = TRUE,
    n1 = length(groups$group1),
    n2 = length(groups$group2),
    allocation_ratio = length(groups$group1) / length(groups$group2),
    alpha = profile$alpha,
    n_boot = profile$n_boot,
    max_removal_pct = profile$max_removal_pct,
    weights = as.list(profile$weights)
  )
  new_welch_score_row(
    scenario, replicate_id, n_boot = profile$n_boot,
    status = "completed",
    original_p = output$original_p,
    original_significant = output$original_significant,
    metrics = output$robustness_metrics,
    elapsed_seconds = elapsed,
    bootstrap_failures = output$bootstrap$n_failed,
    profile_json = jsonlite::toJSON(runtime_profile, auto_unbox = TRUE),
    calibration_version = output$calibration$version,
    calibration_status = output$calibration$status
  )
}

welch_adapter <- function() {
  list(
    primary_decision = welch_primary_decision,
    score = welch_score_analysis,
    run_robustness = welch_score_analysis,
    frozen_weights = WELCH_FROZEN_WEIGHTS
  )
}
