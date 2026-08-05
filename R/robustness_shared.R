# ==============================================================================
# Shared helpers for two-sample and model/TOST robustness engines
#
# Keeps scoring bands, composite weights, fragility summaries, and result-object
# field names aligned without forcing a single mega-engine over structurally
# different deletion units (observations vs model rows).
# ==============================================================================

# Shared bootstrap / removal argument checks (two-sample + model engines).
validate_n_boot_max_removal <- function(n_boot, max_removal_pct) {
  if (!is.numeric(n_boot) || length(n_boot) != 1L || is.na(n_boot) ||
      n_boot < 1 || floor(n_boot) != n_boot) {
    stop("n_boot must be a single positive integer", call. = FALSE)
  }
  if (!is.numeric(max_removal_pct) || length(max_removal_pct) != 1L ||
      is.na(max_removal_pct) || max_removal_pct <= 0 || max_removal_pct > 1) {
    stop("max_removal_pct must be a single number in (0, 1]", call. = FALSE)
  }
}

validate_alpha_weights <- function(alpha, weights) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("alpha must be in (0, 1)", call. = FALSE)
  }

  required_names <- c("jackknife", "fragility", "bootstrap")
  valid_names <- is.numeric(weights) &&
    length(weights) == length(required_names) &&
    !is.null(names(weights)) &&
    !anyNA(names(weights)) &&
    all(nzchar(names(weights))) &&
    !anyDuplicated(names(weights)) &&
    setequal(names(weights), required_names)
  if (!valid_names) {
    stop(
      paste(
        "weights must be a named numeric vector containing exactly:",
        paste(required_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(!is.finite(weights)) || any(weights < 0)) {
    stop("weights must contain only finite, non-negative values", call. = FALSE)
  }
  if (abs(sum(weights) - 1) > 1e-8) {
    stop("weights must sum to 1", call. = FALSE)
  }
}

validate_fragility_capacity <- function(max_k) {
  if (!is.numeric(max_k) || length(max_k) != 1L || is.na(max_k) || max_k < 1) {
    stop(
      paste(
        "Insufficient sample for fragility analysis:",
        "cannot evaluate a removal while retaining the minimum sample size"
      ),
      call. = FALSE
    )
  }
}

# Legacy score bands (simulation Section 3): > 70 Robust; (55, 70]
# Moderately Robust; <= 55 Fragile.  This helper is retained for historical
# callers only.  Current result wrappers use score_label_from_calibration()
# after an exact registry/applicability check and never fall back to these
# thresholds when a method is uncalibrated.
robustness_band_label <- function(score) {
  dplyr::case_when(
    score > 70 ~ "Robust",
    score > 55 ~ "Moderately Robust",
    TRUE ~ "Fragile"
  )
}

# Render a composite score together with its calibration status.  Result
# objects created before method-specific calibration was introduced do not
# contain a `calibration` field; those objects are explicitly identified as
# legacy rather than silently receiving the historical Welch cutoffs.
format_score_interpretation <- function(score, calibration = NULL,
                                        label = NULL) {
  if (!is.numeric(score) || length(score) != 1L || !is.finite(score)) {
    return("NA/100 (calibration status unknown)")
  }

  if (is.null(calibration) || !is.list(calibration)) {
    return(sprintf(
      "%.1f/100 (legacy result: calibration status unknown)", score
    ))
  }

  if (identical(calibration$status, "bands_not_applicable")) {
    return(sprintf(
      "%.1f/100 (no categorical band assigned because observed conclusion is not eligible)",
      score
    ))
  }

  if (!isTRUE(calibration$applicable) ||
      !identical(calibration$status, "validated_method_specific")) {
    return(sprintf(
      "%.1f/100 (categorical bands not calibrated for this method)", score
    ))
  }

  # Applicability resolution guarantees valid cutoffs and labels, but keep
  # this formatter defensive for hand-built/serialized result objects.
  if (is.null(label) || length(label) != 1L || is.na(label) ||
      !nzchar(as.character(label))) {
    label <- "calibrated"
  }
  unit <- calibration$calibration_unit
  calibration_name <- if (identical(unit, "welch_unpaired")) {
    "Welch calibration"
  } else if (is.character(unit) && length(unit) == 1L && !is.na(unit)) {
    sprintf("%s calibration", unit)
  } else {
    "method-specific calibration"
  }
  version <- calibration$version
  if (!is.character(version) || length(version) != 1L || is.na(version) ||
      !nzchar(version)) {
    version <- "version unknown"
  }
  sprintf("%.1f/100 (%s; %s %s)", score, label, calibration_name, version)
}

fragility_index_from_removal <- function(removal_tbl, max_k) {
  valid <- !is.na(removal_tbl$conclusion_match)
  flipped <- removal_tbl[valid & !removal_tbl$conclusion_match, , drop = FALSE]
  if (nrow(flipped) == 0L) {
    evaluated_k <- if (any(valid)) {
      max(removal_tbl$k_removed[valid])
    } else {
      -1L
    }
    if (evaluated_k < max_k) {
      stop(
        sprintf(
          "Fragility removal search ended before max_k (%d of %d removals evaluated)",
          evaluated_k, max_k
        ),
        call. = FALSE
      )
    }
    as.integer(max_k + 1L)
  } else {
    as.integer(min(flipped$k_removed))
  }
}

p_at_fragility_from_removal <- function(removal_tbl, k_frag, max_k) {
  if (k_frag <= max_k) {
    removal_tbl$p_value[removal_tbl$k_removed == k_frag]
  } else {
    NA_real_
  }
}

fragility_component_score <- function(k_frag, max_k) {
  100 * min(k_frag / (max_k + 1), 1)
}

overall_robustness_score <- function(s_jack, frag_comp, s_boot, weights) {
  weights[["jackknife"]] * s_jack +
    weights[["fragility"]] * frag_comp +
    weights[["bootstrap"]] * s_boot
}

# Annotate leave-one-out / jackknife rows with conclusion and influence flags.
annotate_loo_results <- function(df, original_p, original_significant, alpha,
                                 influential_threshold = 0.05) {
  df |>
    mutate(
      significant = p_value < alpha,
      conclusion_match = significant == original_significant,
      influential_delta = abs(p_value - original_p) > influential_threshold,
      influential = influential_delta | !conclusion_match
    )
}

annotate_bootstrap_results <- function(df, original_significant, alpha) {
  df |>
    mutate(
      significant = p_value < alpha,
      conclusion_match = significant == original_significant
    )
}

bootstrap_validity <- function(bootstrap) {
  valid <- is.finite(bootstrap$p_value)
  n_valid <- sum(valid)
  if (n_valid == 0L) {
    stop(
      "No valid bootstrap replicates; the resampled data are too degenerate",
      call. = FALSE
    )
  }
  list(
    valid = valid,
    n_valid = as.integer(n_valid),
    n_failed = as.integer(length(valid) - n_valid)
  )
}

annotate_removal_results <- function(df, original_significant) {
  df |>
    mutate(conclusion_match = significant == original_significant)
}

# Build the shared metrics tibble. Path-specific columns (extreme_*, estimate
# ranges) are filled with NA when not applicable so both result classes expose
# the same metric names.
build_robustness_metrics <- function(s_jack,
                                     jackknife,
                                     k_frag_worst,
                                     p_at_k_frag,
                                     s_boot,
                                     bootstrap,
                                     weights,
                                     n_total,
                                     max_k,
                                     k_frag_extreme = NA_integer_,
                                     estimate_lo = NA_real_,
                                     estimate_hi = NA_real_) {
  frag_comp <- fragility_component_score(k_frag_worst, max_k)
  tibble::tibble(
    jackknife_conclusion_stability = s_jack,
    jackknife_n_influential        = sum(jackknife$influential),
    jackknife_pct_influential      = mean(jackknife$influential) * 100,
    jackknife_p_range_lo           = min(jackknife$p_value, na.rm = TRUE),
    jackknife_p_range_hi           = max(jackknife$p_value, na.rm = TRUE),

    worstcase_fragility_k          = k_frag_worst,
    worstcase_fragility_pct        = 100 * k_frag_worst / n_total,
    worstcase_fragility_component  = frag_comp,
    p_at_fragility                 = p_at_k_frag,

    extreme_fragility_k            = k_frag_extreme,
    extreme_fragility_pct          = if (is.na(k_frag_extreme)) {
      NA_real_
    } else {
      100 * k_frag_extreme / n_total
    },

    bootstrap_reproducibility      = s_boot,
    bootstrap_p_mean               = mean(bootstrap$p_value, na.rm = TRUE),
    bootstrap_p_sd                 = stats::sd(bootstrap$p_value, na.rm = TRUE),

    estimate_range_jackknife_lo    = estimate_lo,
    estimate_range_jackknife_hi    = estimate_hi,

    overall_robustness = overall_robustness_score(
      s_jack, frag_comp, s_boot, weights
    )
  )
}

jackknife_estimate_range <- function(estimate) {
  if (is.null(estimate) || all(is.na(estimate))) {
    return(list(lo = NA_real_, hi = NA_real_))
  }
  list(lo = min(estimate, na.rm = TRUE), hi = max(estimate, na.rm = TRUE))
}

# Cross-class aliases: keep historical primary names and mirror the other
# family so callers can use either metrics / robustness_metrics and
# interpretation_label / robustness_interpretation. Calibration metadata is
# deliberately not inferred here: each analysis wrapper must attach the exact
# method-specific resolution before aliases are synchronized.
align_robustness_result_aliases <- function(out,
                                            style = c("analysis", "model")) {
  style <- match.arg(style)
  if (identical(style, "analysis")) {
    out$metrics <- out$robustness_metrics
    out$interpretation_label <- out$robustness_interpretation
    if (is.null(out$original_estimate)) {
      out$original_estimate <- out$original_mean_diff
    }
  } else {
    out$robustness_metrics <- out$metrics
    out$robustness_interpretation <- out$interpretation_label
    if (is.null(out$original_mean_diff)) {
      out$original_mean_diff <- out$original_estimate
    }
    if (is.null(out$original_statistic)) {
      out$original_statistic <- NA_real_
    }
    if (is.null(out$original_ci)) {
      out$original_ci <- c(NA_real_, NA_real_)
    }
  }
  out
}

# Shared print lines for model / TOST component summaries.
print_robustness_components <- function(x, metrics, p_label = "p") {
  cat("COMPONENTS:\n")
  cat(sprintf("  Jackknife stability:       %5.1f%%  (influential: %d)\n",
              metrics$jackknife_conclusion_stability,
              metrics$jackknife_n_influential))
  cat(sprintf("  Worst-case fragility:      k = %s (%.1f%% of sample)%s\n",
              ifelse(metrics$worstcase_fragility_k > x$max_k,
                     paste0("> ", x$max_k), metrics$worstcase_fragility_k),
              min(metrics$worstcase_fragility_pct, 100 * x$max_k / x$n),
              ifelse(is.na(metrics$p_at_fragility), "",
                     sprintf("  [%s at flip: %.4f]", p_label,
                             metrics$p_at_fragility))))
  cat(sprintf("  Bootstrap reproducibility: %5.1f%%  (mean %s = %.4f)\n",
              metrics$bootstrap_reproducibility, p_label,
              metrics$bootstrap_p_mean))
  if (is.na(metrics$estimate_range_jackknife_lo) ||
      is.na(metrics$estimate_range_jackknife_hi)) {
    cat("  Jackknife estimate range:  NA (joint multi-df term or all fits failed)\n")
  } else {
    cat(sprintf("  Jackknife estimate range:  [%.4f, %.4f]\n",
                metrics$estimate_range_jackknife_lo,
                metrics$estimate_range_jackknife_hi))
  }
  invisible(NULL)
}
