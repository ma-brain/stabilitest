# Exploratory analysis of non-significant conclusions.
#
# Non-significant results have a different estimand from the significant-result
# score bands.  This module therefore reports discrimination and diagnostics,
# but deliberately never assigns Robust/Moderate/Fragile bands or cutoffs.

NON_SIGNIFICANT_STATUS <- "bands_not_applicable"
NON_SIGNIFICANT_AUC_THRESHOLD <- 0.70
NON_SIGNIFICANT_AUC_SPREAD <- 0.20

.non_significant_abort <- function(message) stop(message, call. = FALSE)

.non_significant_normalize <- function(x) {
  value <- tolower(trimws(as.character(x)))
  value <- gsub("[-[:space:]]+", "_", value)
  value
}

.non_significant_conclusion <- function(data) {
  # Prefer the completed analysis conclusion.  A screening or target
  # conclusion is a fallback for compact exploratory artifacts.
  candidates <- c("analysis_conclusion", "screening_conclusion", "target_conclusion")
  present <- candidates[candidates %in% names(data)]
  out <- rep(NA_character_, nrow(data))
  for (column in present) {
    values <- .non_significant_normalize(data[[column]])
    take <- is.na(out) & !is.na(values)
    out[take] <- values[take]
  }
  # Compact artifacts sometimes retain only the p-value or the logical flag;
  # derive the conclusion for those rows without changing any supplied label.
  if ("original_significant" %in% names(data)) {
    values <- data$original_significant
    if (is.logical(values)) {
      take <- is.na(out) & !is.na(values)
      out[take] <- ifelse(values[take], "significant", "non_significant")
    }
  }
  if ("original_p" %in% names(data) && is.numeric(data$original_p)) {
    take <- is.na(out) & is.finite(data$original_p)
    out[take] <- ifelse(data$original_p[take] < 0.05, "significant", "non_significant")
  }
  out
}

.non_significant_validate <- function(data) {
  if (!is.data.frame(data)) .non_significant_abort("replicates must be a data frame")
  required <- c("analysis_family", "truth_class", "overall_score")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    .non_significant_abort(sprintf("replicates missing required columns: %s", paste(missing, collapse = ", ")))
  }
  for (column in c("analysis_family", "truth_class")) {
    if (!is.character(data[[column]]) || anyNA(data[[column]]) || any(!nzchar(data[[column]]))) {
      .non_significant_abort(sprintf("%s must contain non-missing character values", column))
    }
  }
  if (!is.numeric(data$overall_score) || any(!is.finite(data$overall_score))) {
    .non_significant_abort("overall_score must contain finite numeric values")
  }
  if ("status" %in% names(data)) {
    bad <- !is.na(data$status) & data$status != "completed"
    data <- data[!bad, , drop = FALSE]
  }
  data
}

.non_significant_auc <- function(score, true_null) {
  keep <- is.finite(score) & !is.na(true_null)
  score <- as.numeric(score[keep])
  true_null <- as.logical(true_null[keep])
  n_true <- sum(true_null)
  n_false <- sum(!true_null)
  if (!n_true || !n_false) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[true_null]) - n_true * (n_true + 1) / 2) / (n_true * n_false)
}

.non_significant_summary <- function(data, group, components, group_name = "group") {
  rows <- list()
  groups <- sort(unique(as.character(group)))
  for (g in groups) {
    selected <- as.character(group) == g
    for (component in components) {
      values <- data[[component]][selected]
      values <- as.numeric(values[is.finite(values)])
      if (!length(values)) next
      rows[[length(rows) + 1L]] <- data.frame(
        group = g, component = component, n = length(values),
        mean = mean(values), median = stats::median(values),
        sd = if (length(values) > 1L) stats::sd(values) else 0,
        q05 = as.numeric(stats::quantile(values, 0.05, names = FALSE)),
        q95 = as.numeric(stats::quantile(values, 0.95, names = FALSE)),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(group = character(), component = character(), n = integer(),
                      mean = numeric(), median = numeric(), sd = numeric(),
                      q05 = numeric(), q95 = numeric(), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  names(out)[names(out) == "group"] <- group_name
  out
}

.non_significant_components <- function(data) {
  candidates <- c("overall_score", "jackknife_stability", "fragility_component",
                  "fragility_k", "fragility_pct", "bootstrap_reproducibility",
                  "original_p", "effective_p")
  candidates <- candidates[candidates %in% names(data)]
  candidates[vapply(data[candidates], is.numeric, logical(1))]
}

.non_significant_family_diagnostics <- function(data, non_sig, auc_threshold) {
  families <- sort(unique(as.character(data$analysis_family)))
  if (!length(families)) {
    return(data.frame(
      analysis_family = character(), n_non_significant = integer(),
      n_true_null = integer(), n_false_negative = integer(), auc = numeric(),
      auc_pass = logical(), true_null_median = numeric(),
      false_negative_median = numeric(), ordering_difference = numeric(),
      ordering_pass = logical(), stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(families, function(family) {
    selected <- non_sig & data$analysis_family == family
    true_null <- selected & data$truth_class == "null"
    false_negative <- selected & data$truth_class != "null"
    auc <- .non_significant_auc(data$overall_score[selected], data$truth_class[selected] == "null")
    null_median <- if (any(true_null)) stats::median(data$overall_score[true_null]) else NA_real_
    fn_median <- if (any(false_negative)) stats::median(data$overall_score[false_negative]) else NA_real_
    data.frame(
      analysis_family = family,
      n_non_significant = sum(selected), n_true_null = sum(true_null),
      n_false_negative = sum(false_negative), auc = auc,
      auc_pass = isTRUE(is.finite(auc) && auc >= auc_threshold),
      true_null_median = null_median, false_negative_median = fn_median,
      ordering_difference = null_median - fn_median,
      ordering_pass = isTRUE(is.finite(null_median) && is.finite(fn_median) && null_median > fn_median),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.non_significant_auc_groups <- function(data, non_sig, grouping, group_name) {
  values <- as.character(grouping)
  groups <- sort(unique(values[non_sig]))
  if (!length(groups)) {
    out <- data.frame(group = character(), n = integer(), n_true_null = integer(),
                      n_false_negative = integer(), auc = numeric(), stringsAsFactors = FALSE)
    names(out)[[1L]] <- group_name
    return(out)
  }
  rows <- lapply(groups, function(g) {
    selected <- non_sig & values == g
    labels <- data$truth_class[selected] == "null"
    data.frame(group = g, n = sum(selected), n_true_null = sum(labels),
               n_false_negative = sum(!labels),
               auc = .non_significant_auc(data$overall_score[selected], labels),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  names(out)[names(out) == "group"] <- group_name
  out
}

#' Explore whether the robustness score separates true-null non-rejections from
#' non-significant false negatives.
#'
#' The returned status is intentionally always `bands_not_applicable`: this
#' analysis is diagnostic and does not create a second set of categorical
#' labels.  `approved` is retained as an explicit, always-false guard so a
#' caller cannot accidentally attach the significant-result cutoffs.
analyse_non_significant <- function(replicates,
                                    auc_threshold = NON_SIGNIFICANT_AUC_THRESHOLD,
                                    max_auc_spread = NON_SIGNIFICANT_AUC_SPREAD) {
  data <- .non_significant_validate(replicates)
  if (!is.numeric(auc_threshold) || length(auc_threshold) != 1L ||
      !is.finite(auc_threshold) || auc_threshold < 0.5 || auc_threshold > 1) {
    .non_significant_abort("auc_threshold must be a scalar in [0.5, 1]")
  }
  if (!is.numeric(max_auc_spread) || length(max_auc_spread) != 1L ||
      !is.finite(max_auc_spread) || max_auc_spread < 0 || max_auc_spread > 1) {
    .non_significant_abort("max_auc_spread must be a scalar in [0, 1]")
  }

  conclusion <- .non_significant_conclusion(data)
  non_sig <- conclusion %in% c("non_significant", "nonsignificant", "not_significant",
                               "inconclusive", "not_supported")
  true_null <- non_sig & data$truth_class == "null"
  false_negative <- non_sig & data$truth_class != "null"
  groups <- ifelse(true_null, "true_null_non_rejection",
                   ifelse(false_negative, "false_negative", "other"))
  components <- .non_significant_components(data)

  pooled_auc <- .non_significant_auc(data$overall_score[non_sig],
                                     data$truth_class[non_sig] == "null")
  family <- .non_significant_family_diagnostics(data, non_sig, auc_threshold)
  valid_family <- family$n_true_null > 0L & family$n_false_negative > 0L
  discrimination <- list(
    auc = pooled_auc,
    threshold = auc_threshold,
    by_family = family[, c("analysis_family", "n_non_significant", "n_true_null",
                           "n_false_negative", "auc", "auc_pass"), drop = FALSE],
    pass = isTRUE(is.finite(pooled_auc) && pooled_auc >= auc_threshold) &&
      all(family$auc_pass[valid_family]) && any(valid_family)
  )
  ordering <- list(
    expected = "true_null_score_higher_than_false_negative_score",
    by_family = family[, c("analysis_family", "true_null_median",
                           "false_negative_median", "ordering_difference",
                           "ordering_pass"), drop = FALSE],
    pooled_difference = if (any(true_null) && any(false_negative))
      stats::median(data$overall_score[true_null]) - stats::median(data$overall_score[false_negative]) else NA_real_,
    pass = isTRUE(is.finite(if (any(true_null) && any(false_negative))
      stats::median(data$overall_score[true_null]) - stats::median(data$overall_score[false_negative]) else NA_real_)) &&
      all(family$ordering_pass[valid_family]) && any(valid_family)
  )
  auc_values <- family$auc[valid_family & is.finite(family$auc)]
  cross_family_consistency <- list(
    n_families = sum(valid_family),
    families = family$analysis_family[valid_family],
    auc_range = if (length(auc_values)) diff(range(auc_values)) else NA_real_,
    max_auc_spread = max_auc_spread,
    pass = length(auc_values) >= 2L && all(family$auc_pass[valid_family]) &&
      all(family$ordering_pass[valid_family]) && diff(range(auc_values)) <= max_auc_spread
  )

  sample_group <- if ("n" %in% names(data)) as.character(data$n) else rep("unknown", nrow(data))
  stress_group <- if ("design_layer" %in% names(data)) as.character(data$design_layer) else rep("all", nrow(data))
  summaries <- list(
    distributions = .non_significant_summary(data[non_sig, , drop = FALSE],
                                              groups[non_sig], components),
    sample_size = .non_significant_summary(data[non_sig, , drop = FALSE],
                                            sample_group[non_sig],
                                            components, group_name = "sample_size"),
    stress = .non_significant_summary(data[non_sig, , drop = FALSE],
                                      stress_group[non_sig], components, group_name = "design_layer"),
    sample_size_auc = .non_significant_auc_groups(data, non_sig, sample_group, "sample_size"),
    stress_auc = .non_significant_auc_groups(data, non_sig, stress_group, "design_layer")
  )

  list(
    status = NON_SIGNIFICANT_STATUS,
    applicable = FALSE,
    approved = FALSE,
    cutoffs = c(NA_real_, NA_real_),
    lower_cutoff = NA_real_, upper_cutoff = NA_real_,
    cutoff_fragile = NA_real_, cutoff_robust = NA_real_,
    criteria_passed = isTRUE(discrimination$pass && ordering$pass && cross_family_consistency$pass),
    discrimination = discrimination,
    ordering = ordering,
    cross_family_consistency = cross_family_consistency,
    summaries = summaries,
    diagnostics = list(
      n_rows = nrow(data), n_non_significant = sum(non_sig),
      n_true_null_non_rejections = sum(true_null), n_false_negatives = sum(false_negative),
      n_other_non_significant = sum(non_sig & !true_null & !false_negative),
      components = components
    )
  )
}

# US spelling and explicit policy aliases used by report scripts.
analyze_non_significant <- analyse_non_significant
evaluate_non_significant_policy <- analyse_non_significant
analyse_non_significant_results <- analyse_non_significant
evaluate_non_significant_results <- analyse_non_significant

non_significant_registry <- function(replicates, ...) {
  result <- analyse_non_significant(replicates, ...)
  families <- sort(unique(as.character(replicates$analysis_family)))
  data.frame(
    analysis_family = families,
    conclusion_type = "non_significant",
    lower_cutoff = NA_real_, upper_cutoff = NA_real_,
    cutoff_fragile = NA_real_, cutoff_robust = NA_real_,
    status = result$status, applicable = FALSE,
    reason = "dedicated non-significant analysis is exploratory; categorical bands are not defined",
    stringsAsFactors = FALSE
  )
}
