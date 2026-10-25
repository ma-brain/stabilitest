# Frozen single-cutoff threshold fitting for the Welch 2026-3 relaxed-gates
# study (CALIBRATION_SAP_2026-3.md). Additive: nothing here changes the frozen
# 2026-2 constants or functions in thresholds.R, which supply the eligibility
# filter, Wilson and scenario-cluster bounds, and ordering diagnostics.

WELCH3_GATE_CONSTANTS <- list(
  false_reassurance_point_max = 0.10,
  false_reassurance_upper_max = 0.15,
  clear_identification_point_min = 0.60,
  clear_identification_lower_min = 0.50,
  minimum_band_occupancy = 0.05,
  archetype_false_reassurance_max = 0.25,
  neighbourhood = 2L,
  cutoff_range = 50L:90L,
  p_grid = c(0.04, 0.03, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002,
             0.0001),
  baseline_min_gain = 0.02,
  cluster_bootstrap_B = 1000L,
  cluster_bootstrap_seed = 202740001L,
  baseline_bootstrap_seed = 202760001L
)

# Rate of `stable` within each archetype, restricted to rows of one truth class.
.welch3_archetype_rate <- function(data, stable, truth) {
  if (!"archetype_id" %in% names(data)) return(NULL)
  rows <- data$truth == truth
  rates <- tapply(stable[rows], as.character(data$archetype_id[rows]), mean)
  stats::setNames(as.numeric(rates), names(rates))
}

# Metrics for one logical `Stable` label. `stat_label(rows)` must return the
# `Stable` rate for a block of rows; it is only used for cluster bounds.
.welch3_label_metrics <- function(data, stable, stat_label = NULL,
                                  cluster_B = 0L, cluster_seed = 1L) {
  null_rows <- data$truth == "null"
  clear_rows <- data$truth == "clear"
  fr_count <- sum(null_rows & stable)
  fr_n <- sum(null_rows)
  ri_count <- sum(clear_rows & stable)
  ri_n <- sum(clear_rows)

  fr_cluster <- NA_real_
  ri_cluster <- NA_real_
  if (!is.null(stat_label) && !is.null(cluster_B) && as.integer(cluster_B) > 0L) {
    fr_cluster <- welch_cluster_bound(
      data[null_rows, , drop = FALSE], stat_label,
      side = "upper", B = cluster_B, seed = cluster_seed
    )$bound
    ri_cluster <- welch_cluster_bound(
      data[clear_rows, , drop = FALSE], stat_label,
      side = "lower", B = cluster_B, seed = cluster_seed + 1L
    )$bound
  }
  fr_wilson <- welch_wilson_bound(fr_count, fr_n, "upper")
  ri_wilson <- welch_wilson_bound(ri_count, ri_n, "lower")
  list(
    false_reassurance = fr_count / fr_n,
    false_reassurance_count = as.integer(fr_count),
    false_reassurance_n = as.integer(fr_n),
    false_reassurance_wilson_upper = fr_wilson,
    false_reassurance_cluster_upper = fr_cluster,
    false_reassurance_conservative_upper = welch_conservative_bound(
      fr_wilson, fr_cluster, "upper"
    ),
    clear_identification = ri_count / ri_n,
    clear_identification_count = as.integer(ri_count),
    clear_identification_n = as.integer(ri_n),
    clear_identification_wilson_lower = ri_wilson,
    clear_identification_cluster_lower = ri_cluster,
    clear_identification_conservative_lower = welch_conservative_bound(
      ri_wilson, ri_cluster, "lower"
    ),
    band_occupancy = mean(stable),
    archetype_false_reassurance = .welch3_archetype_rate(data, stable, "null"),
    archetype_clear_identification = .welch3_archetype_rate(data, stable, "clear")
  )
}

#' Metrics for one `Stable` cutoff (`score >= cutoff`) on significant rows.
welch3_candidate_metrics <- function(data, cutoff, cluster_B = 1000L,
                                     cluster_seed = WELCH3_GATE_CONSTANTS$cluster_bootstrap_seed) {
  data <- .welch_eligible_scores(data)
  cutoff <- as.integer(cutoff)
  if (is.na(cutoff) || cutoff < 0L || cutoff > 100L) {
    stop("cutoff must be an integer from 0 through 100", call. = FALSE)
  }
  metrics <- .welch3_label_metrics(
    data, data$overall_robustness >= cutoff,
    stat_label = function(rows) mean(rows$overall_robustness >= cutoff),
    cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  ordering <- .welch_ordering_diagnostics(data)
  c(
    list(mapping_type = "single_band",
         cutoffs = c(lower = cutoff, upper = NA_integer_)),
    metrics,
    list(
      pooled_medians = ordering$medians,
      pooled_ordering_ok = ordering$pooled_ok,
      material_reversal = ordering$material_reversal,
      archetype_ordering = ordering$reversals
    )
  )
}

#' Apply the 2026-3 gates to one set of metrics.
#'
#' `scored = FALSE` skips the score-ordering gates, for the p-only comparator.
welch3_candidate_gate <- function(metrics, scored = TRUE) {
  g <- WELCH3_GATE_CONSTANTS
  reasons <- character()
  fail <- function(reason) reasons <<- c(reasons, reason)
  if (!isTRUE(metrics$false_reassurance <= g$false_reassurance_point_max)) {
    fail("false_reassurance_point")
  }
  if (!isTRUE(metrics$false_reassurance_conservative_upper <=
              g$false_reassurance_upper_max)) {
    fail("false_reassurance_upper")
  }
  if (!isTRUE(metrics$clear_identification >= g$clear_identification_point_min)) {
    fail("clear_identification_point")
  }
  if (!isTRUE(metrics$clear_identification_conservative_lower >=
              g$clear_identification_lower_min)) {
    fail("clear_identification_lower")
  }
  if (!isTRUE(metrics$band_occupancy >= g$minimum_band_occupancy)) {
    fail("band_occupancy")
  }
  # Fail closed when archetypes are unknown: the guard cannot be evaluated.
  if (is.null(metrics$archetype_false_reassurance) ||
      !isTRUE(all(metrics$archetype_false_reassurance <=
                  g$archetype_false_reassurance_max))) {
    fail("archetype_false_reassurance")
  }
  if (scored) {
    if (!isTRUE(metrics$pooled_ordering_ok)) fail("pooled_ordering")
    if (isTRUE(metrics$material_reversal)) fail("material_reversal")
  }
  list(feasible = length(reasons) == 0L, reasons = unique(reasons))
}

.welch3_grid_row <- function(label, metrics, gate) {
  worst <- if (is.null(metrics$archetype_false_reassurance)) {
    NA_real_
  } else {
    max(metrics$archetype_false_reassurance)
  }
  data.frame(
    label = label,
    false_reassurance = metrics$false_reassurance,
    false_reassurance_upper = metrics$false_reassurance_conservative_upper,
    clear_identification = metrics$clear_identification,
    clear_identification_lower = metrics$clear_identification_conservative_lower,
    band_occupancy = metrics$band_occupancy,
    worst_archetype_false_reassurance = worst,
    feasible = gate$feasible,
    reasons = paste(gate$reasons, collapse = ","),
    stringsAsFactors = FALSE
  )
}

# Evaluate every grid point. Wilson-only metrics first (cluster bounds can only
# widen the intervals, so a point that fails here cannot pass later); cluster
# bounds are added only for the survivors. `gate_extra` supplies ordering
# fields for the score-based gates.
.welch3_evaluate_grid <- function(data, labels, stat_factory, scored,
                                  gate_extra, cluster_B, cluster_seed) {
  grid <- vector("list", length(labels))
  for (i in seq_along(labels)) {
    metrics <- .welch3_label_metrics(data, labels[[i]])
    gate <- welch3_candidate_gate(c(metrics, gate_extra), scored = scored)
    if (gate$feasible) {
      metrics <- .welch3_label_metrics(
        data, labels[[i]], stat_label = stat_factory(i),
        cluster_B = cluster_B, cluster_seed = cluster_seed
      )
      gate <- welch3_candidate_gate(c(metrics, gate_extra), scored = scored)
    }
    grid[[i]] <- list(metrics = metrics, gate = gate)
  }
  grid
}

# TRUE where the point and its `k` neighbours on each side all pass.
.welch3_neighbourhood_ok <- function(feasible, k) {
  n <- length(feasible)
  vapply(seq_len(n), function(i) {
    idx <- (i - k):(i + k)
    all(idx >= 1L & idx <= n) && all(feasible[idx])
  }, logical(1))
}

.welch3_grid_table <- function(grid, labels) {
  table <- do.call(rbind, lapply(seq_along(grid), function(i) {
    .welch3_grid_row(labels[[i]], grid[[i]]$metrics, grid[[i]]$gate)
  }))
  table$eligible <- .welch3_neighbourhood_ok(
    table$feasible, WELCH3_GATE_CONSTANTS$neighbourhood
  )
  table
}

#' Fit the frozen 2026-3 single-cutoff candidate on training scores.
#'
#' Eligibility requires the gates at every integer in `[U - 2, U + 2]`, all
#' within the 50-90 search range. Among eligible cutoffs: lowest training FR,
#' then highest clear identification, then lowest `U`.
fit_welch3_candidate <- function(training,
                                 cluster_B = WELCH3_GATE_CONSTANTS$cluster_bootstrap_B,
                                 cluster_seed = WELCH3_GATE_CONSTANTS$cluster_bootstrap_seed) {
  g <- WELCH3_GATE_CONSTANTS
  training <- .welch_eligible_scores(training)
  cutoffs <- g$cutoff_range
  score <- training$overall_robustness
  ordering <- .welch_ordering_diagnostics(training)

  grid <- .welch3_evaluate_grid(
    training,
    labels = lapply(cutoffs, function(u) score >= u),
    stat_factory = function(i) {
      cutoff <- cutoffs[[i]]
      function(rows) mean(rows$overall_robustness >= cutoff)
    },
    scored = TRUE,
    gate_extra = list(pooled_ordering_ok = ordering$pooled_ok,
                      material_reversal = ordering$material_reversal),
    cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  table <- .welch3_grid_table(grid, cutoffs)
  historical <- welch_candidate_metrics(
    training, "three_band", 55L, 70L,
    cluster_B = cluster_B, cluster_seed = 202640001L
  )

  if (!any(table$eligible)) {
    return(list(
      status = "no_feasible_thresholds",
      reason = "no_feasible_thresholds",
      mapping_type = "none",
      cutoffs = c(NA_integer_, NA_integer_),
      metrics = NULL, selected = NULL, grid = table,
      p_comparator = NULL, historical_55_70 = historical
    ))
  }
  eligible <- table[table$eligible, , drop = FALSE]
  pick <- order(eligible$false_reassurance, -eligible$clear_identification,
                eligible$label)[[1L]]
  cutoff <- as.integer(eligible$label[[pick]])
  metrics <- welch3_candidate_metrics(
    training, cutoff, cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  list(
    status = "candidate",
    reason = NA_character_,
    mapping_type = "single_band",
    cutoffs = c(cutoff, NA_integer_),
    metrics = metrics,
    selected = eligible[pick, , drop = FALSE],
    grid = table,
    p_comparator = fit_welch3_p_comparator(
      training, target_fr = metrics$false_reassurance,
      cluster_B = cluster_B, cluster_seed = cluster_seed
    ),
    historical_55_70 = historical
  )
}

#' Fit the p-value-only comparator (`original_p <= q`) on training rows.
#'
#' Same gates (minus score ordering) and tie rules as the score candidate, with
#' a neighbourhood of two adjacent grid steps. Among eligible grid points pick
#' the one whose training FR is closest to `target_fr`; ties go to smaller `q`.
#' With no eligible point the status is `no_eligible_comparator`, which the
#' validation step treats as a fail-closed outcome.
fit_welch3_p_comparator <- function(training, target_fr,
                                    cluster_B = WELCH3_GATE_CONSTANTS$cluster_bootstrap_B,
                                    cluster_seed = WELCH3_GATE_CONSTANTS$cluster_bootstrap_seed) {
  training <- .welch_eligible_scores(training)
  if (!"original_p" %in% names(training)) {
    stop("p-only comparator requires an original_p column", call. = FALSE)
  }
  grid_q <- WELCH3_GATE_CONSTANTS$p_grid
  p <- training$original_p
  grid <- .welch3_evaluate_grid(
    training,
    labels = lapply(grid_q, function(q) p <= q),
    stat_factory = function(i) {
      q <- grid_q[[i]]
      function(rows) mean(rows$original_p <= q)
    },
    scored = FALSE, gate_extra = list(),
    cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  table <- .welch3_grid_table(grid, grid_q)
  if (!any(table$eligible)) {
    return(list(status = "no_eligible_comparator", q = NA_real_, grid = table))
  }
  eligible <- table[table$eligible, , drop = FALSE]
  pick <- order(abs(eligible$false_reassurance - target_fr), eligible$label)[[1L]]
  list(
    status = "comparator",
    q = as.numeric(eligible$label[[pick]]),
    selected = eligible[pick, , drop = FALSE],
    grid = table
  )
}

#' Compare the frozen score rule with the frozen p-only comparator.
#'
#' The score beats the baseline when its clear-identification rate exceeds the
#' comparator's by at least `baseline_min_gain` and the one-sided 95% lower
#' bound of the paired scenario-cluster difference is at least zero. Rules are
#' frozen; nothing is rematched on the data passed here.
welch3_baseline_comparison <- function(data, cutoff, q,
                                       B = WELCH3_GATE_CONSTANTS$cluster_bootstrap_B,
                                       seed = WELCH3_GATE_CONSTANTS$baseline_bootstrap_seed) {
  data <- .welch_eligible_scores(data)
  if (!"original_p" %in% names(data)) {
    stop("baseline comparison requires an original_p column", call. = FALSE)
  }
  clear <- data[data$truth == "clear", , drop = FALSE]
  null <- data[data$truth == "null", , drop = FALSE]
  difference <- function(rows) {
    mean(rows$overall_robustness >= cutoff) - mean(rows$original_p <= q)
  }
  bound <- welch_cluster_bound(clear, difference, side = "lower",
                               B = B, seed = seed)
  gain <- difference(clear)
  list(
    clear_identification_score = mean(clear$overall_robustness >= cutoff),
    clear_identification_p_only = mean(clear$original_p <= q),
    false_reassurance_score = mean(null$overall_robustness >= cutoff),
    false_reassurance_p_only = mean(null$original_p <= q),
    gain = gain,
    gain_lower_bound = bound$bound,
    beats_baseline = isTRUE(gain >= WELCH3_GATE_CONSTANTS$baseline_min_gain) &&
      isTRUE(bound$bound >= 0),
    cutoff = as.integer(cutoff),
    q = q,
    seed = as.integer(seed),
    B = as.integer(B)
  )
}

#' Single-opening held-out confirmation of a frozen 2026-3 candidate.
#'
#' Reuses `freeze_welch_candidate()` / `recompute_welch_candidate_hash()`.
#' An unavailable p-value comparator fails closed without opening validation.
validate_frozen_welch3_candidate <- function(frozen, validation,
                                             occupancy_pass = TRUE,
                                             cluster_B = WELCH3_GATE_CONSTANTS$cluster_bootstrap_B,
                                             cluster_seed = WELCH3_GATE_CONSTANTS$cluster_bootstrap_seed) {
  if (!is.list(frozen) || is.null(frozen$candidate_hash)) {
    stop("validation requires a frozen candidate", call. = FALSE)
  }
  if (!identical(recompute_welch_candidate_hash(frozen), frozen$candidate_hash)) {
    stop("frozen candidate hash mismatch", call. = FALSE)
  }
  closed <- function(reason, opened) {
    list(status = "uncalibrated", reason = reason, held_out_opened = opened,
         validation_refit = FALSE, candidate_hash = frozen$candidate_hash)
  }
  if (!identical(frozen$status, "candidate")) {
    return(closed(frozen$reason, FALSE))
  }
  if (!isTRUE(occupancy_pass)) {
    return(closed("validation_occupancy_failure", TRUE))
  }
  comparator <- frozen$p_comparator
  if (is.null(comparator) || !identical(comparator$status, "comparator")) {
    # Known from training alone, so final at the freeze.
    return(closed("p_comparator_unavailable", FALSE))
  }
  cutoff <- frozen$cutoffs[[1L]]
  metrics <- welch3_candidate_metrics(
    validation, cutoff, cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  reasons <- welch3_candidate_gate(metrics, scored = TRUE)$reasons
  baseline <- welch3_baseline_comparison(
    validation, cutoff, comparator$q, B = cluster_B
  )
  if (!isTRUE(baseline$beats_baseline)) reasons <- c(reasons, "p_baseline")
  passed <- length(reasons) == 0L
  list(
    status = if (passed) "validated_method_specific" else "uncalibrated",
    reason = if (passed) NA_character_ else paste(unique(reasons), collapse = ","),
    mapping_type = frozen$mapping_type,
    cutoffs = frozen$cutoffs,
    metrics = metrics,
    baseline = baseline,
    held_out_opened = TRUE,
    validation_refit = FALSE,
    candidate_hash = frozen$candidate_hash
  )
}

#' JSON-friendly metrics for diagnostics: `compact_welch_metrics()` plus named
#' vectors (cutoffs, per-archetype rates, medians) kept as named lists.
compact_welch3_metrics <- function(metrics) {
  if (is.null(metrics)) return(NULL)
  compact <- compact_welch_metrics(metrics)
  for (name in names(compact)) {
    value <- compact[[name]]
    if (is.atomic(value) && !is.null(names(value))) {
      compact[[name]] <- as.list(value)
    }
  }
  compact
}

#' Compact p-value comparator summary for diagnostics (no grid).
compact_welch3_comparator <- function(comparator) {
  if (is.null(comparator)) return(NULL)
  list(
    status = comparator$status,
    q = comparator$q,
    selected = if (is.null(comparator$selected)) NULL
      else as.list(comparator$selected)
  )
}

#' TRUE when a frozen 2026-3 decision is final without opening validation:
#' no feasible cutoff, or a candidate whose p-value comparator is unavailable.
welch3_closed_at_freeze <- function(candidate) {
  identical(candidate$status, "no_feasible_thresholds") ||
    (identical(candidate$status, "candidate") &&
       !identical(candidate$p_comparator$status, "comparator"))
}

welch3_closed_reason <- function(candidate) {
  if (identical(candidate$status, "no_feasible_thresholds")) {
    "no_feasible_thresholds"
  } else {
    "p_comparator_unavailable"
  }
}
