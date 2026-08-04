# Constrained score-band fitting and locked held-out validation.

CALIBRATION_SHARED_CUTOFFS <- c(55L, 70L)
CALIBRATION_FAMILY_IMPROVEMENT <- 0.05
CALIBRATION_MATERIAL_CUTOFF_DIFFERENCE <- 5L
# Shared policy must show usable three-class ordinal discrimination, not only
# FR/RI on the extremes. Perfect null+clear with every borderline dumped into
# robust yields balanced ordinal accuracy 2/3; require ≥ 0.70 so the moderate
# band contributes meaningfully before shared "passes" and blocks family fits.
CALIBRATION_SHARED_MIN_BALANCED_ACCURACY <- 0.70

# Internal decision statuses stay stable for existing tests; Phase 6 / package
# publication vocabulary is exposed via map_calibration_status() / status_public.
CALIBRATION_STATUS_PUBLIC <- c(
  validated = "validated_shared",
  family_specific = "validated_family_specific",
  uncalibrated = "uncalibrated",
  bands_not_applicable = "bands_not_applicable"
)

map_calibration_status <- function(status) {
  status <- as.character(status)
  mapped <- unname(CALIBRATION_STATUS_PUBLIC[status])
  unknown <- is.na(mapped) & !is.na(status)
  if (any(unknown)) {
    .threshold_abort(sprintf(
      "unsupported calibration status for public mapping: %s",
      paste(unique(status[unknown]), collapse = ", ")
    ))
  }
  mapped
}

.threshold_abort <- function(message) stop(message, call. = FALSE)

.threshold_wilson <- function(x, n, side = c("lower", "upper"), conf_level = 0.95) {
  side <- match.arg(side)
  if (!is.numeric(x) || length(x) != 1L || !is.numeric(n) || length(n) != 1L ||
      !is.finite(x) || !is.finite(n) || n <= 0 || x < 0 || x > n ||
      conf_level <= 0 || conf_level >= 1) return(NA_real_)
  # One-sided 95% bounds use the 95th percentile, not the two-sided 97.5th.
  z <- stats::qnorm(conf_level)
  centre <- (x + z^2 / 2) / (n + z^2)
  radius <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
  if (identical(side, "upper")) min(1, centre + radius) else max(0, centre - radius)
}

.threshold_manifest <- function(manifest, label, expected_split) {
  if (is.null(manifest)) return(NULL)
  if (!is.list(manifest)) .threshold_abort(sprintf("%s manifest must be a list", label))
  required <- c("manifest_version", "scenario_manifest_hash", "options")
  missing <- setdiff(required, names(manifest))
  if (length(missing)) .threshold_abort(sprintf("%s manifest missing: %s", label, paste(missing, collapse = ", ")))
  if (!is.character(manifest$manifest_version) || length(manifest$manifest_version) != 1L ||
      is.na(manifest$manifest_version) || !grepl("^calibration-", manifest$manifest_version)) {
    .threshold_abort(sprintf("%s manifest_version is unsupported", label))
  }
  hash <- manifest$scenario_manifest_hash
  if (!is.character(hash) || length(hash) != 1L || is.na(hash) || !nzchar(hash)) {
    .threshold_abort(sprintf("%s scenario_manifest_hash must be non-empty", label))
  }
  if (!is.list(manifest$options)) .threshold_abort(sprintf("%s manifest options must be a list", label))
  if (!is.null(manifest$options$validation_only) &&
      (!is.logical(manifest$options$validation_only) || length(manifest$options$validation_only) != 1L ||
       is.na(manifest$options$validation_only))) {
    .threshold_abort(sprintf("%s manifest validation_only option must be logical", label))
  }
  if (identical(expected_split, "validation") && isFALSE(manifest$options$validation_only)) {
    .threshold_abort(sprintf("%s manifest options are not validation-only", label))
  }
  if (identical(expected_split, "training") && isTRUE(manifest$options$validation_only)) {
    .threshold_abort(sprintf("%s manifest options are validation-only", label))
  }
  split <- if (!is.null(manifest$split)) manifest$split else manifest$options$split
  if (!is.null(split) && (!is.character(split) || length(split) != 1L ||
                          !split %in% c("training", "validation"))) {
    .threshold_abort(sprintf("%s manifest split must be training or validation", label))
  }
  if (!is.null(split) && !identical(split, expected_split)) {
    .threshold_abort(sprintf("%s manifest split does not match expected %s", label, expected_split))
  }
  manifest
}

.threshold_manifest_pair <- function(training_manifest, validation_manifest) {
  if (is.null(training_manifest) || is.null(validation_manifest)) return(invisible(NULL))
  if (!identical(training_manifest$scenario_manifest_hash,
                 validation_manifest$scenario_manifest_hash)) {
    .threshold_abort("training and validation scenario_manifest_hash values differ")
  }
  invisible(NULL)
}

.threshold_hash_object <- function(object) {
  if (exists("calibration_hash_object", mode = "function", inherits = TRUE)) {
    return(calibration_hash_object(object))
  }
  raw <- serialize(object, NULL, version = 2)
  # Stable fallback for standalone sourcing when manifest.R is not loaded.
  hash <- 104729
  for (byte in as.integer(raw)) hash <- (hash * 131 + byte) %% 2147483647
  sprintf("%08x", as.integer(hash))
}

.threshold_cutoffs <- function(cutoffs, name = "cutoffs") {
  if (!is.numeric(cutoffs) || length(cutoffs) != 2L || any(!is.finite(cutoffs)) ||
      any(cutoffs != floor(cutoffs)) || any(cutoffs < 0 | cutoffs > 100) ||
      cutoffs[[1L]] >= cutoffs[[2L]]) {
    .threshold_abort(sprintf("%s must be two ordered integer cutoffs in [0, 100]", name))
  }
  as.integer(cutoffs)
}

.threshold_data <- function(replicates, split = c("training", "validation"), allow_empty = FALSE) {
  split <- match.arg(split)
  if (!is.data.frame(replicates)) .threshold_abort("replicates must be a data frame")
  required <- c("analysis_family", "truth_class", "overall_score")
  missing <- setdiff(required, names(replicates))
  if (length(missing)) .threshold_abort(sprintf("replicates missing columns: %s", paste(missing, collapse = ", ")))
  if (identical(split, "training") && "design_layer" %in% names(replicates) &&
      any(replicates$design_layer %in% "validation", na.rm = TRUE)) {
    .threshold_abort("validation data cannot be passed to training threshold fitting")
  }
  if ("status" %in% names(replicates)) {
    bad <- !replicates$status %in% c("completed", NA_character_)
    replicates <- replicates[!bad, , drop = FALSE]
  }
  if (!allow_empty && !nrow(replicates)) .threshold_abort(sprintf("no usable %s replicates", split))
  if (!is.character(replicates$analysis_family) || anyNA(replicates$analysis_family)) {
    .threshold_abort("analysis_family must be non-missing character")
  }
  if (!is.character(replicates$truth_class) || anyNA(replicates$truth_class)) {
    .threshold_abort("truth_class must be non-missing character")
  }
  replicates
}

.threshold_expected <- function(truth, target = NULL) {
  # The frozen truth vocabulary is ordinal.  Unknown strata are retained as
  # diagnostic rows but do not contribute to balanced ordinal accuracy.
  out <- match(as.character(truth), c("null", "borderline", "clear")) - 1L
  if (!is.null(target)) {
    target <- as.character(target)
    positive <- target %in% c("significant", "equivalent", "noninferior")
    negative <- target %in% c("non_significant", "not_equivalent", "inferior")
    out[is.na(out) & positive] <- 2L
    out[is.na(out) & negative] <- 0L
  }
  out
}

classify_score_band <- function(score, cutoffs = CALIBRATION_SHARED_CUTOFFS) {
  cutoffs <- .threshold_cutoffs(cutoffs)
  if (!is.numeric(score)) .threshold_abort("score must be numeric")
  result <- rep(NA_character_, length(score))
  finite <- is.finite(score)
  result[finite & score <= cutoffs[[1L]]] <- "fragile"
  result[finite & score > cutoffs[[1L]] & score <= cutoffs[[2L]]] <- "moderate"
  result[finite & score > cutoffs[[2L]]] <- "robust"
  result
}

.threshold_ordinal <- function(score, cutoffs) {
  cutoffs <- .threshold_cutoffs(cutoffs)
  out <- rep(NA_integer_, length(score))
  finite <- is.finite(score)
  out[finite & score <= cutoffs[[1L]]] <- 0L
  out[finite & score > cutoffs[[1L]] & score <= cutoffs[[2L]]] <- 1L
  out[finite & score > cutoffs[[2L]]] <- 2L
  out
}

.threshold_metrics <- function(replicates, cutoffs) {
  cutoffs <- .threshold_cutoffs(cutoffs)
  usable <- is.finite(replicates$overall_score)
  if (!any(usable)) {
    return(list(n = 0L, balanced_ordinal_accuracy = NA_real_, false_reassurance = NA_real_,
                robust_identification = NA_real_, false_reassurance_upper = NA_real_,
                robust_identification_lower = NA_real_, median_score = NA_real_, status = "failed"))
  }
  score <- as.numeric(replicates$overall_score[usable])
  truth <- as.character(replicates$truth_class[usable])
  target <- if ("target_conclusion" %in% names(replicates)) replicates$target_conclusion[usable] else NULL
  expected <- .threshold_expected(truth, target)
  observed <- .threshold_ordinal(score, cutoffs)
  known <- !is.na(expected)
  per_class <- vapply(0:2, function(class) {
    rows <- known & expected == class
    if (!any(rows)) NA_real_ else mean(observed[rows] == class)
  }, numeric(1))
  balanced <- if (all(is.na(per_class))) NA_real_ else mean(per_class, na.rm = TRUE)
  null_rows <- truth == "null"
  if (!any(null_rows) && !is.null(target)) {
    null_rows <- target %in% c("not_supported", "non_significant", "not_equivalent", "inferior")
  }
  false <- if (!any(null_rows)) 0 else mean(observed[null_rows] >= 1L, na.rm = TRUE)
  clear_rows <- truth == "clear"
  if (!any(clear_rows) && !is.null(target)) {
    clear_rows <- target %in% c("significant", "equivalent", "noninferior")
  }
  robust <- if (!any(clear_rows)) NA_real_ else mean(observed[clear_rows] == 2L, na.rm = TRUE)
  list(n = sum(usable), balanced_ordinal_accuracy = as.numeric(balanced),
       false_reassurance = as.numeric(false), robust_identification = as.numeric(robust),
       false_reassurance_upper = .threshold_wilson(sum(observed[null_rows] >= 1L, na.rm = TRUE), sum(null_rows), "upper"),
       robust_identification_lower = .threshold_wilson(sum(observed[clear_rows] == 2L, na.rm = TRUE), sum(clear_rows), "lower"),
       median_score = stats::median(score), status = "ok", cutoffs = cutoffs)
}

.threshold_constraints_ok <- function(metrics) {
  isTRUE(is.finite(metrics$false_reassurance)) && metrics$false_reassurance <= 0.05 &&
    isTRUE(is.finite(metrics$false_reassurance_upper)) && metrics$false_reassurance_upper <= 0.10 &&
    isTRUE(is.finite(metrics$robust_identification)) && metrics$robust_identification >= 0.70 &&
    isTRUE(is.finite(metrics$robust_identification_lower)) && metrics$robust_identification_lower >= 0.60
}

.threshold_row <- function(family, cutoffs = c(NA_integer_, NA_integer_), status = "uncalibrated",
                           reason = NA_character_, training = NULL, shared = NULL) {
  data.frame(
    analysis_family = as.character(family),
    lower_cutoff = as.integer(cutoffs[[1L]]), upper_cutoff = as.integer(cutoffs[[2L]]),
    shared_lower = 55L, shared_upper = 70L,
    training_balanced_accuracy = if (is.null(training)) NA_real_ else training$balanced_ordinal_accuracy,
    training_false_reassurance = if (is.null(training)) NA_real_ else training$false_reassurance,
    training_robust_identification = if (is.null(training)) NA_real_ else training$robust_identification,
    training_false_reassurance_upper = if (is.null(training)) NA_real_ else training$false_reassurance_upper,
    training_robust_identification_lower = if (is.null(training)) NA_real_ else training$robust_identification_lower,
    shared_balanced_accuracy = if (is.null(shared)) NA_real_ else shared$balanced_ordinal_accuracy,
    shared_false_reassurance = if (is.null(shared)) NA_real_ else shared$false_reassurance,
    shared_robust_identification = if (is.null(shared)) NA_real_ else shared$robust_identification,
    shared_false_reassurance_upper = if (is.null(shared)) NA_real_ else shared$false_reassurance_upper,
    shared_robust_identification_lower = if (is.null(shared)) NA_real_ else shared$robust_identification_lower,
    heldout_balanced_accuracy = NA_real_, shared_heldout_accuracy = NA_real_,
    heldout_improvement = NA_real_, material_difference = NA_integer_,
    heldout_false_reassurance_upper = NA_real_, heldout_robust_identification_lower = NA_real_,
    median_ordering_ok = NA, stratum_complete = NA, improvement_direction_ok = NA,
    status = as.character(status), reason = as.character(reason), stringsAsFactors = FALSE
  )
}

.threshold_validation_diagnostics <- function(data, minimum_stratum_n = 100L) {
  truths <- c("null", "borderline", "clear")
  counts <- table(factor(data$truth_class, levels = truths))
  complete <- all(as.numeric(counts) >= as.integer(minimum_stratum_n))
  ordering <- TRUE
  layers <- if ("design_layer" %in% names(data)) unique(as.character(data$design_layer)) else "all"
  for (layer in layers) {
    group <- if (identical(layer, "all")) data else data[as.character(data$design_layer) == layer, , drop = FALSE]
    layer_counts <- table(factor(group$truth_class, levels = truths))
    complete <- complete && all(as.numeric(layer_counts) >= as.integer(minimum_stratum_n))
    medians <- vapply(truths, function(truth) {
      values <- group$overall_score[group$truth_class == truth & is.finite(group$overall_score)]
      if (!length(values)) NA_real_ else stats::median(values)
    }, numeric(1))
    present <- is.finite(medians)
    if (sum(present) >= 2L && any(diff(medians[present]) < 0)) ordering <- FALSE
  }
  list(stratum_complete = complete, median_ordering_ok = ordering,
       stratum_counts = counts)
}

.threshold_stratum_accuracy <- function(data, cutoffs) {
  truths <- c("null", "borderline", "clear")
  observed <- .threshold_ordinal(data$overall_score, cutoffs)
  vapply(seq_along(truths), function(i) {
    rows <- data$truth_class == truths[[i]] & is.finite(data$overall_score)
    if (!any(rows)) NA_real_ else mean(observed[rows] == (i - 1L))
  }, numeric(1), USE.NAMES = TRUE) |> stats::setNames(truths)
}

# Search the finite, ordered integer grid.  Ties are resolved by minimizing
# distance to the frozen shared pair, then lexicographically.
fit_family_thresholds <- function(replicates, shared_cutoffs = CALIBRATION_SHARED_CUTOFFS) {
  shared_cutoffs <- .threshold_cutoffs(shared_cutoffs, "shared_cutoffs")
  if (is.data.frame(replicates) && !nrow(replicates)) {
    return(list(status = "uncalibrated", cutoffs = c(NA_integer_, NA_integer_),
                reason = "no_training_replicates"))
  }
  data <- .threshold_data(replicates, "training")
  if (!nrow(data)) return(list(status = "uncalibrated", cutoffs = c(NA_integer_, NA_integer_), reason = "no_training_replicates"))
  shared <- .threshold_metrics(data, shared_cutoffs)
  # Shared bands are always evaluated and retained as the sensitivity result.
  # A failed shared policy does not prevent fitting a pre-specified family
  # exception; only the candidate's own constraints can support it.
  if (!identical(shared$status, "ok")) {
    return(list(status = "uncalibrated", cutoffs = c(NA_integer_, NA_integer_), reason = "no_training_replicates", shared = shared))
  }
  grid <- expand.grid(lower = 0:99, upper = 1:100, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  grid <- grid[grid$lower < grid$upper, , drop = FALSE]
  rows <- lapply(seq_len(nrow(grid)), function(i) {
    cts <- c(grid$lower[[i]], grid$upper[[i]])
    m <- .threshold_metrics(data, cts)
    if (!.threshold_constraints_ok(m)) return(NULL)
    data.frame(lower = cts[[1L]], upper = cts[[2L]], objective = m$balanced_ordinal_accuracy,
               distance = abs(cts[[1L]] - shared_cutoffs[[1L]]) + abs(cts[[2L]] - shared_cutoffs[[2L]]))
  })
  feasible <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
  if (is.null(feasible) || !nrow(feasible)) {
    return(list(status = "uncalibrated", cutoffs = c(NA_integer_, NA_integer_), reason = "no_feasible_thresholds", shared = shared))
  }
  feasible <- feasible[order(-feasible$objective, feasible$distance, feasible$lower, feasible$upper), , drop = FALSE]
  best <- feasible[1L, , drop = FALSE]
  candidate <- .threshold_metrics(data, c(best$lower[[1L]], best$upper[[1L]]))
  list(status = "candidate", cutoffs = as.integer(c(best$lower[[1L]], best$upper[[1L]])),
       shared = shared, candidate = candidate, reason = NA_character_)
}

fit_calibration_candidates <- function(training_replicates,
                                        shared_cutoffs = CALIBRATION_SHARED_CUTOFFS,
                                        sap = list(), training_manifest = NULL) {
  shared_cutoffs <- .threshold_cutoffs(shared_cutoffs, "shared_cutoffs")
  if (!is.data.frame(training_replicates) || !"analysis_family" %in% names(training_replicates)) {
    .threshold_abort("training replicates must contain analysis_family")
  }
  all_families <- sort(unique(as.character(training_replicates$analysis_family)))
  data <- .threshold_data(training_replicates, "training")
  families <- all_families
  shared_by_family <- setNames(lapply(families, function(f) {
    .threshold_metrics(data[data$analysis_family == f, , drop = FALSE], shared_cutoffs)
  }), families)
  rows <- lapply(families, function(f) {
    fit <- fit_family_thresholds(data[data$analysis_family == f, , drop = FALSE], shared_cutoffs)
    if (identical(fit$status, "uncalibrated")) {
      return(.threshold_row(f, reason = fit$reason, shared = shared_by_family[[f]]))
    }
    .threshold_row(f, fit$cutoffs, status = "candidate", training = fit$candidate,
                   shared = shared_by_family[[f]])
  })
  registry <- do.call(rbind, rows)
  rownames(registry) <- NULL
  result <- list(shared_cutoffs = shared_cutoffs, shared_evaluated = TRUE,
                 shared = shared_by_family, registry = registry,
                 training_rows = nrow(data), sap = sap,
                 training_manifest = training_manifest)
  result$candidate_hash <- .threshold_hash_object(registry)
  class(result) <- c("calibration_candidates", "list")
  result
}

.threshold_registry <- function(candidates) {
  if (inherits(candidates, "calibration_candidates")) return(candidates$registry)
  if (is.data.frame(candidates)) return(candidates)
  if (is.list(candidates) && is.data.frame(candidates$registry)) return(candidates$registry)
  .threshold_abort("candidates must be a calibration candidate object or registry")
}

evaluate_calibration_registry <- function(replicates, registry,
                                          use_shared_for_uncalibrated = FALSE) {
  data <- .threshold_data(replicates, "validation")
  registry <- .threshold_registry(registry)
  required <- c("analysis_family", "lower_cutoff", "upper_cutoff")
  if (length(setdiff(required, names(registry)))) .threshold_abort("registry missing threshold columns")
  rows <- lapply(sort(unique(data$analysis_family)), function(f) {
    group <- data[data$analysis_family == f, , drop = FALSE]
    row <- registry[registry$analysis_family == f, , drop = FALSE]
    if (!nrow(row) || is.na(row$lower_cutoff[[1L]])) {
      return(data.frame(analysis_family = f, status = "uncalibrated",
                        cutoffs = I(list(c(NA_integer_, NA_integer_))),
                        balanced_ordinal_accuracy = NA_real_, n = nrow(group)))
    }
    m <- .threshold_metrics(group, c(row$lower_cutoff[[1L]], row$upper_cutoff[[1L]]))
    data.frame(analysis_family = f, status = "evaluated",
               cutoffs = I(list(c(row$lower_cutoff[[1L]], row$upper_cutoff[[1L]]))),
               balanced_ordinal_accuracy = m$balanced_ordinal_accuracy, n = m$n)
  })
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

validate_calibration_candidates <- function(candidates, validation_replicates,
                                             shared_cutoffs = CALIBRATION_SHARED_CUTOFFS,
                                             improvement = CALIBRATION_FAMILY_IMPROVEMENT,
                                             material_difference = CALIBRATION_MATERIAL_CUTOFF_DIFFERENCE,
                                             minimum_stratum_n = 100L) {
  shared_cutoffs <- .threshold_cutoffs(shared_cutoffs, "shared_cutoffs")
  data <- .threshold_data(validation_replicates, "validation")
  registry <- .threshold_registry(candidates)
  if (!is.numeric(improvement) || length(improvement) != 1L || improvement < 0 || improvement > 1) {
    .threshold_abort("improvement must be a scalar in [0, 1]")
  }
  if (!is.numeric(material_difference) || length(material_difference) != 1L || material_difference < 0) {
    .threshold_abort("material_difference must be non-negative")
  }
  if (!is.numeric(minimum_stratum_n) || length(minimum_stratum_n) != 1L ||
      !is.finite(minimum_stratum_n) || minimum_stratum_n < 1 || minimum_stratum_n != floor(minimum_stratum_n)) {
    .threshold_abort("minimum_stratum_n must be a positive integer")
  }
  out <- registry
  for (i in seq_len(nrow(out))) {
    family <- out$analysis_family[[i]]
    group <- data[data$analysis_family == family, , drop = FALSE]
    shared <- .threshold_metrics(group, shared_cutoffs)
    diagnostics <- .threshold_validation_diagnostics(group, minimum_stratum_n)
    out$shared_heldout_accuracy[[i]] <- shared$balanced_ordinal_accuracy
    out$median_ordering_ok[[i]] <- diagnostics$median_ordering_ok
    out$stratum_complete[[i]] <- diagnostics$stratum_complete
    if (is.na(out$lower_cutoff[[i]]) || !nrow(group)) {
      out$status[[i]] <- "uncalibrated"
      out$reason[[i]] <- if (!nrow(group)) "no_validation_replicates" else "training_fit_failed"
      next
    }
    candidate <- .threshold_metrics(group, c(out$lower_cutoff[[i]], out$upper_cutoff[[i]]))
    out$heldout_balanced_accuracy[[i]] <- candidate$balanced_ordinal_accuracy
    out$heldout_false_reassurance_upper[[i]] <- candidate$false_reassurance_upper
    out$heldout_robust_identification_lower[[i]] <- candidate$robust_identification_lower
    out$heldout_improvement[[i]] <- candidate$balanced_ordinal_accuracy - shared$balanced_ordinal_accuracy
    out$material_difference[[i]] <- max(abs(out$lower_cutoff[[i]] - shared_cutoffs[[1L]]),
                                         abs(out$upper_cutoff[[i]] - shared_cutoffs[[2L]]))
    shared_by_truth <- .threshold_stratum_accuracy(group, shared_cutoffs)
    candidate_by_truth <- .threshold_stratum_accuracy(group,
                                                       c(out$lower_cutoff[[i]], out$upper_cutoff[[i]]))
    direction <- candidate_by_truth - shared_by_truth
    out$improvement_direction_ok[[i]] <- all(is.na(direction) | direction >= -1e-12)
    shared_training_ok <- all(is.finite(c(out$shared_false_reassurance[[i]],
                                          out$shared_robust_identification[[i]],
                                          out$shared_false_reassurance_upper[[i]],
                                          out$shared_robust_identification_lower[[i]]))) &&
      out$shared_false_reassurance[[i]] <= 0.05 &&
      out$shared_false_reassurance_upper[[i]] <= 0.10 &&
      out$shared_robust_identification[[i]] >= 0.70 &&
      out$shared_robust_identification_lower[[i]] >= 0.60
    # Ordinal gate: FR/RI alone can ignore the moderate/borderline band.
    shared_ordinal_ok <- isTRUE(is.finite(shared$balanced_ordinal_accuracy)) &&
      shared$balanced_ordinal_accuracy >= CALIBRATION_SHARED_MIN_BALANCED_ACCURACY
    shared_policy_valid <- shared_training_ok && diagnostics$stratum_complete &&
      diagnostics$median_ordering_ok && .threshold_constraints_ok(shared) &&
      shared_ordinal_ok
    accepted <- !shared_policy_valid && diagnostics$stratum_complete && diagnostics$median_ordering_ok &&
      isTRUE(out$improvement_direction_ok[[i]]) &&
      .threshold_constraints_ok(candidate) &&
      isTRUE(out$heldout_improvement[[i]] >= improvement) &&
      isTRUE(out$material_difference[[i]] >= material_difference)
    if (accepted) {
      out$status[[i]] <- "family_specific"
      out$reason[[i]] <- "heldout_improvement_and_material_difference"
    } else if (identical(out$status[[i]], "candidate")) {
      shared_ok <- shared_policy_valid
      if (shared_ok) {
        # Consumers read lower/upper as the active published mapping. When the
        # held-out decision is shared-mapping validation, force those cutoffs
        # even if training proposed a different candidate pair.
        out$lower_cutoff[[i]] <- shared_cutoffs[[1L]]
        out$upper_cutoff[[i]] <- shared_cutoffs[[2L]]
        out$status[[i]] <- "validated"
        out$reason[[i]] <- "shared_mapping_validated"
      } else {
        out$status[[i]] <- "uncalibrated"
        out$reason[[i]] <- if (!shared_ordinal_ok && .threshold_constraints_ok(shared) &&
                                diagnostics$stratum_complete && diagnostics$median_ordering_ok) {
          "shared_ordinal_discrimination_failed"
        } else {
          "shared_and_family_specific_constraints_failed"
        }
      }
    }
  }
  out
}

freeze_calibration_registry <- function(registry) {
  registry <- .threshold_registry(registry)
  registry <- registry[order(registry$analysis_family), , drop = FALSE]
  rownames(registry) <- NULL
  # Hash internal columns only; status_public is a deterministic export view.
  candidate_hash <- .threshold_hash_object(registry)
  if ("status" %in% names(registry)) {
    registry$status_public <- map_calibration_status(registry$status)
  }
  list(registry = registry, candidate_hash = candidate_hash, frozen = TRUE)
}

write_calibration_registry_csv <- function(registry, path) {
  registry <- .threshold_registry(registry)
  if (!"status_public" %in% names(registry) && "status" %in% names(registry)) {
    registry$status_public <- map_calibration_status(registry$status)
  }
  utils::write.csv(registry, path, row.names = FALSE)
  invisible(path)
}

.threshold_assert_disjoint <- function(training, validation) {
  if (!all(c("scenario_id", "replicate_id") %in% names(training)) ||
      !all(c("scenario_id", "replicate_id") %in% names(validation))) return(invisible(TRUE))
  train_keys <- paste(training$scenario_id, training$replicate_id, sep = "\u001f")
  valid_keys <- paste(validation$scenario_id, validation$replicate_id, sep = "\u001f")
  if (any(validation$scenario_id %in% training$scenario_id)) {
    .threshold_abort("training and held-out scenario_id values overlap")
  }
  if (any(valid_keys %in% train_keys)) .threshold_abort("training and held-out data overlap")
  invisible(TRUE)
}

analyse_calibration <- function(training_replicates, validation_replicates,
                                training_manifest = NULL, validation_manifest = NULL,
                                shared_cutoffs = CALIBRATION_SHARED_CUTOFFS,
                                output = NULL, minimum_stratum_n = 100L, ...) {
  if (exists("validate_calibration_replicates", mode = "function", inherits = TRUE)) {
    validate_calibration_replicates(training_replicates)
    validate_calibration_replicates(validation_replicates)
  }
  training <- .threshold_data(training_replicates, "training")
  validation <- .threshold_data(validation_replicates, "validation")
  .threshold_assert_disjoint(training, validation)
  training_manifest <- .threshold_manifest(training_manifest, "training", "training")
  validation_manifest <- .threshold_manifest(validation_manifest, "validation", "validation")
  .threshold_manifest_pair(training_manifest, validation_manifest)
  candidates <- fit_calibration_candidates(training, shared_cutoffs = shared_cutoffs,
                                           training_manifest = training_manifest)
  candidate_hash <- candidates$candidate_hash
  registry <- validate_calibration_candidates(candidates, validation, shared_cutoffs = shared_cutoffs,
                                              minimum_stratum_n = minimum_stratum_n)
  registry$training_manifest_hash <- if (is.null(training_manifest)) NA_character_ else training_manifest$scenario_manifest_hash
  registry$validation_manifest_hash <- if (is.null(validation_manifest)) NA_character_ else validation_manifest$scenario_manifest_hash
  registry$manifest_version <- if (is.null(training_manifest)) NA_character_ else training_manifest$manifest_version
  frozen <- freeze_calibration_registry(registry)
  heldout <- evaluate_calibration_registry(validation, frozen$registry)
  result <- list(
    registry = frozen$registry, candidate_registry = candidates$registry,
    candidate_hash = candidate_hash, registry_hash = frozen$candidate_hash,
    shared_cutoffs = .threshold_cutoffs(shared_cutoffs),
    shared_evaluated = TRUE, validation = list(refit = FALSE, metrics = heldout),
    training_manifest = training_manifest, validation_manifest = validation_manifest
  )
  if (!is.null(output)) {
    if (!dir.exists(output) && !dir.create(output, recursive = TRUE, showWarnings = FALSE)) {
      .threshold_abort("unable to create analysis output directory")
    }
    saveRDS(result, file.path(output, "calibration-registry.rds"), version = 2)
    write_calibration_registry_csv(frozen$registry, file.path(output, "calibration-registry.csv"))
  }
  class(result) <- c("calibration_analysis", "list")
  result
}

# British spelling is retained as a public alias for manuscript scripts.
analyze_calibration <- analyse_calibration

# Stable aliases used by report scripts and downstream review tooling.
fit_thresholds <- function(replicates, split = "training", ...) {
  if (!identical(split, "training")) {
    .threshold_abort("validation data cannot be passed to threshold fitting")
  }
  fit_family_thresholds(replicates, ...)
}
fit_calibration_registry <- fit_calibration_candidates
validate_heldout_calibration <- validate_calibration_candidates

evaluate_shared_bands <- function(replicates, shared_cutoffs = CALIBRATION_SHARED_CUTOFFS) {
  data <- .threshold_data(replicates, "validation")
  families <- sort(unique(data$analysis_family))
  rows <- lapply(families, function(f) {
    m <- .threshold_metrics(data[data$analysis_family == f, , drop = FALSE], shared_cutoffs)
    data.frame(analysis_family = f, lower_cutoff = shared_cutoffs[[1L]],
               upper_cutoff = shared_cutoffs[[2L]], status = m$status,
               balanced_ordinal_accuracy = m$balanced_ordinal_accuracy,
               false_reassurance = m$false_reassurance,
               robust_identification = m$robust_identification, n = m$n,
               stringsAsFactors = FALSE)
  })
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}
