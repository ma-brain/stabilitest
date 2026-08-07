# Frozen-candidate held-out validation for the binary-proportion study.
#
# Validates a frozen Track A'' candidate on held-out data once, with no refit
# and no second candidate.  Acceptance takes the more conservative of the Wilson
# and scenario-cluster bootstrap bounds (seed 20260808, B = 1000).

.prop_hash_object <- function(object) {
  if (exists("calibration_hash_object", mode = "function", inherits = TRUE)) {
    return(calibration_hash_object(object))
  }
  path <- tempfile("prop-hash-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  unname(as.character(tools::md5sum(path)))
}

# Scenario-cluster bootstrap bound on a statistic.  Resamples whole scenarios
# with replacement (matching the lm_ancova cluster bound discipline).
prop_cluster_bound <- function(data, statistic, side = c("lower", "upper"),
                                B = 1000L, seed = 20260808L,
                                conf_level = 0.95) {
  side <- match.arg(side)
  if (!is.data.frame(data) || !"scenario_id" %in% names(data)) {
    stop("cluster data must contain scenario_id", call. = FALSE)
  }
  if (!is.function(statistic)) stop("statistic must be a function", call. = FALSE)
  clusters <- unique(as.character(data$scenario_id))
  if (!length(clusters)) {
    stop("at least one scenario cluster is required", call. = FALSE)
  }
  estimate <- statistic(data)
  if (!is.numeric(estimate) || length(estimate) != 1L || !is.finite(estimate)) {
    stop("statistic must return one finite numeric value", call. = FALSE)
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))
  draws <- vapply(seq_len(as.integer(B)), function(i) {
    selected <- sample(clusters, size = length(clusters), replace = TRUE)
    rows <- do.call(rbind, lapply(selected, function(cluster) {
      data[data$scenario_id == cluster, , drop = FALSE]
    }))
    value <- statistic(rows)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("statistic returned a non-finite bootstrap value", call. = FALSE)
    }
    as.numeric(value)
  }, numeric(1))

  alpha <- 1 - conf_level
  bound <- if (identical(side, "upper")) {
    as.numeric(stats::quantile(draws, conf_level, names = FALSE, type = 6))
  } else {
    as.numeric(stats::quantile(draws, alpha, names = FALSE, type = 6))
  }
  list(
    estimate = as.numeric(estimate),
    bound = bound,
    side = side,
    draws = draws,
    conf_level = conf_level,
    B = as.integer(B),
    seed = as.integer(seed),
    clusters = clusters,
    n_clusters = length(clusters),
    cluster = "scenario"
  )
}

.prop_quota_ok <- function(data, min_n = 100L) {
  counts <- table(as.character(data$scenario_id))
  length(counts) > 0L && all(counts >= min_n)
}

# Validate a frozen Track A'' candidate on held-out data once.  No refit; the
# cutoff is taken from the frozen candidate.  Acceptance uses the more
# conservative of the Wilson and scenario-cluster bootstrap bounds.
validate_binary_proportion_candidate <- function(frozen, validation,
                                                  scenario_manifest_hash,
                                                  training_manifest_hash,
                                                  validation_manifest_hash,
                                                  cluster_B = 1000L,
                                                  cluster_seed = 20260808L) {
  if (!is.list(frozen) || is.null(frozen$candidate_hash)) {
    stop("validate_binary_proportion_candidate requires a frozen candidate object",
         call. = FALSE)
  }
  if (!identical(frozen$scenario_manifest_hash, scenario_manifest_hash) ||
      !identical(frozen$training_manifest_hash, training_manifest_hash)) {
    stop("frozen candidate manifest hashes do not match validation inputs",
         call. = FALSE)
  }
  if (!identical(frozen$status, "candidate")) {
    return(list(
      status = "uncalibrated",
      reason = frozen$reason %||% "no_feasible_thresholds",
      cutoff = frozen$cutoff,
      validation_refit = FALSE,
      held_out_opened = FALSE,
      validation_manifest_hash = validation_manifest_hash
    ))
  }

  cutoff <- as.integer(frozen$cutoff)
  metrics <- prop_cutoff_metrics(validation, cutoff)

  if (!.prop_quota_ok(validation, min_n = 100L)) {
    return(list(
      status = "uncalibrated",
      reason = "quota_shortfall",
      cutoff = cutoff,
      metrics = metrics,
      validation_refit = FALSE,
      held_out_opened = TRUE,
      validation_manifest_hash = validation_manifest_hash,
      candidate_hash = frozen$candidate_hash
    ))
  }

  fr_cluster <- prop_cluster_bound(
    validation[as.character(validation$truth_class) == "null", , drop = FALSE],
    function(rows) {
      value <- prop_cutoff_metrics(rows, cutoff)$false_reassurance
      if (!is.finite(value)) 1 else as.numeric(value)
    },
    side = "upper", B = cluster_B, seed = cluster_seed
  )
  ri_cluster <- prop_cluster_bound(
    validation[as.character(validation$truth_class) == "clear", , drop = FALSE],
    function(rows) {
      value <- prop_cutoff_metrics(rows, cutoff)$robust_identification
      if (!is.finite(value)) 0 else as.numeric(value)
    },
    side = "lower", B = cluster_B, seed = cluster_seed + 1L
  )

  # Conservative bounds: worst (max FR upper, min RI lower) of Wilson and cluster.
  fr_upper <- max(metrics$false_reassurance_upper, fr_cluster$bound, na.rm = TRUE)
  ri_lower <- min(metrics$robust_identification_lower, ri_cluster$bound, na.rm = TRUE)

  reasons <- character()
  if (!isTRUE(metrics$false_reassurance <= 0.05) || !isTRUE(fr_upper <= 0.10)) {
    reasons <- c(reasons, "false_reassurance")
  }
  if (!isTRUE(metrics$robust_identification >= 0.70) || !isTRUE(ri_lower >= 0.60)) {
    reasons <- c(reasons, "robust_identification")
  }

  status <- if (length(reasons) == 0L) "validated_method_specific" else "uncalibrated"
  list(
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = ",") else NA_character_,
    cutoff = cutoff,
    metrics = metrics,
    conservative_bounds = list(
      false_reassurance_upper = fr_upper,
      robust_identification_lower = ri_lower,
      wilson = list(
        false_reassurance_upper = metrics$false_reassurance_upper,
        robust_identification_lower = metrics$robust_identification_lower
      ),
      cluster = list(
        false_reassurance_upper = fr_cluster$bound,
        robust_identification_lower = ri_cluster$bound
      )
    ),
    validation_refit = FALSE,
    held_out_opened = TRUE,
    validation_manifest_hash = validation_manifest_hash,
    candidate_hash = frozen$candidate_hash,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
}
