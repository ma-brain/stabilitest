# Frozen-candidate held-out validation for the isolated ANCOVA study.

.ancova_hash_object <- function(object) {
  if (exists("calibration_hash_object", mode = "function", inherits = TRUE)) {
    return(calibration_hash_object(object))
  }
  path <- tempfile("ancova-hash-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  unname(as.character(tools::md5sum(path)))
}

ancova_cluster_bound <- function(data, statistic, side = c("lower", "upper"),
                                 B = 1000L, seed = 1L, conf_level = 0.95) {
  side <- match.arg(side)
  if (!is.data.frame(data) || !"scenario_id" %in% names(data)) {
    stop("cluster data must contain scenario_id", call. = FALSE)
  }
  if (!is.function(statistic)) stop("statistic must be a function", call. = FALSE)
  clusters <- unique(as.character(data$scenario_id))
  if (!length(clusters)) stop("at least one scenario cluster is required", call. = FALSE)
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
  draws <- vapply(seq_len(as.integer(B)), function(iteration) {
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

.ancova_block_median_ordering <- function(data) {
  if (!all(c("sample_size", "baseline_r2") %in% names(data))) {
    metrics <- ancova_cutoff_metrics(data, c(0L, 1L))
    return(isTRUE(metrics$median_ordering_ok))
  }
  keys <- unique(data.frame(
    sample_size = data$sample_size,
    baseline_r2 = data$baseline_r2,
    stringsAsFactors = FALSE
  ))
  for (i in seq_len(nrow(keys))) {
    block <- data[
      data$sample_size == keys$sample_size[[i]] &
        data$baseline_r2 == keys$baseline_r2[[i]],
      , drop = FALSE
    ]
    medians <- vapply(c("null", "borderline", "clear"), function(level) {
      scores <- block$overall_score[block$truth_class == level]
      if (!length(scores)) NA_real_ else stats::median(scores)
    }, numeric(1))
    if (!isTRUE(
      is.finite(medians[["null"]]) && is.finite(medians[["borderline"]]) &&
        is.finite(medians[["clear"]]) &&
        medians[["null"]] < medians[["borderline"]] &&
        medians[["borderline"]] < medians[["clear"]]
    )) {
      return(FALSE)
    }
  }
  TRUE
}

.ancova_quota_ok <- function(data, min_n = 100L) {
  counts <- table(as.character(data$scenario_id))
  length(counts) > 0L && all(counts >= min_n)
}

freeze_lm_ancova_candidate <- function(fit, scenario_manifest_hash,
                                       training_manifest_hash) {
  if (!is.list(fit) || is.null(fit$status)) {
    stop("fit must be a cutoff-fitting result", call. = FALSE)
  }
  if (!identical(fit$status, "candidate")) {
    return(list(
      status = "uncalibrated",
      reason = fit$reason %||% "no_feasible_thresholds",
      cutoffs = fit$cutoffs %||% c(NA_integer_, NA_integer_),
      scenario_manifest_hash = scenario_manifest_hash,
      training_manifest_hash = training_manifest_hash,
      held_out_opened = FALSE,
      validation_refit = FALSE,
      candidate_hash = .ancova_hash_object(list(
        status = "uncalibrated",
        reason = fit$reason %||% "no_feasible_thresholds",
        cutoffs = fit$cutoffs %||% c(NA_integer_, NA_integer_),
        scenario_manifest_hash = scenario_manifest_hash,
        training_manifest_hash = training_manifest_hash
      ))
    ))
  }

  payload <- list(
    status = "candidate",
    cutoffs = as.integer(fit$cutoffs),
    metrics = fit$metrics,
    selected_row = fit$selected_row,
    welch_comparator = fit$welch_comparator,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    held_out_opened = FALSE,
    validation_refit = FALSE
  )
  payload$candidate_hash <- .ancova_hash_object(payload)
  payload
}

validate_lm_ancova_candidate <- function(frozen, validation,
                                         scenario_manifest_hash,
                                         training_manifest_hash,
                                         validation_manifest_hash,
                                         cluster_B = 1000L,
                                         cluster_seed = 20260806L) {
  if (!is.list(frozen) || is.null(frozen$candidate_hash)) {
    stop("validate_lm_ancova_candidate requires a frozen candidate object", call. = FALSE)
  }
  if (!identical(frozen$scenario_manifest_hash, scenario_manifest_hash) ||
      !identical(frozen$training_manifest_hash, training_manifest_hash)) {
    stop("frozen candidate manifest hashes do not match validation inputs", call. = FALSE)
  }
  if (!identical(frozen$status, "candidate")) {
    return(list(
      status = "uncalibrated",
      reason = frozen$reason %||% "no_feasible_thresholds",
      cutoffs = frozen$cutoffs,
      validation_refit = FALSE,
      held_out_opened = FALSE,
      validation_manifest_hash = validation_manifest_hash
    ))
  }

  cutoffs <- as.integer(frozen$cutoffs)
  metrics <- ancova_cutoff_metrics(validation, cutoffs)
  if (!.ancova_quota_ok(validation, min_n = 100L)) {
    return(list(
      status = "uncalibrated",
      reason = "quota_shortfall",
      cutoffs = cutoffs,
      metrics = metrics,
      validation_refit = FALSE,
      held_out_opened = TRUE,
      validation_manifest_hash = validation_manifest_hash,
      candidate_hash = frozen$candidate_hash
    ))
  }

  fr_cluster <- ancova_cluster_bound(
    validation[as.character(validation$truth_class) == "null", , drop = FALSE],
    function(rows) {
      value <- ancova_cutoff_metrics(rows, cutoffs)$false_reassurance
      if (!is.finite(value)) 1 else as.numeric(value)
    },
    side = "upper", B = cluster_B, seed = cluster_seed
  )
  ri_cluster <- ancova_cluster_bound(
    validation[as.character(validation$truth_class) == "clear", , drop = FALSE],
    function(rows) {
      value <- ancova_cutoff_metrics(rows, cutoffs)$robust_identification
      if (!is.finite(value)) 0 else as.numeric(value)
    },
    side = "lower", B = cluster_B, seed = cluster_seed + 1L
  )
  bal_cluster <- ancova_cluster_bound(
    validation,
    function(rows) {
      value <- ancova_cutoff_metrics(rows, cutoffs)$balanced_accuracy
      if (!is.finite(value)) 0 else as.numeric(value)
    },
    side = "lower", B = cluster_B, seed = cluster_seed + 2L
  )

  fr_upper <- max(metrics$false_reassurance_upper, fr_cluster$bound, na.rm = TRUE)
  ri_lower <- min(metrics$robust_identification_lower, ri_cluster$bound, na.rm = TRUE)
  bal_lower <- bal_cluster$bound

  reasons <- character()
  if (!isTRUE(metrics$false_reassurance <= 0.05) || !isTRUE(fr_upper <= 0.10)) {
    reasons <- c(reasons, "false_reassurance")
  }
  if (!isTRUE(metrics$robust_identification >= 0.70) || !isTRUE(ri_lower >= 0.60)) {
    reasons <- c(reasons, "robust_identification")
  }
  if (!isTRUE(metrics$balanced_accuracy >= 0.70) || !isTRUE(bal_lower >= 0.65)) {
    reasons <- c(reasons, "balanced_accuracy")
  }
  if (!isTRUE(metrics$minimum_class_accuracy >= 0.60)) {
    reasons <- c(reasons, "class_accuracy")
  }
  if (!isTRUE(.ancova_block_median_ordering(validation))) {
    reasons <- c(reasons, "median_ordering")
  }

  status <- if (length(reasons) == 0L) "validated_method_specific" else "uncalibrated"
  list(
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = ",") else NA_character_,
    cutoffs = cutoffs,
    metrics = metrics,
    conservative_bounds = list(
      false_reassurance_upper = fr_upper,
      robust_identification_lower = ri_lower,
      balanced_accuracy_lower = bal_lower,
      wilson = list(
        false_reassurance_upper = metrics$false_reassurance_upper,
        robust_identification_lower = metrics$robust_identification_lower
      ),
      cluster = list(
        false_reassurance_upper = fr_cluster$bound,
        robust_identification_lower = ri_cluster$bound,
        balanced_accuracy_lower = bal_lower
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
