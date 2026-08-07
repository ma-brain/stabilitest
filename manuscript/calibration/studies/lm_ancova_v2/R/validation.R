# Frozen-candidate held-out validation for ANCOVA v2 (single cutoff L).
# Validates once without refitting. Borderline / diagnostic_only rows never
# enter acceptance.

.ancova_v2_hash_object <- function(object) {
  if (exists("calibration_hash_object", mode = "function", inherits = TRUE)) {
    return(calibration_hash_object(object))
  }
  path <- tempfile("ancova-v2-hash-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  unname(as.character(tools::md5sum(path)))
}

ancova_v2_cluster_bound <- function(data, statistic, side = c("lower", "upper"),
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

.ancova_v2_quota_ok <- function(data, min_n = 100L) {
  data <- .ancova_v2_eligible_rows(data)
  counts <- table(as.character(data$scenario_id))
  length(counts) > 0L && all(counts >= min_n)
}

freeze_lm_ancova_v2_candidate <- function(fit, scenario_manifest_hash,
                                          training_manifest_hash) {
  if (!is.list(fit) || is.null(fit$status)) {
    stop("fit must be a cutoff-fitting result", call. = FALSE)
  }
  if (!exists("%||%", mode = "function", inherits = TRUE)) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
  }
  if (!identical(fit$status, "candidate")) {
    payload <- list(
      status = "uncalibrated",
      reason = fit$reason %||% "no_feasible_thresholds",
      cutoff = fit$cutoff %||% NA_integer_,
      scenario_manifest_hash = scenario_manifest_hash,
      training_manifest_hash = training_manifest_hash,
      held_out_opened = FALSE,
      validation_refit = FALSE
    )
    payload$candidate_hash <- .ancova_v2_hash_object(payload)
    return(payload)
  }

  payload <- list(
    status = "candidate",
    cutoff = as.integer(fit$cutoff),
    metrics = fit$metrics,
    selected_row = fit$selected_row,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    held_out_opened = FALSE,
    validation_refit = FALSE
  )
  payload$candidate_hash <- .ancova_v2_hash_object(payload)
  payload
}

validate_lm_ancova_v2_candidate <- function(frozen, validation,
                                            scenario_manifest_hash,
                                            training_manifest_hash,
                                            validation_manifest_hash,
                                            cluster_B = 1000L,
                                            cluster_seed = 20260806L) {
  if (!exists("%||%", mode = "function", inherits = TRUE)) {
    `%||%` <- function(x, y) if (is.null(x)) y else x
  }
  if (!is.list(frozen) || is.null(frozen$candidate_hash)) {
    stop("validate_lm_ancova_v2_candidate requires a frozen candidate object",
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
  metrics <- ancova_v2_cutoff_metrics(validation, cutoff)
  if (!.ancova_v2_quota_ok(validation, min_n = 100L)) {
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

  eligible_validation <- .ancova_v2_eligible_rows(validation)
  fr_cluster <- ancova_v2_cluster_bound(
    eligible_validation[as.character(eligible_validation$truth_class) == "null",
                        , drop = FALSE],
    function(rows) {
      value <- ancova_v2_cutoff_metrics(rows, cutoff)$false_reassurance
      if (!is.finite(value)) 1 else as.numeric(value)
    },
    side = "upper", B = cluster_B, seed = cluster_seed
  )
  ri_cluster <- ancova_v2_cluster_bound(
    eligible_validation[as.character(eligible_validation$truth_class) == "clear",
                        , drop = FALSE],
    function(rows) {
      value <- ancova_v2_cutoff_metrics(rows, cutoff)$not_fragile_identification
      if (!is.finite(value)) 0 else as.numeric(value)
    },
    side = "lower", B = cluster_B, seed = cluster_seed + 1L
  )

  fr_upper <- max(metrics$false_reassurance_upper, fr_cluster$bound, na.rm = TRUE)
  ri_lower <- min(
    metrics$not_fragile_identification_lower, ri_cluster$bound, na.rm = TRUE
  )

  reasons <- character()
  if (!isTRUE(metrics$false_reassurance <= 0.05) || !isTRUE(fr_upper <= 0.10)) {
    reasons <- c(reasons, "false_reassurance")
  }
  if (!isTRUE(metrics$not_fragile_identification >= 0.70) ||
      !isTRUE(ri_lower >= 0.60)) {
    reasons <- c(reasons, "not_fragile_identification")
  }

  status <- if (length(reasons) == 0L) "validated_method_specific" else "uncalibrated"
  list(
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = ",") else NA_character_,
    cutoff = cutoff,
    metrics = metrics,
    conservative_bounds = list(
      false_reassurance_upper = fr_upper,
      not_fragile_identification_lower = ri_lower,
      wilson = list(
        false_reassurance_upper = metrics$false_reassurance_upper,
        not_fragile_identification_lower = metrics$not_fragile_identification_lower
      ),
      cluster = list(
        false_reassurance_upper = fr_cluster$bound,
        not_fragile_identification_lower = ri_cluster$bound
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
