# Deterministic binary-proportion categorical-band cutoff fitting (Track A'').
#
# Two-band decision at a single integer cutoff L: Fragile iff score <= L,
# Not-fragile iff score > L.  Training gates (all required):
#   FR <= 0.05 with Wilson upper <= 0.10   (false reassurance: P(score > L | null, significant))
#   RI >= 0.70 with Wilson lower >= 0.60   (robust identification: P(score > L | clear, significant))
# Deterministic tie-breaks: highest RI, then FR safety margin, then smallest L.

.prop_eligible_rows <- function(data) {
  if (!is.data.frame(data)) {
    stop("proportion cutoff data must be a data frame", call. = FALSE)
  }
  required <- c("truth_class", "overall_score")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("missing columns: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if ("analysis_conclusion" %in% names(data)) {
    conclusion <- tolower(gsub("[- ]", "_", as.character(data$analysis_conclusion)))
    data <- data[conclusion == "significant", , drop = FALSE]
  }
  if ("status" %in% names(data)) {
    data <- data[is.na(data$status) | data$status == "completed", , drop = FALSE]
  }
  if (!nrow(data)) {
    stop("no eligible significant proportion replicates", call. = FALSE)
  }
  if (any(!is.finite(data$overall_score))) {
    stop("overall_score must contain finite values", call. = FALSE)
  }
  data
}

# Wilson interval bound (defers to the shared helper when available).
.prop_wilson <- function(x, n, side = c("lower", "upper"), conf_level = 0.95) {
  side <- match.arg(side)
  if (!is.finite(n) || n <= 0) return(NA_real_)
  if (exists(".threshold_wilson", mode = "function", inherits = TRUE)) {
    return(.threshold_wilson(x, n, side = side, conf_level = conf_level))
  }
  z <- stats::qnorm(conf_level)
  centre <- (x + z^2 / 2) / (n + z^2)
  radius <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
  if (identical(side, "upper")) min(1, centre + radius) else max(0, centre - radius)
}

# Metrics for a single cutoff L.  Fragile iff score <= L; Not-fragile iff > L.
# FR = P(Not-fragile | null, significant) = P(score > L | null, significant).
# RI = P(Not-fragile | clear, significant) = P(score > L | clear, significant).
prop_cutoff_metrics <- function(data, cutoff) {
  data <- .prop_eligible_rows(data)
  cutoff <- as.integer(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff) || cutoff < 0L || cutoff > 100L) {
    stop("cutoff must be one integer in [0, 100]", call. = FALSE)
  }

  truth <- as.character(data$truth_class)
  null_rows <- truth == "null"
  clear_rows <- truth == "clear"
  fr_n <- sum(null_rows)
  ri_n <- sum(clear_rows)
  # Not-fragile = score > cutoff.
  fr_count <- sum(null_rows & data$overall_score > cutoff)
  ri_count <- sum(clear_rows & data$overall_score > cutoff)
  fr_point <- if (fr_n == 0L) NA_real_ else fr_count / fr_n
  ri_point <- if (ri_n == 0L) NA_real_ else ri_count / ri_n

  # Per-truth-stratum diagnostics (median score + Not-fragile rate).
  strata <- c("null", "clear")
  medians <- vapply(strata, function(level) {
    scores <- data$overall_score[truth == level]
    if (!length(scores)) NA_real_ else stats::median(scores)
  }, numeric(1))
  not_fragile_rate <- vapply(strata, function(level) {
    rows <- truth == level
    if (!any(rows)) NA_real_ else mean(data$overall_score[rows] > cutoff)
  }, numeric(1))

  list(
    cutoff = cutoff,
    n = nrow(data),
    false_reassurance = fr_point,
    false_reassurance_n = as.integer(fr_n),
    false_reassurance_count = as.integer(fr_count),
    false_reassurance_upper = .prop_wilson(fr_count, fr_n, "upper"),
    robust_identification = ri_point,
    robust_identification_n = as.integer(ri_n),
    robust_identification_count = as.integer(ri_count),
    robust_identification_lower = .prop_wilson(ri_count, ri_n, "lower"),
    median_score = medians,
    not_fragile_rate = not_fragile_rate
  )
}

# Frozen training feasibility gate for one cutoff.
prop_training_feasible <- function(metrics) {
  reasons <- character()
  if (!isTRUE(is.finite(metrics$false_reassurance)) ||
      metrics$false_reassurance > 0.05) {
    reasons <- c(reasons, "false_reassurance_point")
  }
  if (!isTRUE(is.finite(metrics$false_reassurance_upper)) ||
      metrics$false_reassurance_upper > 0.10) {
    reasons <- c(reasons, "false_reassurance_upper")
  }
  if (!isTRUE(is.finite(metrics$robust_identification)) ||
      metrics$robust_identification < 0.70) {
    reasons <- c(reasons, "robust_identification_point")
  }
  if (!isTRUE(is.finite(metrics$robust_identification_lower)) ||
      metrics$robust_identification_lower < 0.60) {
    reasons <- c(reasons, "robust_identification_lower")
  }
  list(
    feasible = length(reasons) == 0L,
    reasons = reasons,
    constraint_safety_margin = if (length(reasons)) {
      NA_real_
    } else {
      min(
        0.05 - metrics$false_reassurance,
        0.10 - metrics$false_reassurance_upper,
        metrics$robust_identification - 0.70,
        metrics$robust_identification_lower - 0.60
      )
    }
  )
}

# Full-grid search over L in {0..100}; deterministic tie-breaks:
# highest RI, then FR safety margin, then smallest L.
fit_fisher_exact_cutoffs <- function(training) {
  training <- .prop_eligible_rows(training)
  grid_rows <- vector("list", 101L)
  for (L in 0:100) {
    metrics <- prop_cutoff_metrics(training, L)
    gate <- prop_training_feasible(metrics)
    grid_rows[[L + 1L]] <- data.frame(
      cutoff = L,
      false_reassurance = metrics$false_reassurance,
      false_reassurance_upper = metrics$false_reassurance_upper,
      robust_identification = metrics$robust_identification,
      robust_identification_lower = metrics$robust_identification_lower,
      fr_safety_margin = 0.05 - metrics$false_reassurance,
      constraint_safety_margin = gate$constraint_safety_margin,
      feasible = gate$feasible,
      stringsAsFactors = FALSE
    )
  }
  grid <- do.call(rbind, grid_rows)
  rownames(grid) <- NULL

  feasible <- grid[isTRUE(grid$feasible) | grid$feasible %in% TRUE, , drop = FALSE]
  if (!nrow(feasible)) {
    return(list(
      status = "uncalibrated",
      reason = "no_feasible_thresholds",
      cutoff = NA_integer_,
      metrics = NULL,
      grid = grid
    ))
  }

  # Tie-break: highest RI, then FR safety margin, then smallest L.
  order_idx <- order(
    -feasible$robust_identification,
    -feasible$fr_safety_margin,
    feasible$cutoff
  )
  selected <- feasible[order_idx[[1L]], , drop = FALSE]
  cutoff <- as.integer(selected$cutoff)
  metrics <- prop_cutoff_metrics(training, cutoff)
  list(
    status = "candidate",
    reason = NA_character_,
    cutoff = cutoff,
    metrics = metrics,
    grid = grid,
    selected_row = selected
  )
}

# Hash helper (defers to the shared manifest helper when available).
.prop_hash_object <- function(object) {
  if (exists("calibration_hash_object", mode = "function", inherits = TRUE)) {
    return(calibration_hash_object(object))
  }
  path <- tempfile("prop-hash-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  unname(as.character(tools::md5sum(path)))
}

# Freeze a Track A'' candidate (+ optional Track D' curve) with manifest hashes.
# The candidate hash is recorded before held-out is opened.
freeze_binary_proportion_candidate <- function(fit, track_d = NULL,
                                                scenario_manifest_hash,
                                                training_manifest_hash) {
  if (!is.list(fit) || is.null(fit$status)) {
    stop("fit must be a cutoff-fitting result", call. = FALSE)
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
    payload$candidate_hash <- .prop_hash_object(payload)
    return(payload)
  }

  payload <- list(
    status = "candidate",
    cutoff = as.integer(fit$cutoff),
    metrics = fit$metrics,
    selected_row = fit$selected_row,
    track_d = track_d,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    held_out_opened = FALSE,
    validation_refit = FALSE
  )
  payload$candidate_hash <- .prop_hash_object(payload)
  payload
}
