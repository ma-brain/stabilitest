# Deterministic ANCOVA v2 two-band cutoff fitting (single integer L).
# Fragile if score <= L; else Not fragile. Borderline / diagnostic_only
# rows are excluded from fitting and acceptance metrics.

.ancova_v2_eligible_rows <- function(data) {
  if (!is.data.frame(data)) {
    stop("ANCOVA v2 cutoff data must be a data frame", call. = FALSE)
  }
  required <- c("truth_class", "overall_score")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("missing columns: %s", paste(missing, collapse = ", ")), call. = FALSE)
  }
  if ("analysis_conclusion" %in% names(data)) {
    conclusion <- tolower(gsub("[- ]", "_", as.character(data$analysis_conclusion)))
    data <- data[conclusion == "significant", , drop = FALSE]
  }
  if ("status" %in% names(data)) {
    data <- data[is.na(data$status) | data$status == "completed", , drop = FALSE]
  }
  if ("diagnostic_only" %in% names(data)) {
    diag <- data$diagnostic_only
    data <- data[is.na(diag) | !as.logical(diag), , drop = FALSE]
  }
  if ("design_layer" %in% names(data)) {
    data <- data[
      is.na(data$design_layer) | as.character(data$design_layer) != "stress",
      , drop = FALSE
    ]
  }
  truth <- as.character(data$truth_class)
  data <- data[truth %in% c("null", "clear"), , drop = FALSE]
  if (!nrow(data)) {
    stop("no eligible significant ANCOVA v2 null/clear replicates", call. = FALSE)
  }
  if (any(!is.finite(data$overall_score))) {
    stop("overall_score must contain finite values", call. = FALSE)
  }
  data
}

.ancova_v2_assign_band <- function(score, cutoff) {
  cutoff <- as.integer(cutoff)
  ifelse(score <= cutoff, "fragile", "not_fragile")
}

.ancova_v2_wilson <- function(x, n, side = c("lower", "upper"), conf_level = 0.95) {
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

ancova_v2_cutoff_metrics <- function(data, cutoff) {
  data <- .ancova_v2_eligible_rows(data)
  cutoff <- as.integer(cutoff)
  if (length(cutoff) != 1L || is.na(cutoff)) {
    stop("cutoff must be a single integer L", call. = FALSE)
  }
  bands <- .ancova_v2_assign_band(data$overall_score, cutoff)
  truth <- as.character(data$truth_class)
  expected <- c(null = "fragile", clear = "not_fragile")

  class_accuracy <- lapply(names(expected), function(level) {
    rows <- truth == level
    if (!any(rows)) return(NA_real_)
    mean(bands[rows] == expected[[level]])
  })
  names(class_accuracy) <- names(expected)

  null_rows <- truth == "null"
  clear_rows <- truth == "clear"
  fr_n <- sum(null_rows)
  ri_n <- sum(clear_rows)
  fr_count <- sum(null_rows & data$overall_score > cutoff)
  ri_count <- sum(clear_rows & data$overall_score > cutoff)
  fr_point <- if (fr_n == 0L) NA_real_ else fr_count / fr_n
  ri_point <- if (ri_n == 0L) NA_real_ else ri_count / ri_n

  list(
    cutoff = cutoff,
    n = nrow(data),
    false_reassurance = fr_point,
    false_reassurance_n = as.integer(fr_n),
    false_reassurance_count = as.integer(fr_count),
    false_reassurance_upper = .ancova_v2_wilson(fr_count, fr_n, "upper"),
    not_fragile_identification = ri_point,
    not_fragile_identification_n = as.integer(ri_n),
    not_fragile_identification_count = as.integer(ri_count),
    not_fragile_identification_lower = .ancova_v2_wilson(ri_count, ri_n, "lower"),
    class_accuracy = class_accuracy
  )
}

ancova_v2_training_feasible <- function(metrics) {
  reasons <- character()
  if (!isTRUE(is.finite(metrics$false_reassurance)) ||
      metrics$false_reassurance > 0.05) {
    reasons <- c(reasons, "false_reassurance_point")
  }
  if (!isTRUE(is.finite(metrics$false_reassurance_upper)) ||
      metrics$false_reassurance_upper > 0.10) {
    reasons <- c(reasons, "false_reassurance_upper")
  }
  if (!isTRUE(is.finite(metrics$not_fragile_identification)) ||
      metrics$not_fragile_identification < 0.70) {
    reasons <- c(reasons, "not_fragile_identification_point")
  }
  if (!isTRUE(is.finite(metrics$not_fragile_identification_lower)) ||
      metrics$not_fragile_identification_lower < 0.60) {
    reasons <- c(reasons, "not_fragile_identification_lower")
  }
  fr_safety_margin <- if (isTRUE(is.finite(metrics$false_reassurance_upper))) {
    0.10 - metrics$false_reassurance_upper
  } else {
    NA_real_
  }
  list(
    feasible = length(reasons) == 0L,
    reasons = reasons,
    fr_safety_margin = fr_safety_margin,
    constraint_safety_margin = if (length(reasons)) {
      NA_real_
    } else {
      min(
        0.05 - metrics$false_reassurance,
        fr_safety_margin,
        metrics$not_fragile_identification - 0.70,
        metrics$not_fragile_identification_lower - 0.60
      )
    }
  )
}

fit_lm_ancova_v2_cutoffs <- function(training) {
  training <- .ancova_v2_eligible_rows(training)
  grid_rows <- vector("list", 101L)
  for (L in 0:100) {
    metrics <- ancova_v2_cutoff_metrics(training, L)
    gate <- ancova_v2_training_feasible(metrics)
    grid_rows[[L + 1L]] <- data.frame(
      cutoff = as.integer(L),
      false_reassurance = metrics$false_reassurance,
      false_reassurance_upper = metrics$false_reassurance_upper,
      not_fragile_identification = metrics$not_fragile_identification,
      not_fragile_identification_lower = metrics$not_fragile_identification_lower,
      fr_safety_margin = gate$fr_safety_margin,
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

  # Tie-breaks: highest clear identification, then greatest FR safety margin,
  # then smallest L.
  order_idx <- order(
    -feasible$not_fragile_identification,
    -feasible$fr_safety_margin,
    feasible$cutoff
  )
  selected <- feasible[order_idx[[1L]], , drop = FALSE]
  cutoff <- as.integer(selected$cutoff)
  metrics <- ancova_v2_cutoff_metrics(training, cutoff)
  list(
    status = "candidate",
    reason = NA_character_,
    cutoff = cutoff,
    metrics = metrics,
    grid = grid,
    selected_row = selected
  )
}
