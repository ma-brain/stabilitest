# Deterministic ANCOVA categorical-band cutoff fitting.

.ancova_eligible_rows <- function(data) {
  if (!is.data.frame(data)) stop("ANCOVA cutoff data must be a data frame", call. = FALSE)
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
  if (!nrow(data)) stop("no eligible significant ANCOVA replicates", call. = FALSE)
  if (any(!is.finite(data$overall_score))) {
    stop("overall_score must contain finite values", call. = FALSE)
  }
  data
}

.ancova_assign_band <- function(score, cutoffs) {
  lower <- as.integer(cutoffs[[1L]])
  upper <- as.integer(cutoffs[[2L]])
  ifelse(score <= lower, "fragile",
         ifelse(score <= upper, "moderate", "robust"))
}

.ancova_wilson <- function(x, n, side = c("lower", "upper"), conf_level = 0.95) {
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

ancova_cutoff_metrics <- function(data, cutoffs) {
  data <- .ancova_eligible_rows(data)
  cutoffs <- as.integer(cutoffs)
  if (length(cutoffs) != 2L || anyNA(cutoffs) || cutoffs[[1L]] >= cutoffs[[2L]]) {
    stop("cutoffs must be two ordered integers", call. = FALSE)
  }
  bands <- .ancova_assign_band(data$overall_score, cutoffs)
  truth <- as.character(data$truth_class)
  expected <- c(null = "fragile", borderline = "moderate", clear = "robust")

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
  fr_count <- sum(null_rows & data$overall_score > cutoffs[[1L]])
  ri_count <- sum(clear_rows & data$overall_score > cutoffs[[2L]])
  fr_point <- if (fr_n == 0L) NA_real_ else fr_count / fr_n
  ri_point <- if (ri_n == 0L) NA_real_ else ri_count / ri_n

  medians <- vapply(names(expected), function(level) {
    scores <- data$overall_score[truth == level]
    if (!length(scores)) NA_real_ else stats::median(scores)
  }, numeric(1))
  names(medians) <- names(expected)
  median_ordering_ok <- isTRUE(
    is.finite(medians[["null"]]) && is.finite(medians[["borderline"]]) &&
      is.finite(medians[["clear"]]) &&
      medians[["null"]] < medians[["borderline"]] &&
      medians[["borderline"]] < medians[["clear"]]
  )

  balanced <- mean(unlist(class_accuracy))
  list(
    cutoffs = cutoffs,
    n = nrow(data),
    false_reassurance = fr_point,
    false_reassurance_n = as.integer(fr_n),
    false_reassurance_count = as.integer(fr_count),
    false_reassurance_upper = .ancova_wilson(fr_count, fr_n, "upper"),
    robust_identification = ri_point,
    robust_identification_n = as.integer(ri_n),
    robust_identification_count = as.integer(ri_count),
    robust_identification_lower = .ancova_wilson(ri_count, ri_n, "lower"),
    class_accuracy = class_accuracy,
    minimum_class_accuracy = min(unlist(class_accuracy), na.rm = TRUE),
    balanced_accuracy = balanced,
    medians = medians,
    median_ordering_ok = median_ordering_ok
  )
}

ancova_training_feasible <- function(metrics) {
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
  if (!isTRUE(is.finite(metrics$balanced_accuracy)) ||
      metrics$balanced_accuracy < 0.70) {
    reasons <- c(reasons, "balanced_accuracy")
  }
  if (!isTRUE(is.finite(metrics$minimum_class_accuracy)) ||
      metrics$minimum_class_accuracy < 0.60) {
    reasons <- c(reasons, "minimum_class_accuracy")
  }
  if (!isTRUE(metrics$median_ordering_ok)) {
    reasons <- c(reasons, "median_ordering")
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
        metrics$robust_identification_lower - 0.60,
        metrics$balanced_accuracy - 0.70,
        metrics$minimum_class_accuracy - 0.60
      )
    }
  )
}

fit_lm_ancova_cutoffs <- function(training) {
  training <- .ancova_eligible_rows(training)
  grid_rows <- list()
  index <- 0L
  for (lower in 0:99) {
    for (upper in (lower + 1L):100L) {
      metrics <- ancova_cutoff_metrics(training, c(lower, upper))
      gate <- ancova_training_feasible(metrics)
      index <- index + 1L
      grid_rows[[index]] <- data.frame(
        lower_cutoff = lower,
        upper_cutoff = upper,
        balanced_accuracy = metrics$balanced_accuracy,
        minimum_class_accuracy = metrics$minimum_class_accuracy,
        false_reassurance = metrics$false_reassurance,
        false_reassurance_upper = metrics$false_reassurance_upper,
        robust_identification = metrics$robust_identification,
        robust_identification_lower = metrics$robust_identification_lower,
        median_ordering_ok = metrics$median_ordering_ok,
        constraint_safety_margin = gate$constraint_safety_margin,
        feasible = gate$feasible,
        stringsAsFactors = FALSE
      )
    }
  }
  grid <- do.call(rbind, grid_rows)
  rownames(grid) <- NULL
  welch_comparator <- ancova_cutoff_metrics(training, c(55L, 70L))

  feasible <- grid[isTRUE(grid$feasible) | grid$feasible %in% TRUE, , drop = FALSE]
  if (!nrow(feasible)) {
    return(list(
      status = "uncalibrated",
      reason = "no_feasible_thresholds",
      cutoffs = c(NA_integer_, NA_integer_),
      metrics = NULL,
      grid = grid,
      welch_comparator = list(cutoffs = c(55L, 70L), metrics = welch_comparator)
    ))
  }

  order_idx <- order(
    -feasible$balanced_accuracy,
    -feasible$minimum_class_accuracy,
    -feasible$constraint_safety_margin,
    feasible$lower_cutoff,
    feasible$upper_cutoff
  )
  selected <- feasible[order_idx[[1L]], , drop = FALSE]
  cutoffs <- c(as.integer(selected$lower_cutoff), as.integer(selected$upper_cutoff))
  metrics <- ancova_cutoff_metrics(training, cutoffs)
  list(
    status = "candidate",
    reason = NA_character_,
    cutoffs = cutoffs,
    metrics = metrics,
    grid = grid,
    selected_row = selected,
    welch_comparator = list(cutoffs = c(55L, 70L), metrics = welch_comparator)
  )
}
