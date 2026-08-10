# Frozen categorical threshold fitting for prospective Welch training scores.

WELCH_GATE_CONSTANTS <- list(
  false_reassurance_point_max = 0.05,
  false_reassurance_upper_max = 0.10,
  clear_identification_point_min = 0.70,
  clear_identification_lower_min = 0.60,
  balanced_ordinal_accuracy_min = 0.70,
  minimum_band_occupancy = 0.05,
  material_reversal_tolerance = 5,
  cluster_bootstrap_B = 1000L,
  cluster_bootstrap_seed = 202640001L
)

.welch_eligible_scores <- function(data) {
  if (!is.data.frame(data)) stop("Welch score input must be a data frame", call. = FALSE)
  required <- c("scenario_id", "truth", "overall_robustness")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("Welch score input is missing: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if ("status" %in% names(data)) {
    data <- data[data$status == "completed", , drop = FALSE]
  }
  if ("original_significant" %in% names(data)) {
    data <- data[!is.na(data$original_significant) & data$original_significant,
                 , drop = FALSE]
  }
  if (!nrow(data)) stop("no completed significant Welch scores", call. = FALSE)
  if (any(!is.finite(data$overall_robustness))) {
    stop("overall_robustness must be finite", call. = FALSE)
  }
  if (!all(c("null", "clear") %in% data$truth)) {
    stop("Welch score input requires null and clear strata",
         call. = FALSE)
  }
  data
}

welch_wilson_bound <- function(x, n, side = c("lower", "upper"),
                               conf_level = 0.95) {
  side <- match.arg(side)
  x <- as.numeric(x)
  n <- as.numeric(n)
  if (!is.finite(n) || n <= 0 || !is.finite(x) || x < 0 || x > n) {
    return(NA_real_)
  }
  z <- stats::qnorm(conf_level)
  centre <- (x + z^2 / 2) / (n + z^2)
  radius <- z * sqrt(x * (n - x) / n + z^2 / 4) / (n + z^2)
  if (identical(side, "upper")) {
    min(1, centre + radius)
  } else {
    max(0, centre - radius)
  }
}

welch_cluster_bound <- function(data, statistic,
                                side = c("lower", "upper"),
                                B = 1000L, seed = 202640001L,
                                conf_level = 0.95) {
  side <- match.arg(side)
  if (!is.data.frame(data) || !"scenario_id" %in% names(data)) {
    stop("cluster data must contain scenario_id", call. = FALSE)
  }
  if (!is.function(statistic)) stop("statistic must be a function", call. = FALSE)
  B <- as.integer(B)
  if (is.na(B) || B < 1L) stop("B must be a positive integer", call. = FALSE)
  clusters <- unique(as.character(data$scenario_id))
  if (!length(clusters)) stop("at least one scenario cluster is required", call. = FALSE)
  estimate <- statistic(data)
  if (!is.numeric(estimate) || length(estimate) != 1L || !is.finite(estimate)) {
    stop("cluster statistic must return one finite value", call. = FALSE)
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))

  draws <- vapply(seq_len(B), function(index) {
    selected <- sample(clusters, length(clusters), replace = TRUE)
    rows <- do.call(rbind, lapply(selected, function(cluster) {
      data[data$scenario_id == cluster, , drop = FALSE]
    }))
    value <- statistic(rows)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      stop("cluster statistic returned a non-finite value", call. = FALSE)
    }
    value
  }, numeric(1))
  probability <- if (identical(side, "upper")) conf_level else 1 - conf_level
  list(
    estimate = as.numeric(estimate),
    bound = as.numeric(stats::quantile(draws, probability, names = FALSE, type = 6)),
    side = side,
    B = B,
    seed = as.integer(seed),
    n_clusters = as.integer(length(clusters)),
    draws = draws
  )
}

welch_conservative_bound <- function(wilson, cluster,
                                     side = c("lower", "upper")) {
  side <- match.arg(side)
  values <- c(wilson, cluster)
  values <- values[is.finite(values)]
  if (!length(values)) return(NA_real_)
  if (identical(side, "upper")) max(values) else min(values)
}

welch_assign_band <- function(score, mapping_type = c("three_band", "two_band"),
                              lower, upper = NA_integer_) {
  mapping_type <- match.arg(mapping_type)
  lower <- as.integer(lower)
  if (is.na(lower) || lower < 0L || lower > 100L) {
    stop("lower cutoff must be an integer from 0 through 100", call. = FALSE)
  }
  if (identical(mapping_type, "two_band")) {
    return(ifelse(score <= lower, "fragile", "not_fragile"))
  }
  upper <- as.integer(upper)
  if (is.na(upper) || upper <= lower || upper > 100L) {
    stop("three-band cutoffs must satisfy 0 <= lower < upper <= 100",
         call. = FALSE)
  }
  ifelse(score <= lower, "fragile", ifelse(score <= upper, "moderate", "robust"))
}

.welch_ordering_diagnostics <- function(data) {
  medians <- vapply(c("null", "borderline", "clear"), function(truth) {
    values <- data$overall_robustness[data$truth == truth]
    if (!length(values)) NA_real_ else stats::median(values)
  }, numeric(1))
  pooled_ok <- all(is.finite(medians)) &&
    medians[["null"]] <= medians[["borderline"]] &&
    medians[["borderline"]] <= medians[["clear"]]

  if (!"archetype_id" %in% names(data)) {
    return(list(medians = medians, pooled_ok = pooled_ok,
                material_reversal = !pooled_ok, reversals = data.frame()))
  }
  blocks <- unique(as.character(data$archetype_id))
  rows <- lapply(blocks, function(block) {
    subset <- data[data$archetype_id == block, , drop = FALSE]
    block_medians <- vapply(c("null", "borderline", "clear"), function(truth) {
      values <- subset$overall_robustness[subset$truth == truth]
      if (!length(values)) NA_real_ else stats::median(values)
    }, numeric(1))
    material <- any(!is.finite(block_medians)) ||
      block_medians[["null"]] > block_medians[["borderline"]] +
        WELCH_GATE_CONSTANTS$material_reversal_tolerance ||
      block_medians[["borderline"]] > block_medians[["clear"]] +
        WELCH_GATE_CONSTANTS$material_reversal_tolerance
    data.frame(
      archetype_id = block,
      median_null = block_medians[["null"]],
      median_borderline = block_medians[["borderline"]],
      median_clear = block_medians[["clear"]],
      material_reversal = material,
      stringsAsFactors = FALSE
    )
  })
  reversals <- do.call(rbind, rows)
  list(
    medians = medians,
    pooled_ok = pooled_ok,
    material_reversal = any(reversals$material_reversal),
    reversals = reversals
  )
}

.welch_candidate_metrics_base <- function(data, mapping_type, lower, upper) {
  bands <- welch_assign_band(data$overall_robustness, mapping_type, lower, upper)
  null_rows <- data$truth == "null"
  clear_rows <- data$truth == "clear"
  reassuring <- bands != "fragile"
  clear_label <- if (identical(mapping_type, "three_band")) "robust" else "not_fragile"

  fr_count <- sum(null_rows & reassuring)
  fr_n <- sum(null_rows)
  clear_count <- sum(clear_rows & bands == clear_label)
  clear_n <- sum(clear_rows)
  expected <- if (identical(mapping_type, "three_band")) {
    c(null = "fragile", borderline = "moderate", clear = "robust")
  } else {
    c(null = "fragile", clear = "not_fragile")
  }
  class_accuracy <- vapply(names(expected), function(truth) {
    rows <- data$truth == truth
    mean(bands[rows] == expected[[truth]])
  }, numeric(1))
  occupancy <- prop.table(table(factor(
    bands,
    levels = if (identical(mapping_type, "three_band")) {
      c("fragile", "moderate", "robust")
    } else {
      c("fragile", "not_fragile")
    }
  )))
  ordering <- .welch_ordering_diagnostics(data)
  list(
    mapping_type = mapping_type,
    cutoffs = c(lower = as.integer(lower), upper = as.integer(upper)),
    false_reassurance = fr_count / fr_n,
    false_reassurance_count = as.integer(fr_count),
    false_reassurance_n = as.integer(fr_n),
    false_reassurance_wilson_upper = welch_wilson_bound(fr_count, fr_n, "upper"),
    clear_identification = clear_count / clear_n,
    clear_identification_count = as.integer(clear_count),
    clear_identification_n = as.integer(clear_n),
    clear_identification_wilson_lower = welch_wilson_bound(
      clear_count, clear_n, "lower"
    ),
    class_accuracy = class_accuracy,
    balanced_ordinal_accuracy = mean(class_accuracy),
    band_occupancy = occupancy,
    minimum_band_occupancy = min(occupancy),
    pooled_medians = ordering$medians,
    pooled_ordering_ok = ordering$pooled_ok,
    material_reversal = ordering$material_reversal,
    archetype_ordering = ordering$reversals,
    bands = bands
  )
}

welch_candidate_metrics <- function(data,
                                    mapping_type = c("three_band", "two_band"),
                                    lower, upper = NA_integer_,
                                    cluster_B = 1000L,
                                    cluster_seed = 202640001L) {
  mapping_type <- match.arg(mapping_type)
  data <- .welch_eligible_scores(data)
  if (identical(mapping_type, "three_band") && !"borderline" %in% data$truth) {
    stop("three-band metrics require the borderline truth stratum",
         call. = FALSE)
  }
  metrics <- .welch_candidate_metrics_base(data, mapping_type, lower, upper)
  fr_cluster <- NA_real_
  clear_cluster <- NA_real_
  if (!is.null(cluster_B) && as.integer(cluster_B) > 0L) {
    null_data <- data[data$truth == "null", , drop = FALSE]
    clear_data <- data[data$truth == "clear", , drop = FALSE]
    fr_cluster <- welch_cluster_bound(
      null_data,
      function(rows) mean(rows$overall_robustness > lower),
      side = "upper", B = cluster_B, seed = cluster_seed
    )$bound
    clear_threshold <- if (identical(mapping_type, "three_band")) upper else lower
    clear_cluster <- welch_cluster_bound(
      clear_data,
      function(rows) mean(rows$overall_robustness > clear_threshold),
      side = "lower", B = cluster_B, seed = cluster_seed + 1L
    )$bound
  }
  metrics$false_reassurance_cluster_upper <- fr_cluster
  metrics$clear_identification_cluster_lower <- clear_cluster
  metrics$false_reassurance_conservative_upper <- welch_conservative_bound(
    metrics$false_reassurance_wilson_upper, fr_cluster, "upper"
  )
  metrics$clear_identification_conservative_lower <- welch_conservative_bound(
    metrics$clear_identification_wilson_lower, clear_cluster, "lower"
  )
  metrics
}

welch_candidate_gate <- function(metrics,
                                 mapping_type = c("three_band", "two_band")) {
  mapping_type <- match.arg(mapping_type)
  g <- WELCH_GATE_CONSTANTS
  reasons <- character()
  if (!isTRUE(metrics$false_reassurance <= g$false_reassurance_point_max)) {
    reasons <- c(reasons, "false_reassurance_point")
  }
  if (!isTRUE(metrics$false_reassurance_conservative_upper <=
              g$false_reassurance_upper_max)) {
    reasons <- c(reasons, "false_reassurance_upper")
  }
  if (!isTRUE(metrics$clear_identification >= g$clear_identification_point_min)) {
    reasons <- c(reasons, "clear_identification_point")
  }
  if (!isTRUE(metrics$clear_identification_conservative_lower >=
              g$clear_identification_lower_min)) {
    reasons <- c(reasons, "clear_identification_lower")
  }
  if (identical(mapping_type, "three_band") &&
      !isTRUE(metrics$balanced_ordinal_accuracy >=
              g$balanced_ordinal_accuracy_min)) {
    reasons <- c(reasons, "balanced_ordinal_accuracy")
  }
  if (!isTRUE(metrics$minimum_band_occupancy >= g$minimum_band_occupancy)) {
    reasons <- c(reasons, "band_occupancy")
  }
  if (!isTRUE(metrics$pooled_ordering_ok)) reasons <- c(reasons, "pooled_ordering")
  if (isTRUE(metrics$material_reversal)) reasons <- c(reasons, "material_reversal")

  distances <- c(
    (g$false_reassurance_point_max - metrics$false_reassurance) /
      g$false_reassurance_point_max,
    (g$false_reassurance_upper_max -
       metrics$false_reassurance_conservative_upper) /
      g$false_reassurance_upper_max,
    (metrics$clear_identification - g$clear_identification_point_min) /
      (1 - g$clear_identification_point_min),
    (metrics$clear_identification_conservative_lower -
       g$clear_identification_lower_min) /
      (1 - g$clear_identification_lower_min),
    (metrics$minimum_band_occupancy - g$minimum_band_occupancy) /
      (1 - g$minimum_band_occupancy)
  )
  if (identical(mapping_type, "three_band")) {
    distances <- c(
      distances,
      (metrics$balanced_ordinal_accuracy - g$balanced_ordinal_accuracy_min) /
        (1 - g$balanced_ordinal_accuracy_min)
    )
  }
  list(
    feasible = length(reasons) == 0L,
    reasons = unique(reasons),
    minimum_standardized_distance = min(distances)
  )
}

.welch_grid_row <- function(metrics, gate) {
  data.frame(
    mapping_type = metrics$mapping_type,
    lower_cutoff = unname(metrics$cutoffs[["lower"]]),
    upper_cutoff = unname(metrics$cutoffs[["upper"]]),
    balanced_ordinal_accuracy = metrics$balanced_ordinal_accuracy,
    false_reassurance = metrics$false_reassurance,
    false_reassurance_upper = metrics$false_reassurance_conservative_upper,
    clear_identification = metrics$clear_identification,
    clear_identification_lower = metrics$clear_identification_conservative_lower,
    minimum_band_occupancy = metrics$minimum_band_occupancy,
    pooled_ordering_ok = metrics$pooled_ordering_ok,
    material_reversal = metrics$material_reversal,
    minimum_standardized_distance = gate$minimum_standardized_distance,
    feasible = gate$feasible,
    reasons = paste(gate$reasons, collapse = ","),
    stringsAsFactors = FALSE
  )
}

.fit_welch_mapping <- function(data, mapping_type, cluster_B, cluster_seed) {
  specs <- if (identical(mapping_type, "three_band")) {
    do.call(rbind, lapply(0:99, function(lower) {
      data.frame(lower = lower, upper = (lower + 1L):100L)
    }))
  } else {
    data.frame(lower = 0:100, upper = NA_integer_)
  }
  base_rows <- vector("list", nrow(specs))
  base_metrics <- vector("list", nrow(specs))
  for (i in seq_len(nrow(specs))) {
    metrics <- welch_candidate_metrics(
      data, mapping_type, specs$lower[[i]], specs$upper[[i]], cluster_B = 0L
    )
    gate <- welch_candidate_gate(metrics, mapping_type)
    base_metrics[[i]] <- metrics
    base_rows[[i]] <- .welch_grid_row(metrics, gate)
  }
  grid <- do.call(rbind, base_rows)
  possible <- which(grid$feasible)
  passing <- list()
  pass_rows <- list()
  if (length(possible)) {
    for (j in seq_along(possible)) {
      i <- possible[[j]]
      metrics <- welch_candidate_metrics(
        data, mapping_type, specs$lower[[i]], specs$upper[[i]],
        cluster_B = cluster_B, cluster_seed = cluster_seed
      )
      gate <- welch_candidate_gate(metrics, mapping_type)
      grid[i, ] <- .welch_grid_row(metrics, gate)
      if (gate$feasible) {
        passing[[length(passing) + 1L]] <- metrics
        pass_rows[[length(pass_rows) + 1L]] <- grid[i, , drop = FALSE]
      }
    }
  }
  if (!length(passing)) return(list(selected = NULL, grid = grid))
  candidates <- do.call(rbind, pass_rows)
  order_index <- order(
    -candidates$balanced_ordinal_accuracy,
    -candidates$minimum_standardized_distance,
    -candidates$lower_cutoff,
    -ifelse(is.na(candidates$upper_cutoff), -1, candidates$upper_cutoff),
    candidates$lower_cutoff,
    candidates$upper_cutoff,
    na.last = TRUE
  )
  list(
    selected = list(
      row = candidates[order_index[[1L]], , drop = FALSE],
      metrics = passing[[order_index[[1L]]]]
    ),
    grid = grid
  )
}

fit_welch_candidate <- function(training, cluster_B = 1000L,
                                cluster_seed = 202640001L) {
  training <- .welch_eligible_scores(training)
  historical <- welch_candidate_metrics(
    training, "three_band", 55L, 70L,
    cluster_B = cluster_B, cluster_seed = cluster_seed
  )
  three <- .fit_welch_mapping(training, "three_band", cluster_B, cluster_seed)
  if (!is.null(three$selected)) {
    selected <- three$selected
    return(list(
      status = "candidate",
      reason = NA_character_,
      mapping_type = "three_band",
      cutoffs = c(
        as.integer(selected$row$lower_cutoff),
        as.integer(selected$row$upper_cutoff)
      ),
      metrics = selected$metrics,
      selected = selected$row,
      three_band_grid = three$grid,
      two_band_grid = NULL,
      historical_55_70 = historical
    ))
  }

  two <- .fit_welch_mapping(training, "two_band", cluster_B, cluster_seed + 10L)
  if (!is.null(two$selected)) {
    selected <- two$selected
    return(list(
      status = "candidate",
      reason = NA_character_,
      mapping_type = "two_band",
      cutoffs = c(as.integer(selected$row$lower_cutoff), NA_integer_),
      metrics = selected$metrics,
      selected = selected$row,
      three_band_grid = three$grid,
      two_band_grid = two$grid,
      historical_55_70 = historical
    ))
  }
  list(
    status = "no_feasible_thresholds",
    reason = "no_feasible_thresholds",
    mapping_type = "none",
    cutoffs = c(NA_integer_, NA_integer_),
    metrics = NULL,
    selected = NULL,
    three_band_grid = three$grid,
    two_band_grid = two$grid,
    historical_55_70 = historical
  )
}
