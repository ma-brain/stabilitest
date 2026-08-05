# Operating-characteristic summaries for calibration artifacts.

.summary_required <- function(x, columns, name = "replicates") {
  if (!is.data.frame(x)) .summary_abort(sprintf("%s must be a data frame", name))
  missing <- setdiff(columns, names(x))
  if (length(missing) > 0L) {
    .summary_abort(sprintf("%s is missing columns: %s", name, paste(missing, collapse = ", ")))
  }
}

.summary_group_rows <- function(x, columns) {
  if (length(columns) == 0L) return(structure(list(seq_len(nrow(x))), names = "overall"))
  key <- do.call(paste, c(x[columns], sep = "\u001f"))
  split(seq_len(nrow(x)), key, drop = TRUE)
}

.summary_group_table <- function(x, columns, fun) {
  groups <- .summary_group_rows(x, columns)
  if (length(groups) == 0L) return(data.frame())
  rows <- lapply(names(groups), function(key) {
    indices <- groups[[key]]
    values <- x[indices, , drop = FALSE]
    fields <- if (length(columns) > 0L) as.list(values[1L, columns, drop = FALSE]) else list()
    fields <- lapply(fields, function(value) value[[1L]])
    c(fields, fun(values))
  })
  out <- do.call(rbind, lapply(rows, function(row) as.data.frame(row, stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out
}

.summary_score_data <- function(replicates, cutoffs) {
  .summary_required(replicates, c("overall_score", "truth_class"))
  x <- .summary_as_completed(replicates)
  if (nrow(x) == 0L) .summary_abort("replicates contains no completed rows")
  score <- as.numeric(x$overall_score)
  if (anyNA(score) || any(!is.finite(score)) || any(score < 0 | score > 100)) {
    .summary_abort("overall_score must contain finite values in [0, 100]")
  }
  x$.score_band <- .score_band(score, cutoffs)
  x$.target_supported <- .target_supported(x)
  x
}

.summary_observed_conclusion <- function(replicates) {
  columns <- intersect(c("screening_conclusion", "analysis_conclusion", "conclusion"), names(replicates))
  if (!length(columns)) return(rep(NA_character_, nrow(replicates)))
  observed <- rep(NA_character_, nrow(replicates))
  for (column in columns) {
    value <- vapply(replicates[[column]], .summary_conclusion, character(1))
    fill <- is.na(observed) & !is.na(value) & nzchar(value)
    observed[fill] <- value[fill]
  }
  observed
}

band_applicable_conclusion <- function(data) {
  observed <- .summary_observed_conclusion(data)
  if (all(is.na(observed))) return(rep(TRUE, nrow(data)))
  observed %in% c("significant", "equivalent", "noninferior")
}

.summary_is_tost <- function(data) {
  if ("calibration_unit" %in% names(data) &&
      any(grepl("^tost_", as.character(data$calibration_unit)))) return(TRUE)
  if (!"target_conclusion" %in% names(data)) return(FALSE)
  target <- tolower(gsub("[- ]", "_", as.character(data$target_conclusion)))
  any(target %in% c("equivalent", "noninferior", "not_equivalent", "inferior"))
}

.summary_applicable_metrics <- function(rows) {
  applicable <- band_applicable_conclusion(rows)
  if (length(applicable)) rows <- rows[applicable, , drop = FALSE]
  if (!nrow(rows)) {
    return(c(false_reassurance = 0L, false_reassurance_n = 0L,
             false_reassurance_point = NA_real_, false_reassurance_lower = NA_real_,
             false_reassurance_upper = NA_real_, false_reassurance_mc_se = NA_real_,
             robust_identification = 0L, robust_identification_n = 0L,
             robust_identification_point = NA_real_, robust_identification_lower = NA_real_,
             robust_identification_upper = NA_real_, robust_identification_mc_se = NA_real_))
  }
  target <- if ("target_conclusion" %in% names(rows)) {
    tolower(gsub("[- ]", "_", as.character(rows$target_conclusion)))
  } else rep(NA_character_, nrow(rows))
  truth <- as.character(rows$truth_class)
  tost <- .summary_is_tost(rows)
  if (tost) {
    false_universe <- target %in% c("not_equivalent", "inferior") & truth == "null"
    if (!any(false_universe)) false_universe <- target %in% c("not_equivalent", "inferior")
    id_universe <- target %in% c("equivalent", "noninferior") & truth == "clear"
    if (!any(id_universe)) id_universe <- target %in% c("equivalent", "noninferior")
  } else {
    false_universe <- truth == "null"
    id_universe <- truth == "clear" & (is.na(target) | target == "significant")
  }
  false_rows <- !is.na(rows$.score_band) & rows$.score_band %in% c("moderate", "robust") & false_universe
  id_rows <- !is.na(rows$.score_band) & rows$.score_band == "robust" & id_universe
  false <- .summary_rate(sum(false_rows), sum(false_universe, na.rm = TRUE))
  identified <- .summary_rate(sum(id_rows), sum(id_universe, na.rm = TRUE))
  c(false_reassurance = sum(false_rows), false_reassurance_n = sum(false_universe, na.rm = TRUE),
    false_reassurance_point = false$point, false_reassurance_lower = false$lower,
    false_reassurance_upper = false$upper, false_reassurance_mc_se = false$mc_se,
    robust_identification = sum(id_rows), robust_identification_n = sum(id_universe, na.rm = TRUE),
    robust_identification_point = identified$point, robust_identification_lower = identified$lower,
    robust_identification_upper = identified$upper, robust_identification_mc_se = identified$mc_se)
}

.summary_operating_row <- function(x) {
  n <- nrow(x)
  if (n == 0L) {
    return(list(n = 0L, attempted = 0L, fragile = 0L, moderate = 0L, robust = 0L))
  }
  bands <- table(factor(x$.score_band, levels = c("fragile", "moderate", "robust")))
  list(n = as.integer(n), attempted = as.integer(n), fragile = as.integer(bands[["fragile"]]),
       moderate = as.integer(bands[["moderate"]]), robust = as.integer(bands[["robust"]]))
}

.summary_ordinal <- function(x) {
  truth_levels <- c("null", "borderline", "clear")
  band_levels <- c("fragile", "moderate", "robust")
  keep <- x$truth_class %in% truth_levels & x$.score_band %in% band_levels
  if (!any(keep)) return(list(accuracy = NA_real_, weighted_accuracy = NA_real_, complete = FALSE,
                             missing_truth = truth_levels))
  counts <- table(factor(x$truth_class[keep], levels = truth_levels))
  complete <- all(counts > 0L)
  by_class <- vapply(truth_levels, function(truth) {
    rows <- which(keep & x$truth_class == truth)
    if (length(rows) == 0L) return(NA_real_)
    mean(x$.score_band[rows] == band_levels[[match(truth, truth_levels)]])
  }, numeric(1))
  ordinal_distance <- abs(match(x$.score_band[keep], band_levels) -
                            match(x$truth_class[keep], truth_levels))
  weighted <- mean(1 - ordinal_distance / (length(truth_levels) - 1L))
  list(accuracy = if (complete) mean(by_class) else NA_real_,
       weighted_accuracy = if (complete) weighted else NA_real_,
       complete = complete, missing_truth = truth_levels[counts == 0L])
}

.summary_operating_metrics <- function(rows) {
  .summary_applicable_metrics(rows)
}

.summary_band_metrics <- function(rows) {
  applicable <- band_applicable_conclusion(rows)
  rows <- rows[applicable, , drop = FALSE]
  calibration <- .summary_rate(sum(rows$.target_supported, na.rm = TRUE), nrow(rows))
  c(calibration_rate = calibration$point,
    calibration_rate_point = calibration$point, calibration_rate_lower = calibration$lower,
    calibration_rate_upper = calibration$upper, calibration_rate_mc_se = calibration$mc_se,
    conclusion_rate = calibration$point, conclusion_rate_lower = calibration$lower,
    conclusion_rate_upper = calibration$upper, conclusion_rate_mc_se = calibration$mc_se)
}

#' Summarise score operating characteristics under the frozen score bands.
#' @export
score_operating_characteristics <- function(replicates, cutoffs = c(55, 70)) {
  cutoffs <- .summary_cutoffs(cutoffs)
  x <- .summary_score_data(replicates, cutoffs)
  applicable <- band_applicable_conclusion(x)
  applicable_rows <- x[applicable, , drop = FALSE]
  applicable_metrics <- .summary_applicable_metrics(x)
  false <- applicable_metrics[["false_reassurance"]]
  false_universe <- applicable_metrics[["false_reassurance_n"]]
  identified <- applicable_metrics[["robust_identification"]]
  identification_universe <- applicable_metrics[["robust_identification_n"]]
  false_ci <- .summary_rate(sum(false), sum(false_universe, na.rm = TRUE))
  id_ci <- .summary_rate(sum(identified), sum(identification_universe, na.rm = TRUE))
  ordinal <- .summary_ordinal(applicable_rows)
  truth_levels <- c("null", "borderline", "clear")
  by_truth <- do.call(rbind, lapply(truth_levels, function(truth) {
    rows <- x[x$truth_class == truth, , drop = FALSE]
    as.data.frame(c(truth_class = truth, present = nrow(rows) > 0L,
                    .summary_operating_row(rows), .summary_operating_metrics(rows)),
                  stringsAsFactors = FALSE)
  }))
  rownames(by_truth) <- NULL
  identity_column <- if ("calibration_unit" %in% names(x)) "calibration_unit" else "analysis_family"
  by_unit <- if (identity_column %in% names(x)) {
    .summary_group_table(x, identity_column, function(rows) {
      .summary_operating_metrics(rows)
    })
  } else data.frame()
  band_levels <- c("fragile", "moderate", "robust")
  by_band <- do.call(rbind, lapply(band_levels, function(band) {
    rows <- x[x$.score_band == band, , drop = FALSE]
    as.data.frame(c(score_band = band, .summary_operating_row(rows),
                    .summary_operating_metrics(rows), .summary_band_metrics(rows)),
                  stringsAsFactors = FALSE)
  }))
  rownames(by_band) <- NULL
  list(
    cutoffs = cutoffs, n = nrow(x), completed = nrow(x),
    false_reassurance = false_ci, robust_identification = id_ci,
    mc_se = if (any(is.finite(c(false_ci$mc_se, id_ci$mc_se))))
      max(c(false_ci$mc_se, id_ci$mc_se), na.rm = TRUE) else NA_real_,
    balanced_ordinal_accuracy = ordinal$accuracy,
    balanced_ordinal_complete = ordinal$complete,
    balanced_ordinal_missing_truth = ordinal$missing_truth,
    ordinal_accuracy = ordinal$accuracy,
    weighted_ordinal_accuracy = ordinal$weighted_accuracy,
    by_truth = by_truth, by_calibration_unit = by_unit, by_family = by_unit, by_band = by_band
  )
}

#' Check median score ordering by truth class and design layer.
#' @export
check_median_ordering <- function(replicates) {
  .summary_required(replicates, c("overall_score", "truth_class"))
  x <- .summary_as_completed(replicates)
  applicable <- band_applicable_conclusion(x)
  x <- x[applicable, , drop = FALSE]
  if (nrow(x) == 0L) {
    return(list(ordered = FALSE, complete = FALSE, core_ordered = FALSE,
                stress_ordered = NA, medians = setNames(rep(NA_real_, 3L),
                                                        c("null", "borderline", "clear")),
                by_layer = data.frame(), reversal = TRUE,
                reason = "no_completed_replicates"))
  }
  if (!"design_layer" %in% names(x)) x$design_layer <- "all"
  layers <- unique(as.character(x$design_layer))
  truth_levels <- c("null", "borderline", "clear")
  rows <- lapply(layers, function(layer) {
    values <- x$overall_score[x$design_layer == layer]
    truth <- as.character(x$truth_class[x$design_layer == layer])
    medians <- vapply(truth_levels, function(level) {
      scores <- values[truth == level]
      if (length(scores) == 0L) NA_real_ else stats::median(scores)
    }, numeric(1))
    present <- is.finite(medians)
    complete <- all(present)
    ordered <- complete && all(diff(medians) >= 0)
    data.frame(design_layer = layer, null = medians[[1L]],
               borderline = medians[[2L]], clear = medians[[3L]],
               complete = complete, missing_truth = paste(truth_levels[!present], collapse = ","),
               ordered = ordered, stringsAsFactors = FALSE)
  })
  by_layer <- do.call(rbind, rows)
  rownames(by_layer) <- NULL
  median_row <- if ("core" %in% by_layer$design_layer) {
    which(by_layer$design_layer == "core")[[1L]]
  } else 1L
  medians <- setNames(as.numeric(by_layer[median_row, truth_levels]), truth_levels)
  core_ordered <- if ("core" %in% by_layer$design_layer) by_layer$ordered[by_layer$design_layer == "core"] else FALSE
  stress_ordered <- if ("stress" %in% by_layer$design_layer) by_layer$ordered[by_layer$design_layer == "stress"] else TRUE
  complete <- isTRUE(core_ordered) && ("stress" %in% by_layer$design_layer || isTRUE(stress_ordered))
  list(ordered = complete, complete = complete, core_ordered = core_ordered,
       stress_ordered = stress_ordered,
       medians = medians, by_layer = by_layer,
       reversal = !isTRUE(core_ordered) || !isTRUE(stress_ordered))
}

#' Cluster bootstrap a scalar statistic by scenario.
#' @export
cluster_bootstrap_metrics <- function(replicates, statistic, B = 1000L, seed = 1L,
                                      conf_level = 0.95) {
  if (!is.data.frame(replicates)) .summary_abort("replicates must be a data frame")
  if (!is.function(statistic)) .summary_abort("statistic must be a function")
  B <- .validate_count(B, "B"); if (B < 1L) .summary_abort("B must be positive")
  seed <- .validate_count(seed, "seed", upper = .Machine$integer.max)
  conf_level <- .validate_probability(conf_level, "conf_level")
  if (!"scenario_id" %in% names(replicates)) .summary_abort("replicates must contain scenario_id")
  clusters <- unique(as.character(replicates$scenario_id))
  if (length(clusters) < 1L) .summary_abort("at least one scenario cluster is required")
  estimate <- statistic(replicates)
  if (!is.numeric(estimate) || length(estimate) != 1L || !is.finite(estimate)) {
    .summary_abort("statistic must return one finite numeric value")
  }
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(as.integer(seed))
  draws <- vapply(seq_len(B), function(iteration) {
    selected <- sample(clusters, size = length(clusters), replace = TRUE)
    rows <- do.call(rbind, lapply(selected, function(cluster) {
      replicates[replicates$scenario_id == cluster, , drop = FALSE]
    }))
    value <- statistic(rows)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      .summary_abort("statistic returned a non-finite value during bootstrap")
    }
    as.numeric(value)
  }, numeric(1))
  tails <- (1 - conf_level) / 2
  list(estimate = as.numeric(estimate), draws = draws,
       lower = as.numeric(stats::quantile(draws, tails, names = FALSE, type = 6)),
       upper = as.numeric(stats::quantile(draws, 1 - tails, names = FALSE, type = 6)),
       conf_level = conf_level, B = as.integer(B), seed = as.integer(seed),
       clusters = clusters, n_clusters = length(clusters), cluster = "scenario")
}

.summary_failure_group <- function(x, columns = character()) {
  .summary_group_table(x, columns, function(rows) {
    attempted <- nrow(rows); completed <- sum(rows$status == "completed", na.rm = TRUE)
    failed <- sum(rows$status == "failed", na.rm = TRUE)
    excluded <- sum(rows$status == "excluded", na.rm = TRUE)
    c(attempted = attempted, completed = completed, successful = completed,
      excluded = excluded, failed = failed,
      completion_rate = if (attempted) completed / attempted else NA_real_,
      failure_rate = if (attempted) failed / attempted else NA_real_)
  })
}

#' Summarise attempted and completed calibration replicates.
#' @export
completion_rates <- function(replicates, by = character()) {
  .summary_required(replicates, "status")
  if (!is.character(by)) .summary_abort("by must be character column names")
  if (length(setdiff(by, names(replicates))) > 0L) .summary_abort("completion grouping column is missing")
  attempted <- nrow(replicates); completed <- sum(replicates$status == "completed", na.rm = TRUE)
  failed <- sum(replicates$status == "failed", na.rm = TRUE)
  excluded <- sum(replicates$status == "excluded", na.rm = TRUE)
  overall <- list(attempted = as.integer(attempted), completed = as.integer(completed), successful = as.integer(completed),
                  excluded = as.integer(excluded), failed = as.integer(failed),
                  completion_rate = if (attempted) completed / attempted else NA_real_,
                  failure_rate = if (attempted) failed / attempted else NA_real_)
  list(attempted = overall$attempted, completed = overall$completed, successful = overall$successful,
       excluded = overall$excluded, failed = overall$failed,
       completion_rate = overall$completion_rate, failure_rate = overall$failure_rate,
       overall = overall, by_group = .summary_failure_group(replicates, by))
}

#' Summarise failure counts and failure classes without dropping denominators.
#' @export
failure_rates <- function(replicates, by = character()) {
  .summary_required(replicates, c("status", "failure_class"))
  result <- completion_rates(replicates, by)
  failed <- replicates[replicates$status == "failed", , drop = FALSE]
  if (nrow(failed) == 0L) {
    classes <- data.frame(failure_class = character(), failed = integer(), rate = numeric(), stringsAsFactors = FALSE)
  } else {
    counts <- sort(table(failed$failure_class), decreasing = FALSE)
    classes <- data.frame(failure_class = names(counts), failed = as.integer(counts),
                          rate = as.integer(counts) / nrow(replicates), stringsAsFactors = FALSE)
  }
  result$by_failure_class <- classes
  result
}

summarise_completion <- completion_rates
summarise_failures <- failure_rates

#' Check the frozen minimums and Monte Carlo precision criteria in the SAP.
#' @export
monte_carlo_target_met <- function(summary, sap = list()) {
  if (!is.list(summary)) .summary_abort("summary must be a list")
  if (!is.list(sap)) .summary_abort("sap must be a list")
  value <- function(name, default = NULL) if (!is.null(summary[[name]])) summary[[name]] else default
  sap_value <- function(names, default) {
    hit <- names[names %in% names(sap)]
    if (length(hit)) sap[[hit[[1L]]]] else default
  }
  completed <- value("completed", value("n", NA_real_)); heldout <- value("heldout_n", value("validation_n", NA_real_))
  mc_se <- value("mc_se", value("monte_carlo_se", NA_real_))
  min_completed <- sap_value(c("min_completed", "min_completed_full"), 500)
  min_heldout <- sap_value(c("min_heldout", "heldout_minimum"), 100)
  max_mc_se <- sap_value(c("max_mc_se", "mc_se_max"), 0.02)
  false <- value("false_reassurance", list()); robust <- value("robust_identification", list())
  false_point <- false$point %||% false$estimate %||% NA_real_; false_upper <- false$upper %||% NA_real_
  robust_point <- robust$point %||% robust$estimate %||% NA_real_; robust_lower <- robust$lower %||% NA_real_
  checks <- c(
    completed_minimum = is.finite(completed) && completed >= min_completed,
    heldout_minimum = is.finite(heldout) && heldout >= min_heldout,
    monte_carlo_precision = is.finite(mc_se) && mc_se <= max_mc_se,
    false_reassurance_point = is.finite(false_point) && false_point <= sap_value("false_reassurance_max", 0.05),
    false_reassurance_upper = is.finite(false_upper) && false_upper <= sap_value("false_reassurance_upper_max", 0.10),
    robust_identification_point = is.finite(robust_point) && robust_point >= sap_value("robust_identification_min", 0.70),
    robust_identification_lower = is.finite(robust_lower) && robust_lower >= sap_value("robust_identification_lower_min", 0.60)
  )
  list(met = isTRUE(all(checks)), failed_criteria = names(checks)[!checks],
       criteria = checks, completed = completed, heldout_n = heldout, mc_se = mc_se)
}
