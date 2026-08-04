# Calibration adapters for equivalence and non-inferiority (TOST/NI).

#' Classify a data-generating truth against configured bounds.
#' @export
classify_tost_truth <- function(effect, type = c("equivalence", "noninferiority"),
                                margin = NULL, delta_L = NULL, delta_U = NULL,
                                higher_is_better = TRUE, endpoint = "mean") {
  type <- match.arg(type)
  if (endpoint == "or") {
    if (!is.null(margin)) { delta_L <- 1 / margin; delta_U <- margin }
    if (type == "equivalence") return(if (effect > delta_L && effect < delta_U) "equivalent" else "not_equivalent")
    return(if (higher_is_better) if (effect >= 1 / margin) "noninferior" else "inferior" else if (effect <= margin) "noninferior" else "inferior")
  }
  if (type == "equivalence") {
    if (!is.null(margin)) { delta_L <- -margin; delta_U <- margin }
    return(if (effect > delta_L && effect < delta_U) "equivalent" else "not_equivalent")
  }
  if (higher_is_better) if (effect >= -margin) "noninferior" else "inferior" else if (effect <= margin) "noninferior" else "inferior"
}

#' Generate endpoint-specific TOST calibration data.
#' @export
generate_tost <- function(endpoint = c("mean", "prop", "or"),
                          type = c("equivalence", "noninferiority"),
                          n_per_group = 40, n = NULL, mean_difference = NULL,
                          probability_control = 0.4, probability_treatment = NULL,
                          odds_ratio = NULL, margin = NULL, delta_L = NULL,
                          delta_U = NULL, paired = FALSE, higher_is_better = TRUE,
                          seed = NULL, truth_effect = NULL, ...) {
  endpoint <- match.arg(endpoint); type <- match.arg(type)
  if (endpoint != "mean" && isTRUE(paired)) stop("paired = TRUE is only supported for endpoint = \"mean\"", call. = FALSE)
  n_per_group <- if (is.null(n)) n_per_group else as.integer(n / 2)
  if (!is.numeric(n_per_group) || length(n_per_group) != 1L || n_per_group < 8) stop("n_per_group must be >= 8", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  if (endpoint == "mean") {
    if (is.null(mean_difference)) mean_difference <- if (type == "equivalence") 0 else 0.25
    if (paired) {
      baseline <- rnorm(n_per_group)
      g1 <- baseline + rnorm(n_per_group, mean = mean_difference / 2, sd = .7)
      g2 <- baseline + rnorm(n_per_group, mean = -mean_difference / 2, sd = .7)
    } else {
      g1 <- rnorm(n_per_group, mean = mean_difference, sd = 1)
      g2 <- rnorm(n_per_group, mean = 0, sd = 1)
    }
    truth <- if (is.null(truth_effect)) mean_difference else truth_effect
  } else if (endpoint == "prop") {
    if (is.null(probability_treatment)) probability_treatment <- probability_control + if (type == "equivalence") 0 else .1
    probability_treatment <- max(min(probability_treatment, .99), .01)
    g1 <- rbinom(n_per_group, 1, probability_treatment)
    g2 <- rbinom(n_per_group, 1, probability_control)
    truth <- if (is.null(truth_effect)) probability_treatment - probability_control else truth_effect
  } else {
    if (is.null(odds_ratio)) odds_ratio <- if (type == "equivalence") 1 else 1.2
    p1 <- plogis(qlogis(probability_control) + log(odds_ratio))
    g1 <- rbinom(n_per_group, 1, p1)
    g2 <- rbinom(n_per_group, 1, probability_control)
    truth <- if (is.null(truth_effect)) odds_ratio else truth_effect
  }
  truth_class <- classify_tost_truth(truth, type = type, margin = margin,
                                     delta_L = delta_L, delta_U = delta_U,
                                     higher_is_better = higher_is_better, endpoint = endpoint)
  out <- list(group1 = g1, group2 = g2, endpoint = endpoint, type = type,
              paired = paired, higher_is_better = higher_is_better,
              margin = margin, delta_L = delta_L, delta_U = delta_U,
              truth_effect = truth, truth_class = truth_class,
              seed = seed)
  class(out) <- c("calibration_tost_data", "list")
  out
}

.tost_args <- function(data, endpoint, type, margin, delta_L, delta_U, paired,
                       higher_is_better, alpha, n_boot, max_removal_pct, seed, dots) {
  if (!inherits(data, "calibration_tost_data") && is.list(data) && !is.null(data$group1)) {
    endpoint <- data$endpoint %||% endpoint; type <- data$type %||% type
    if (is.null(margin)) margin <- data$margin
    if (is.null(delta_L)) delta_L <- data$delta_L
    if (is.null(delta_U)) delta_U <- data$delta_U
    paired <- if (missing(paired)) data$paired else paired
    higher_is_better <- if (missing(higher_is_better)) data$higher_is_better else higher_is_better
  }
  list(group1 = data$group1, group2 = data$group2, endpoint = endpoint, type = type,
       margin = margin, delta_L = delta_L, delta_U = delta_U, paired = paired,
       higher_is_better = higher_is_better, alpha = alpha, n_boot = n_boot,
       max_removal_pct = max_removal_pct, seed = seed, dots = dots)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Run the configured TOST/NI test for screening.
#' @export
screen_tost <- function(data, endpoint = c("mean", "prop", "or"),
                        type = c("equivalence", "noninferiority"), margin = NULL,
                        delta_L = NULL, delta_U = NULL, paired = FALSE,
                        higher_is_better = TRUE, alpha = 0.05, n_boot = 1000,
                        max_removal_pct = 0.30, seed = 123, ...) {
  if (is.list(data) && !is.null(data$group1)) {
    if (missing(endpoint) && !is.null(data$endpoint)) endpoint <- data$endpoint
    if (missing(type) && !is.null(data$type)) type <- data$type
    if (missing(paired) && !is.null(data$paired)) paired <- data$paired
    if (missing(higher_is_better) && !is.null(data$higher_is_better)) higher_is_better <- data$higher_is_better
    if (is.null(margin)) margin <- data$margin
    if (is.null(delta_L)) delta_L <- data$delta_L
    if (is.null(delta_U)) delta_U <- data$delta_U
  }
  endpoint <- match.arg(endpoint); type <- match.arg(type)
  a <- .tost_args(data, endpoint, type, margin, delta_L, delta_U, paired,
                  higher_is_better, alpha, n_boot, max_removal_pct, seed, list(...))
  res <- tryCatch(do.call(stabilitest::robustness_tost, c(a[setdiff(names(a), "dots")], a$dots)),
                  error = function(e) stop(e))
  list(p_eff = res$original_p, original_p = res$original_p,
       conclusion = if (res$original_p < alpha) if (type == "equivalence") "equivalent" else "noninferior" else if (type == "equivalence") "not_equivalent" else "inferior",
       significant = res$original_p < alpha, analysis = res, status = "completed")
}

#' Execute a complete TOST/NI calibration replicate.
#' @export
run_tost_adapter <- function(data, endpoint = c("mean", "prop", "or"),
                             type = c("equivalence", "noninferiority"), margin = NULL,
                             delta_L = NULL, delta_U = NULL, paired = FALSE,
                             higher_is_better = TRUE, alpha = 0.05, n_boot = 1000,
                             max_removal_pct = 0.30, seed = 123, ...) {
  if (is.list(data) && !is.null(data$group1)) {
    if (missing(endpoint) && !is.null(data$endpoint)) endpoint <- data$endpoint
    if (missing(type) && !is.null(data$type)) type <- data$type
    if (missing(paired) && !is.null(data$paired)) paired <- data$paired
    if (missing(higher_is_better) && !is.null(data$higher_is_better)) higher_is_better <- data$higher_is_better
    if (is.null(margin)) margin <- data$margin
    if (is.null(delta_L)) delta_L <- data$delta_L
    if (is.null(delta_U)) delta_U <- data$delta_U
  }
  endpoint <- match.arg(endpoint); type <- match.arg(type)
  tryCatch({
    screening <- screen_tost(data, endpoint, type, margin, delta_L, delta_U,
                             paired, higher_is_better, alpha, n_boot, max_removal_pct, seed, ...)
    list(status = "completed", screening = screening, analysis = screening$analysis,
         original_p = screening$analysis$original_p, effective_p = screening$analysis$original_p,
         failure_stage = NA_character_, failure_class = NA_character_, failure_message = NA_character_)
  }, error = function(e) {
    cls <- if (grepl("sparse|zero|degenerate|variance", conditionMessage(e), ignore.case = TRUE)) "sparse_degenerate" else "test_failure"
    list(status = "failed", screening = NULL, analysis = NULL, original_p = NA_real_, effective_p = NA_real_,
         failure_stage = "screening", failure_class = cls, failure_message = conditionMessage(e))
  })
}

calibrate_tost <- run_tost_adapter
