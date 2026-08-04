# Calibration adapters for equivalence and non-inferiority (TOST/NI).

#' Validate the endpoint/type-specific TOST bounds.
.validate_tost_bounds <- function(endpoint, type, margin = NULL,
                                  delta_L = NULL, delta_U = NULL) {
  endpoint <- match.arg(endpoint, c("mean", "prop", "or"))
  type <- match.arg(type, c("equivalence", "noninferiority"))
  scalar <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x))
      stop(sprintf("%s must be a single finite numeric", name), call. = FALSE)
  }
  if (!is.null(margin)) {
    scalar(margin, "margin")
    if (margin <= if (endpoint == "or") 1 else 0)
      stop(if (endpoint == "or") "OR margin must be > 1" else "margin must be > 0", call. = FALSE)
  }
  has_asym <- !is.null(delta_L) || !is.null(delta_U)
  if (has_asym) {
    if (is.null(delta_L) || is.null(delta_U))
      stop("delta_L and delta_U must be supplied together", call. = FALSE)
    scalar(delta_L, "delta_L"); scalar(delta_U, "delta_U")
    if (endpoint == "or" && (delta_L <= 0 || delta_U <= 0))
      stop("OR bounds must be positive", call. = FALSE)
    if (!(delta_L < delta_U))
      stop("delta_L must be strictly less than delta_U", call. = FALSE)
  }
  if (type == "noninferiority") {
    if (has_asym) stop("delta_L/delta_U are only supported for equivalence", call. = FALSE)
    if (is.null(margin)) stop("Non-inferiority requires margin", call. = FALSE)
  } else if (is.null(margin) && !has_asym) {
    stop("Equivalence requires margin or delta_L/delta_U", call. = FALSE)
  } else if (!is.null(margin) && has_asym) {
    stop("Supply either margin or delta_L/delta_U, not both", call. = FALSE)
  }
  invisible(TRUE)
}

#' Classify a data-generating truth against configured bounds.
#' @export
classify_tost_truth <- function(effect, type = c("equivalence", "noninferiority"),
                                margin = NULL, delta_L = NULL, delta_U = NULL,
                                higher_is_better = TRUE, endpoint = "mean") {
  type <- match.arg(type)
  endpoint <- match.arg(endpoint, c("mean", "prop", "or"))
  if (!is.numeric(effect) || length(effect) != 1L || is.na(effect) || !is.finite(effect))
    stop("effect must be a single finite numeric", call. = FALSE)
  if (!is.logical(higher_is_better) || length(higher_is_better) != 1L || is.na(higher_is_better)) stop("higher_is_better must be a single non-missing logical", call. = FALSE)
  .validate_tost_bounds(endpoint, type, margin, delta_L, delta_U)
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
                          odds_ratio = NULL, margin = NULL, equivalence_margin = NULL,
                          delta_L = NULL,
                          delta_U = NULL, paired = FALSE, higher_is_better = TRUE,
                          seed = NULL, truth_effect = NULL, ...) {
  endpoint <- match.arg(endpoint); type <- match.arg(type)
  # The scenario registry uses the descriptive `equivalence_margin` key for
  # the frozen smoke vignette.  Keep `margin` as the public API while accepting
  # the registry spelling as an alias for equivalence (and reject ambiguity).
  if (!is.null(equivalence_margin)) {
    if (type != "equivalence") {
      stop("equivalence_margin is only supported for equivalence TOST", call. = FALSE)
    }
    if (!is.null(margin) && !identical(as.numeric(margin), as.numeric(equivalence_margin))) {
      stop("margin and equivalence_margin must agree when both are supplied", call. = FALSE)
    }
    margin <- equivalence_margin
  }
  if (!is.logical(paired) || length(paired) != 1L || is.na(paired)) stop("paired must be a single non-missing logical", call. = FALSE)
  if (!is.logical(higher_is_better) || length(higher_is_better) != 1L || is.na(higher_is_better)) stop("higher_is_better must be a single non-missing logical", call. = FALSE)
  if (endpoint != "mean" && isTRUE(paired)) stop("paired = TRUE is only supported for endpoint = \"mean\"", call. = FALSE)
  .validate_tost_bounds(endpoint, type, margin, delta_L, delta_U)
  scalar <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x))
      stop(sprintf("%s must be a single finite numeric", name), call. = FALSE)
  }
  if (is.null(n)) {
    scalar(n_per_group, "n_per_group")
    if (n_per_group < 8 || n_per_group > .Machine$integer.max || n_per_group != floor(n_per_group))
      stop("n_per_group must be an integer >= 8", call. = FALSE)
    n_per_group <- as.integer(n_per_group)
  } else {
    scalar(n, "n")
    if (n < 16 || n > .Machine$integer.max || n != floor(n) || n %% 2 != 0)
      stop("n must be an even integer >= 16", call. = FALSE)
    n_per_group <- as.integer(n / 2)
  }
  if (!is.null(seed)) {
    scalar(seed, "seed")
    if (seed < 0 || seed > .Machine$integer.max - 1 || seed != floor(seed)) stop("seed must be a non-negative integer", call. = FALSE)
    set.seed(as.integer(seed))
  }
  scalar(probability_control, "probability_control")
  if (probability_control < 0 || probability_control > 1) stop("probability_control must be in [0, 1]", call. = FALSE)
  if (!is.null(probability_treatment)) {
    scalar(probability_treatment, "probability_treatment")
    if (probability_treatment < 0 || probability_treatment > 1) stop("probability_treatment must be in [0, 1]", call. = FALSE)
  }
  if (!is.null(mean_difference)) { scalar(mean_difference, "mean_difference") }
  if (!is.null(odds_ratio)) { scalar(odds_ratio, "odds_ratio"); if (odds_ratio <= 0) stop("odds_ratio must be > 0", call. = FALSE) }
  if (!is.null(truth_effect)) scalar(truth_effect, "truth_effect")
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

.tost_prop_degenerate <- function(data) {
  if (!is.list(data) || is.null(data$group1) || is.null(data$group2)) return(FALSE)
  # A constant arm gives a zero within-arm variance.  The Wald RD test cannot
  # form a finite standard error for the all-zero/all-one sparse tables.
  any(vapply(list(data$group1, data$group2), function(x) {
    length(x) < 2L || anyNA(x) || length(unique(x)) < 2L
  }, logical(1L)))
}

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
  if (endpoint == "prop" && .tost_prop_degenerate(data)) {
    stop("sparse_degenerate: proportion endpoint has a constant arm", call. = FALSE)
  }
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
    cls <- if (endpoint == "prop" && .tost_prop_degenerate(data)) {
      "sparse_degenerate"
    } else if (grepl("sparse|zero|degenerate|variance", conditionMessage(e), ignore.case = TRUE)) {
      "sparse_degenerate"
    } else "test_failure"
    list(status = "failed", screening = NULL, analysis = NULL, original_p = NA_real_, effective_p = NA_real_,
         failure_stage = "screening", failure_class = cls, failure_message = conditionMessage(e))
  })
}

calibrate_tost <- run_tost_adapter
