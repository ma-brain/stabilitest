# Deterministic uncertainty helpers for calibration summaries.

.summary_abort <- function(message) stop(message, call. = FALSE)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(left, right) if (is.null(left)) right else left
}

.validate_probability <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value <= 0 || value >= 1) {
    .summary_abort(sprintf("%s must be one finite value in (0, 1)", name))
  }
  as.numeric(value)
}

.validate_count <- function(value, name, upper = Inf) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0 || floor(value) != value || value > upper) {
    .summary_abort(sprintf("%s must be an integer in [0, %s]", name, upper))
  }
  as.numeric(value)
}

#' Wilson score interval for a binomial proportion.
#'
#' The default is the one-sided confidence convention frozen in the SAP: the
#' normal quantile is `qnorm(conf_level)`, and both endpoint columns are
#' returned so callers can use `upper` for false-reassurance and `lower` for
#' robust-identification decisions.  Set `sides = "two.sided"` when a usual
#' two-sided interval is needed.
#' @export
wilson_interval <- function(x, n, conf_level = 0.95,
                            sides = c("one.sided", "two.sided")) {
  sides <- match.arg(sides)
  if (!is.numeric(x) || !is.numeric(n) || length(x) != length(n) ||
      length(x) < 1L || anyNA(x) || anyNA(n) || any(!is.finite(x)) ||
      any(!is.finite(n)) || any(x < 0) || any(n <= 0) || any(x > n) ||
      any(floor(x) != x) || any(floor(n) != n)) {
    .summary_abort("x and n must be finite binomial counts with 0 <= x <= n")
  }
  conf_level <- .validate_probability(conf_level, "conf_level")
  tail_probability <- if (sides == "one.sided") conf_level else (1 + conf_level) / 2
  z <- stats::qnorm(tail_probability)
  denominator <- 1 + z^2 / n
  estimate <- x / n
  centre <- (estimate + z^2 / (2 * n)) / denominator
  half <- z / denominator * sqrt(estimate * (1 - estimate) / n + z^2 / (4 * n^2))
  lower <- pmax(0, centre - half)
  upper <- pmin(1, centre + half)
  result <- list(
    x = as.numeric(x), n = as.numeric(n), estimate = as.numeric(estimate),
    lower = as.numeric(lower), upper = as.numeric(upper),
    conf_level = conf_level, sides = sides, z = z
  )
  if (length(x) == 1L) result else result
}

.summary_as_completed <- function(replicates) {
  if (!is.data.frame(replicates)) .summary_abort("replicates must be a data frame")
  if (!"status" %in% names(replicates)) return(replicates)
  replicates[replicates$status == "completed", , drop = FALSE]
}

.summary_cutoffs <- function(cutoffs) {
  if (is.list(cutoffs)) {
    if (!is.null(cutoffs$lower) && !is.null(cutoffs$upper)) {
      cutoffs <- c(cutoffs$lower, cutoffs$upper)
    } else if (!is.null(cutoffs$moderate) && !is.null(cutoffs$robust)) {
      cutoffs <- c(cutoffs$moderate, cutoffs$robust)
    }
  }
  if (!is.numeric(cutoffs) || length(cutoffs) != 2L || anyNA(cutoffs) ||
      any(!is.finite(cutoffs)) || any(cutoffs < 0 | cutoffs > 100) ||
      cutoffs[[1L]] >= cutoffs[[2L]]) {
    .summary_abort("cutoffs must be two ordered finite values in [0, 100]")
  }
  as.numeric(cutoffs)
}

.score_band <- function(score, cutoffs) {
  cutoffs <- .summary_cutoffs(cutoffs)
  ifelse(is.na(score), NA_character_,
         ifelse(score <= cutoffs[[1L]], "fragile",
                ifelse(score <= cutoffs[[2L]], "moderate", "robust")))
}

.summary_conclusion <- function(value) {
  if (is.list(value)) {
    if (length(value) == 0L) return(NA_character_)
    value <- value[[1L]]
    if (is.list(value)) {
      candidates <- c("conclusion", "analysis_conclusion", "screening_conclusion",
                      "original_significant", "significant", "equivalent", "noninferior")
      hit <- candidates[candidates %in% names(value)]
      if (length(hit) > 0L) value <- value[[hit[[1L]]]] else value <- NA_character_
    }
  }
  if (length(value) != 1L || is.na(value)) return(NA_character_)
  if (is.logical(value)) return(if (isTRUE(value)) "significant" else "non_significant")
  if (!is.character(value) || !nzchar(value)) return(NA_character_)
  tolower(gsub("[- ]", "_", value))
}

.target_supported <- function(replicates) {
  if (!"target_conclusion" %in% names(replicates)) return(rep(TRUE, nrow(replicates)))
  observed <- if ("analysis_conclusion" %in% names(replicates)) {
    vapply(replicates$analysis_conclusion, .summary_conclusion, character(1))
  } else if ("screening_conclusion" %in% names(replicates)) {
    vapply(replicates$screening_conclusion, .summary_conclusion, character(1))
  } else rep(NA_character_, nrow(replicates))
  target <- tolower(gsub("[- ]", "_", as.character(replicates$target_conclusion)))
  target[target %in% c("noninferiority", "non_inferiority")] <- "noninferior"
  observed[observed %in% c("noninferiority", "non_inferiority")] <- "noninferior"
  # A scalar/list conclusion may expose a boolean field rather than a label.
  exact <- !is.na(observed) & observed == target
  if ("screening_conclusion" %in% names(replicates)) {
    screen <- vapply(replicates$screening_conclusion, .summary_conclusion, character(1))
    exact <- exact | (!is.na(screen) & screen == target)
  }
  exact
}

.summary_rate <- function(x, n) {
  x <- as.numeric(x); n <- as.numeric(n)
  if (n <= 0) return(list(count = 0, n = 0, point = NA_real_, estimate = NA_real_,
                          lower = NA_real_, upper = NA_real_, mc_se = NA_real_))
  ci <- wilson_interval(x, n)
  list(count = as.integer(x), n = as.integer(n), point = ci$estimate,
       estimate = ci$estimate, lower = ci$lower, upper = ci$upper,
       mc_se = sqrt(ci$estimate * (1 - ci$estimate) / n))
}
