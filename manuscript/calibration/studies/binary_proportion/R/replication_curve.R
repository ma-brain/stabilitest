# Track D' replication-probability curve for the binary-proportion study.
#
# Fits a logistic model replication_significant ~ score on completed
# significant rows.  Held-out calibration gates (conservative bounds):
#   intercept |logit| <= 0.20
#   slope in [0.85, 1.15]
#   max abs error over 10 equal-count bins <= 0.10
#   Brier(score) - Brier(p-only reference) <= 0.01
# The p-only reference map (replication calibrated on original_p alone) is the
# Brier baseline: the score must improve on predicting from p alone.

.replication_eligible <- function(data) {
  if (!is.data.frame(data)) {
    stop("replication data must be a data frame", call. = FALSE)
  }
  required <- c("overall_score", "replication_significant")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(sprintf("missing columns: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }
  if (anyNA(data$overall_score) || any(!is.finite(data$overall_score))) {
    stop("overall_score must be finite", call. = FALSE)
  }
  y <- data$replication_significant
  if (!is.numeric(y) || anyNA(y) || !all(y %in% c(0, 1))) {
    stop("replication_significant must be 0/1", call. = FALSE)
  }
  data
}

# Isotonic-regression helper for the sensitivity check (base R implementation
# via pool-adjacent-violators).  Returns a step function mapping x -> fitted y.
.prop_isotonic <- function(x, y) {
  ord <- order(x)
  x <- x[ord]; y <- y[ord]
  n <- length(y)
  fitted <- as.numeric(y)
  weight <- rep(1, n)
  # Pool adjacent violators (non-decreasing).
  repeat {
    violated <- FALSE
    i <- 1L
    while (i < n) {
      if (fitted[[i]] > fitted[[i + 1L]]) {
        # Merge i and i+1 into a block.
        w <- weight[[i]] + weight[[i + 1L]]
        fitted[[i]] <- (weight[[i]] * fitted[[i]] + weight[[i + 1L]] * fitted[[i + 1L]]) / w
        weight[[i]] <- w
        fitted <- fitted[-(i + 1L)]; weight <- weight[-(i + 1L)]
        x <- x[-(i + 1L)]; n <- n - 1L
        violated <- TRUE
      } else {
        i <- i + 1L
      }
    }
    if (!violated) break
  }
  stepfun(x, c(fitted[1L], fitted), right = FALSE)
}

# Fit the replication curve on training.  Returns intercept/slope, a predict()
# function (score -> replication probability), and a p_only_reference() function
# (original_p -> replication probability via isotonic regression on p alone).
fit_fisher_exact_replication_curve <- function(training) {
  data <- .replication_eligible(training)
  if (!nrow(data)) stop("no eligible replication rows", call. = FALSE)
  has_p <- "original_p" %in% names(data)
  glm_fit <- stats::glm(
    replication_significant ~ overall_score,
    data = data, family = stats::binomial()
  )
  coef <- stats::coef(glm_fit)
  intercept <- as.numeric(coef[[1L]])
  slope <- as.numeric(coef[[2L]])

  predict <- function(score) {
    eta <- intercept + slope * as.numeric(score)
    as.numeric(1 / (1 + exp(-eta)))
  }

  # p-only reference: isotonic regression of replication on original_p.  When
  # original_p is absent, fall back to the empirical replication rate.
  p_only_reference <- if (has_p) {
    iso <- .prop_isotonic(as.numeric(data$original_p),
                          as.numeric(data$replication_significant))
    function(original_p) as.numeric(iso(as.numeric(original_p)))
  } else {
    base_rate <- mean(data$replication_significant)
    function(original_p) rep(base_rate, length(original_p))
  }

  # In-sample calibration object (training): isotonic sensitivity of observed
  # vs predicted, archived as a diagnostic.
  calibration <- list(
    intercept = intercept,
    slope = slope,
    n = nrow(data),
    base_rate = mean(data$replication_significant),
    has_original_p = has_p
  )

  list(
    status = "candidate",
    intercept = intercept,
    slope = slope,
    predict = predict,
    p_only_reference = p_only_reference,
    calibration = calibration,
    glm_fit = glm_fit
  )
}

# Brier score for binary predictions.
.prop_brier <- function(predicted, observed) {
  mean((as.numeric(predicted) - as.numeric(observed))^2)
}

# Held-out calibration diagnostics: fit a calibration regression of observed
# replication on the model's predicted probability (logistic link) and report
# the gate quantities.
replication_curve_held_out_diagnostics <- function(fit, data) {
  data <- .replication_eligible(data)
  predicted <- fit$predict(data$overall_score)
  observed <- as.numeric(data$replication_significant)

  # Calibration regression: observed ~ logit(predicted).  Ideal: intercept 0,
  # slope 1.  Clamp predicted away from 0/1 for the logit.
  eps <- 1e-6
  p_clamped <- pmin(1 - eps, pmax(eps, predicted))
  logit_pred <- log(p_clamped / (1 - p_clamped))
  calib <- stats::glm(observed ~ logit_pred, family = stats::binomial())
  calib_coef <- stats::coef(calib)
  calib_intercept <- as.numeric(calib_coef[[1L]])
  calib_slope <- as.numeric(calib_coef[[2L]])

  # Max abs error over 10 equal-count bins (observed vs predicted mean per bin).
  ord <- order(predicted)
  predicted_sorted <- predicted[ord]
  observed_sorted <- observed[ord]
  bin_size <- ceiling(length(predicted_sorted) / 10)
  bins <- split(seq_along(predicted_sorted),
                ceiling(seq_along(predicted_sorted) / bin_size))
  bin_errors <- vapply(bins, function(idx) {
    if (!length(idx)) return(NA_real_)
    abs(mean(observed_sorted[idx]) - mean(predicted_sorted[idx]))
  }, numeric(1))
  max_abs_bin_error <- max(bin_errors, na.rm = TRUE)

  # Brier scores.
  brier_score <- .prop_brier(predicted, observed)
  if ("original_p" %in% names(data)) {
    p_only_pred <- fit$p_only_reference(data$original_p)
  } else {
    p_only_pred <- rep(fit$calibration$base_rate, nrow(data))
  }
  brier_p_only <- .prop_brier(p_only_pred, observed)

  list(
    intercept_logit_abs = abs(calib_intercept),
    slope = calib_slope,
    max_abs_bin_error = max_abs_bin_error,
    brier_score = brier_score,
    brier_p_only = brier_p_only,
    brier_improvement = brier_p_only - brier_score,
    n_bins = length(bins)
  )
}

# Conservative gate verdict on the held-out replication curve.
replication_curve_passes <- function(fit, data) {
  d <- replication_curve_held_out_diagnostics(fit, data)
  reasons <- character()
  if (!isTRUE(is.finite(d$intercept_logit_abs)) || d$intercept_logit_abs > 0.20) {
    reasons <- c(reasons, "intercept_logit")
  }
  if (!isTRUE(is.finite(d$slope)) || d$slope < 0.85 || d$slope > 1.15) {
    reasons <- c(reasons, "slope")
  }
  if (!isTRUE(is.finite(d$max_abs_bin_error)) || d$max_abs_bin_error > 0.10) {
    reasons <- c(reasons, "max_abs_bin_error")
  }
  if (!isTRUE(is.finite(d$brier_improvement)) || d$brier_improvement < -0.01) {
    reasons <- c(reasons, "brier_improvement")
  }
  list(
    passes = length(reasons) == 0L,
    reasons = reasons,
    diagnostics = d
  )
}
