# Deterministic two-sided Welch power approximation and effect solver.

.validate_welch_design <- function(n1, n2, sd1, sd2, alpha) {
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  sd1 <- as.numeric(sd1)
  sd2 <- as.numeric(sd2)
  alpha <- as.numeric(alpha)
  if (length(n1) != 1L || length(n2) != 1L || n1 < 2L || n2 < 2L) {
    stop("n1 and n2 must each be at least 2", call. = FALSE)
  }
  if (length(sd1) != 1L || length(sd2) != 1L ||
      !is.finite(sd1) || !is.finite(sd2) || sd1 <= 0 || sd2 <= 0) {
    stop("sd1 and sd2 must be finite and positive", call. = FALSE)
  }
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be in (0, 1)", call. = FALSE)
  }
  list(n1 = n1, n2 = n2, sd1 = sd1, sd2 = sd2, alpha = alpha)
}

welch_degrees_freedom <- function(n1, n2, sd1, sd2) {
  design <- .validate_welch_design(n1, n2, sd1, sd2, alpha = 0.05)
  v1 <- design$sd1^2 / design$n1
  v2 <- design$sd2^2 / design$n2
  (v1 + v2)^2 /
    (v1^2 / (design$n1 - 1L) + v2^2 / (design$n2 - 1L))
}

#' Approximate two-sided Welch-test power using a noncentral t distribution.
welch_power <- function(delta, n1, n2, sd1, sd2, alpha = 0.05) {
  design <- .validate_welch_design(n1, n2, sd1, sd2, alpha)
  delta <- as.numeric(delta)
  if (length(delta) != 1L || !is.finite(delta)) {
    stop("delta must be a finite scalar", call. = FALSE)
  }
  v1 <- design$sd1^2 / design$n1
  v2 <- design$sd2^2 / design$n2
  standard_error <- sqrt(v1 + v2)
  degrees_freedom <- (v1 + v2)^2 /
    (v1^2 / (design$n1 - 1L) + v2^2 / (design$n2 - 1L))
  critical <- stats::qt(1 - design$alpha / 2, df = degrees_freedom)
  ncp <- delta / standard_error
  stats::pt(-critical, df = degrees_freedom, ncp = ncp) +
    1 - stats::pt(critical, df = degrees_freedom, ncp = ncp)
}

#' Solve the positive mean difference for a frozen Welch power target.
solve_welch_delta <- function(target_power, n1, n2, sd1, sd2,
                              alpha = 0.05) {
  design <- .validate_welch_design(n1, n2, sd1, sd2, alpha)
  target_power <- as.numeric(target_power)
  if (length(target_power) != 1L || !is.finite(target_power) ||
      target_power <= design$alpha || target_power >= 1) {
    stop("target_power must be strictly between alpha and 1", call. = FALSE)
  }

  standard_error <- sqrt(
    design$sd1^2 / design$n1 + design$sd2^2 / design$n2
  )
  upper <- 12 * standard_error
  while (welch_power(
    upper, design$n1, design$n2, design$sd1, design$sd2, design$alpha
  ) < target_power) {
    upper <- upper * 2
    if (!is.finite(upper)) {
      stop("unable to bracket the requested Welch power target", call. = FALSE)
    }
  }

  stats::uniroot(
    function(delta) {
      welch_power(
        delta, design$n1, design$n2, design$sd1, design$sd2, design$alpha
      ) - target_power
    },
    interval = c(0, upper),
    tol = .Machine$double.eps^0.75
  )$root
}
