# Exact Fisher power for balanced two-arm binary-proportion designs.
#
# No simulation: enumerate every 2x2 table (x0, x1), x_g ~ Bin(n, p_g), and sum
# the binomial-weighted probability of tables significant at alpha.  This is the
# same machinery as the committed feasibility projection scripts
# (tools/feasibility-projection-power0{90,95}.R) factored into reusable functions.

# p-value of fisher.test for every table (x0, x1), 0..n each (balanced arms).
# P[x0 + 1, x1 + 1] = p-value with control = x0 successes, active = x1.
.fisher_p_matrix <- function(n, alpha = 0.05) {
  P <- matrix(NA_real_, n + 1L, n + 1L)
  for (x0 in 0:n) for (x1 in 0:n) {
    P[x0 + 1L, x1 + 1L] <- stats::fisher.test(
      matrix(c(x1, n - x1, x0, n - x0), 2L)
    )$p.value
  }
  P
}

# Enumerated exact power of Fisher's test at (p0, p1), balanced n/arm.
exact_fisher_power <- function(n, p0, p1, alpha = 0.05) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be a positive integer", call. = FALSE)
  if (!(p0 >= 0 && p0 <= 1) || !(p1 >= 0 && p1 <= 1)) {
    stop("p0 and p1 must be probabilities in [0, 1]", call. = FALSE)
  }
  P <- .fisher_p_matrix(n, alpha = alpha)
  weights <- outer(stats::dbinom(0:n, n, p0), stats::dbinom(0:n, n, p1))
  sum(weights[P <= alpha])
}

# Per-process cache of solved active-arm probabilities.  The exact-power solve
# builds a full (n+1)x(n+1) Fisher p-value matrix (O(n^2) fisher.test calls),
# so memoizing by the (n, p0, target, alpha) key makes the thousands of
# replicate draws per scenario instant after the first solve.  Each distinct
# scenario solves at most once per process.
.prop_effect_cache <- new.env(parent = emptyenv())

.prop_effect_cache_key <- function(n, p0, target, alpha) {
  sprintf("%d|%a|%a|%a", as.integer(n), as.numeric(p0),
          as.numeric(target), as.numeric(alpha))
}

# Solve for the active-arm probability p1 such that the enumerated exact Fisher
# power equals the target.  Returns p0 when target is 0 (null truth).  Uses
# uniroot on the exact power curve; tolerance 1e-6 matches the projection
# scripts and the SAP.  Results are memoized per (n, p0, target, alpha).
solve_prop_effect <- function(n, p0, target, alpha = 0.05) {
  n <- as.integer(n)
  if (n < 1L) stop("n must be a positive integer", call. = FALSE)
  if (!(p0 >= 0 && p0 <= 1)) stop("p0 must be a probability in [0, 1]", call. = FALSE)
  target <- as.numeric(target)
  if (identical(target, 0) || isTRUE(all.equal(target, 0))) {
    return(p0)
  }
  if (!(target > alpha && target < 1)) {
    stop("target must be 0 or in (alpha, 1)", call. = FALSE)
  }
  key <- .prop_effect_cache_key(n, p0, target, alpha)
  cached <- .prop_effect_cache[[key]]
  if (!is.null(cached)) return(cached)
  # Cache the p-value matrix once (exact_fisher_power rebuilds it per call).
  P <- .fisher_p_matrix(n, alpha = alpha)
  power_at <- function(p1) {
    weights <- outer(stats::dbinom(0:n, n, p0), stats::dbinom(0:n, n, p1))
    sum(weights[P <= alpha])
  }
  upper <- 1 - 1e-6
  if (power_at(upper) < target) {
    stop(sprintf(
      "target power %.4f is unreachable at n=%d, p0=%.3f (max power %.4f)",
      target, n, p0, power_at(upper)
    ), call. = FALSE)
  }
  p1 <- stats::uniroot(
    function(p1) power_at(p1) - target,
    interval = c(p0 + 1e-6, upper), tol = 1e-6
  )$root
  .prop_effect_cache[[key]] <- p1
  p1
}

# Clear the per-process effect cache (test isolation).
.reset_prop_effect_cache <- function() {
  rm(list = ls(.prop_effect_cache), envir = .prop_effect_cache)
  invisible(NULL)
}
