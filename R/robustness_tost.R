# ==============================================================================
# Equivalence (TOST) and non-inferiority robustness
#
# Decision rule (not just a new test family):
#   * Equivalence (Schuirmann TOST): conclude equivalent iff both one-sided
#     tests reject at alpha, i.e. p_lower < alpha AND p_upper < alpha.
#     Equivalent CI rule: the (1 - 2*alpha) CI for the effect lies entirely
#     inside [delta_L, delta_U].
#   * Non-inferiority: conclude NI iff the appropriate one-sided test vs the
#     margin rejects at alpha (direction depends on higher_is_better).
#
# Endpoints (`endpoint` argument):
#   * "mean" — Welch / paired t on mean difference (group1 − group2)
#   * "prop" — Wald TOST/NI on risk difference RD = p1 − p2 (binary 0/1)
#   * "or"   — Wald TOST/NI on log(OR); user margins on the OR scale
#
# Methods (base R; statistically honest limitations):
#   * Mean: exact t one-sided tests (same as v1).
#   * Prop: normal/Wald z-tests on RD with SE =
#       sqrt(p1(1-p1)/n1 + p2(1-p2)/n2). Not Farrington–Manning score;
#       Wald can be anticonservative for sparse tables / extreme p.
#   * OR: Wald z on log(OR) with Haldane–Anscombe 0.5 continuity when any
#       2×2 cell is zero. Margins supplied on OR scale; testing on log scale.
#
# Robustness adapter: jackknife / worst-case / bootstrap reuse
# robustness_engine() with an effective p-value that preserves
#   original_significant <=> (p_eff < alpha)
# namely:
#   * Equivalence: p_eff = max(p_lower, p_upper)
#   * Non-inferiority: p_eff = the one-sided NI p-value
# Score bands are shared with the ANCOVA/Cox engines and are not separately
# calibrated for equivalence/NI (any endpoint).
# Rank-based TOST remains deferred.
# ==============================================================================

# Resolve margins: symmetric `margin` or asymmetric delta_L / delta_U
.resolve_tost_margins <- function(type, margin, delta_L, delta_U) {
  type <- match.arg(type, c("equivalence", "noninferiority"))
  has_asym <- !is.null(delta_L) || !is.null(delta_U)
  has_sym  <- !is.null(margin)

  if (type == "equivalence") {
    if (has_asym) {
      if (is.null(delta_L) || is.null(delta_U)) {
        stop("For asymmetric equivalence supply both delta_L and delta_U",
             call. = FALSE)
      }
      if (has_sym) {
        stop("Supply either margin or delta_L/delta_U, not both",
             call. = FALSE)
      }
      if (!is.numeric(delta_L) || length(delta_L) != 1L || is.na(delta_L) ||
          !is.numeric(delta_U) || length(delta_U) != 1L || is.na(delta_U)) {
        stop("delta_L and delta_U must be single non-missing numerics",
             call. = FALSE)
      }
      if (!(delta_L < delta_U)) {
        stop("delta_L must be strictly less than delta_U", call. = FALSE)
      }
      return(list(delta_L = delta_L, delta_U = delta_U, margin = NA_real_))
    }
    if (!has_sym) {
      stop("Equivalence requires margin > 0, or delta_L < delta_U",
           call. = FALSE)
    }
    if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
        margin <= 0) {
      stop("margin must be a single positive number", call. = FALSE)
    }
    return(list(delta_L = -margin, delta_U = margin, margin = margin))
  }

  # non-inferiority
  if (has_asym) {
    stop("Non-inferiority uses margin (and higher_is_better); ",
         "delta_L/delta_U are for equivalence only", call. = FALSE)
  }
  if (!has_sym) {
    stop("Non-inferiority requires a positive margin", call. = FALSE)
  }
  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
      margin <= 0) {
    stop("margin must be a single positive number", call. = FALSE)
  }
  list(delta_L = NA_real_, delta_U = NA_real_, margin = margin)
}

# OR-scale margins: equivalence [1/margin, margin] (margin > 1) or asymmetric
# OR bounds; NI uses multiplicative margin > 1.
.resolve_tost_or_margins <- function(type, margin, delta_L, delta_U) {
  type <- match.arg(type, c("equivalence", "noninferiority"))
  has_asym <- !is.null(delta_L) || !is.null(delta_U)
  has_sym  <- !is.null(margin)

  if (type == "equivalence") {
    if (has_asym) {
      if (is.null(delta_L) || is.null(delta_U)) {
        stop("For asymmetric equivalence supply both delta_L and delta_U",
             call. = FALSE)
      }
      if (has_sym) {
        stop("Supply either margin or delta_L/delta_U, not both",
             call. = FALSE)
      }
      if (!is.numeric(delta_L) || length(delta_L) != 1L || is.na(delta_L) ||
          !is.numeric(delta_U) || length(delta_U) != 1L || is.na(delta_U)) {
        stop("delta_L and delta_U must be single non-missing numerics",
             call. = FALSE)
      }
      if (!(delta_L > 0 && delta_U > 0 && delta_L < delta_U)) {
        stop("OR equivalence bounds require 0 < delta_L < delta_U",
             call. = FALSE)
      }
      return(list(delta_L = delta_L, delta_U = delta_U, margin = NA_real_,
                  log_L = log(delta_L), log_U = log(delta_U)))
    }
    if (!has_sym) {
      stop("OR equivalence requires margin > 1 (bounds [1/margin, margin]), ",
           "or 0 < delta_L < delta_U on the OR scale",
           call. = FALSE)
    }
    if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
        margin <= 1) {
      stop("For OR equivalence, margin must be a single number > 1 ",
           "(symmetric bounds [1/margin, margin])",
           call. = FALSE)
    }
    return(list(delta_L = 1 / margin, delta_U = margin, margin = margin,
                log_L = -log(margin), log_U = log(margin)))
  }

  if (has_asym) {
    stop("Non-inferiority uses margin (and higher_is_better); ",
         "delta_L/delta_U are for equivalence only", call. = FALSE)
  }
  if (!has_sym) {
    stop("OR non-inferiority requires margin > 1", call. = FALSE)
  }
  if (!is.numeric(margin) || length(margin) != 1L || is.na(margin) ||
      margin <= 1) {
    stop("For OR non-inferiority, margin must be a single number > 1",
         call. = FALSE)
  }
  list(delta_L = NA_real_, delta_U = NA_real_, margin = margin,
       log_L = NA_real_, log_U = NA_real_)
}

.tost_ci_level <- function(alpha) {
  ci_level <- 1 - 2 * alpha
  if (ci_level <= 0 || ci_level >= 1) {
    ci_level <- max(min(1 - 2 * alpha, 1 - .Machine$double.eps),
                    .Machine$double.eps)
  }
  ci_level
}

#' Internal helper returning the Welch / paired-t TOST or NI summary used by
#' \code{robustness_tost()}. Effect is always group1 − group2 (paired: within-pair
#' difference).
#'
#' @param group1,group2 Numeric vectors (equal length if `paired`).
#' @param type `"equivalence"` or `"noninferiority"`.
#' @param delta_L,delta_U Equivalence bounds (required for equivalence).
#' @param margin Positive NI margin magnitude (required for non-inferiority).
#' @param alpha Significance level for each one-sided test.
#' @param paired Logical; paired t if `TRUE`.
#' @param higher_is_better For NI only: if `TRUE` (default), group1 is
#'   non-inferior when the mean difference exceeds `-margin`; if `FALSE`
#'   (lower-is-better outcomes), non-inferior when the difference is below
#'   `+margin`.
#' @return A list with `p_eff`, one-sided p-values, estimate, CI, etc.
#' @keywords internal
#' @noRd
tost_t_test <- function(group1, group2, type,
                        delta_L, delta_U, margin,
                        alpha = 0.05, paired = FALSE,
                        higher_is_better = TRUE) {

  if (length(group1) < 2L || length(group2) < 2L) return(NULL)
  if (paired && length(group1) != length(group2)) return(NULL)

  # (1 - 2*alpha) CI matches the TOST decision at level alpha
  ci_level <- .tost_ci_level(alpha)

  tt_ci <- tryCatch(
    stats::t.test(group1, group2, paired = paired, conf.level = ci_level),
    error = function(e) NULL
  )
  if (is.null(tt_ci)) return(NULL)

  estimate <- if (paired) {
    unname(tt_ci$estimate)
  } else {
    unname(tt_ci$estimate[1] - tt_ci$estimate[2])
  }
  conf.int <- tt_ci$conf.int
  statistic <- unname(tt_ci$statistic)
  df <- unname(tt_ci$parameter)

  if (type == "equivalence") {
    t_lo <- tryCatch(
      stats::t.test(group1, group2, paired = paired,
                    alternative = "greater", mu = delta_L),
      error = function(e) NULL
    )
    t_hi <- tryCatch(
      stats::t.test(group1, group2, paired = paired,
                    alternative = "less", mu = delta_U),
      error = function(e) NULL
    )
    if (is.null(t_lo) || is.null(t_hi)) return(NULL)
    p_lower <- unname(t_lo$p.value)
    p_upper <- unname(t_hi$p.value)
    if (is.na(p_lower) || is.na(p_upper)) return(NULL)
    p_eff <- max(p_lower, p_upper)
    concluded <- p_eff < alpha
    # CI rule (same decision when CI exists)
    ci_inside <- isTRUE(conf.int[1] > delta_L && conf.int[2] < delta_U)

    return(list(
      p_eff = p_eff,
      p_lower = p_lower,
      p_upper = p_upper,
      p_ni = NA_real_,
      estimate = estimate,
      conf.int = conf.int,
      conf.level = ci_level,
      statistic = statistic,
      df = df,
      concluded = concluded,
      ci_inside = ci_inside,
      method = if (paired) "Paired t TOST" else "Welch t TOST"
    ))
  }

  # Non-inferiority
  if (isTRUE(higher_is_better)) {
    # H0: Δ ≤ -margin  vs  H1: Δ > -margin
    t_ni <- tryCatch(
      stats::t.test(group1, group2, paired = paired,
                    alternative = "greater", mu = -margin),
      error = function(e) NULL
    )
    ni_bound <- -margin
    ni_alt <- "greater"
  } else {
    # H0: Δ ≥ +margin  vs  H1: Δ < +margin  (lower-is-better)
    t_ni <- tryCatch(
      stats::t.test(group1, group2, paired = paired,
                    alternative = "less", mu = margin),
      error = function(e) NULL
    )
    ni_bound <- margin
    ni_alt <- "less"
  }
  if (is.null(t_ni) || is.na(t_ni$p.value)) return(NULL)
  p_ni <- unname(t_ni$p.value)
  p_eff <- p_ni
  concluded <- p_eff < alpha

  list(
    p_eff = p_eff,
    p_lower = NA_real_,
    p_upper = NA_real_,
    p_ni = p_ni,
    estimate = estimate,
    conf.int = conf.int,
    conf.level = ci_level,
    statistic = statistic,
    df = df,
    concluded = concluded,
    ci_inside = NA,
    ni_bound = ni_bound,
    ni_alternative = ni_alt,
    method = if (paired) "Paired t non-inferiority" else "Welch t non-inferiority"
  )
}

#' Internal Wald TOST / NI on risk difference RD = p1 − p2.
#' @keywords internal
#' @noRd
tost_prop_test <- function(group1, group2, type,
                           delta_L, delta_U, margin,
                           alpha = 0.05,
                           higher_is_better = TRUE) {
  n1 <- length(group1)
  n2 <- length(group2)
  if (n1 < 2L || n2 < 2L) return(NULL)

  p1 <- mean(group1)
  p2 <- mean(group2)
  rd <- p1 - p2
  se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  if (!is.finite(se) || se <= 0) return(NULL)

  ci_level <- .tost_ci_level(alpha)
  z_ci <- stats::qnorm(1 - alpha)
  conf.int <- c(rd - z_ci * se, rd + z_ci * se)
  statistic <- rd / se

  if (type == "equivalence") {
    # H0: RD <= delta_L vs H1: RD > delta_L
    p_lower <- stats::pnorm((rd - delta_L) / se, lower.tail = FALSE)
    # H0: RD >= delta_U vs H1: RD < delta_U
    p_upper <- stats::pnorm((rd - delta_U) / se, lower.tail = TRUE)
    if (is.na(p_lower) || is.na(p_upper)) return(NULL)
    p_eff <- max(p_lower, p_upper)
    concluded <- p_eff < alpha
    ci_inside <- isTRUE(conf.int[1] > delta_L && conf.int[2] < delta_U)

    return(list(
      p_eff = p_eff,
      p_lower = p_lower,
      p_upper = p_upper,
      p_ni = NA_real_,
      estimate = rd,
      conf.int = conf.int,
      conf.level = ci_level,
      statistic = statistic,
      df = NA_real_,
      se = se,
      concluded = concluded,
      ci_inside = ci_inside,
      method = "Wald RD TOST"
    ))
  }

  if (isTRUE(higher_is_better)) {
    # H0: RD <= -margin vs H1: RD > -margin
    ni_bound <- -margin
    p_ni <- stats::pnorm((rd - ni_bound) / se, lower.tail = FALSE)
    ni_alt <- "greater"
  } else {
    # H0: RD >= +margin vs H1: RD < +margin
    ni_bound <- margin
    p_ni <- stats::pnorm((rd - ni_bound) / se, lower.tail = TRUE)
    ni_alt <- "less"
  }
  if (is.na(p_ni)) return(NULL)

  list(
    p_eff = p_ni,
    p_lower = NA_real_,
    p_upper = NA_real_,
    p_ni = p_ni,
    estimate = rd,
    conf.int = conf.int,
    conf.level = ci_level,
    statistic = statistic,
    df = NA_real_,
    se = se,
    concluded = p_ni < alpha,
    ci_inside = NA,
    ni_bound = ni_bound,
    ni_alternative = ni_alt,
    method = "Wald RD non-inferiority"
  )
}

#' Internal Wald TOST / NI on log(OR); margins on OR scale.
#' Uses Haldane–Anscombe 0.5 continuity when any 2×2 cell is zero.
#' @keywords internal
#' @noRd
tost_or_test <- function(group1, group2, type,
                         delta_L, delta_U, margin,
                         alpha = 0.05,
                         higher_is_better = TRUE,
                         log_L = NA_real_, log_U = NA_real_) {
  n1 <- length(group1)
  n2 <- length(group2)
  if (n1 < 2L || n2 < 2L) return(NULL)

  a <- sum(group1)
  b <- n1 - a
  c <- sum(group2)
  d <- n2 - c
  # Haldane–Anscombe when any cell is zero (keeps log(OR) / SE finite)
  if (min(a, b, c, d) == 0) {
    a <- a + 0.5
    b <- b + 0.5
    c <- c + 0.5
    d <- d + 0.5
  }
  or_hat <- (a * d) / (b * c)
  log_or <- log(or_hat)
  se <- sqrt(1 / a + 1 / b + 1 / c + 1 / d)
  if (!is.finite(se) || se <= 0 || !is.finite(log_or)) return(NULL)

  ci_level <- .tost_ci_level(alpha)
  z_ci <- stats::qnorm(1 - alpha)
  log_ci <- c(log_or - z_ci * se, log_or + z_ci * se)
  conf.int <- exp(log_ci) # OR-scale CI for reporting / CI rule
  statistic <- log_or / se

  if (type == "equivalence") {
    if (is.na(log_L) || is.na(log_U)) {
      log_L <- log(delta_L)
      log_U <- log(delta_U)
    }
    p_lower <- stats::pnorm((log_or - log_L) / se, lower.tail = FALSE)
    p_upper <- stats::pnorm((log_or - log_U) / se, lower.tail = TRUE)
    if (is.na(p_lower) || is.na(p_upper)) return(NULL)
    p_eff <- max(p_lower, p_upper)
    concluded <- p_eff < alpha
    ci_inside <- isTRUE(conf.int[1] > delta_L && conf.int[2] < delta_U)

    return(list(
      p_eff = p_eff,
      p_lower = p_lower,
      p_upper = p_upper,
      p_ni = NA_real_,
      estimate = or_hat,
      estimate_log = log_or,
      conf.int = conf.int,
      conf.level = ci_level,
      statistic = statistic,
      df = NA_real_,
      se = se,
      concluded = concluded,
      ci_inside = ci_inside,
      method = "Wald log(OR) TOST"
    ))
  }

  # NI on log(OR): higher_is_better => OR > 1/margin; else OR < margin
  if (isTRUE(higher_is_better)) {
    ni_bound_or <- 1 / margin
    ni_bound_log <- -log(margin)
    p_ni <- stats::pnorm((log_or - ni_bound_log) / se, lower.tail = FALSE)
    ni_alt <- "greater"
  } else {
    ni_bound_or <- margin
    ni_bound_log <- log(margin)
    p_ni <- stats::pnorm((log_or - ni_bound_log) / se, lower.tail = TRUE)
    ni_alt <- "less"
  }
  if (is.na(p_ni)) return(NULL)

  list(
    p_eff = p_ni,
    p_lower = NA_real_,
    p_upper = NA_real_,
    p_ni = p_ni,
    estimate = or_hat,
    estimate_log = log_or,
    conf.int = conf.int,
    conf.level = ci_level,
    statistic = statistic,
    df = NA_real_,
    se = se,
    concluded = p_ni < alpha,
    ci_inside = NA,
    ni_bound = ni_bound_or,
    ni_alternative = ni_alt,
    method = "Wald log(OR) non-inferiority"
  )
}

.tost_dispatch <- function(endpoint, group1, group2, type,
                           bounds, alpha, paired, higher_is_better) {
  if (endpoint == "mean") {
    return(tost_t_test(
      group1, group2, type = type,
      delta_L = bounds$delta_L, delta_U = bounds$delta_U,
      margin = bounds$margin, alpha = alpha, paired = paired,
      higher_is_better = higher_is_better
    ))
  }
  if (endpoint == "prop") {
    return(tost_prop_test(
      group1, group2, type = type,
      delta_L = bounds$delta_L, delta_U = bounds$delta_U,
      margin = bounds$margin, alpha = alpha,
      higher_is_better = higher_is_better
    ))
  }
  tost_or_test(
    group1, group2, type = type,
    delta_L = bounds$delta_L, delta_U = bounds$delta_U,
    margin = bounds$margin, alpha = alpha,
    higher_is_better = higher_is_better,
    log_L = bounds$log_L, log_U = bounds$log_U
  )
}

.tost_effect_label <- function(endpoint) {
  switch(endpoint,
         mean = "mean difference",
         prop = "risk difference",
         or = "odds ratio",
         endpoint)
}

#' Robustness analysis for TOST equivalence or non-inferiority
#'
#' Quantifies how stable an **equivalence** (two one-sided tests, TOST) or
#' **non-inferiority** conclusion is under jackknife leave-one-out, greedy
#' worst-case removal, and bootstrap resampling — the same composite-score
#' machinery as [robustness_lm()] / [robustness_surv()].
#'
#' The decision rule differs from ordinary superiority testing:
#' \describe{
#'   \item{Equivalence}{Conclude equivalent if both one-sided tests reject at
#'     `alpha` (Schuirmann), equivalently if the `(1 - 2 * alpha)` CI for the
#'     effect lies inside `[delta_L, delta_U]` (or the symmetric margin
#'     bounds).}
#'   \item{Non-inferiority}{Conclude NI if the one-sided test vs the margin
#'     rejects at `alpha`. With `higher_is_better = TRUE` (default), NI means
#'     the effect exceeds the lower NI bound (group1 is not worse than group2
#'     by more than `margin`). With `higher_is_better = FALSE` (lower-is-better
#'     outcomes such as adverse-event rates), NI means the effect is below
#'     the upper NI bound.}
#' }
#'
#' Endpoints (`endpoint`):
#' \describe{
#'   \item{`"mean"`}{Welch or paired **t** on mean difference (group1 − group2).
#'     Symmetric bounds `[-margin, margin]`.}
#'   \item{`"prop"`}{Wald **z** TOST/NI on risk difference RD = p1 − p2 for
#'     individual-level binary (0/1 or logical) outcomes. Symmetric bounds
#'     `[-margin, margin]` on the probability scale. Not a Farrington–Manning
#'     score test.}
#'   \item{`"or"`}{Wald **z** TOST/NI on log(OR). User margins are on the **OR
#'     scale**: symmetric equivalence uses `[1/margin, margin]` with
#'     `margin > 1`; NI with `higher_is_better = TRUE` tests OR > `1/margin`,
#'     else OR < `margin`. Zero cells use Haldane–Anscombe 0.5 continuity.}
#' }
#'
#' For the robustness engine, an **effective p-value** preserves
#' `p < alpha` semantics: for equivalence,
#' `p_eff = max(p_lower, p_upper)`; for non-inferiority, `p_eff` is the
#' one-sided NI p-value. Conclusion flips are therefore flips of the TOST/NI
#' decision, not of a two-sided superiority test.
#'
#' @param group1,group2 For `endpoint = "mean"`, numeric continuous vectors.
#'   For `"prop"` / `"or"`, binary 0/1 or logical vectors (same convention as
#'   [robustness_analysis()] proportion tests). Missing values are not
#'   allowed. Each group (or the number of pairs for paired mean) must have
#'   at least 4 observations.
#' @param type `"equivalence"` (TOST) or `"noninferiority"`.
#' @param endpoint `"mean"` (default), `"prop"` (risk difference), or `"or"`
#'   (odds ratio). Alias `test` is accepted for the same argument.
#' @param margin Positive margin. For `"mean"` / `"prop"`: symmetric
#'   equivalence `[-margin, margin]` or NI magnitude. For `"or"`: must be
#'   `> 1`; equivalence bounds are `[1/margin, margin]`. Ignored if
#'   `delta_L` / `delta_U` are supplied (equivalence only).
#' @param delta_L,delta_U Asymmetric equivalence bounds with `delta_L < delta_U`
#'   (OR scale for `endpoint = "or"`, with both bounds positive). Mutually
#'   exclusive with `margin`. Not used for non-inferiority.
#' @param alpha One-sided significance level for each TOST/NI test
#'   (default 0.05). Equivalence uses level `alpha` in each direction
#'   (overall TOST level `alpha`).
#' @param paired If `TRUE`, use a paired t-test on within-pair differences
#'   (`endpoint = "mean"` only; rejected for `"prop"` / `"or"`).
#' @param higher_is_better For non-inferiority only; see Details.
#' @param n_boot,max_removal_pct,weights,seed As in [robustness_analysis()].
#' @param test Optional alias for `endpoint` (same choices). Prefer
#'   `endpoint` in new code.
#'
#' @return Object of class `"robustness_tost"` (also `"robustness_model"`),
#'   with the usual engine fields plus TOST/NI metadata (`tost_type`,
#'   `endpoint`, margins, one-sided p-values, `(1 - 2 * alpha)` CI).
#'
#' @section Limitations:
#' Binary methods use **Wald** normal approximations (RD or log OR), which can
#' be anticonservative with sparse tables; Farrington–Manning score and exact
#' unconditional procedures are not implemented. Rank-based TOST is still
#' deferred. Composite score bands are **not** separately calibrated for
#' equivalence/NI conclusions (any endpoint).
#'
#' @examples
#' set.seed(1)
#' g1 <- rnorm(40, 0, 1)
#' g2 <- rnorm(40, 0.05, 1)
#' # Symmetric equivalence margin of 0.5 (mean difference)
#' res <- robustness_tost(g1, g2, type = "equivalence", margin = 0.5,
#'                        n_boot = 100, seed = 1)
#' print(res)
#'
#' # Non-inferiority (higher is better), margin 0.3
#' res_ni <- robustness_tost(g1, g2, type = "noninferiority", margin = 0.3,
#'                           n_boot = 100, seed = 1)
#'
#' # Binary risk-difference equivalence
#' set.seed(2)
#' b1 <- rbinom(50, 1, 0.45)
#' b2 <- rbinom(50, 1, 0.48)
#' res_rd <- robustness_tost(b1, b2, type = "equivalence", endpoint = "prop",
#'                           margin = 0.2, n_boot = 50, seed = 2)
#'
#' # Odds-ratio NI (higher success is better), OR margin 1.25
#' res_or <- robustness_tost(b1, b2, type = "noninferiority", endpoint = "or",
#'                           margin = 1.25, n_boot = 50, seed = 2)
#'
#' @seealso [robustness_analysis()], [robustness_lm()]
#' @export
robustness_tost <- function(group1, group2,
                            type = c("equivalence", "noninferiority"),
                            endpoint = c("mean", "prop", "or"),
                            margin = NULL,
                            delta_L = NULL,
                            delta_U = NULL,
                            alpha = 0.05,
                            paired = FALSE,
                            higher_is_better = TRUE,
                            n_boot = 1000,
                            max_removal_pct = 0.30,
                            weights = c(jackknife = 0.4, fragility = 0.4,
                                        bootstrap = 0.2),
                            seed = 123,
                            test = NULL) {

  type <- match.arg(type)
  endpoint_choices <- c("mean", "prop", "or")
  if (!is.null(test)) {
    test <- match.arg(test, endpoint_choices)
    if (!missing(endpoint)) {
      endpoint <- match.arg(endpoint, endpoint_choices)
      if (!identical(endpoint, test)) {
        stop("endpoint and test must agree when both are supplied",
             call. = FALSE)
      }
    }
    endpoint <- test
  } else {
    endpoint <- match.arg(endpoint, endpoint_choices)
  }

  if (!is.logical(paired) || length(paired) != 1L || is.na(paired)) {
    stop("paired must be a single non-missing logical", call. = FALSE)
  }
  if (!is.logical(higher_is_better) || length(higher_is_better) != 1L ||
      is.na(higher_is_better)) {
    stop("higher_is_better must be a single non-missing logical",
         call. = FALSE)
  }

  if (endpoint %in% c("prop", "or")) {
    if (isTRUE(paired)) {
      stop("paired = TRUE is only supported for endpoint = \"mean\"",
           call. = FALSE)
    }
    group1 <- coerce_binary(group1, "group1")
    group2 <- coerce_binary(group2, "group2")
    bounds <- if (endpoint == "or") {
      .resolve_tost_or_margins(type, margin, delta_L, delta_U)
    } else {
      .resolve_tost_margins(type, margin, delta_L, delta_U)
    }
  } else {
    stopifnot(is.numeric(group1), is.numeric(group2))
    if (anyNA(group1) || anyNA(group2)) {
      stop("group1 and group2 must not contain missing values", call. = FALSE)
    }
    bounds <- .resolve_tost_margins(type, margin, delta_L, delta_U)
  }

  if (paired) {
    if (length(group1) != length(group2)) {
      stop("Paired tests require equal length vectors", call. = FALSE)
    }
    if (length(group1) < 4L) {
      stop("Paired tests require at least 4 pairs", call. = FALSE)
    }
  } else {
    if (length(group1) < 4L || length(group2) < 4L) {
      stop("Each group must have at least 4 observations", call. = FALSE)
    }
  }

  # Build row-oriented data for robustness_engine (case deletion on rows)
  if (paired) {
    data <- data.frame(g1 = group1, g2 = group2)
    min_n <- 4L
    fit_fun <- function(d) {
      if (nrow(d) < 2L) return(NULL)
      res <- .tost_dispatch(endpoint, d$g1, d$g2, type, bounds, alpha,
                            paired = TRUE, higher_is_better)
      if (is.null(res)) return(NULL)
      list(p = res$p_eff, estimate = res$estimate)
    }
  } else {
    data <- data.frame(
      value = c(group1, group2),
      arm = factor(rep(c("g1", "g2"),
                       c(length(group1), length(group2))),
                   levels = c("g1", "g2"))
    )
    min_n <- 8L
    fit_fun <- function(d) {
      g1 <- d$value[d$arm == "g1"]
      g2 <- d$value[d$arm == "g2"]
      if (length(g1) < 2L || length(g2) < 2L) return(NULL)
      res <- .tost_dispatch(endpoint, g1, g2, type, bounds, alpha,
                            paired = FALSE, higher_is_better)
      if (is.null(res)) return(NULL)
      list(p = res$p_eff, estimate = res$estimate)
    }
  }

  original_detail <- .tost_dispatch(
    endpoint, group1, group2, type, bounds, alpha, paired, higher_is_better
  )
  if (is.null(original_detail)) {
    stop("TOST/NI test could not be computed on the full dataset",
         call. = FALSE)
  }

  out <- robustness_engine(data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed, min_n = min_n)

  effect_lab <- .tost_effect_label(endpoint)
  out$tost_type <- type
  out$endpoint <- endpoint
  out$paired <- paired
  out$higher_is_better <- higher_is_better
  out$margin <- bounds$margin
  out$delta_L <- bounds$delta_L
  out$delta_U <- bounds$delta_U
  out$p_lower <- original_detail$p_lower
  out$p_upper <- original_detail$p_upper
  out$p_ni <- original_detail$p_ni
  out$original_ci <- original_detail$conf.int
  out$ci_level <- original_detail$conf.level
  out$ci_inside_bounds <- original_detail$ci_inside
  out$original_statistic <- original_detail$statistic
  out$df <- original_detail$df
  out$method <- original_detail$method
  out$term <- if (type == "equivalence") {
    sprintf("TOST equivalence (%s)", effect_lab)
  } else {
    sprintf("Non-inferiority (%s)", effect_lab)
  }
  out$model <- out$method
  out$type <- out$method
  class(out) <- c("robustness_tost", "robustness_model", "list")
  out
}

#' @rdname robustness_tost
#' @param x A `robustness_tost` object
#' @param ... Unused
#' @export
print.robustness_tost <- function(x, ...) {
  m <- x$metrics
  endpoint <- if (!is.null(x$endpoint)) x$endpoint else "mean"
  effect_lab <- .tost_effect_label(endpoint)
  cat("================================================\n")
  cat("   TOST / NON-INFERIORITY ROBUSTNESS ANALYSIS\n")
  cat("================================================\n\n")
  cat(sprintf("METHOD: %s\n", x$method))
  cat(sprintf("ENDPOINT: %s | DECISION: %s | alpha = %.2f (one-sided) | n = %d\n",
              endpoint, x$tost_type, x$alpha, x$n))
  if (x$tost_type == "equivalence") {
    cat(sprintf("BOUNDS: [%.4g, %.4g]\n", x$delta_L, x$delta_U))
  } else {
    cat(sprintf("MARGIN: %.4g | higher_is_better = %s\n",
                x$margin, x$higher_is_better))
  }
  if (identical(endpoint, "or")) {
    cat(sprintf("\nORIGINAL: odds ratio (g1 vs g2) = %.4f\n",
                x$original_estimate))
  } else {
    cat(sprintf("\nORIGINAL: %s (g1 - g2) = %.4f\n",
                effect_lab, x$original_estimate))
  }
  if (all(is.finite(x$original_ci))) {
    cat(sprintf("          (1 - 2*alpha) = %.0f%% CI [%.4f, %.4f]\n",
                100 * x$ci_level, x$original_ci[1], x$original_ci[2]))
  }
  if (x$tost_type == "equivalence") {
    cat(sprintf("          p_lower = %.4f, p_upper = %.4f, p_eff = max = %.4f\n",
                x$p_lower, x$p_upper, x$original_p))
    cat(sprintf("          conclusion: %s\n",
                ifelse(x$original_significant, "equivalent", "not equivalent")))
  } else {
    cat(sprintf("          p_NI = %.4f\n", x$p_ni))
    cat(sprintf("          conclusion: %s\n",
                ifelse(x$original_significant, "non-inferior",
                       "not non-inferior")))
  }

  cat(sprintf("\nOVERALL ROBUSTNESS: %.1f/100 (%s)\n\n",
              m$overall_robustness, x$interpretation_label))
  cat("COMPONENTS:\n")
  cat(sprintf("  Jackknife stability:       %5.1f%%  (influential: %d)\n",
              m$jackknife_conclusion_stability, m$jackknife_n_influential))
  cat(sprintf("  Worst-case fragility:      k = %s (%.1f%% of sample)%s\n",
              ifelse(m$worstcase_fragility_k > x$max_k,
                     paste0("> ", x$max_k), m$worstcase_fragility_k),
              min(m$worstcase_fragility_pct, 100 * x$max_k / x$n),
              ifelse(is.na(m$p_at_fragility), "",
                     sprintf("  [p_eff at flip: %.4f]", m$p_at_fragility))))
  cat(sprintf("  Bootstrap reproducibility: %5.1f%%  (mean p_eff = %.4f)\n",
              m$bootstrap_reproducibility, m$bootstrap_p_mean))
  cat(sprintf("  Jackknife estimate range:  [%.4f, %.4f]\n\n",
              m$estimate_range_jackknife_lo, m$estimate_range_jackknife_hi))
  if (length(x$removed_rows) > 0 && m$worstcase_fragility_k <= x$max_k) {
    cat("Rows removed by worst-case analysis (in order):\n  ")
    cat(x$removed_rows[seq_len(m$worstcase_fragility_k)], sep = ", ")
    cat("\n  -> review these units for data quality / clinical plausibility\n")
  }
  invisible(x)
}
