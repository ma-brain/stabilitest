# ==============================================================================
# Robustness Analysis Framework for Two-Sample Comparisons — v2 (July 2026)
#
# Revision of the January 2026 version following methodological review
# (see manuscript/methodological_review.md). Key changes:
#   * NEW: worst-case (greedy, AMIP-style) removal analysis; its fragility
#     index replaces the grand-mean fragility in the composite score
#   * Grand-mean removal retained as descriptive "extreme-value removal"
#   * Bootstrap metric relabeled "reproducibility probability" (Goodman 1992);
#     percentile interval of bootstrap p-values (not a "CI for the p-value")
#   * Composite score rescaled (fragility component now spans 0-100) and
#     weights exposed as an argument (default 0.4 / 0.4 / 0.2)
#   * generate_interpretation() rewritten (v1 referenced out-of-scope objects
#     and compared a count against a p-value; errored when interpret = TRUE)
#   * Mean difference reported as group1 - group2 (v1 sign was flipped)
#   * Influential observations flagged by significance flip OR |delta p|
# ==============================================================================

# Coerce logical/numeric to 0/1 for two-group proportion tests; reject otherwise.
coerce_binary <- function(x, name) {
  if (is.logical(x)) {
    if (anyNA(x)) stop(sprintf("%s must not contain missing values", name))
    return(as.numeric(x))
  }
  if (!is.numeric(x)) {
    stop(sprintf("%s must be numeric 0/1 or logical for proportion tests", name))
  }
  if (anyNA(x)) stop(sprintf("%s must not contain missing values", name))
  # Allow integerish 0/1 only (reject 0.5, 2, etc.)
  if (!all(x %in% c(0, 1))) {
    stop(sprintf("%s must be binary (0/1 or logical) for proportion tests", name))
  }
  as.numeric(x)
}

# 2x2 contingency table: rows = outcome (success, failure), cols = group
prop_table_2x2 <- function(g1, g2) {
  matrix(
    c(sum(g1), length(g1) - sum(g1),
      sum(g2), length(g2) - sum(g2)),
    nrow = 2,
    dimnames = list(outcome = c("success", "failure"),
                    group = c("group1", "group2"))
  )
}

# Hodges–Lehmann two-sample location shift: median of all pairwise diffs x_i - y_j
hodges_lehmann_unpaired <- function(x, y) {
  as.numeric(stats::median(outer(x, y, `-`)))
}

# Hodges–Lehmann paired / one-sample: median of Walsh averages of within-pair diffs
hodges_lehmann_paired <- function(x, y) {
  d <- x - y
  n <- length(d)
  idx <- which(lower.tri(matrix(0, n, n), diag = TRUE), arr.ind = TRUE)
  walsh <- (d[idx[, 1L]] + d[idx[, 2L]]) / 2
  as.numeric(stats::median(walsh))
}

# Brunner–Munzel test (Neubert & Brunner 2007 corrected formulas; base R).
# Unpaired only. Estimate is P(X < Y) + 0.5 P(X = Y) with X = group1, Y = group2.
brunner_munzel_test <- function(x, y, alpha = 0.05) {
  n1 <- length(x)
  n2 <- length(y)
  r1_place <- rank(x, ties.method = "average")
  r2_place <- rank(y, ties.method = "average")
  r <- rank(c(x, y), ties.method = "average")
  r1 <- r[seq_len(n1)]
  r2 <- r[n1 + seq_len(n2)]
  m1 <- mean(r1)
  m2 <- mean(r2)
  pst <- (m2 - (n2 + 1) / 2) / n1
  v1 <- sum((r1 - r1_place - m1 + (n1 + 1) / 2)^2) / (n1 - 1)
  v2 <- sum((r2 - r2_place - m2 + (n2 + 1) / 2)^2) / (n2 - 1)
  denom <- sqrt(n1 * v1 + n2 * v2)
  if (!is.finite(denom) || denom < .Machine$double.eps) {
    statistic <- NA_real_
    dfbm <- NA_real_
    p.value <- NA_real_
    conf.int <- c(NA_real_, NA_real_)
  } else {
    statistic <- n1 * n2 * (m2 - m1) / (n1 + n2) / denom
    dfbm <- ((n1 * v1 + n2 * v2)^2) /
      (((n1 * v1)^2) / (n1 - 1) + ((n2 * v2)^2) / (n2 - 1))
    p.value <- 2 * stats::pt(-abs(statistic), df = dfbm)
    se <- sqrt(v1 / (n1 * n2^2) + v2 / (n2 * n1^2))
    conf.int <- pst + c(-1, 1) * stats::qt(1 - alpha / 2, df = dfbm) * se
  }
  attr(conf.int, "conf.level") <- 1 - alpha
  estimate <- c("P(X<Y)+.5*P(X=Y)" = pst)
  structure(
    list(
      statistic = c("Brunner-Munzel" = unname(statistic)),
      parameter = c(df = unname(dfbm)),
      p.value = unname(p.value),
      conf.int = conf.int,
      estimate = estimate,
      null.value = c("P(X<Y)+.5*P(X=Y)" = 0.5),
      alternative = "two.sided",
      method = "Brunner-Munzel Test",
      data.name = "x and y"
    ),
    class = "htest"
  )
}

#' Comprehensive robustness analysis for two-sample comparisons
#'
#' Supports continuous location tests and two-group binary proportion tests
#' (Fisher exact, chi-square, and `stats::prop.test`). Proportion tests take
#' individual-level binary outcomes (0/1 or logical) so jackknife, worst-case
#' removal, and bootstrap operate on the same observation units as the
#' continuous API. Barnard's exact test is not implemented in this version.
#'
#' Rank-based options: `"wilcoxon"` (Mann–Whitney / Wilcoxon rank-sum) assumes
#' exchangeable distributions under the null (equal shapes/variances for a pure
#' location interpretation); `"brunner_munzel"` is the unpaired nonparametric
#' Behrens–Fisher alternative that allows unequal variances. Prefer
#' `"brunner_munzel"` when heteroscedasticity is plausible; prefer `"wilcoxon"`
#' for the classical Mann–Whitney setting. Brunner–Munzel is unpaired only.
#' For `"wilcoxon"`, `"wilcoxon.paired"`, and `"brunner_munzel"`, the reported
#' effect size is the Hodges–Lehmann location shift (median of all pairwise
#' differences for unpaired tests; median of Walsh averages of within-pair
#' differences for paired Wilcoxon), stored in `original_mean_diff`.
#'
#' @param group1 Numeric vector of observations in group 1; for proportion
#'   tests (`"fisher"`, `"chisq"`, `"prop"`), binary 0/1 or logical
#' @param group2 Numeric vector of observations in group 2; same coding rules
#'   as `group1`
#' @param test_type `"t.test"` (Welch, default), `"paired.t.test"`,
#'   `"wilcoxon"`, `"wilcoxon.paired"`, `"brunner_munzel"` (unpaired
#'   Brunner–Munzel), or proportion tests `"fisher"` (Fisher's exact),
#'   `"chisq"` (`stats::chisq.test` on the 2x2 table), `"prop"`
#'   (`stats::prop.test`)
#' @param alpha Significance level (default 0.05)
#' @param n_boot Bootstrap iterations (default 1000); must be a single
#'   positive integer
#' @param max_removal_pct Maximum proportion of observations removed in the
#'   removal analyses (default 0.30); must be in `(0, 1]`
#' @param influential_threshold |delta p| threshold for flagging influential
#'   observations in the jackknife (default 0.05); observations whose removal
#'   flips the significance conclusion are always flagged
#' @param weights Named numeric vector of composite-score weights,
#'   c(jackknife, fragility, bootstrap); must sum to 1. The fragility weight
#'   applies to the WORST-CASE fragility component. Pre-specify in the SAP.
#' @param seed RNG seed for the bootstrap (default 123)
#' @param interpret If TRUE, generate a text interpretation (default FALSE)
#' @param correct Continuity correction for `"chisq"` and `"prop"` (default
#'   `TRUE`, matching `stats::chisq.test` / `stats::prop.test` defaults).
#'   Ignored for other `test_type` values.
#'
#' @return An object of class `"robustness_analysis"` (a named list) with:
#' \describe{
#'   \item{original_p, original_significant, original_statistic,
#'     original_mean_diff, original_ci}{Full-sample test results (effect
#'     field is a mean difference, Hodges–Lehmann shift, or proportion
#'     difference depending on `test_type`).}
#'   \item{robustness_metrics}{Component scores (jackknife conclusion
#'     stability, worst-case fragility, bootstrap reproducibility, overall
#'     composite) and related diagnostics. Alias: `metrics` (same tibble).}
#'   \item{robustness_interpretation}{Label: `"Robust"`,
#'     `"Moderately Robust"`, or `"Fragile"`. Alias: `interpretation_label`.}
#'   \item{original_mean_diff}{Effect summary (mean / HL / proportion
#'     difference). Alias: `original_estimate`.}
#'   \item{jackknife, worstcase, extreme, bootstrap}{Component analysis
#'     results (nested lists with tibbles plus summary scalars).}
#'   \item{sample_info, weights, alpha, max_removal_pct, max_k, n}{Analysis
#'     metadata (`n` is total sample size / pairs as appropriate).}
#'   \item{interpretation}{Optional text blocks when `interpret = TRUE`;
#'     otherwise `NULL`.}
#' }
#'
#' @examples
#' # Continuous two-sample (modest bootstrap for examples)
#' res <- robustness_analysis(pain_treatment, pain_placebo,
#'                            test_type = "t.test", n_boot = 50, seed = 1)
#' print(res)
#'
#' # Binary proportion (Fisher exact)
#' set.seed(2)
#' g1 <- rbinom(20, 1, 0.3)
#' g2 <- rbinom(20, 1, 0.6)
#' res_prop <- robustness_analysis(g1, g2, test_type = "fisher",
#'                                 n_boot = 30, seed = 2)
#' @export
robustness_analysis <- function(group1, group2,
                                test_type = c("t.test", "paired.t.test",
                                              "wilcoxon", "wilcoxon.paired",
                                              "brunner_munzel",
                                              "fisher", "chisq", "prop"),
                                alpha = 0.05, n_boot = 1000,
                                max_removal_pct = 0.30,
                                influential_threshold = 0.05,
                                weights = c(jackknife = 0.4, fragility = 0.4,
                                            bootstrap = 0.2),
                                seed = 123, interpret = FALSE,
                                correct = TRUE) {

  test_type <- match.arg(test_type)
  paired <- test_type %in% c("paired.t.test", "wilcoxon.paired")
  is_prop <- test_type %in% c("fisher", "chisq", "prop")
  is_rank <- test_type %in% c("wilcoxon", "wilcoxon.paired", "brunner_munzel")

  # --- validation -------------------------------------------------------------
  if (is_prop) {
    if (!is.logical(correct) || length(correct) != 1L || is.na(correct)) {
      stop("correct must be a single non-missing logical")
    }
    group1 <- coerce_binary(group1, "group1")
    group2 <- coerce_binary(group2, "group2")
  } else {
    stopifnot(is.numeric(group1), is.numeric(group2))
    if (anyNA(group1) || anyNA(group2)) {
      stop("group1 and group2 must not contain missing values", call. = FALSE)
    }
  }
  if (paired) {
    if (length(group1) != length(group2)) stop("Paired tests require equal length vectors")
    if (length(group1) < 4) stop("Paired tests require at least 4 pairs")
  } else {
    if (length(group1) < 4 || length(group2) < 4) stop("Each group must have at least 4 observations")
  }
  validate_alpha_weights(alpha, weights)
  validate_n_boot_max_removal(n_boot, max_removal_pct)

  # --- test wrapper -----------------------------------------------------------
  perform_test <- function(g1, g2) {
    if (is_prop) {
      tab <- prop_table_2x2(g1, g2)
      result <- switch(test_type,
        "fisher" = stats::fisher.test(tab),
        "chisq"  = suppressWarnings(stats::chisq.test(tab, correct = correct)),
        "prop"   = suppressWarnings(
          stats::prop.test(c(sum(g1), sum(g2)),
                           c(length(g1), length(g2)),
                           correct = correct)
        )
      )
      # Effect size is always the difference in success proportions (g1 - g2)
      mean_diff <- mean(g1) - mean(g2)
      statistic <- if (!is.null(result$statistic)) unname(result$statistic) else NA_real_
      conf.int <- if (!is.null(result$conf.int)) result$conf.int else c(NA_real_, NA_real_)
      return(list(p.value = result$p.value, statistic = statistic,
                  mean_diff = mean_diff, conf.int = conf.int,
                  stochastic_superiority = NA_real_))
    }
    result <- switch(test_type,
      "t.test"          = t.test(g1, g2),
      "paired.t.test"   = t.test(g1, g2, paired = TRUE),
      "wilcoxon"        = {
        # conf.int can fail on tiny/degenerate jackknife or removal subsets
        tryCatch(
          wilcox.test(g1, g2, exact = FALSE, conf.int = TRUE),
          error = function(e) wilcox.test(g1, g2, exact = FALSE)
        )
      },
      "wilcoxon.paired" = {
        tryCatch(
          wilcox.test(g1, g2, paired = TRUE, exact = FALSE, conf.int = TRUE),
          error = function(e) wilcox.test(g1, g2, paired = TRUE, exact = FALSE)
        )
      },
      "brunner_munzel"  = brunner_munzel_test(g1, g2, alpha = alpha)
    )
    # Effect sizes: mean difference for t-tests; Hodges–Lehmann for rank tests
    mean_diff <- switch(test_type,
      "t.test"          = unname(result$estimate[1] - result$estimate[2]),
      "paired.t.test"   = unname(result$estimate),
      "wilcoxon"        = hodges_lehmann_unpaired(g1, g2),
      "wilcoxon.paired" = hodges_lehmann_paired(g1, g2),
      "brunner_munzel"  = hodges_lehmann_unpaired(g1, g2)
    )
    # Wilcoxon CI is for the HL shift; BM CI is for stochastic superiority —
    # only attach a CI to original_mean_diff when it matches that parameter
    conf.int <- switch(test_type,
      "brunner_munzel" = c(NA_real_, NA_real_),
      if (!is.null(result$conf.int)) result$conf.int else c(NA_real_, NA_real_)
    )
    stochastic_superiority <- if (test_type == "brunner_munzel") {
      unname(result$estimate)
    } else {
      NA_real_
    }
    list(p.value  = result$p.value,
         statistic = unname(result$statistic),
         mean_diff = mean_diff,
         conf.int  = conf.int,
         stochastic_superiority = stochastic_superiority)
  }

  original <- perform_test(group1, group2)
  original_significant <- original$p.value < alpha

  n_total <- if (paired) length(group1) else length(group1) + length(group2)
  max_k   <- floor(n_total * max_removal_pct)

  # ============================================================================
  # 1. JACKKNIFE (leave-one-out)
  # ============================================================================
  if (paired) {
    jackknife <- map_dfr(seq_along(group1), \(i) {
      test <- perform_test(group1[-i], group2[-i])
      tibble(unit = i, label = paste0("pair-", i),
             p_value = test$p.value, statistic = test$statistic)
    })
  } else {
    jackknife <- bind_rows(
      map_dfr(seq_along(group1), \(i) {
        test <- perform_test(group1[-i], group2)
        tibble(unit = i, label = paste0("G1-", i),
               p_value = test$p.value, statistic = test$statistic)
      }),
      map_dfr(seq_along(group2), \(i) {
        test <- perform_test(group1, group2[-i])
        tibble(unit = i, label = paste0("G2-", i),
               p_value = test$p.value, statistic = test$statistic)
      })
    )
  }

  jackknife <- annotate_loo_results(
    jackknife, original$p.value, original_significant, alpha,
    influential_threshold = influential_threshold
  )

  # ============================================================================
  # 2. EXTREME-VALUE REMOVAL (descriptive; grand-mean / abs-difference ranked)
  #    Retained from v1 for continuity. NOT used in the composite score:
  #    under a true effect it preferentially removes genuine responders and
  #    is not a worst-case bound (see manuscript/methodological_review.md, issue 1.2).
  # ============================================================================
  run_removal <- function(order_fun) {
    # order_fun returns, for the CURRENT data, index of next unit to drop
    g1 <- group1; g2 <- group2
    pairs_left <- seq_along(group1)          # bookkeeping for paired case
    out <- tibble(k_removed = 0L, p_value = original$p.value,
                  statistic = original$statistic,
                  significant = original_significant)
    for (k in seq_len(max_k)) {
      sel <- order_fun(g1, g2)
      if (is.null(sel)) break
      if (paired) {
        g1 <- g1[-sel$idx]; g2 <- g2[-sel$idx]
        if (length(g1) < 4) break
      } else {
        if (sel$group == 1) g1 <- g1[-sel$idx] else g2 <- g2[-sel$idx]
        if (length(g1) < 4 || length(g2) < 4) break
      }
      test <- perform_test(g1, g2)
      out <- bind_rows(out, tibble(k_removed = k, p_value = test$p.value,
                                   statistic = test$statistic,
                                   significant = test$p.value < alpha))
    }
    annotate_removal_results(out, original_significant)
  }

  if (paired) {
    extreme_next <- function(g1, g2) {
      d <- abs(g1 - g2)
      list(idx = which.max(d))
    }
  } else {
    extreme_next <- function(g1, g2) {
      gm <- mean(c(g1, g2))
      d1 <- abs(g1 - gm); d2 <- abs(g2 - gm)
      if (max(d1) >= max(d2)) list(group = 1, idx = which.max(d1))
      else                    list(group = 2, idx = which.max(d2))
    }
  }
  extreme <- run_removal(extreme_next)

  # ============================================================================
  # 3. WORST-CASE REMOVAL (greedy, in the spirit of AMIP;
  #    Broderick, Giordano & Meager 2023). At each step, remove the unit whose
  #    deletion moves the p-value furthest TOWARD overturning the conclusion.
  #    The resulting fragility index is a conservative sensitivity bound.
  # ============================================================================
  worst_next <- function(g1, g2) {
    target <- if (original_significant) 1 else -1   # push p up if significant
    best <- NULL
    if (paired) {
      if (length(g1) <= 4) return(NULL)
      for (i in seq_along(g1)) {
        p <- perform_test(g1[-i], g2[-i])$p.value
        if (is.null(best) || target * p > target * best$p) best <- list(idx = i, p = p)
      }
    } else {
      if (length(g1) > 4) {
        for (i in seq_along(g1)) {
          p <- perform_test(g1[-i], g2)$p.value
          if (is.null(best) || target * p > target * best$p) best <- list(group = 1, idx = i, p = p)
        }
      }
      if (length(g2) > 4) {
        for (i in seq_along(g2)) {
          p <- perform_test(g1, g2[-i])$p.value
          if (is.null(best) || target * p > target * best$p) best <- list(group = 2, idx = i, p = p)
        }
      }
    }
    best
  }
  worstcase <- run_removal(worst_next)

  k_frag_extreme <- fragility_index_from_removal(extreme, max_k)
  k_frag_worst   <- fragility_index_from_removal(worstcase, max_k)
  p_at_k_frag    <- p_at_fragility_from_removal(worstcase, k_frag_worst, max_k)

  # ============================================================================
  # 4. BOOTSTRAP (reproducibility probability)
  #    Resampling preserves the observed effect, so conclusion stability here
  #    estimates the probability a replicate sample reaches the same
  #    conclusion (~ power at the observed effect size). It reflects strength
  #    of evidence, NOT robustness to contamination.
  # ============================================================================
  set.seed(seed)
  bootstrap <- map_dfr(seq_len(n_boot), \(i) {
    if (paired) {
      idx <- sample(seq_along(group1), replace = TRUE)
      test <- perform_test(group1[idx], group2[idx])
    } else {
      test <- perform_test(sample(group1, replace = TRUE),
                           sample(group2, replace = TRUE))
    }
    tibble(iteration = i, p_value = test$p.value,
           statistic = test$statistic, significant = test$p.value < alpha)
  }) |>
    annotate_bootstrap_results(original_significant, alpha)

  # ============================================================================
  # COMPOSITE SCORE
  #   fragility component uses the WORST-CASE index, rescaled to [0, 100]
  #   (v1's `100 - fragility%` could not fall below ~70; see review, 1.4)
  # ============================================================================
  s_jack <- mean(jackknife$conclusion_match) * 100
  s_boot <- mean(bootstrap$conclusion_match) * 100

  # Shared metrics constructor (bands calibrated by simulation; see manuscript
  # Section 3). Boundaries: > 70 Robust; (55, 70] Moderately Robust; ≤ 55 Fragile.
  robustness_score <- build_robustness_metrics(
    s_jack = s_jack,
    jackknife = jackknife,
    k_frag_worst = k_frag_worst,
    p_at_k_frag = p_at_k_frag,
    s_boot = s_boot,
    bootstrap = bootstrap,
    weights = weights,
    n_total = n_total,
    max_k = max_k,
    k_frag_extreme = k_frag_extreme
  )
  robustness_interpretation <- robustness_band_label(
    robustness_score$overall_robustness
  )

  # --- sample info ------------------------------------------------------------
  effect_type <- if (is_prop) {
    "prop_diff"
  } else if (is_rank) {
    "hodges_lehmann"
  } else {
    "mean_diff"
  }
  sample_info <- if (paired) {
    list(test_type = test_type, n_pairs = length(group1),
         group1_mean = mean(group1), group1_sd = sd(group1),
         group2_mean = mean(group2), group2_sd = sd(group2),
         diff_mean = mean(group1 - group2), diff_sd = sd(group1 - group2),
         effect_type = effect_type)
  } else if (is_prop) {
    list(test_type = test_type,
         n1 = length(group1), n2 = length(group2), n_total = n_total,
         group1_prop = mean(group1), group2_prop = mean(group2),
         prop_diff = mean(group1) - mean(group2),
         correct = if (test_type %in% c("chisq", "prop")) correct else NA,
         effect_type = effect_type)
  } else {
    info <- list(test_type = test_type,
         n1 = length(group1), n2 = length(group2), n_total = n_total,
         group1_mean = mean(group1), group1_sd = sd(group1), group1_median = median(group1),
         group2_mean = mean(group2), group2_sd = sd(group2), group2_median = median(group2),
         effect_type = effect_type)
    if (test_type == "brunner_munzel") {
      info$stochastic_superiority <- original$stochastic_superiority
    }
    info
  }

  out <- list(
    original_p           = original$p.value,
    original_significant = original_significant,
    original_statistic   = original$statistic,
    original_mean_diff   = original$mean_diff,
    original_ci          = original$conf.int,
    n                    = n_total,

    robustness_metrics        = robustness_score,
    robustness_interpretation = robustness_interpretation,
    weights                   = weights,
    alpha                     = alpha,
    max_removal_pct           = max_removal_pct,
    max_k                     = max_k,

    jackknife = list(
      results = jackknife,
      n_influential = sum(jackknife$influential),
      influential_observations = jackknife |> filter(influential),
      p_range = range(jackknife$p_value)
    ),
    worstcase = list(
      results = worstcase,
      fragility_index = k_frag_worst,
      fragility_pct   = 100 * k_frag_worst / n_total,
      p_at_fragility  = p_at_k_frag
    ),
    extreme = list(
      results = extreme,
      fragility_index = k_frag_extreme,
      fragility_pct   = 100 * k_frag_extreme / n_total
    ),
    bootstrap = list(
      results = bootstrap,
      reproducibility = s_boot,
      p_percentile_interval = quantile(bootstrap$p_value, c(0.025, 0.975)),
      p_mean = mean(bootstrap$p_value),
      p_sd   = sd(bootstrap$p_value)
    ),
    sample_info = sample_info
  )

  out <- align_robustness_result_aliases(out, style = "analysis")
  # Keep an explicit NULL slot when interpret = FALSE so `$interpretation`
  # does not partial-match the `interpretation_label` alias.
  if (interpret) {
    out[["interpretation"]] <- generate_interpretation(out)
  } else {
    out["interpretation"] <- list(NULL)
  }
  class(out) <- c("robustness_analysis", "list")
  out
}

# ==============================================================================
# Text interpretation (rewritten; takes the full result object, no free vars)
# ==============================================================================
generate_interpretation <- function(x) {
  m     <- x$robustness_metrics
  alpha <- x$alpha
  test_name <- switch(x$sample_info$test_type,
    "t.test"          = "Welch two-sample t-test",
    "paired.t.test"   = "paired t-test",
    "wilcoxon"        = "Wilcoxon rank-sum test",
    "wilcoxon.paired" = "Wilcoxon signed-rank test",
    "brunner_munzel"  = "Brunner-Munzel test",
    "fisher"          = "Fisher's exact test",
    "chisq"           = "chi-square test of independence",
    "prop"            = "two-sample proportion test")

  overall <- sprintf(
    "The %s yielded a %s result (p = %.4f, alpha = %.2f). Overall robustness score: %.1f/100 ('%s'; weights %s).",
    test_name,
    ifelse(x$original_significant, "statistically significant", "non-significant"),
    x$original_p, alpha, m$overall_robustness, x$robustness_interpretation,
    paste(sprintf("%s=%.2f", names(x$weights), x$weights), collapse = ", "))

  jack_level <- case_when(
    m$jackknife_conclusion_stability > 95 ~ "excellent",
    m$jackknife_conclusion_stability > 85 ~ "good",
    m$jackknife_conclusion_stability > 70 ~ "moderate",
    TRUE ~ "poor")
  jackknife_text <- sprintf(
    "Jackknife leave-one-out analysis showed %s stability (%.1f%% of tests kept the original conclusion); %d observation(s) flagged as influential. Leave-one-out p-values ranged from %.4f to %.4f. Note that single-observation influence shrinks as ~1/n, so high jackknife stability alone is weak evidence of robustness in larger samples.",
    jack_level, m$jackknife_conclusion_stability, x$jackknife$n_influential,
    m$jackknife_p_range_lo, m$jackknife_p_range_hi)

  if (x$worstcase$fragility_index > x$max_k) {
    worstcase_text <- sprintf(
      "Worst-case removal analysis: the conclusion survived greedy adversarial removal of up to %d observations (%.0f%% of the sample) - no removal set of that size overturns it.",
      x$max_k, 100 * x$max_removal_pct)
  } else {
    frag_level <- case_when(
      m$worstcase_fragility_pct > 10 ~ "low",
      m$worstcase_fragility_pct > 5  ~ "moderate",
      TRUE ~ "high")
    worstcase_text <- sprintf(
      "Worst-case removal analysis indicated %s fragility: greedy removal of the %d most damaging observations (%.1f%% of the sample) overturned the conclusion (p: %.4f -> %.4f). The greedy set suffices to overturn the conclusion; an even smaller set may exist.",
      frag_level, x$worstcase$fragility_index, m$worstcase_fragility_pct,
      x$original_p, x$worstcase$p_at_fragility)
  }

  bootstrap_text <- sprintf(
    "Bootstrap reproducibility probability: %.1f%% of %d resamples reached the same conclusion (mean p = %.4f, SD = %.4f; 2.5th-97.5th percentile interval [%.4f, %.4f]). This reflects strength of evidence at the observed effect size, not robustness to contamination: marginal p-values yield low reproducibility even in clean data.",
    m$bootstrap_reproducibility, nrow(x$bootstrap$results),
    m$bootstrap_p_mean, m$bootstrap_p_sd,
    x$bootstrap$p_percentile_interval[1], x$bootstrap$p_percentile_interval[2])

  recommendation <- if (m$overall_robustness > 70) {
    "The result is stable across all sensitivity analyses and can be reported with confidence."
  } else if (m$overall_robustness > 55) {
    "The result shows moderate robustness. Report the component metrics transparently, review flagged observations for data quality and clinical plausibility, and add a rank-based supplementary analysis. Interpret with appropriate caution."
  } else {
    "The result is fragile. Treat it as exploratory: review influential observations, run non-parametric alternatives, investigate data quality, and seek confirmation in an independent, adequately powered study."
  }

  report <- paste(
    "ROBUSTNESS ANALYSIS INTERPRETATION",
    "==================================", "",
    "OVERALL:", overall, "",
    "JACKKNIFE:", jackknife_text, "",
    "WORST-CASE REMOVAL:", worstcase_text, "",
    "BOOTSTRAP:", bootstrap_text, "",
    "RECOMMENDATION:", recommendation, sep = "\n")

  list(overall = overall, jackknife = jackknife_text,
       worstcase = worstcase_text, bootstrap = bootstrap_text,
       recommendation = recommendation, report = report)
}

# ==============================================================================
# Print method
# ==============================================================================
#' @rdname robustness_analysis
#' @param x A `robustness_analysis` object
#' @param show_interpretation If TRUE, print the text interpretation when present
#' @param ... Unused
#' @export
print.robustness_analysis <- function(x, show_interpretation = TRUE, ...) {
  m <- x$robustness_metrics
  cat("================================================\n")
  cat("   ROBUSTNESS ANALYSIS SUMMARY (v2)\n")
  cat("================================================\n\n")
  cat(sprintf("TEST: %s | alpha = %.2f\n\n", x$sample_info$test_type, x$alpha))

  cat("ORIGINAL ANALYSIS:\n")
  if (!is.null(x$sample_info$n_pairs)) {
    cat(sprintf("  n = %d pairs; mean difference (g1 - g2): %.3f (SD %.3f)\n",
                x$sample_info$n_pairs, x$sample_info$diff_mean, x$sample_info$diff_sd))
    if (identical(x$sample_info$effect_type, "hodges_lehmann") &&
        !is.na(x$original_mean_diff)) {
      if (all(is.finite(x$original_ci))) {
        cat(sprintf("  Hodges-Lehmann shift (g1 - g2): %.3f (95%% CI %.3f, %.3f)\n",
                    x$original_mean_diff, x$original_ci[1], x$original_ci[2]))
      } else {
        cat(sprintf("  Hodges-Lehmann shift (g1 - g2): %.3f\n",
                    x$original_mean_diff))
      }
    }
  } else if (!is.null(x$sample_info$group1_prop)) {
    cat(sprintf("  n1 = %d, n2 = %d\n", x$sample_info$n1, x$sample_info$n2))
    cat(sprintf("  Proportions: p1 = %.3f, p2 = %.3f (diff p1 - p2 = %.3f)\n",
                x$sample_info$group1_prop, x$sample_info$group2_prop,
                x$sample_info$prop_diff))
  } else {
    cat(sprintf("  n1 = %d, n2 = %d\n", x$sample_info$n1, x$sample_info$n2))
    if (!is.na(x$original_mean_diff)) {
      if (identical(x$sample_info$effect_type, "hodges_lehmann")) {
        if (all(is.finite(x$original_ci))) {
          cat(sprintf("  Hodges-Lehmann shift (g1 - g2): %.3f (95%% CI %.3f, %.3f)\n",
                      x$original_mean_diff, x$original_ci[1], x$original_ci[2]))
        } else {
          cat(sprintf("  Hodges-Lehmann shift (g1 - g2): %.3f\n",
                      x$original_mean_diff))
        }
        if (!is.null(x$sample_info$stochastic_superiority) &&
            !is.na(x$sample_info$stochastic_superiority)) {
          cat(sprintf("  Stochastic superiority P(X<Y)+.5P(X=Y): %.3f\n",
                      x$sample_info$stochastic_superiority))
        }
      } else {
        cat(sprintf("  Mean difference (g1 - g2): %.3f (95%% CI %.3f, %.3f)\n",
                    x$original_mean_diff, x$original_ci[1], x$original_ci[2]))
      }
    }
  }
  if (is.na(x$original_statistic)) {
    cat(sprintf("  p = %.4f (%s)\n\n",
                x$original_p,
                ifelse(x$original_significant, "significant", "non-significant")))
  } else {
    cat(sprintf("  Statistic = %.3f, p = %.4f (%s)\n\n",
                x$original_statistic, x$original_p,
                ifelse(x$original_significant, "significant", "non-significant")))
  }

  cat(sprintf("OVERALL ROBUSTNESS: %.1f/100 (%s)\n\n",
              m$overall_robustness, x$robustness_interpretation))

  cat("COMPONENTS:\n")
  cat(sprintf("  Jackknife stability:        %5.1f%%  (influential: %d)\n",
              m$jackknife_conclusion_stability, x$jackknife$n_influential))
  cat(sprintf("  Worst-case fragility:       k = %s (%.1f%% of sample)%s\n",
              ifelse(x$worstcase$fragility_index > x$max_k,
                     paste0("> ", x$max_k), x$worstcase$fragility_index),
              min(m$worstcase_fragility_pct, 100 * x$max_removal_pct),
              ifelse(is.na(m$p_at_fragility), "",
                     sprintf("  [p at flip: %.4f]", m$p_at_fragility))))
  cat(sprintf("  Extreme-value fragility:    k = %s (descriptive)\n",
              ifelse(x$extreme$fragility_index > x$max_k,
                     paste0("> ", x$max_k), x$extreme$fragility_index)))
  cat(sprintf("  Bootstrap reproducibility:  %5.1f%%  (mean p = %.4f, PI [%.4f, %.4f])\n\n",
              m$bootstrap_reproducibility, m$bootstrap_p_mean,
              x$bootstrap$p_percentile_interval[1],
              x$bootstrap$p_percentile_interval[2]))

  if (x$jackknife$n_influential > 0) {
    cat("Influential observations (review for data quality):\n")
    print(x$jackknife$influential_observations |>
            select(label, p_value, significant))
    cat("\n")
  }
  if (!is.null(x[["interpretation"]]) && show_interpretation) {
    cat(x[["interpretation"]]$report, "\n")
  }
  invisible(x)
}

# ==============================================================================
# Plot method (p-value trajectories + bootstrap distribution)
# ==============================================================================
#' @rdname robustness_analysis
#' @export
plot.robustness_analysis <- function(x, ...) {
  traj <- bind_rows(
    x$worstcase$results |> mutate(method = "Worst-case removal"),
    x$extreme$results   |> mutate(method = "Extreme-value removal")
  )
  p1 <- traj |>
    ggplot(aes(k_removed, p_value, colour = method)) +
    geom_hline(yintercept = x$alpha, linetype = "dashed") +
    geom_line(linewidth = 0.8) + geom_point(size = 1.8) +
    labs(title = "Removal analyses",
         subtitle = sprintf("Worst-case fragility index = %s",
                            ifelse(x$worstcase$fragility_index > x$max_k,
                                   paste0("> ", x$max_k),
                                   x$worstcase$fragility_index)),
         x = "Observations removed", y = "p-value", colour = NULL) +
    theme_minimal() + theme(legend.position = "bottom")

  p2 <- x$bootstrap$results |>
    ggplot(aes(p_value)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.75) +
    geom_vline(xintercept = x$original_p, colour = "red", linewidth = 0.8) +
    geom_vline(xintercept = x$alpha, linetype = "dashed") +
    labs(title = "Bootstrap p-value distribution",
         subtitle = sprintf("Reproducibility = %.1f%%",
                            x$robustness_metrics$bootstrap_reproducibility),
         x = "p-value", y = "Count") +
    theme_minimal()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    print(p1 / p2)
  } else {
    print(p1); print(p2)
  }
  invisible(list(trajectories = p1, bootstrap = p2))
}

