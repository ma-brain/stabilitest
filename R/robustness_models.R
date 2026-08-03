# ==============================================================================
# Robustness Analysis — model-based extensions (July 2026)
#
# robustness_lm()   : linear models / ANCOVA (treatment effect adjusted for
#                     covariates, e.g. change ~ arm + baseline)
# robustness_surv() : time-to-event endpoints via Cox proportional hazards
#                     (Wald p-value; with a single binary arm the score test
#                     is asymptotically the log-rank test)
#
# Both reuse a common case-deletion engine: jackknife, greedy worst-case
# removal (AMIP-style), and case-resampling bootstrap operate on ROWS of the
# analysis dataset, so covariate adjustment is preserved throughout.
# Companion to robustness_analysis.R (two-sample version); see
# methodological_review.md for the rationale behind the v2 metrics.
# ==============================================================================

# ------------------------------------------------------------------------------
# Internal engine: everything is expressed through fit_fun(data) -> list(
#   p = p-value of the term of interest, estimate = its coefficient)
# fit_fun must return NULL if the model cannot be fitted (e.g. a factor level
# disappears after case deletion); such candidates are skipped.
# ------------------------------------------------------------------------------
robustness_engine <- function(data, fit_fun, alpha, n_boot, max_removal_pct,
                              weights, seed, min_n = 10) {

  n <- nrow(data)
  if (n < min_n) stop(sprintf("Need at least %d rows", min_n))
  original <- fit_fun(data)
  if (is.null(original)) stop("Model could not be fitted on the full dataset")
  original_significant <- original$p < alpha
  max_k <- floor(n * max_removal_pct)

  safe_fit <- function(d) tryCatch(fit_fun(d), error = \(e) NULL,
                                   warning = \(w) suppressWarnings(fit_fun(d)))

  # --- jackknife --------------------------------------------------------------
  jackknife <- map_dfr(seq_len(n), \(i) {
    f <- safe_fit(data[-i, , drop = FALSE])
    tibble(row = i,
           p_value  = if (is.null(f)) NA_real_ else f$p,
           estimate = if (is.null(f)) NA_real_ else f$estimate)
  }) |>
    filter(!is.na(p_value)) |>
    mutate(significant      = p_value < alpha,
           conclusion_match = significant == original_significant,
           influential      = !conclusion_match |
                              abs(p_value - original$p) > 0.05)

  # --- worst-case greedy removal ----------------------------------------------
  keep <- seq_len(n)
  target <- if (original_significant) 1 else -1
  worstcase <- tibble(k_removed = 0L, p_value = original$p,
                      estimate = original$estimate,
                      significant = original_significant)
  removed_rows <- integer(0)
  for (k in seq_len(max_k)) {
    if (length(keep) <= min_n) break
    best <- NULL
    for (idx in seq_along(keep)) {
      f <- safe_fit(data[keep[-idx], , drop = FALSE])
      if (is.null(f)) next
      if (is.null(best) || target * f$p > target * best$p) {
        best <- list(pos = idx, p = f$p, estimate = f$estimate)
      }
    }
    if (is.null(best)) break
    removed_rows <- c(removed_rows, keep[best$pos])
    keep <- keep[-best$pos]
    worstcase <- bind_rows(worstcase,
      tibble(k_removed = k, p_value = best$p, estimate = best$estimate,
             significant = best$p < alpha))
    if ((best$p < alpha) != original_significant) break
  }
  worstcase <- worstcase |>
    mutate(conclusion_match = significant == original_significant)

  flipped <- worstcase |> filter(!conclusion_match)
  k_frag  <- if (nrow(flipped) == 0) max_k + 1L else min(flipped$k_removed)
  p_at_k  <- if (k_frag <= max_k) worstcase$p_value[worstcase$k_removed == k_frag] else NA_real_

  # --- case-resampling bootstrap ------------------------------------------------
  set.seed(seed)
  bootstrap <- map_dfr(seq_len(n_boot), \(b) {
    f <- safe_fit(data[sample(n, replace = TRUE), , drop = FALSE])
    tibble(iteration = b,
           p_value  = if (is.null(f)) NA_real_ else f$p,
           estimate = if (is.null(f)) NA_real_ else f$estimate)
  }) |>
    filter(!is.na(p_value)) |>
    mutate(significant      = p_value < alpha,
           conclusion_match = significant == original_significant)

  # --- composite ----------------------------------------------------------------
  s_jack    <- mean(jackknife$conclusion_match) * 100
  s_boot    <- mean(bootstrap$conclusion_match) * 100
  frag_comp <- 100 * min(k_frag / (max_k + 1), 1)
  score     <- weights[["jackknife"]] * s_jack +
               weights[["fragility"]] * frag_comp +
               weights[["bootstrap"]] * s_boot

  list(
    original_p = original$p, original_estimate = original$estimate,
    original_significant = original_significant,
    n = n, max_k = max_k, alpha = alpha, weights = weights,
    jackknife = jackknife,
    worstcase = worstcase,
    bootstrap = bootstrap,
    removed_rows = removed_rows,
    metrics = tibble(
      jackknife_conclusion_stability = s_jack,
      jackknife_n_influential        = sum(jackknife$influential),
      worstcase_fragility_k          = k_frag,
      worstcase_fragility_pct        = 100 * k_frag / n,
      p_at_fragility                 = p_at_k,
      bootstrap_reproducibility      = s_boot,
      bootstrap_p_mean               = mean(bootstrap$p_value),
      estimate_range_jackknife_lo    = min(jackknife$estimate),
      estimate_range_jackknife_hi    = max(jackknife$estimate),
      overall_robustness             = score),
    interpretation_label = dplyr::case_when(
      score > 70 ~ "Robust",          # bands calibrated by simulation
      score > 55 ~ "Moderately Robust",
      TRUE ~ "Fragile")
  )
}

# ------------------------------------------------------------------------------
#' Robustness analysis for a linear model / ANCOVA term
#'
#' @param formula Model formula, e.g. change ~ arm + baseline
#' @param data Data frame (one row per subject)
#' @param term Coefficient whose p-value defines the conclusion,
#'   e.g. "armActive". Must match a row of summary(lm(...))$coefficients.
#' @param alpha,n_boot,max_removal_pct,weights,seed As in robustness_analysis()
#'
#' @examples
#' # res <- robustness_lm(change ~ arm + baseline, dat, term = "armActive")
#' @export
robustness_lm <- function(formula, data, term,
                          alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
                          weights = c(jackknife = 0.4, fragility = 0.4,
                                      bootstrap = 0.2),
                          seed = 123) {

  fit_fun <- function(d) {
    fit <- lm(formula, data = d)
    ct  <- summary(fit)$coefficients
    if (!term %in% rownames(ct)) return(NULL)
    list(p = ct[term, "Pr(>|t|)"], estimate = ct[term, "Estimate"])
  }
  out <- robustness_engine(data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$model <- deparse(formula)
  out$type <- "Linear model (lm)"
  class(out) <- c("robustness_model", "list")
  out
}

# ------------------------------------------------------------------------------
#' Robustness analysis for a Cox proportional hazards term
#'
#' @param formula Survival formula, e.g. survival::Surv(time, event) ~ arm + age
#' @param data Data frame (one row per subject)
#' @param term Coefficient of interest, e.g. "armActive"
#' @param alpha,n_boot,max_removal_pct,weights,seed As in robustness_analysis()
#'
#' @details Uses the Wald p-value of the term. Case deletion/resampling acts on
#'   subjects, so censoring patterns are handled naturally. Removing whole
#'   subjects (with their follow-up) differs from the event-flip fragility of
#'   Walsh et al.; interpret as a removal fragility index.
#' @export
robustness_surv <- function(formula, data, term,
                            alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
                            weights = c(jackknife = 0.4, fragility = 0.4,
                                        bootstrap = 0.2),
                            seed = 123) {

  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for robustness_surv()")
  }
  fit_fun <- function(d) {
    fit <- survival::coxph(formula, data = d)
    ct  <- summary(fit)$coefficients
    if (!term %in% rownames(ct)) return(NULL)
    list(p = ct[term, "Pr(>|z|)"], estimate = ct[term, "coef"])
  }
  out <- robustness_engine(data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$model <- deparse(formula)
  out$type <- "Cox proportional hazards"
  class(out) <- c("robustness_model", "list")
  out
}

# ------------------------------------------------------------------------------
#' Print a model-based robustness analysis
#'
#' @param x A `robustness_model` object from [robustness_lm()] or
#'   [robustness_surv()]
#' @param ... Unused
#' @return `x`, invisibly
#' @export
print.robustness_model <- function(x, ...) {
  m <- x$metrics
  cat("================================================\n")
  cat("   MODEL-BASED ROBUSTNESS ANALYSIS\n")
  cat("================================================\n\n")
  cat(sprintf("MODEL: %s  [%s]\n", x$model, x$type))
  cat(sprintf("TERM:  %s | alpha = %.2f | n = %d\n\n", x$term, x$alpha, x$n))
  cat(sprintf("ORIGINAL: estimate = %.4f, p = %.4f (%s)\n",
              x$original_estimate, x$original_p,
              ifelse(x$original_significant, "significant", "non-significant")))
  if (x$type == "Cox proportional hazards") {
    cat(sprintf("          HR = %.3f\n", exp(x$original_estimate)))
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
                     sprintf("  [p at flip: %.4f]", m$p_at_fragility))))
  cat(sprintf("  Bootstrap reproducibility: %5.1f%%  (mean p = %.4f)\n",
              m$bootstrap_reproducibility, m$bootstrap_p_mean))
  cat(sprintf("  Jackknife estimate range:  [%.4f, %.4f]\n\n",
              m$estimate_range_jackknife_lo, m$estimate_range_jackknife_hi))
  if (length(x$removed_rows) > 0 && m$worstcase_fragility_k <= x$max_k) {
    cat("Rows removed by worst-case analysis (in order):\n  ")
    cat(x$removed_rows[seq_len(m$worstcase_fragility_k)], sep = ", ")
    cat("\n  -> review these subjects for data quality / clinical plausibility\n")
  }
  invisible(x)
}

