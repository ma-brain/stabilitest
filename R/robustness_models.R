# ==============================================================================
# Robustness Analysis — model-based extensions (July 2026)
#
# robustness_lm()   : linear models / ANCOVA (single-coef Wald/t or multi-df
#                     joint F via drop1 for factor term labels)
# robustness_surv() : time-to-event endpoints via Cox proportional hazards
#                     (Wald p-value; with a single binary arm the score test
#                     is asymptotically the log-rank test)
# robustness_glm()  : binomial logistic GLM terms (logit link only in v1;
#                     multi-df factors still out of scope for glm)
#
# All reuse a common case-deletion engine: jackknife, greedy worst-case
# removal (AMIP-style), and case-resampling bootstrap operate on ROWS of the
# analysis dataset, so covariate adjustment is preserved throughout.
# Companion to robustness_analysis.R (two-sample version); see
# manuscript/methodological_review.md for the rationale behind the v2 metrics.
# ==============================================================================

# ------------------------------------------------------------------------------
# Term resolution (shared by lm now; glm / surv can reuse later)
#
# Matching rules (v1):
#   1. Exact match to one coefficient row name -> single-coef Wald/t.
#   2. Exact match to a terms() term label with >= 2 design-matrix columns
#      (non-aliased) -> joint multi-df test.
#   3. Exact match to a term label with exactly 1 non-aliased column whose
#      coefficient name differs (e.g. binary factor "arm" -> "armA") ->
#      resolve to that single coefficient.
#   4. Otherwise, or if (1) and a multi-column term label collide on the same
#      string, error with an unambiguous message.
# ------------------------------------------------------------------------------
resolve_model_term <- function(fit, term) {
  if (!is.character(term) || length(term) != 1L || !nzchar(term)) {
    stop("`term` must be a non-empty string", call. = FALSE)
  }

  ct <- stats::coef(summary(fit))
  if (is.null(ct) || is.null(rownames(ct))) {
    stop("Model has no coefficient table to match `term` against", call. = FALSE)
  }
  coef_names <- rownames(ct)

  tt <- stats::terms(fit)
  term_labels <- attr(tt, "term.labels")
  mm <- stats::model.matrix(fit)
  assign <- attr(mm, "assign")
  if (is.null(assign)) {
    stop("Model matrix has no 'assign' attribute; cannot resolve multi-df terms",
         call. = FALSE)
  }

  exact_coef <- term %in% coef_names
  exact_tl <- length(term_labels) > 0L && term %in% term_labels

  coef_cols_for_label <- function(label) {
    idx <- match(label, term_labels)
    cols <- which(assign == idx)
    # Drop aliased / NA coefficients not present in the summary table
    intersect(colnames(mm)[cols], coef_names)
  }

  if (exact_coef && exact_tl) {
    cols <- coef_cols_for_label(term)
    if (length(cols) > 1L) {
      stop(sprintf(
        paste0("Ambiguous term '%s': matches both a coefficient row and a ",
               "multi-df term label (%d columns: %s). Use a specific ",
               "coefficient name for a single-df test, or a unique term label."),
        term, length(cols), paste(cols, collapse = ", ")),
        call. = FALSE)
    }
    # Continuous covariate / 1-df term whose label equals the coef name
    return(list(
      type = "single",
      term = term,
      coef_name = term,
      coef_names = term,
      ndf = 1L
    ))
  }

  if (exact_coef) {
    return(list(
      type = "single",
      term = term,
      coef_name = term,
      coef_names = term,
      ndf = 1L
    ))
  }

  if (exact_tl) {
    cols <- coef_cols_for_label(term)
    if (length(cols) == 0L) {
      stop(sprintf(
        "Term label '%s' has no estimable coefficients (possibly fully aliased)",
        term), call. = FALSE)
    }
    if (length(cols) == 1L) {
      return(list(
        type = "single",
        term = term,
        coef_name = cols[[1L]],
        coef_names = cols,
        ndf = 1L
      ))
    }
    return(list(
      type = "joint",
      term = term,
      coef_name = NA_character_,
      coef_names = cols,
      ndf = length(cols)
    ))
  }

  stop(sprintf(
    paste0("Term '%s' not found in model coefficients or term labels.\n",
           "  Coefficients: %s\n",
           "  Term labels:  %s"),
    term,
    paste(coef_names, collapse = ", "),
    if (length(term_labels) == 0L) "(none)" else paste(term_labels, collapse = ", ")
  ), call. = FALSE)
}

# Extract p (and estimate for single-coef) from a fitted lm given a resolved term.
# Returns NULL when the term cannot be tested on this subset (e.g. a factor
# level disappeared). Joint multi-df terms use drop1(..., test = "F").
lm_term_test <- function(fit, term_spec) {
  if (identical(term_spec$type, "single")) {
    ct <- summary(fit)$coefficients
    cn <- term_spec$coef_name
    if (is.null(ct) || !cn %in% rownames(ct)) return(NULL)
    p <- ct[cn, "Pr(>|t|)"]
    est <- ct[cn, "Estimate"]
    if (is.na(p)) return(NULL)
    return(list(p = unname(p), estimate = unname(est)))
  }

  # Joint F via marginal drop1 (base R; no car dependency)
  scope <- stats::as.formula(paste("~", term_spec$term),
                             env = environment(formula(fit)))
  d1 <- tryCatch(
    stats::drop1(fit, scope = scope, test = "F"),
    error = function(e) NULL
  )
  if (is.null(d1) || !term_spec$term %in% rownames(d1)) return(NULL)
  p <- d1[term_spec$term, "Pr(>F)"]
  if (is.na(p)) return(NULL)
  list(
    p = unname(p),
    estimate = NA_real_,
    statistic = unname(d1[term_spec$term, "F value"]),
    ndf = unname(d1[term_spec$term, "Df"]),
    # RSS residual df for the full model is in the <none> row's implicit df;
    # drop1 does not always expose ddf per row — recover from the fit.
    ddf = unname(fit$df.residual)
  )
}

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
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0, 1)")
  if (abs(sum(weights) - 1) > 1e-8 || length(weights) != 3 || any(weights < 0)) {
    stop("weights must be 3 non-negative values summing to 1")
  }
  original <- fit_fun(data)
  if (is.null(original) || is.na(original$p)) {
    stop("Model could not be fitted on the full dataset")
  }
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
      estimate_range_jackknife_lo    = if (all(is.na(jackknife$estimate))) {
        NA_real_
      } else {
        min(jackknife$estimate, na.rm = TRUE)
      },
      estimate_range_jackknife_hi    = if (all(is.na(jackknife$estimate))) {
        NA_real_
      } else {
        max(jackknife$estimate, na.rm = TRUE)
      },
      overall_robustness             = score),
    interpretation_label = dplyr::case_when(
      score > 70 ~ "Robust",          # > 70; bands calibrated by simulation
      score > 55 ~ "Moderately Robust",  # (55, 70]
      TRUE ~ "Fragile")                  # ≤ 55
  )
}

# ------------------------------------------------------------------------------
#' Robustness analysis for a linear model / ANCOVA term
#'
#' @param formula Model formula, e.g. `change ~ arm + baseline`
#' @param data Data frame (one row per subject)
#' @param term Term whose p-value defines the conclusion. Either:
#'   - a single coefficient row name from `summary(lm(...))$coefficients`
#'     (e.g. `"armActive"`), tested with the usual coefficient t / Wald p-value; or
#'   - a multi-df term label from `attr(terms(lm(...)), "term.labels")`
#'     (e.g. a 3-level factor `"arm"`), tested jointly via
#'     `drop1(..., test = "F")`. For multi-df terms the stored `estimate` is
#'     `NA` and `term_info` records the joint F test (ndf / statistic).
#' @param alpha,n_boot,max_removal_pct,weights,seed As in [robustness_analysis()]
#'
#' @details Single-coefficient `term` strings keep the previous Wald/t
#'   behaviour. Multi-df factors are detected when `term` matches a term label
#'   that expands to more than one non-aliased design-matrix column. Ambiguous
#'   matches error. Conclusion for the robustness pipeline is `p < alpha`
#'   (joint F p-value for multi-df terms).
#'
#' @examples
#' # res <- robustness_lm(change ~ arm + baseline, dat, term = "armActive")
#' # res <- robustness_lm(change ~ arm + baseline, dat, term = "arm")  # joint F
#' @export
robustness_lm <- function(formula, data, term,
                          alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
                          weights = c(jackknife = 0.4, fragility = 0.4,
                                      bootstrap = 0.2),
                          seed = 123) {

  fit0 <- stats::lm(formula, data = data)
  term_spec <- resolve_model_term(fit0, term)
  # Enrich term_info from the full-data test (F statistic / ndf for joint)
  full_test <- lm_term_test(fit0, term_spec)
  if (is.null(full_test) || is.na(full_test$p)) {
    stop("Model could not be fitted on the full dataset", call. = FALSE)
  }
  if (identical(term_spec$type, "joint")) {
    term_spec$statistic <- full_test$statistic
    term_spec$ndf <- as.integer(full_test$ndf)
    term_spec$ddf <- as.integer(full_test$ddf)
    term_spec$test <- "joint_F"
  } else {
    term_spec$test <- "wald_t"
  }

  fit_fun <- function(d) {
    fit <- tryCatch(stats::lm(formula, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    lm_term_test(fit, term_spec)
  }
  out <- robustness_engine(data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$term_info <- term_spec
  out$sample_info <- list(
    test = term_spec$test,
    term = term,
    coef_names = term_spec$coef_names,
    ndf = term_spec$ndf,
    ddf = if (is.null(term_spec$ddf)) NA_integer_ else term_spec$ddf,
    statistic = if (is.null(term_spec$statistic)) NA_real_ else term_spec$statistic
  )
  out$model <- paste(deparse(formula), collapse = " ")
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
#' Robustness analysis for a binomial logistic GLM term
#'
#' @param formula Model formula, e.g. `y ~ arm + x`. Offsets may be supplied
#'   via `offset(...)` in the formula.
#' @param data Data frame (one row per subject)
#' @param term Coefficient whose Wald p-value defines the conclusion,
#'   e.g. `"armA"`. Must match a single row of
#'   `summary(glm(...))$coefficients` (multi-df factors are out of scope).
#' @param family A family *object*. Currently only
#'   `binomial(link = "logit")` (the default) is supported. Gaussian models
#'   should use [robustness_lm()]; Poisson is deferred to a follow-up.
#' @param alpha,n_boot,max_removal_pct,weights,seed As in
#'   [robustness_analysis()]. Note that `weights` are **composite score
#'   weights**, not observation weights for `glm()`.
#' @param obs_weights Optional numeric vector of observation (case) weights
#'   of length `nrow(data)`, passed to `stats::glm(..., weights = obs_weights)`.
#'   `NULL` (default) fits an unweighted GLM. Distinct from the composite
#'   score `weights` argument.
#'
#' @details Uses the Wald `Pr(>|z|)` p-value of the named coefficient from
#'   `summary.glm`. Case deletion / resampling acts on rows of `data`.
#'
#'   Complete or quasi-complete separation is handled only via `converged`
#'   and finite p-values: failed full-data fits error; failed subsets are
#'   skipped. Firth / bias-reduced logistic regression is not supported.
#'   Score bands are shared with the ANCOVA / Cox engines and are not
#'   separately calibrated for GLM.
#'
#' @examples
#' # res <- robustness_glm(y ~ arm + x, dat, term = "armA", family = binomial())
#' @export
robustness_glm <- function(formula, data, term,
                           family = stats::binomial(),
                           alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
                           weights = c(jackknife = 0.4, fragility = 0.4,
                                       bootstrap = 0.2),
                           obs_weights = NULL,
                           seed = 123) {

  if (is.character(family)) {
    stop("family must be a family object, e.g. binomial()", call. = FALSE)
  }
  fam_name <- family$family
  link_name <- family$link

  if (identical(fam_name, "gaussian")) {
    stop("Use robustness_lm() for Gaussian linear models", call. = FALSE)
  }
  if (startsWith(fam_name, "quasi")) {
    stop("Quasi-families are not supported (no routine Wald z p-values via summary.glm); use binomial() or analyse overdispersion separately",
         call. = FALSE)
  }
  if (identical(fam_name, "binomial") && !identical(link_name, "logit")) {
    stop('robustness_glm() currently supports the logit link only',
         call. = FALSE)
  }
  if (!identical(fam_name, "binomial") || !identical(link_name, "logit")) {
    stop('robustness_glm() currently supports binomial(link = "logit") only',
         call. = FALSE)
  }

  if (!is.null(obs_weights)) {
    if (!is.numeric(obs_weights) || length(obs_weights) != nrow(data)) {
      stop("obs_weights must be NULL or a numeric vector of length nrow(data)",
           call. = FALSE)
    }
  }

  # Align obs_weights with engine row subsets / bootstrap replicates via a
  # private row-id column (not referenced by typical formulas).
  data <- as.data.frame(data)
  data$.__row_id__ <- seq_len(nrow(data))

  fit_fun <- function(d) {
    w <- if (is.null(obs_weights)) NULL else obs_weights[d$.__row_id__]
    # glm() evaluates `weights` via model.frame NSE; pass the arg only when
    # non-NULL (a bare `weights = w` with w = NULL looks up `w` in `data`).
    fit <- tryCatch({
      if (is.null(w)) {
        stats::glm(formula, data = d, family = family)
      } else {
        d$.__obs_w__ <- w
        stats::glm(formula, data = d, family = family, weights = .__obs_w__)
      }
    }, error = function(e) NULL)
    if (is.null(fit) || !isTRUE(fit$converged)) return(NULL)
    ct <- summary(fit)$coefficients
    if (is.null(ct) || !term %in% rownames(ct)) return(NULL)
    p_col <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), colnames(ct))[1]
    if (is.na(p_col) || length(p_col) == 0) return(NULL)
    p <- ct[term, p_col]
    est <- ct[term, "Estimate"]
    if (is.na(p) || is.na(est)) return(NULL)
    list(p = unname(p), estimate = unname(est))
  }

  out <- robustness_engine(data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$model <- paste(deparse(formula), collapse = " ")
  out$family <- fam_name
  out$link <- link_name
  out$type <- sprintf("GLM (%s, %s)", fam_name, link_name)
  class(out) <- c("robustness_model", "list")
  out
}

# ------------------------------------------------------------------------------
#' Print a model-based robustness analysis
#'
#' @param x A `robustness_model` object from [robustness_lm()],
#'   [robustness_surv()], or [robustness_glm()]
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
  ti <- x$term_info
  if (!is.null(ti) && identical(ti$type, "joint")) {
    cat(sprintf("ORIGINAL: joint F (ndf = %d, ddf = %d",
                ti$ndf, ti$ddf))
    if (!is.null(ti$statistic) && !is.na(ti$statistic)) {
      cat(sprintf(", F = %.3f", ti$statistic))
    }
    cat(sprintf("), p = %.4f (%s)\n",
                x$original_p,
                ifelse(x$original_significant, "significant", "non-significant")))
    cat("          estimate = NA (joint multi-df term)\n")
  } else if (is.na(x$original_estimate)) {
    cat(sprintf("ORIGINAL: estimate = NA, p = %.4f (%s)\n",
                x$original_p,
                ifelse(x$original_significant, "significant", "non-significant")))
  } else {
    cat(sprintf("ORIGINAL: estimate = %.4f, p = %.4f (%s)\n",
                x$original_estimate, x$original_p,
                ifelse(x$original_significant, "significant", "non-significant")))
  }
  if (x$type == "Cox proportional hazards" && !is.na(x$original_estimate)) {
    cat(sprintf("          HR = %.3f\n", exp(x$original_estimate)))
  }
  if (startsWith(x$type, "GLM (binomial") && !is.na(x$original_estimate)) {
    cat(sprintf("          OR = %.3f\n", exp(x$original_estimate)))
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
  if (is.na(m$estimate_range_jackknife_lo) || is.na(m$estimate_range_jackknife_hi)) {
    cat("  Jackknife estimate range:  NA (joint multi-df term or all fits failed)\n\n")
  } else {
    cat(sprintf("  Jackknife estimate range:  [%.4f, %.4f]\n\n",
                m$estimate_range_jackknife_lo, m$estimate_range_jackknife_hi))
  }
  if (length(x$removed_rows) > 0 && m$worstcase_fragility_k <= x$max_k) {
    cat("Rows removed by worst-case analysis (in order):\n  ")
    cat(x$removed_rows[seq_len(m$worstcase_fragility_k)], sep = ", ")
    cat("\n  -> review these subjects for data quality / clinical plausibility\n")
  }
  invisible(x)
}

