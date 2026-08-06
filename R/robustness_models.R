# ==============================================================================
# Robustness Analysis — model-based extensions (July 2026)
#
# robustness_lm()   : linear models / ANCOVA (single-coef Wald/t or multi-df
#                     joint F via drop1 for factor term labels)
# robustness_surv() : time-to-event endpoints via Cox proportional hazards
#                     (single-coef Wald z, or multi-df joint LRT via
#                     drop1(..., test = "Chisq"); with a single binary arm the
#                     score test is asymptotically the log-rank test)
# robustness_glm()  : binomial(logit) or poisson(log) GLM terms (single-coef
#                     Wald z, or multi-df joint LRT via drop1 test = "Chisq")
#
# All reuse a common case-deletion engine: jackknife, greedy worst-case
# removal (AMIP-style), and case-resampling bootstrap operate on ROWS of the
# analysis dataset, so covariate adjustment is preserved throughout.
# Numeric scores and component metrics are always retained. Categorical labels
# are suppressed until the exact method-specific calibration unit is validated;
# the only active validated unit is the narrow significant `welch_unpaired`
# configuration. `lm_ancova` (v1) and `lm_ancova_v2` (Track A jackknife-light)
# both closed Gate B fail-closed as uncalibrated / no_feasible_thresholds;
# interactive `robustness_lm()` defaults still resolve to `lm_ancova` and keep
# labels suppressed. Welch 55/70 is not an ANCOVA fallback.
# Companion to robustness_analysis.R (two-sample version); see
# manuscript/methodological_review.md for the rationale behind the v2 metrics.
# ==============================================================================

analysis_data_from_fit <- function(data, fit) {
  omitted <- fit$na.action
  if (is.null(omitted)) return(data)
  data[-as.integer(omitted), , drop = FALSE]
}

# ------------------------------------------------------------------------------
# Term resolution (shared by lm / glm / surv)
#
# Matching rules (v1):
#   1. Exact match to one coefficient row name -> single-coef Wald/t/z.
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

  # Map a terms() label to estimable coefficient names.
  # coxph stores assign as a named list of design-column indices (and often
  # drops the model frame), so model.matrix(fit) can fail without the original
  # data in scope — use fit$assign when available.
  coef_cols_for_label <- function(label) {
    if (inherits(fit, "coxph") && is.list(fit$assign)) {
      idx <- fit$assign[[label]]
      if (is.null(idx) || length(idx) == 0L) return(character(0))
      # Indices refer to the design / coefficient order used by coxph
      all_coefs <- names(stats::coef(fit))
      if (is.null(all_coefs)) all_coefs <- coef_names
      cols <- all_coefs[idx]
      return(intersect(cols[!is.na(cols)], coef_names))
    }

    mm <- stats::model.matrix(fit)
    assign <- attr(mm, "assign")
    if (is.null(assign)) {
      stop("Model matrix has no 'assign' attribute; cannot resolve multi-df terms",
           call. = FALSE)
    }
    idx <- match(label, term_labels)
    cols <- which(assign == idx)
    # Drop aliased / NA coefficients not present in the summary table
    intersect(colnames(mm)[cols], coef_names)
  }

  exact_coef <- term %in% coef_names
  exact_tl <- length(term_labels) > 0L && term %in% term_labels

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

# Extract p (and estimate for single-coef) from a fitted glm given a resolved
# term. Joint multi-df terms use drop1(..., test = "Chisq") (LRT).
glm_term_test <- function(fit, term_spec) {
  if (identical(term_spec$type, "single")) {
    ct <- summary(fit)$coefficients
    cn <- term_spec$coef_name
    if (is.null(ct) || !cn %in% rownames(ct)) return(NULL)
    p_col <- intersect(c("Pr(>|z|)", "Pr(>|t|)"), colnames(ct))[1]
    if (is.na(p_col) || length(p_col) == 0) return(NULL)
    p <- ct[cn, p_col]
    est <- ct[cn, "Estimate"]
    if (is.na(p) || is.na(est)) return(NULL)
    return(list(p = unname(p), estimate = unname(est)))
  }

  scope <- stats::as.formula(paste("~", term_spec$term),
                             env = environment(formula(fit)))
  d1 <- tryCatch(
    stats::drop1(fit, scope = scope, test = "Chisq"),
    error = function(e) NULL
  )
  if (is.null(d1) || !term_spec$term %in% rownames(d1)) return(NULL)
  p <- d1[term_spec$term, "Pr(>Chi)"]
  if (is.na(p)) return(NULL)
  list(
    p = unname(p),
    estimate = NA_real_,
    statistic = unname(d1[term_spec$term, "LRT"]),
    ndf = unname(d1[term_spec$term, "Df"])
  )
}

# Extract p (and estimate for single-coef) from a fitted coxph given a resolved
# term. Joint multi-df terms use drop1(..., test = "Chisq") (LRT; survival S3).
surv_term_test <- function(fit, term_spec) {
  if (identical(term_spec$type, "single")) {
    ct <- summary(fit)$coefficients
    cn <- term_spec$coef_name
    if (is.null(ct) || !cn %in% rownames(ct)) return(NULL)
    p <- ct[cn, "Pr(>|z|)"]
    est <- ct[cn, "coef"]
    if (is.na(p) || is.na(est)) return(NULL)
    return(list(p = unname(p), estimate = unname(est)))
  }

  scope <- stats::as.formula(paste("~", term_spec$term),
                             env = environment(formula(fit)))
  d1 <- tryCatch(
    stats::drop1(fit, scope = scope, test = "Chisq"),
    error = function(e) NULL
  )
  if (is.null(d1) || !term_spec$term %in% rownames(d1)) return(NULL)
  p <- d1[term_spec$term, "Pr(>Chi)"]
  if (is.na(p)) return(NULL)
  list(
    p = unname(p),
    estimate = NA_real_,
    statistic = unname(d1[term_spec$term, "LRT"]),
    ndf = unname(d1[term_spec$term, "Df"])
  )
}

# ------------------------------------------------------------------------------
# Internal engine: everything is expressed through fit_fun(data) -> list(
#   p = p-value of the term of interest, estimate = its coefficient)
# fit_fun must return NULL if the model cannot be fitted (e.g. a factor level
# disappears after case deletion); such candidates are skipped.
# ------------------------------------------------------------------------------
robustness_engine <- function(data, fit_fun, alpha, n_boot, max_removal_pct,
                              weights, seed, min_n = 10,
                              influential_threshold = 0.05) {

  n <- nrow(data)
  if (n < min_n) stop(sprintf("Need at least %d rows", min_n))
  validate_alpha_weights(alpha, weights)
  validate_n_boot_max_removal(n_boot, max_removal_pct)
  original <- fit_fun(data)
  if (is.null(original) || is.na(original$p)) {
    stop("Model could not be fitted on the full dataset")
  }
  original_significant <- original$p < alpha
  max_k <- min(floor(n * max_removal_pct), n - min_n)
  validate_fragility_capacity(max_k)

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
    annotate_loo_results(original$p, original_significant, alpha,
                         influential_threshold = influential_threshold)

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
  worstcase <- annotate_removal_results(worstcase, original_significant)

  k_frag <- fragility_index_from_removal(worstcase, max_k)
  p_at_k <- p_at_fragility_from_removal(worstcase, k_frag, max_k)

  # --- case-resampling bootstrap ------------------------------------------------
  set.seed(seed)
  bootstrap <- map_dfr(seq_len(n_boot), \(b) {
    f <- safe_fit(data[sample(n, replace = TRUE), , drop = FALSE])
    tibble(iteration = b,
           p_value  = if (is.null(f)) NA_real_ else f$p,
           estimate = if (is.null(f)) NA_real_ else f$estimate)
  }) |>
    filter(!is.na(p_value)) |>
    annotate_bootstrap_results(original_significant, alpha)

  # --- composite ----------------------------------------------------------------
  s_jack <- mean(jackknife$conclusion_match) * 100
  s_boot <- mean(bootstrap$conclusion_match) * 100
  est_rng <- jackknife_estimate_range(jackknife$estimate)
  metrics <- build_robustness_metrics(
    s_jack = s_jack,
    jackknife = jackknife,
    k_frag_worst = k_frag,
    p_at_k_frag = p_at_k,
    s_boot = s_boot,
    bootstrap = bootstrap,
    weights = weights,
    n_total = n,
    max_k = max_k,
    estimate_lo = est_rng$lo,
    estimate_hi = est_rng$hi
  )

  out <- list(
    original_p = original$p, original_estimate = original$estimate,
    original_significant = original_significant,
    n = n, max_k = max_k, max_removal_pct = max_removal_pct,
    alpha = alpha, weights = weights,
    jackknife = jackknife,
    worstcase = worstcase,
    bootstrap = bootstrap,
    removed_rows = removed_rows,
    metrics = metrics
  )
  out
}

# ------------------------------------------------------------------------------
# LM / ANCOVA calibration profile (machine-checkable structural eligibility)
# ------------------------------------------------------------------------------
coefficient_term_label <- function(fit, coefficient) {
  mm <- stats::model.matrix(fit)
  index <- match(coefficient, colnames(mm))
  if (is.na(index)) return(NA_character_)
  assignment <- attr(mm, "assign")[[index]]
  if (is.na(assignment) || assignment == 0L) return(NA_character_)
  attr(stats::terms(fit), "term.labels")[[assignment]]
}

lm_calibration_profile <- function(fit, term_spec, original_n, alpha, n_boot,
                                   weights, max_removal_pct) {
  frame <- stats::model.frame(fit)
  response <- stats::model.response(frame)
  labels <- attr(stats::terms(fit), "term.labels")
  target_label <- coefficient_term_label(fit, term_spec$coef_name)
  direct_labels <- labels[grepl("^[.A-Za-z][.A-Za-z0-9_]*$", labels)]
  baseline_labels <- setdiff(direct_labels, target_label)
  target <- if (length(target_label) == 1L && !is.na(target_label) &&
                target_label %in% names(frame)) {
    frame[[target_label]]
  } else {
    NULL
  }
  baseline <- if (length(baseline_labels) == 1L &&
                  baseline_labels %in% names(frame)) {
    frame[[baseline_labels]]
  } else {
    NULL
  }
  omitted <- !is.null(fit$na.action) || stats::nobs(fit) != original_n
  canonical <- identical(term_spec$type, "single") &&
    identical(as.integer(term_spec$ndf), 1L) && is.numeric(response) &&
    is.factor(target) && nlevels(target) == 2L &&
    length(labels) == 2L && length(direct_labels) == 2L &&
    is.numeric(baseline) && !omitted

  list(
    version = "lm-profile-1",
    canonical_ancova = canonical,
    term_type = term_spec$type,
    term_df = as.integer(term_spec$ndf),
    target_term = target_label,
    treatment_levels = if (is.factor(target)) nlevels(target) else NA_integer_,
    baseline_count = as.integer(length(baseline_labels)),
    response_numeric = is.numeric(response),
    baseline_numeric = is.numeric(baseline),
    additive_direct_terms = length(labels) == 2L && length(direct_labels) == 2L,
    omitted_rows = omitted,
    n = as.integer(stats::nobs(fit)),
    alpha = alpha,
    n_boot = as.integer(n_boot),
    weights = weights,
    max_removal_pct = max_removal_pct
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
#'   Robustness calculations use exactly the rows retained by the full fitted
#'   model after its `na.action`. At least one further row must be removable
#'   while retaining the engine's minimum analysis size; otherwise the function
#'   raises an insufficient-sample error rather than returning an unevaluated
#'   fragility score.
#'   Numeric scores and component metrics remain available for every ANCOVA
#'   result. The `lm_ancova` calibration unit is currently uncalibrated, so its
#'   categorical label is suppressed (`NA`) even when the term is significant.
#'
#' @return An object of class `"robustness_model"` (a named list) with:
#' \describe{
#'   \item{original_p, original_estimate, original_significant}{Full-data
#'     term test (`estimate` is `NA` for multi-df joint tests).}
#'   \item{metrics}{Tibble of jackknife / worst-case fragility / bootstrap
#'     component scores and the overall composite. Alias:
#'     `robustness_metrics` (same tibble). Shared metric columns match
#'     [robustness_analysis()] where meanings align.}
#'   \item{interpretation_label}{A calibrated categorical label when a
#'     method-specific calibration is applicable; otherwise `NA`. Scores and
#'     component metrics are retained when the label is suppressed. Alias:
#'     `robustness_interpretation`.}
#'   \item{calibration}{Method-specific calibration metadata, including
#'     applicability, status, cutoffs, version, and provenance.}
#'   \item{original_estimate}{Term estimate (`NA` for multi-df joint tests).
#'     Alias: `original_mean_diff`.}
#'   \item{jackknife, worstcase, bootstrap, removed_rows}{Component analysis
#'     tibbles and the greedy-removal path (flat tibbles; nesting differs
#'     from [robustness_analysis()]).}
#'   \item{term, term_info, sample_info, model, type, n, max_k, alpha,
#'     max_removal_pct, weights}{Model and analysis metadata.}
#' }
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   change = c(rnorm(15, -5), rnorm(15, 0)),
#'   arm = factor(rep(c("Placebo", "Active"), each = 15),
#'                levels = c("Placebo", "Active")),
#'   baseline = rnorm(30, 50, 10)
#' )
#' res <- robustness_lm(change ~ arm + baseline, dat,
#'                      term = "armActive", n_boot = 25, seed = 1)
#' print(res)
#'
#' # Multi-df factor: joint F via drop1()
#' res_joint <- robustness_lm(change ~ arm + baseline, dat,
#'                            term = "arm", n_boot = 25, seed = 1)
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
  analysis_data <- analysis_data_from_fit(data, fit0)
  out <- robustness_engine(analysis_data, fit_fun, alpha, n_boot, max_removal_pct,
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
  out$n_boot <- as.integer(n_boot)
  out$analysis_profile <- lm_calibration_profile(
    fit0, term_spec,
    original_n = nrow(data),
    alpha = alpha,
    n_boot = as.integer(n_boot),
    weights = weights,
    max_removal_pct = max_removal_pct
  )
  out <- attach_result_calibration(
    out,
    calibration_unit = calibration_unit_for_model("lm"),
    endpoint = "coefficient",
    conclusion_type = superiority_conclusion_type(out$original_significant)
  )
  class(out) <- c("robustness_model", "list")
  out
}

# ------------------------------------------------------------------------------
#' Robustness analysis for a Cox proportional hazards term
#'
#' @param formula Survival formula, e.g. survival::Surv(time, event) ~ arm + age
#' @param data Data frame (one row per subject)
#' @param term Term whose p-value defines the conclusion. Either:
#'   - a single coefficient row name from `summary(coxph(...))$coefficients`
#'     (e.g. `"armActive"`), tested with the Wald z p-value; or
#'   - a multi-df term label from `attr(terms(coxph(...)), "term.labels")`
#'     (e.g. a 3-level factor `"arm"`), tested jointly via
#'     `drop1(..., test = "Chisq")` (likelihood-ratio test; survival S3 method).
#'     For multi-df terms the stored `estimate` is `NA` and `term_info` records
#'     the joint LRT (ndf / statistic).
#' @param alpha,n_boot,max_removal_pct,weights,seed As in [robustness_analysis()]
#'
#' @details Single-coefficient `term` strings keep the previous Wald behaviour.
#'   Multi-df factors are detected when `term` matches a term label that expands
#'   to more than one non-aliased design-matrix column (same rules as
#'   [robustness_lm()]). Case deletion/resampling acts on subjects, so censoring
#'   patterns are handled naturally. Removing whole subjects (with their
#'   follow-up) differs from the event-flip fragility of Walsh et al.; interpret
#'   as a removal fragility index.
#'
#'   Robustness calculations use exactly the rows retained by the full fitted
#'   model after its `na.action`. At least one further row must be removable
#'   while retaining the engine's minimum analysis size.
#'   Numeric scores and component metrics remain available for every Cox
#'   result. The `cox_ph` calibration unit is currently uncalibrated, so its
#'   categorical label is suppressed (`NA`) even when the term is significant.
#'
#' @return An object of class `"robustness_model"` (a named list). Same engine
#'   fields as [robustness_lm()] (`original_p`, `metrics`,
#'   `interpretation_label`, jackknife / worst-case / bootstrap tibbles, plus
#'   `term`, `term_info`, `model`, `type`, and related metadata).
#'
#' @examples
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   set.seed(1)
#'   dat <- data.frame(
#'     time = rexp(30, 0.1),
#'     event = rbinom(30, 1, 0.8),
#'     arm = factor(rep(c("A", "B"), each = 15)),
#'     age = rnorm(30, 60, 8)
#'   )
#'   res <- robustness_surv(survival::Surv(time, event) ~ arm + age, dat,
#'                          term = "armB", n_boot = 15, seed = 1)
#'   print(res)
#' }
#' @export
robustness_surv <- function(formula, data, term,
                            alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
                            weights = c(jackknife = 0.4, fragility = 0.4,
                                        bootstrap = 0.2),
                            seed = 123) {

  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required for robustness_surv()")
  }

  # coxph stores `data = <symbol>` and drop1/model.frame evaluate that symbol
  # in the *formula* environment. Localise a copy so lookups hit this frame
  # (or the fit_fun frame) rather than utils::data / a missing symbol.
  fit_cox <- function(d) {
    fml_local <- stats::as.formula(paste(deparse(formula), collapse = " "),
                                   env = environment())
    survival::coxph(fml_local, data = d)
  }

  fit0 <- fit_cox(data)
  term_spec <- resolve_model_term(fit0, term)
  full_test <- surv_term_test(fit0, term_spec)
  if (is.null(full_test) || is.na(full_test$p)) {
    stop("Model could not be fitted on the full dataset", call. = FALSE)
  }
  if (identical(term_spec$type, "joint")) {
    term_spec$statistic <- full_test$statistic
    term_spec$ndf <- as.integer(full_test$ndf)
    term_spec$test <- "joint_LRT"
  } else {
    term_spec$test <- "wald_z"
  }

  fit_fun <- function(d) {
    fit <- tryCatch(fit_cox(d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    surv_term_test(fit, term_spec)
  }
  analysis_data <- analysis_data_from_fit(data, fit0)
  out <- robustness_engine(analysis_data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$term_info <- term_spec
  out$sample_info <- list(
    test = term_spec$test,
    term = term,
    coef_names = term_spec$coef_names,
    ndf = term_spec$ndf,
    statistic = if (is.null(term_spec$statistic)) NA_real_ else term_spec$statistic
  )
  out$model <- paste(deparse(formula), collapse = " ")
  out$type <- "Cox proportional hazards"
  out <- attach_result_calibration(
    out,
    calibration_unit = calibration_unit_for_model("cox"),
    endpoint = "hazard_ratio",
    conclusion_type = superiority_conclusion_type(out$original_significant)
  )
  class(out) <- c("robustness_model", "list")
  out
}

# ------------------------------------------------------------------------------
#' Robustness analysis for a GLM term (binomial logit or Poisson log)
#'
#' @param formula Model formula, e.g. `y ~ arm + x`. Offsets may be supplied
#'   via `offset(...)` in the formula (standard `stats::glm` support).
#' @param data Data frame (one row per subject)
#' @param term Term whose p-value defines the conclusion. Either:
#'   - a single coefficient row name from `summary(glm(...))$coefficients`
#'     (e.g. `"armA"`), tested with the Wald z / t p-value; or
#'   - a multi-df term label from `attr(terms(glm(...)), "term.labels")`
#'     (e.g. a 3-level factor `"arm"`), tested jointly via
#'     `drop1(..., test = "Chisq")` (likelihood-ratio test). For multi-df
#'     terms the stored `estimate` is `NA` and `term_info` records the joint
#'     LRT (ndf / statistic).
#' @param family A family *object*. Supported:
#'   `binomial(link = "logit")` (default) and `poisson(link = "log")`.
#'   Gaussian models should use [robustness_lm()]; other links and
#'   quasi-families are rejected.
#' @param alpha,n_boot,max_removal_pct,weights,seed As in
#'   [robustness_analysis()]. Note that `weights` are **composite score
#'   weights**, not observation weights for `glm()`.
#' @param obs_weights Optional numeric vector of observation (case) weights
#'   of length `nrow(data)`, passed to `stats::glm(..., weights = obs_weights)`.
#'   `NULL` (default) fits an unweighted GLM. Distinct from the composite
#'   score `weights` argument.
#'
#' @details Single-coefficient `term` strings keep the previous Wald behaviour.
#'   Multi-df factors use the same `resolve_model_term()` rules as
#'   [robustness_lm()]. Case deletion / resampling acts on rows of `data`.
#'   Rows omitted by the full fitted model are excluded from all robustness
#'   calculations; `obs_weights` remain aligned through the private row ID.
#'   At least one retained row must be removable while keeping the minimum
#'   analysis size.
#'
#'   Complete or quasi-complete separation is handled only via `converged`
#'   and finite p-values: failed full-data fits error; failed subsets are
#'   skipped. Firth / bias-reduced logistic regression is not supported.
#'   Numeric scores and component metrics remain available for every GLM
#'   result, but the `glm_binomial` and `glm_poisson` units are currently
#'   uncalibrated and their categorical labels are suppressed (`NA`). The
#'   historical Task 15 broad-family score bands are not transferable.
#'
#' @return An object of class `"robustness_model"` (a named list). Same engine
#'   fields as [robustness_lm()], plus `family` and `link` recording the GLM
#'   family used.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   y = rbinom(40, 1, 0.45),
#'   arm = factor(rep(c("A", "B"), each = 20)),
#'   x = rnorm(40)
#' )
#' res <- robustness_glm(y ~ arm + x, dat, term = "armB",
#'                       family = binomial(), n_boot = 20, seed = 1)
#' print(res)
#'
#' # Poisson log-link
#' set.seed(2)
#' count_dat <- data.frame(
#'   count = rpois(40, lambda = 3),
#'   arm = factor(rep(c("A", "B"), each = 20)),
#'   x = rnorm(40)
#' )
#' res_pois <- robustness_glm(count ~ arm + x, count_dat, term = "armB",
#'                            family = poisson(), n_boot = 20, seed = 2)
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
    stop("Quasi-families are not supported (no routine Wald z p-values via summary.glm); use binomial() / poisson() or analyse overdispersion separately",
         call. = FALSE)
  }
  if (identical(fam_name, "binomial") && !identical(link_name, "logit")) {
    stop('robustness_glm() currently supports the logit link only for binomial',
         call. = FALSE)
  }
  if (identical(fam_name, "poisson") && !identical(link_name, "log")) {
    stop('robustness_glm() currently supports the log link only for poisson',
         call. = FALSE)
  }
  if (!((identical(fam_name, "binomial") && identical(link_name, "logit")) ||
        (identical(fam_name, "poisson") && identical(link_name, "log")))) {
    stop('robustness_glm() currently supports binomial(link = "logit") and poisson(link = "log") only',
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

  fit_glm_once <- function(d) {
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
    fit
  }

  fit0 <- fit_glm_once(data)
  if (is.null(fit0)) {
    stop("Model could not be fitted on the full dataset", call. = FALSE)
  }
  term_spec <- resolve_model_term(fit0, term)
  full_test <- glm_term_test(fit0, term_spec)
  if (is.null(full_test) || is.na(full_test$p)) {
    stop("Model could not be fitted on the full dataset", call. = FALSE)
  }
  if (identical(term_spec$type, "joint")) {
    term_spec$statistic <- full_test$statistic
    term_spec$ndf <- as.integer(full_test$ndf)
    term_spec$test <- "joint_LRT"
  } else {
    term_spec$test <- "wald_z"
  }

  fit_fun <- function(d) {
    fit <- fit_glm_once(d)
    if (is.null(fit)) return(NULL)
    glm_term_test(fit, term_spec)
  }

  analysis_data <- analysis_data_from_fit(data, fit0)
  out <- robustness_engine(analysis_data, fit_fun, alpha, n_boot, max_removal_pct,
                           weights, seed)
  out$term <- term
  out$term_info <- term_spec
  out$sample_info <- list(
    test = term_spec$test,
    term = term,
    coef_names = term_spec$coef_names,
    ndf = term_spec$ndf,
    statistic = if (is.null(term_spec$statistic)) NA_real_ else term_spec$statistic
  )
  out$model <- paste(deparse(formula), collapse = " ")
  out$family <- fam_name
  out$link <- link_name
  out$type <- sprintf("GLM (%s, %s)", fam_name, link_name)
  out <- attach_result_calibration(
    out,
    calibration_unit = calibration_unit_for_model("glm", family = fam_name),
    endpoint = "coefficient",
    conclusion_type = superiority_conclusion_type(out$original_significant)
  )
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
    test_lab <- if (identical(ti$test, "joint_F")) "joint F" else "joint LRT"
    cat(sprintf("ORIGINAL: %s (ndf = %d", test_lab, ti$ndf))
    if (!is.null(ti$ddf) && !is.na(ti$ddf)) {
      cat(sprintf(", ddf = %d", ti$ddf))
    }
    if (!is.null(ti$statistic) && !is.na(ti$statistic)) {
      stat_lab <- if (identical(ti$test, "joint_F")) "F" else "LRT"
      cat(sprintf(", %s = %.3f", stat_lab, ti$statistic))
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
  if (identical(x$type, "Cox proportional hazards") &&
      !is.null(ti) && identical(ti$type, "single") &&
      !is.na(x$original_estimate)) {
    cat(sprintf("          HR = %.3f\n", exp(x$original_estimate)))
  } else if (identical(x$type, "Cox proportional hazards") &&
             is.null(ti) && !is.na(x$original_estimate)) {
    # Backward-compatible objects without term_info
    cat(sprintf("          HR = %.3f\n", exp(x$original_estimate)))
  }
  if (!is.null(x$family) && !is.na(x$original_estimate) &&
      (is.null(ti) || identical(ti$type, "single"))) {
    if (identical(x$family, "binomial") && identical(x$link, "logit")) {
      cat(sprintf("          OR = %.3f\n", exp(x$original_estimate)))
    } else if (identical(x$family, "poisson") && identical(x$link, "log")) {
      cat(sprintf("          IRR = %.3f\n", exp(x$original_estimate)))
    }
  }
  cat(sprintf("\nOVERALL ROBUSTNESS: %s\n\n",
              format_score_interpretation(
                m$overall_robustness,
                x[["calibration"]],
                x$interpretation_label
              )))
  print_robustness_components(x, m, p_label = "p")
  cat("\n")
  if (length(x$removed_rows) > 0 && m$worstcase_fragility_k <= x$max_k) {
    cat("Rows removed by worst-case analysis (in order):\n  ")
    cat(x$removed_rows[seq_len(m$worstcase_fragility_k)], sep = ", ")
    cat("\n  -> review these subjects for data quality / clinical plausibility\n")
  }
  invisible(x)
}
