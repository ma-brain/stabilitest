# Calibration generators and thin screening adapters for model-based analyses.
#
# The generators deliberately return metadata alongside the data.  Calibration
# runners use that metadata to classify truth and to retain failures instead of
# silently dropping datasets that cannot be fitted.

.scenario_parts <- function(x) {
  if (is.data.frame(x)) {
    if (nrow(x) != 1L) stop("model scenario must contain one row", call. = FALSE)
    out <- as.list(x[1L, , drop = FALSE])
    out$parameters <- x$parameters[[1L]]
    out$analysis_family <- as.character(x$analysis_family[[1L]])
    return(out)
  }
  x
}

.model_params <- function(x = list()) {
  x <- .scenario_parts(x)
  if (!is.list(x)) stop("model generator parameters must be a list", call. = FALSE)
  if (!is.null(x$parameters) && is.list(x$parameters)) x <- x$parameters
  if (!is.null(x$generator) && is.list(x$generator)) x <- x$generator
  x
}

.model_arg <- function(x, names, default = NULL) {
  hit <- names[names %in% names(x)]
  if (length(hit)) x[[hit[[1L]]]] else default
}

.model_analysis <- function(scenario, term = NULL, alpha = 0.05) {
  scenario <- .scenario_parts(scenario)
  a <- if (is.list(scenario) && is.list(scenario$analysis)) scenario$analysis else list()
  if (is.list(scenario) && is.list(scenario$parameters) &&
      is.list(scenario$parameters$analysis)) {
    a <- utils::modifyList(a, scenario$parameters$analysis)
  }
  family <- scenario$analysis_family %||% a$family %||% NULL
  list(
    term = if (is.null(term)) a$term %||% "treatment" else term,
    alpha = as.numeric(a$alpha %||% alpha),
    formula = a$formula,
    family = family
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.factor_levels <- function(k) {
  k <- max(2L, as.integer(k %||% 2L))
  head(c("A", "B", "C", "D", "E", "F"), k)
}

.assignment <- function(n, levels, imbalance = 0) {
  imbalance <- max(-0.9, min(0.9, as.numeric(imbalance %||% 0)))
  k <- length(levels)
  probs <- if (k == 2L) c(0.5 - imbalance / 2, 0.5 + imbalance / 2) else {
    p <- rep((1 - abs(imbalance)) / k, k)
    p[2L] <- p[2L] + abs(imbalance)
    p / sum(p)
  }
  factor(sample(levels, n, replace = TRUE, prob = probs), levels = levels)
}

.truth_row <- function(data, truth, status = "ok", failure = NULL, weights = NULL) {
  if (!".row_id" %in% names(data)) data$.row_id <- seq_len(nrow(data))
  list(
    data = as.data.frame(data),
    truth = truth,
    truth_metadata = truth,
    weights = weights,
    status = status,
    failed = !identical(status, "ok"),
    failure = failure,
    failure_class = if (is.null(failure)) NA_character_ else failure$class,
    failure_message = if (is.null(failure)) NA_character_ else failure$message
  )
}

#' Generate a linear/ANCOVA calibration dataset.
#' @export
generate_lm <- function(scenario = list(), seed = NULL, ...) {
  p <- .model_params(utils::modifyList(.model_params(scenario), list(...)))
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- max(12L, as.integer(.model_arg(p, c("n", "sample_size"), 80L)))
  levels <- .factor_levels(.model_arg(p, "factor_levels", 2L))
  imbalance <- as.numeric(.model_arg(p, c("imbalance", "allocation_imbalance"), 0))
  treatment <- .assignment(n, levels, imbalance)
  baseline <- rnorm(n, 0, 1)
  effect <- as.numeric(.model_arg(p, c("effect", "effect_size", "treatment_effect"), 0.5))
  cov_effect <- as.numeric(.model_arg(p, c("covariate_effect", "prognostic_effect"), 0.3))
  sigma <- as.numeric(.model_arg(p, c("noise_sd", "sd", "residual_sd"), 1))
  linear_effect <- effect * (as.integer(treatment) - 1L)
  hetero <- isTRUE(.model_arg(p, c("heteroscedastic", "heteroskedastic"), FALSE))
  hetero_strength <- as.numeric(.model_arg(p, "hetero_strength", 0.7))
  scale <- if (hetero) sigma * (1 + hetero_strength * abs(baseline)) else rep(sigma, n)
  outcome <- as.numeric(.model_arg(p, "intercept", 0)) + linear_effect + cov_effect * baseline +
    rnorm(n, 0, scale)
  data <- data.frame(
    .row_id = seq_len(n), outcome = outcome, treatment = treatment, baseline = baseline
  )
  missing_rate <- max(0, min(0.95, as.numeric(.model_arg(p, "missing_rate", 0))))
  missing_ids <- integer()
  if (missing_rate > 0) {
    m <- max(1L, floor(n * missing_rate))
    missing_ids <- sample(seq_len(n), m)
    data$baseline[missing_ids] <- NA_real_
  }
  term <- if (length(levels) > 2L) "treatment" else "treatmentB"
  truth <- list(
    family = "lm", term = term, effect = effect, covariate_effect = cov_effect,
    factor_levels = levels, imbalance = imbalance,
    prognostic = abs(cov_effect) > 0, heteroscedastic = hetero,
    missing_rate = missing_rate, missing_row_ids = missing_ids,
    row_id = data$.row_id, target = if (effect == 0) "null" else "effect"
  )
  .truth_row(data, truth)
}

#' Generate a binomial-logit calibration dataset.
#' @export
generate_binomial <- function(scenario = list(), seed = NULL, ...) {
  p <- .model_params(utils::modifyList(.model_params(scenario), list(...)))
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- max(12L, as.integer(.model_arg(p, c("n", "sample_size"), 100L)))
  levels <- .factor_levels(.model_arg(p, "factor_levels", 2L))
  imbalance <- as.numeric(.model_arg(p, c("imbalance", "allocation_imbalance"), 0))
  treatment <- .assignment(n, levels, imbalance)
  baseline <- rnorm(n)
  prevalence <- as.numeric(.model_arg(p, c("prevalence", "baseline_prevalence"), 0.35))
  prevalence <- max(0.01, min(0.99, prevalence))
  intercept_arg <- .model_arg(p, c("intercept", "baseline_log_odds"), NULL)
  intercept <- if (is.null(intercept_arg)) qlogis(prevalence) else as.numeric(intercept_arg)
  effect_arg <- .model_arg(p, c("effect", "treatment_log_odds", "log_odds", "log_or"), NULL)
  effect <- if (is.null(effect_arg)) {
    log(as.numeric(.model_arg(p, c("odds_ratio", "or"), 1.5)))
  } else as.numeric(effect_arg)
  cov_effect <- as.numeric(.model_arg(p, c("covariate_effect", "prognostic_effect"), 0.25))
  sep <- isTRUE(.model_arg(p, c("separation", "complete_separation"), FALSE))
  eta <- intercept + effect * (as.integer(treatment) - 1L) + cov_effect * baseline
  probability <- plogis(eta)
  outcome <- if (sep) as.integer(treatment != levels[[1L]]) else rbinom(n, 1L, probability)
  data <- data.frame(.row_id = seq_len(n), outcome = outcome,
                     treatment = treatment, baseline = baseline)
  weights <- .model_arg(p, "weights", NULL)
  if (isTRUE(weights)) weights <- sample(1:3, n, replace = TRUE)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (length(weights) != n || any(!is.finite(weights)) || any(weights <= 0)) {
      stop("weights must be positive and have length n", call. = FALSE)
    }
  }
  truth <- list(
    family = "binomial", term = if (length(levels) > 2L) "treatment" else "treatmentB",
    effect = effect, intercept = intercept, odds_ratio = exp(effect), prevalence = prevalence,
    covariate_effect = cov_effect, factor_levels = levels, imbalance = imbalance,
    separation = sep, degenerate_outcome = length(unique(outcome)) < 2L,
    row_id = data$.row_id, target = if (effect == 0) "null" else "effect"
  )
  .truth_row(data, truth, weights = weights)
}

#' Generate a Poisson-log calibration dataset with optional exposure offsets.
#' @export
generate_poisson <- function(scenario = list(), seed = NULL, ...) {
  p <- .model_params(utils::modifyList(.model_params(scenario), list(...)))
  if (!is.null(seed)) set.seed(as.integer(seed))
  n <- max(12L, as.integer(.model_arg(p, c("n", "sample_size"), 100L)))
  levels <- .factor_levels(.model_arg(p, "factor_levels", 2L))
  imbalance <- as.numeric(.model_arg(p, c("imbalance", "allocation_imbalance"), 0))
  treatment <- .assignment(n, levels, imbalance)
  baseline <- rnorm(n)
  exposure_on <- !identical(.model_arg(p, c("exposure", "offset"), FALSE), FALSE)
  exposure <- if (exposure_on) runif(n, 0.5, 2.5) else rep(1, n)
  rate <- as.numeric(.model_arg(p, c("rate", "baseline_rate"), 0.8))
  intercept_arg <- .model_arg(p, c("intercept", "baseline_log_rate"), NULL)
  intercept <- if (is.null(intercept_arg)) log(rate) else as.numeric(intercept_arg)
  effect_arg <- .model_arg(p, c("effect", "treatment_log_rate", "log_rate", "log_irr"), NULL)
  effect <- if (is.null(effect_arg)) {
    log(as.numeric(.model_arg(p, c("incidence_rate_ratio", "irr"), 1.5)))
  } else as.numeric(effect_arg)
  cov_effect <- as.numeric(.model_arg(p, c("covariate_effect", "prognostic_effect"), 0.15))
  eta <- intercept + effect * (as.integer(treatment) - 1L) + cov_effect * baseline + log(exposure)
  overdisp <- isTRUE(.model_arg(p, c("overdispersion", "overdispersed"), FALSE))
  lambda <- exp(eta)
  if (overdisp) lambda <- lambda * rgamma(n, shape = 4, rate = 4)
  outcome <- rpois(n, lambda)
  data <- data.frame(.row_id = seq_len(n), outcome = outcome,
                     treatment = treatment, baseline = baseline, exposure = exposure)
  weights <- .model_arg(p, "weights", NULL)
  if (isTRUE(weights)) weights <- sample(1:3, n, replace = TRUE)
  if (!is.null(weights)) {
    weights <- as.numeric(weights)
    if (length(weights) != n || any(!is.finite(weights)) || any(weights <= 0)) {
      stop("weights must be positive and have length n", call. = FALSE)
    }
  }
  truth <- list(
    family = "poisson", term = if (length(levels) > 2L) "treatment" else "treatmentB",
    effect = effect, intercept = intercept, rate = rate, incidence_rate_ratio = exp(effect),
    covariate_effect = cov_effect, factor_levels = levels, imbalance = imbalance,
    exposure = exposure_on, exposure_range = range(exposure), overdispersion = overdisp,
    degenerate_outcome = length(unique(outcome)) < 2L,
    row_id = data$.row_id, target = if (effect == 0) "null" else "effect"
  )
  .truth_row(data, truth, weights = weights)
}

.model_formula <- function(data, scenario, formula = NULL, family = NULL) {
  if (!is.null(formula)) return(formula)
  a <- .model_analysis(scenario)
  if (!is.null(a$formula)) return(stats::as.formula(a$formula))
  if (identical(family, "poisson") && "exposure" %in% names(data)) {
    return(outcome ~ treatment + baseline + offset(log(exposure)))
  }
  outcome ~ treatment + baseline
}

.model_failure <- function(class, message, fit = NULL, data = NULL, stage = "screening") {
  list(
    status = "failed", failed = TRUE, failure_class = as.character(class),
    failure_message = as.character(message), failure = list(class = class, message = message),
    failure_stage = stage,
    fit = fit, n = if (is.null(data)) NA_integer_ else nrow(data), p_value = NA_real_,
    estimate = NA_real_, conclusion = NA
  )
}

.model_success <- function(test, fit, term_spec, data, weights = NULL) {
  omitted <- fit$na.action
  omitted_ids <- if (is.null(omitted)) integer() else data$.row_id[as.integer(omitted)]
  list(
    status = "ok", failed = FALSE, failure_class = NA_character_,
    failure_message = NA_character_, failure = NULL, fit = fit,
    p_value = unname(test$p), estimate = unname(test$estimate),
    conclusion = isTRUE(test$p < 0.05), significant = isTRUE(test$p < 0.05),
    term_info = term_spec, n = stats::nobs(fit), omitted_row_ids = omitted_ids,
    row_ids = data$.row_id, weights = weights
  )
}

.namespace_function <- function(name) {
  if (requireNamespace("stabilitest", quietly = TRUE) &&
      exists(name, envir = asNamespace("stabilitest"), inherits = FALSE)) {
    get(name, envir = asNamespace("stabilitest"), inherits = FALSE)
  } else NULL
}

.safe_term_test <- function(fit, term, family) {
  resolve <- .namespace_function("resolve_model_term")
  test_fun <- .namespace_function(if (identical(family, "lm")) "lm_term_test" else "glm_term_test")
  if (is.null(resolve) || is.null(test_fun)) {
    stop("stabilitest model term helpers are unavailable; load the package first", call. = FALSE)
  }
  spec <- resolve(fit, term)
  result <- test_fun(fit, spec)
  list(result = result, spec = spec)
}

.screen_lm_fit <- function(data, formula, term, alpha = 0.05) {
  data <- as.data.frame(data)
  if (!".row_id" %in% names(data)) data$.row_id <- seq_len(nrow(data))
  fit <- tryCatch(stats::lm(formula, data = data), error = function(e) e)
  if (inherits(fit, "error")) return(.model_failure("fit_error", conditionMessage(fit), data = data))
  if (anyNA(stats::coef(fit))) {
    return(.model_failure("aliased_term", "linear model contains aliased coefficients", fit, data))
  }
  resolved <- tryCatch(.safe_term_test(fit, term, "lm"), error = function(e) e)
  if (inherits(resolved, "error")) {
    cls <- if (grepl("aliased|estimable|coefficient", conditionMessage(resolved), ignore.case = TRUE)) "aliased_term" else "term_error"
    return(.model_failure(cls, conditionMessage(resolved), fit, data))
  }
  if (is.null(resolved$result) || !is.finite(resolved$result$p)) {
    return(.model_failure("aliased_term", "term has no finite test statistic", fit, data))
  }
  out <- .model_success(resolved$result, fit, resolved$spec, data)
  out$conclusion <- isTRUE(resolved$result$p < alpha)
  out$significant <- out$conclusion
  out$alpha <- alpha
  out
}

#' Screen a linear-model dataset using the same term test as robustness_lm().
#' @export
primary_decision_lm <- function(data, scenario = list(), term = NULL,
                                formula = NULL, alpha = NULL) {
  a <- .model_analysis(scenario, term = term, alpha = alpha %||% 0.05)
  .screen_lm_fit(data, .model_formula(data, scenario, formula, "lm"), a$term, a$alpha)
}

.fit_glm_calibration <- function(data, formula, family, obs_weights = NULL, control = NULL) {
  d <- as.data.frame(data)
  if (!".row_id" %in% names(d)) d$.row_id <- seq_len(nrow(d))
  if (!is.null(obs_weights)) d$.__obs_w__ <- obs_weights
  warnings <- character()
  fit <- withCallingHandlers(
    tryCatch(
      if (is.null(obs_weights) && is.null(control)) stats::glm(formula, data = d, family = family)
      else if (is.null(obs_weights)) stats::glm(formula, data = d, family = family, control = control)
      else if (is.null(control)) stats::glm(formula, data = d, family = family, weights = .__obs_w__)
      else stats::glm(formula, data = d, family = family, weights = .__obs_w__, control = control),
      error = function(e) e
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = warnings, data = d)
}

.screen_glm_fit <- function(data, formula, term, family, alpha = 0.05,
                            obs_weights = NULL, control = NULL) {
  data <- as.data.frame(data)
  if (!".row_id" %in% names(data)) data$.row_id <- seq_len(nrow(data))
  fam <- if (is.character(family)) as.character(family[[1L]]) else family$family
  if (is.character(family)) family <- switch(fam, binomial = stats::binomial(), poisson = stats::poisson(), family)
  if (!fam %in% c("binomial", "poisson")) return(.model_failure("unsupported_family", "only binomial and poisson are supported", data = data))
  expected_link <- if (identical(fam, "binomial")) "logit" else "log"
  if (!identical(family$link, expected_link)) {
    return(.model_failure("unsupported_link", sprintf("%s models require the %s link", fam, expected_link), data = data))
  }
  if (!is.null(obs_weights) &&
      (!is.numeric(obs_weights) || length(obs_weights) != nrow(data) ||
       any(!is.finite(obs_weights)) || any(obs_weights <= 0))) {
    return(.model_failure("invalid_weights", "obs_weights must be positive and aligned with data rows", data = data))
  }
  if (identical(fam, "binomial")) {
    y <- data$outcome
    if (length(unique(y[!is.na(y)])) < 2L) return(.model_failure("degenerate_outcome", "binomial outcome has fewer than two observed levels", data = data))
  }
  fitted <- .fit_glm_calibration(data, formula, family, obs_weights, control)
  fit <- fitted$fit
  if (inherits(fit, "error")) return(.model_failure("fit_error", conditionMessage(fit), data = data))
  if (!isTRUE(fit$converged)) return(.model_failure("non_convergence", "GLM did not converge", fit, data))
  if (anyNA(stats::coef(fit))) return(.model_failure("aliased_term", "GLM contains aliased coefficients", fit, data))
  if (identical(fam, "binomial") && (any(grepl("probabilities numerically 0 or 1", fitted$warnings)) || any(abs(stats::coef(fit)) > 30))) {
    return(.model_failure("separation", "binomial GLM indicates complete or quasi-complete separation", fit, data))
  }
  resolved <- tryCatch(.safe_term_test(fit, term, fam), error = function(e) e)
  if (inherits(resolved, "error")) return(.model_failure("term_error", conditionMessage(resolved), fit, data))
  if (is.null(resolved$result) || !is.finite(resolved$result$p)) return(.model_failure("aliased_term", "term has no finite test statistic", fit, data))
  out <- .model_success(resolved$result, fit, resolved$spec, data, obs_weights)
  out$conclusion <- isTRUE(resolved$result$p < alpha)
  out$significant <- out$conclusion
  out$alpha <- alpha
  out$warnings <- fitted$warnings
  out
}

#' Screen a binomial-logit or Poisson-log dataset using robustness_glm().
#' @export
primary_decision_glm <- function(data, scenario = list(), term = NULL,
                                 family = NULL, formula = NULL,
                                 alpha = NULL, obs_weights = NULL, control = NULL) {
  a <- .model_analysis(scenario, term = term, alpha = alpha %||% 0.05)
  if (is.null(family)) {
    family <- if (identical(a$family, "poisson")) stats::poisson() else stats::binomial()
  }
  fam_name <- if (is.character(family)) family else family$family
  .screen_glm_fit(data, .model_formula(data, scenario, formula, fam_name),
                  a$term, family, a$alpha, obs_weights, control)
}

run_robustness_lm <- function(data, scenario = list(), formula = NULL, term = NULL, ...) {
  a <- .model_analysis(scenario, term = term)
  tryCatch(
    do.call(stabilitest::robustness_lm, c(list(
      formula = .model_formula(data, scenario, formula, "lm"), data = data, term = a$term
    ), list(...))),
    error = function(e) .model_failure("analysis_error", conditionMessage(e), data = data, stage = "robustness")
  )
}

run_robustness_glm <- function(data, scenario = list(), formula = NULL, term = NULL,
                               family = NULL, obs_weights = NULL, ...) {
  a <- .model_analysis(scenario, term = term)
  if (is.null(family)) {
    family <- if (identical(a$family, "poisson")) stats::poisson() else stats::binomial()
  }
  fam_name <- if (is.character(family)) family[[1L]] else family$family
  if (is.character(family)) family <- switch(fam_name, binomial = stats::binomial(), poisson = stats::poisson(), family)
  expected_link <- if (identical(fam_name, "binomial")) "logit" else if (identical(fam_name, "poisson")) "log" else NA_character_
  if (!is.na(expected_link) && !identical(family$link, expected_link)) {
    return(.model_failure("unsupported_link", sprintf("%s models require the %s link", fam_name, expected_link), data = data, stage = "robustness"))
  }
  tryCatch(
    do.call(stabilitest::robustness_glm, c(list(
      formula = .model_formula(data, scenario, formula, fam_name),
      data = data, term = a$term, family = family, obs_weights = obs_weights
    ), list(...))),
    error = function(e) .model_failure("analysis_error", conditionMessage(e), data = data, stage = "robustness")
  )
}

lm_model_adapter <- function() list(
  generate = generate_lm, generate_data = generate_lm,
  primary_decision = primary_decision_lm,
  run_robustness = run_robustness_lm,
  robustness_analysis = run_robustness_lm
)

glm_model_adapter <- function(family = stats::binomial()) list(
  generate = if (identical(if (is.character(family)) family[[1L]] else family$family, "poisson")) generate_poisson else generate_binomial,
  generate_data = if (identical(if (is.character(family)) family[[1L]] else family$family, "poisson")) generate_poisson else generate_binomial,
  primary_decision = function(data, scenario, ...) primary_decision_glm(data, scenario, family = family, ...),
  run_robustness = function(data, scenario, ...) run_robustness_glm(data, scenario, family = family, ...),
  robustness_analysis = function(data, scenario, ...) run_robustness_glm(data, scenario, family = family, ...)
)

calibration_model_adapters <- function() list(
  lm = lm_model_adapter(), binomial = glm_model_adapter(stats::binomial()),
  poisson = glm_model_adapter(stats::poisson())
)

# Friendly aliases used by calibration runners.
lm_adapter <- lm_model_adapter
glm_adapter <- glm_model_adapter
generate_lm_ancova <- generate_lm
screen_lm <- primary_decision_lm
screen_glm <- primary_decision_glm

# Generic hooks used by the screening runner.  They intentionally dispatch
# only model families; the two-sample and survival adapters provide their own
# family-specific hooks in later calibration tasks.
primary_decision <- function(data, scenario, ...) {
  family <- if (is.list(scenario)) scenario$analysis_family else NULL
  if (length(family) != 1L || is.na(family)) {
    family <- if (is.list(scenario$parameters) && !is.null(scenario$parameters$analysis$family)) {
      scenario$parameters$analysis$family
    } else "lm"
  }
  switch(as.character(family),
         lm = primary_decision_lm(data, scenario, ...),
         binomial = primary_decision_glm(data, scenario, family = stats::binomial(), ...),
         poisson = primary_decision_glm(data, scenario, family = stats::poisson(), ...),
         stop(sprintf("No model adapter for analysis family '%s'", family), call. = FALSE))
}

calibration_robustness_analysis <- function(data, scenario, ...) {
  family <- if (is.list(scenario)) scenario$analysis_family else NULL
  if (length(family) != 1L || is.na(family)) family <- "lm"
  switch(as.character(family),
         lm = run_robustness_lm(data, scenario, ...),
         binomial = run_robustness_glm(data, scenario, family = stats::binomial(), ...),
         poisson = run_robustness_glm(data, scenario, family = stats::poisson(), ...),
         stop(sprintf("No model adapter for analysis family '%s'", family), call. = FALSE))
}
