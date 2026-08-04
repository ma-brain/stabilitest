# Calibration adapter for Cox proportional-hazards endpoints.

.cox_default_formula <- function() survival::Surv(time, event) ~ treatment

#' Generate a deterministic time-to-event calibration sample.
#' @export
generate_cox <- function(n = 160, hazard_ratio = 1.5, censoring_rate = 0.2,
                         event_rate = NULL, distribution = c("exponential", "weibull"),
                         shape = 1, scale = 10, seed = NULL,
                         time_varying_effect = FALSE, censoring_seed = NULL,
                         ...) {
  if (!requireNamespace("survival", quietly = TRUE))
    stop("Package 'survival' is required for generate_cox()", call. = FALSE)
  distribution <- match.arg(distribution)
  if (length(n) != 1L || n < 10 || n != as.integer(n)) stop("n must be an integer >= 10", call. = FALSE)
  if (!is.numeric(censoring_rate) || length(censoring_rate) != 1L || censoring_rate < 0 || censoring_rate > 1)
    stop("censoring_rate must be in [0, 1]", call. = FALSE)
  if (!is.numeric(hazard_ratio) || length(hazard_ratio) != 1L || hazard_ratio <= 0)
    stop("hazard_ratio must be positive", call. = FALSE)
  if (!is.null(seed)) set.seed(seed)
  treatment <- factor(rep(c("control", "treatment"), length.out = n),
                      levels = c("control", "treatment"))
  lp <- if (isTRUE(time_varying_effect)) log(hazard_ratio) * (1 + seq_len(n) / n) else log(hazard_ratio)
  arm_rate <- exp(lp * (treatment == "treatment"))
  event_time <- if (distribution == "exponential") {
    rexp(n, rate = arm_rate / scale)
  } else {
    rweibull(n, shape = shape, scale = scale / arm_rate^(1 / shape))
  }
  if (is.null(censoring_seed)) censoring_seed <- if (is.null(seed)) NULL else as.integer(seed) + 100000L
  if (!is.null(censoring_seed)) set.seed(censoring_seed)
  if (!is.null(event_rate)) {
    if (!is.numeric(event_rate) || length(event_rate) != 1L || event_rate < 0 || event_rate > 1)
      stop("event_rate must be in [0, 1]", call. = FALSE)
    event <- rbinom(n, 1, event_rate)
    censor_time <- event_time
    observed_time <- event_time
  } else if (censoring_rate >= 1) {
    event <- integer(n)
    censor_time <- rexp(n, rate = 1 / scale)
    observed_time <- censor_time
  } else if (censoring_rate <= 0) {
    event <- rep.int(1L, n)
    censor_time <- rep.int(Inf, n)
    observed_time <- event_time
  } else {
    # Draw an exact (up to rounding) number of censored records, with the
    # censoring seed controlling both the selected records and their times.
    # This preserves the configured fraction while avoiding the old reversed
    # exponential-rate parameterization and keeps censoring_seed meaningful.
    n_censored <- as.integer(round(censoring_rate * n))
    censored <- rep.int(FALSE, n)
    if (n_censored > 0L) censored[sample.int(n, n_censored)] <- TRUE
    event <- as.integer(!censored)
    censor_time <- rep.int(Inf, n)
    if (n_censored > 0L) {
      censor_time[censored] <- event_time[censored] * stats::runif(
        n_censored, min = .05, max = .95
      )
    }
    observed_time <- ifelse(censored, censor_time, event_time)
  }
  dat <- data.frame(time = pmax(observed_time, .Machine$double.eps),
                    event = event, treatment = treatment)
  attr(dat, "calibration_metadata") <- list(
    distribution = distribution, shape = shape, hazard_ratio = hazard_ratio,
    censoring_rate = censoring_rate, event_rate = mean(event),
    realized_censoring_rate = mean(event == 0L),
    time_varying_effect = isTRUE(time_varying_effect),
    stress = if (isTRUE(time_varying_effect)) "non_ph" else NULL,
    seed = seed, censoring_seed = censoring_seed,
    failure_hint = if (!any(event)) if (isTRUE(censoring_rate >= 1)) "all_censored" else "no_event" else NULL
  )
  dat
}

generate_survival <- generate_cox

.cox_resolve <- function(formula, data, term) {
  formula <- stats::as.formula(paste(deparse(formula), collapse = " "), env = environment())
  fit <- survival::coxph(formula, data = data)
  if (sum(data$event > 0, na.rm = TRUE) == 0L) stop("no event observations", call. = FALSE)
  if (length(unique(data$event[!is.na(data$event)])) < 2L && any(data$event == 0))
    stop("all observations are censored", call. = FALSE)
  if (any(!is.finite(stats::coef(fit))) || (!is.null(fit$iter) && fit$iter >= 20L))
    stop("nonconvergent Cox model", call. = FALSE)
  spec <- stabilitest:::resolve_model_term(fit, term)
  test <- stabilitest:::surv_term_test(fit, spec)
  if (is.null(test) && identical(spec$type, "joint")) {
    d1 <- tryCatch(stats::drop1(fit, scope = stats::as.formula(paste("~", term)),
                                test = "Chisq"), error = function(e) NULL)
    if (!is.null(d1) && term %in% rownames(d1)) {
      test <- list(p = unname(d1[term, "Pr(>Chi)"]), estimate = NA_real_,
                   statistic = unname(d1[term, "LRT"]), ndf = unname(d1[term, "Df"]))
    }
    if (is.null(test) && length(attr(stats::terms(fit), "term.labels")) == 1L) {
      lt <- fit$logtest
      if (!is.null(lt) && length(lt) >= 3L)
        test <- list(p = unname(lt[[3L]]), estimate = NA_real_,
                     statistic = unname(lt[[1L]]), ndf = unname(lt[[2L]]))
    }
  }
  if (is.null(test)) stop("nonconvergent Cox model", call. = FALSE)
  if (length(test$p) != 1L || is.na(test$p)) stop("nonconvergent Cox model", call. = FALSE)
  list(fit = fit, spec = spec, test = test)
}

#' Run the ordinary Cox screening test used as the calibration comparator.
#' @export
screen_cox <- function(data, term = "treatment", formula = NULL, alpha = 0.05) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Package 'survival' is required", call. = FALSE)
  if (is.null(formula)) formula <- .cox_default_formula()
  resolved <- .cox_resolve(formula, data, term)
  spec <- resolved$spec
  if (identical(spec$type, "joint")) {
    spec$test <- "joint_LRT"
    spec$ndf <- resolved$test$ndf
  } else spec$test <- "wald_z"
  list(p = unname(resolved$test$p), original_p = unname(resolved$test$p),
       estimate = unname(resolved$test$estimate),
       conclusion = if (resolved$test$p < alpha) "significant" else "non_significant",
       significant = resolved$test$p < alpha, test = spec$test,
       ndf = spec$ndf, term_info = spec, status = "completed")
}
screen_survival <- screen_cox

#' Execute screening and robustness for one Cox calibration replicate.
#' @export
run_cox_adapter <- function(data, term = "treatment", formula = NULL, alpha = 0.05,
                            n_boot = 1000, max_removal_pct = 0.30, seed = 123, ...) {
  if (is.null(formula)) formula <- .cox_default_formula()
  out <- tryCatch({
    screening <- screen_cox(data, term = term, formula = formula, alpha = alpha)
    analysis <- do.call(stabilitest::robustness_surv,
      c(list(formula = formula, data = data, term = term, alpha = alpha,
             n_boot = n_boot, max_removal_pct = max_removal_pct, seed = seed), list(...)))
    list(status = "completed", screening = screening, analysis = analysis,
         original_p = analysis$original_p, effective_p = analysis$original_p,
         failure_stage = NA_character_, failure_class = NA_character_, failure_message = NA_character_)
  }, error = function(e) {
    metadata <- attr(data, "calibration_metadata")
    hint <- if (is.list(metadata)) metadata$failure_hint else NULL
    cls <- if (!is.null(hint)) hint else if (grepl("all.*censor", conditionMessage(e), ignore.case = TRUE)) "all_censored" else if (grepl("event", conditionMessage(e), ignore.case = TRUE)) "no_event" else if (grepl("converg|finite", conditionMessage(e), ignore.case = TRUE)) "nonconvergent" else if (grepl("level|factor|term", conditionMessage(e), ignore.case = TRUE)) "vanished_factor" else "model_fit"
    list(status = "failed", screening = NULL, analysis = NULL,
         original_p = NA_real_, effective_p = NA_real_, failure_stage = "screening",
         failure_class = cls, failure_message = conditionMessage(e))
  })
  out
}

calibrate_survival <- run_cox_adapter
