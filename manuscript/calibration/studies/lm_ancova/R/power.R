# Analytic ANCOVA power for the balanced two-arm additive model.

ancova_nominal_power <- function(beta, n, alpha = 0.05, residual_sd = 1,
                                 allocation = 0.5) {
  n <- as.integer(n)
  n1 <- as.integer(round(n * allocation))
  n0 <- n - n1
  if (n1 < 2L || n0 < 2L) {
    stop("allocation must leave at least two observations per arm", call. = FALSE)
  }
  df <- n - 3L
  se <- residual_sd * sqrt(1 / n1 + 1 / n0)
  ncp <- beta / se
  critical <- stats::qt(1 - alpha / 2, df)
  stats::pt(-critical, df, ncp = ncp) +
    1 - stats::pt(critical, df, ncp = ncp)
}

solve_ancova_effect <- function(n, target_power, alpha = 0.05,
                                residual_sd = 1, allocation = 0.5) {
  target_power <- as.numeric(target_power)
  if (identical(target_power, 0) || isTRUE(all.equal(target_power, alpha))) {
    return(0)
  }
  if (!(target_power > alpha && target_power < 1)) {
    stop("target_power must be 0 or in (alpha, 1)", call. = FALSE)
  }
  stats::uniroot(
    function(beta) {
      ancova_nominal_power(
        beta, n, alpha = alpha, residual_sd = residual_sd,
        allocation = allocation
      ) - target_power
    },
    interval = c(0, 10 * residual_sd),
    tol = .Machine$double.eps^0.75
  )$root
}

verify_ancova_power <- function(scenario, draws, seed) {
  scenario <- if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) stop("scenario must contain one row", call. = FALSE)
    as.list(scenario[1L, , drop = FALSE])
  } else {
    scenario
  }
  parameters <- scenario$parameters
  if (is.list(parameters) && length(parameters) == 1L && is.list(parameters[[1L]])) {
    parameters <- parameters[[1L]]
  }
  generator <- parameters$generator %||% list()
  analysis <- parameters$analysis %||% list()
  alpha <- as.numeric(analysis$alpha %||% 0.05)
  term <- as.character(analysis$term %||% "treatmentB")
  formula <- stats::as.formula(analysis$formula %||% "outcome ~ treatment + baseline")
  target_power <- as.numeric(generator$target_power %||% 0)
  draws <- as.integer(draws)
  set.seed(as.integer(seed))
  replicate_seeds <- sample.int(.Machine$integer.max, draws)

  significant <- logical(draws)
  for (i in seq_len(draws)) {
    generated <- generate_lm_ancova(scenario, seed = replicate_seeds[[i]])
    fit <- stats::lm(formula, data = generated$data)
    ct <- stats::coef(summary(fit))
    if (!term %in% rownames(ct)) {
      significant[[i]] <- FALSE
      next
    }
    significant[[i]] <- ct[term, "Pr(>|t|)"] < alpha
  }

  list(
    draws = draws,
    seed = as.integer(seed),
    target_power = target_power,
    achieved_power = mean(significant),
    used_robustness_score = FALSE
  )
}
