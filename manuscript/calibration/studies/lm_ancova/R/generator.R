# Power-targeted canonical ANCOVA data generation for the isolated study.

.lm_ancova_generator_params <- function(scenario = list(), ...) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) stop("ANCOVA scenario must contain one row", call. = FALSE)
    scenario <- as.list(scenario[1L, , drop = FALSE])
  }
  # Tibble list-columns become list(list(generator=...)) under as.list().
  # Unwrap once so both dataframe rows and as.list(row) paths resolve the
  # same generator payload.
  if (is.list(scenario$parameters) &&
      length(scenario$parameters) == 1L &&
      is.list(scenario$parameters[[1L]]) &&
      !is.null(scenario$parameters[[1L]]$generator)) {
    scenario$parameters <- scenario$parameters[[1L]]
  }
  params <- scenario
  if (is.list(params$parameters)) params <- params$parameters
  if (is.list(params$generator)) params <- params$generator
  utils::modifyList(params, list(...))
}

.lm_ancova_balanced_treatment <- function(n, allocation = 0.5) {
  n <- as.integer(n)
  n1 <- as.integer(round(n * allocation))
  n0 <- n - n1
  if (n1 < 1L || n0 < 1L) {
    stop("allocation must leave at least one observation per arm", call. = FALSE)
  }
  factor(sample(c(rep("A", n0), rep("B", n1))), levels = c("A", "B"))
}

.lm_ancova_residual <- function(n, residual_sd, stress = NULL, baseline = NULL,
                                treatment = NULL) {
  stress <- as.character(stress %||% "")
  if (identical(stress, "heteroscedastic")) {
    scale <- residual_sd * (1 + 0.7 * abs(baseline))
    return(stats::rnorm(n, 0, scale))
  }
  if (identical(stress, "heavy_tails")) {
    return(residual_sd * stats::rt(n, df = 3) / sqrt(3))
  }
  stats::rnorm(n, 0, residual_sd)
}

generate_lm_ancova <- function(scenario = list(), seed = NULL, ...) {
  p <- .lm_ancova_generator_params(scenario, ...)
  if (!is.null(seed)) set.seed(as.integer(seed))

  n <- as.integer(p$n %||% 80L)
  baseline_r2 <- as.numeric(p$baseline_r2 %||% 0.40)
  if (!(baseline_r2 >= 0 && baseline_r2 < 1)) {
    stop("baseline_r2 must be in [0, 1)", call. = FALSE)
  }
  target_power <- as.numeric(p$target_power %||% 0)
  allocation <- as.numeric(p$allocation %||% 0.5)
  residual_sd <- as.numeric(p$residual_sd %||% 1)
  effect_direction <- as.numeric(p$effect_direction %||% 1)
  if (!effect_direction %in% c(-1, 1)) {
    stop("effect_direction must be -1 or 1", call. = FALSE)
  }
  stress <- p$stress %||% NULL

  gamma <- sqrt(baseline_r2 / max(1e-12, 1 - baseline_r2))
  beta_abs <- solve_ancova_effect(
    n = n,
    target_power = target_power,
    alpha = 0.05,
    residual_sd = residual_sd,
    allocation = allocation
  )
  beta <- effect_direction * beta_abs

  treatment <- .lm_ancova_balanced_treatment(n, allocation = allocation)
  baseline <- stats::rnorm(n)
  treated <- as.integer(treatment == "B")

  mean_outcome <- beta * treated + gamma * baseline
  if (identical(as.character(stress), "nonlinear_baseline")) {
    mean_outcome <- beta * treated + gamma * (baseline + 0.35 * baseline^2)
  }
  if (identical(as.character(stress), "interaction")) {
    mean_outcome <- beta * treated + gamma * baseline + 0.5 * beta * treated * baseline
  }

  outcome <- mean_outcome + .lm_ancova_residual(
    n, residual_sd, stress = stress, baseline = baseline, treatment = treatment
  )

  data <- data.frame(
    .row_id = seq_len(n),
    outcome = outcome,
    treatment = treatment,
    baseline = baseline,
    stringsAsFactors = FALSE
  )

  missing_ids <- integer()
  missing_rate <- as.numeric(p$missing_rate %||% 0)
  if (identical(as.character(stress), "missing_baseline") || missing_rate > 0) {
    missing_rate <- max(missing_rate, 0.10)
    m <- max(1L, floor(n * missing_rate))
    missing_ids <- sample(seq_len(n), m)
    data$baseline[missing_ids] <- NA_real_
  }

  truth <- list(
    family = "lm_ancova",
    term = "treatmentB",
    effect = beta,
    beta = beta,
    gamma = gamma,
    baseline_r2 = baseline_r2,
    target_power = target_power,
    allocation = allocation,
    residual_sd = residual_sd,
    effect_direction = effect_direction,
    stress = stress,
    missing_rate = if (length(missing_ids)) missing_rate else 0,
    missing_row_ids = missing_ids,
    row_id = data$.row_id,
    target = if (isTRUE(all.equal(beta, 0))) "null" else "effect"
  )
  .truth_row(data, truth)
}
