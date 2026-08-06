# Individual-level binary-proportion data generation for the isolated study.
#
# Generates 0/1 vectors per arm matching the engine's input contract.  The
# active-arm probability p1 is solved so the *enumerated exact* Fisher power
# equals the frozen target (no normal approximation).  Stress switches are
# diagnostic only and never enter cutoff fitting or acceptance.

.binary_proportion_generator_params <- function(scenario = list(), ...) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) {
      stop("binary_proportion scenario must contain one row", call. = FALSE)
    }
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

# Beta-binomial draw: overdispersion introduces within-arm correlation so the
# marginal rate is unchanged but the variance exceeds the binomial.  rho is the
# intra-class correlation; rho = 0 recovers the binomial.  Draws n 0/1 values
# via a per-arm Beta(rate * (1-rho)/rho, (1-rate) * (1-rho)/rho) mixing variable.
.rbeta_binomial_01 <- function(n, rate, rho) {
  if (rho <= 0 || rate <= 0 || rate >= 1) {
    return(stats::rbinom(n, 1L, rate))
  }
  a <- rate * (1 - rho) / rho
  b <- (1 - rate) * (1 - rho) / rho
  prob <- stats::rbeta(n, a, b)
  stats::rbinom(n, 1L, prob)
}

generate_binary_proportion <- function(scenario = list(), seed = NULL, ...) {
  p <- .binary_proportion_generator_params(scenario, ...)
  if (!is.null(seed)) set.seed(as.integer(seed))

  n_per_arm <- as.integer(p$n_per_arm %||% 100L)
  if (n_per_arm < 4L) {
    stop("n_per_arm must be at least 4", call. = FALSE)
  }
  p0 <- as.numeric(p$p0 %||% 0.25)
  if (!(p0 >= 0 && p0 <= 1)) {
    stop("p0 must be a probability in [0, 1]", call. = FALSE)
  }
  target_power <- as.numeric(p$target_power %||% 0)
  allocation <- as.numeric(p$allocation %||% 0.5)
  if (!(allocation > 0 && allocation < 1)) {
    stop("allocation must be in (0, 1)", call. = FALSE)
  }
  effect_direction <- as.numeric(p$effect_direction %||% 1)
  if (!effect_direction %in% c(-1, 1)) {
    stop("effect_direction must be -1 or 1", call. = FALSE)
  }
  stress <- p$stress %||% NULL
  stress <- if (is.null(stress)) NULL else as.character(stress)

  # Rare-events stress overrides the base control rate.
  if (identical(stress, "rare_events")) {
    p0 <- as.numeric(p$rare_p0 %||% 0.03)
  }

  # Solve the active-arm probability for the exact-power target.
  p1 <- if (identical(target_power, 0) || isTRUE(all.equal(target_power, 0))) {
    p0
  } else {
    solve_prop_effect(n = n_per_arm, p0 = p0, target = target_power)
  }
  if (identical(effect_direction, -1)) {
    # Active benefit lowers the event rate; mirror p1 about p0.
    p1 <- 1 - p1
    p0 <- 1 - p0
  }
  effect <- p1 - p0

  # Arm sizes honour the allocation fraction (active arm fraction).
  n_active <- as.integer(round(2L * n_per_arm * allocation))
  n_control <- 2L * n_per_arm - n_active
  if (n_active < 4L || n_control < 4L) {
    stop("allocation must leave at least four observations per arm",
         call. = FALSE)
  }

  active <- if (identical(stress, "overdispersion")) {
    rho <- as.numeric(p$overdispersion_rho %||% 0.10)
    .rbeta_binomial_01(n_active, p1, rho)
  } else {
    stats::rbinom(n_active, 1L, p1)
  }
  control <- if (identical(stress, "overdispersion")) {
    rho <- as.numeric(p$overdispersion_rho %||% 0.10)
    .rbeta_binomial_01(n_control, p0, rho)
  } else {
    stats::rbinom(n_control, 1L, p0)
  }

  # Misclassification flips a fraction of outcomes (5% default).
  misclassification_rate <- as.numeric(p$misclassification_rate %||% 0)
  if (identical(stress, "misclassification")) {
    misclassification_rate <- as.numeric(p$misclassification_rate %||% 0.05)
    flip <- function(x, rate) {
      m <- stats::rbinom(1L, length(x), rate)
      if (m == 0L) return(x)
      idx <- sample.int(length(x), m)
      x[idx] <- 1L - x[idx]
      x
    }
    active <- flip(active, misclassification_rate)
    control <- flip(control, misclassification_rate)
  }

  # Missing outcomes: a fraction of control-arm outcomes set to NA.
  missing_rate <- as.numeric(p$missing_rate %||% 0)
  missing_ids <- integer()
  if (identical(stress, "missing_outcomes")) {
    missing_rate <- as.numeric(p$missing_rate %||% 0.10)
    m <- max(1L, floor(n_control * missing_rate))
    missing_ids <- sample.int(n_control, m)
    control[missing_ids] <- NA_integer_
  }

  # The engine's input contract is two per-arm 0/1 vectors, so the primary
  # `data` is a list(group1 = active, group2 = control) matching
  # .two_sample_data().  A long-form frame is kept alongside for inspection
  # and for adapters that prefer one row per subject.  group1 = active,
  # group2 = control.
  n_total <- n_active + n_control
  long_form <- data.frame(
    .row_id = seq_len(n_total),
    arm = factor(c(rep("Active", n_active), rep("Control", n_control)),
                 levels = c("Control", "Active")),
    response = c(active, control),
    stringsAsFactors = FALSE
  )
  # group1/group2 keep the missing-outcome NAs in the control arm intact so the
  # screening decision sees the same degeneracy the engine would.
  data <- list(group1 = active, group2 = control, long_form = long_form)

  truth <- list(
    family = "binary_proportion",
    calibration_unit = "fisher_exact",
    p0 = p0,
    p1 = p1,
    effect = effect,
    target_power = target_power,
    allocation = allocation,
    effect_direction = effect_direction,
    n_per_arm = n_per_arm,
    n_active = n_active,
    n_control = n_control,
    active_events = as.integer(sum(active, na.rm = TRUE)),
    control_events = as.integer(sum(control, na.rm = TRUE)),
    stress = stress,
    misclassification_rate = misclassification_rate,
    overdispersion_rho = if (identical(stress, "overdispersion")) {
      as.numeric(p$overdispersion_rho %||% 0.10)
    } else 0,
    missing_rate = if (length(missing_ids)) missing_rate else 0,
    missing_row_ids = missing_ids,
    row_id = long_form$.row_id,
    target = if (isTRUE(all.equal(effect, 0))) "null" else "effect"
  )
  # Build the provenance structure directly rather than via .truth_row(): the
  # engine consumes two per-arm vectors that may differ in length (2:1 stress),
  # and .truth_row() coerces its data argument with as.data.frame(), which
  # would recycle unequal vectors.  Field names mirror .truth_row() exactly so
  # downstream schema and adapter code is unaffected.
  list(
    data = data,
    truth = truth,
    truth_metadata = truth,
    weights = NULL,
    status = "ok",
    failed = FALSE,
    failure = NULL,
    failure_class = NA_character_,
    failure_message = NA_character_
  )
}
