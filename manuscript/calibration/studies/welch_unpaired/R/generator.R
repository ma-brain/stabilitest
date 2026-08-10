# Deterministic data generation for the prospective Welch calibration study.

.welch_scenario_list <- function(scenario) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) {
      stop("Welch scenario must contain exactly one row", call. = FALSE)
    }
    scenario <- as.list(scenario[1L, , drop = FALSE])
  }
  if (!is.list(scenario)) {
    stop("Welch scenario must be a list or one-row data frame", call. = FALSE)
  }
  scenario
}

welch_replicate_seed <- function(scenario_seed, replicate_id) {
  scenario_seed <- as.double(scenario_seed)
  replicate_id <- as.double(replicate_id)
  if (length(scenario_seed) != 1L || !is.finite(scenario_seed) ||
      length(replicate_id) != 1L || !is.finite(replicate_id) ||
      replicate_id < 1 || replicate_id != floor(replicate_id)) {
    stop("scenario_seed and positive integer replicate_id are required",
         call. = FALSE)
  }
  as.integer((scenario_seed + 104729 * replicate_id) %% 2147483646 + 1)
}

.with_welch_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(code)
}

.welch_standard_draw <- function(n, distribution) {
  switch(
    as.character(distribution),
    normal = stats::rnorm(n),
    gross_error_treatment = stats::rnorm(n),
    student_t5 = stats::rt(n, df = 5) / sqrt(5 / 3),
    centered_lognormal = {
      raw <- exp(stats::rnorm(n))
      (raw - exp(0.5)) / sqrt((exp(1) - 1) * exp(1))
    },
    stop(sprintf("unsupported Welch distribution: %s", distribution),
         call. = FALSE)
  )
}

#' Generate one deterministic Welch calibration data set.
#'
#' Group 1 carries the positive latent mean difference. For gross-error rows,
#' `latent_value` and `injected_offset` distinguish the clean estimand from
#' the observed contaminated values.
generate_welch_data <- function(scenario, replicate_id) {
  p <- .welch_scenario_list(scenario)
  required <- c(
    "scenario_id", "truth", "distribution", "n_group1", "n_group2",
    "sd_group1", "sd_group2", "delta", "seed"
  )
  missing <- required[!vapply(required, function(name) {
    !is.null(p[[name]]) && length(p[[name]]) == 1L
  }, logical(1))]
  if (length(missing)) {
    stop(sprintf("Welch scenario is missing: %s", paste(missing, collapse = ", ")),
         call. = FALSE)
  }

  n1 <- as.integer(p$n_group1)
  n2 <- as.integer(p$n_group2)
  sd1 <- as.numeric(p$sd_group1)
  sd2 <- as.numeric(p$sd_group2)
  delta <- as.numeric(p$delta)
  if (n1 < 2L || n2 < 2L) stop("group sizes must each be at least 2", call. = FALSE)
  if (!is.finite(sd1) || !is.finite(sd2) || sd1 <= 0 || sd2 <= 0) {
    stop("group standard deviations must be finite and positive", call. = FALSE)
  }
  if (!is.finite(delta)) stop("scenario delta must be resolved and finite", call. = FALSE)

  replicate_seed <- welch_replicate_seed(p$seed, replicate_id)
  .with_welch_seed(replicate_seed, {
    distribution <- as.character(p$distribution)
    latent1 <- delta + sd1 * .welch_standard_draw(n1, distribution)
    latent2 <- sd2 * .welch_standard_draw(n2, distribution)
    injected1 <- numeric(n1)

    if (identical(distribution, "gross_error_treatment")) {
      fraction <- as.numeric(p$contamination_fraction %||% 0.05)
      offset <- as.numeric(p$contamination_offset %||% 8)
      if (!is.finite(fraction) || fraction <= 0 || fraction >= 1) {
        stop("contamination_fraction must be in (0, 1)", call. = FALSE)
      }
      if (!is.finite(offset)) {
        stop("contamination_offset must be finite", call. = FALSE)
      }
      contaminated_n <- max(1L, floor(n1 * fraction))
      contaminated <- sample.int(n1, contaminated_n, replace = FALSE)
      injected1[contaminated] <- offset
    }

    latent <- c(latent1, latent2)
    injected <- c(injected1, numeric(n2))
    data.frame(
      scenario_id = rep(as.character(p$scenario_id), n1 + n2),
      replicate_id = rep(as.integer(replicate_id), n1 + n2),
      replicate_seed = rep(replicate_seed, n1 + n2),
      group = factor(
        c(rep("group1", n1), rep("group2", n2)),
        levels = c("group1", "group2")
      ),
      value = latent + injected,
      latent_value = latent,
      injected_offset = injected,
      latent_delta = rep(delta, n1 + n2),
      truth = rep(as.character(p$truth), n1 + n2),
      distribution = rep(distribution, n1 + n2),
      stringsAsFactors = FALSE
    )
  })
}
