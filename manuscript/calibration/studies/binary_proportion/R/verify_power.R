# Independent Monte Carlo power verifier.
#
# Uses only stats::fisher.test p-values on generated replicates to confirm the
# achieved power matches the frozen target.  This is deliberately independent of
# the enumerated exact-power solver: it catches any drift between the analytic
# power formula and the actual test behaviour on generated binary data.
# Production draws: 10000, master seed 20260808, tolerance 0.02.
# Unit-test draws: 2000, tolerance 0.04.

verify_prop_power <- function(scenario, draws = 10000L, seed = 20260808L,
                              tolerance = 0.02) {
  scenario <- if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) {
      stop("scenario must contain one row", call. = FALSE)
    }
    as.list(scenario[1L, , drop = FALSE])
  } else {
    scenario
  }
  parameters <- scenario$parameters
  if (is.list(parameters) && length(parameters) == 1L &&
      is.list(parameters[[1L]])) {
    parameters <- parameters[[1L]]
  }
  generator <- parameters$generator %||% list()
  analysis <- parameters$analysis %||% list()
  alpha <- as.numeric(analysis$alpha %||% 0.05)
  target_power <- as.numeric(generator$target_power %||% 0)

  draws <- as.integer(draws)
  set.seed(as.integer(seed))
  replicate_seeds <- sample.int(.Machine$integer.max, draws)

  significant <- logical(draws)
  for (i in seq_len(draws)) {
    generated <- generate_binary_proportion(scenario, seed = replicate_seeds[[i]])
    g1 <- generated$data$group1
    g2 <- generated$data$group2
    # Drop any missing outcomes (missing_outcomes stress) before testing, so the
    # verifier measures power on complete cases exactly as the engine would.
    g2 <- g2[!is.na(g2)]
    tab <- matrix(c(sum(g1), length(g1) - sum(g1),
                    sum(g2), length(g2) - sum(g2)), nrow = 2L)
    significant[[i]] <- stats::fisher.test(tab)$p.value < alpha
  }

  achieved <- mean(significant)
  list(
    draws = draws,
    seed = as.integer(seed),
    target_power = target_power,
    achieved_power = achieved,
    within_tolerance = abs(achieved - target_power) <= tolerance,
    tolerance = tolerance,
    used_robustness_score = FALSE
  )
}
