# Replication draws for the binary-proportion calibration.
#
# One primary-test-only replicate per completed significant row: regenerate the
# scenario's data at the same truth and record the Fisher p-value / conclusion.
# This is the input to the Track D' replication-probability curve (fitted on the
# same training rows regardless of whether Track A'' proceeds).  The seed stream
# is dedicated (master 20260808) and disjoint from the screening replicate and
# bootstrap seed columns.

.prop_replication_master_seed <- 20260808L

# Dedicated replication seed, distinct from the screening replicate_seed() and
# bootstrap_seed() at the same coordinates.  Uses the shared hash machinery with
# a unique tag so the stream can never collide with the other seed columns.
prop_replication_seed <- function(scenario_seed, replicate_id) {
  scenario_seed <- .calibration_scalar_integer(scenario_seed, "scenario_seed")
  replicate_id <- .calibration_scalar_integer(replicate_id, "replicate_id",
                                               positive = TRUE)
  .calibration_set_rng_kind()
  .calibration_hash_integer("binary_proportion_replication",
                            .prop_replication_master_seed,
                            scenario_seed, replicate_id)
}

# One primary-test-only replicate.  Returns the Fisher p-value and conclusion on
# a fresh draw at the scenario's frozen truth; no robustness score is computed.
prop_replication_draw <- function(scenario, replicate_id) {
  scenario_seed <- if (is.data.frame(scenario)) {
    as.integer(scenario$scenario_seed[[1L]])
  } else {
    as.integer(scenario$scenario_seed %||% NA_integer_)
  }
  if (is.na(scenario_seed)) {
    stop("scenario must carry a scenario_seed for replication draws",
         call. = FALSE)
  }
  rseed <- prop_replication_seed(scenario_seed, replicate_id)
  generated <- generate_binary_proportion(scenario, seed = rseed)
  groups <- .two_sample_data(generated$data)
  analysis <- .two_sample_analysis(scenario)
  alpha <- .two_sample_alpha(analysis, scenario)
  # Drop missing outcomes (missing_outcomes stress) before testing.
  g1 <- groups$group1
  g2 <- groups$group2[!is.na(groups$group2)]
  tab <- matrix(c(sum(g1), length(g1) - sum(g1),
                  sum(g2), length(g2) - sum(g2)), nrow = 2L)
  p <- stats::fisher.test(tab)$p.value
  list(
    p = p,
    significant = p < alpha,
    replication_seed = rseed,
    overall_score = NULL
  )
}
