#!/usr/bin/env Rscript

# Locked ANCOVA v3 Phase 1 analysis entrypoint (Track E violation detection).
# Categorical-cutoff fitting / held-out validation are out of scope for Phase 1.

analyse_lm_ancova_v3_calibration <- function(replicates,
                                             cluster_B = 1000L,
                                             cluster_seed = 20260807L,
                                             ...) {
  analyse_lm_ancova_v3_track_e(
    replicates,
    cluster_B = cluster_B,
    cluster_seed = cluster_seed
  )
}
