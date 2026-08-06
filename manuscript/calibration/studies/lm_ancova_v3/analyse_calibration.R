#!/usr/bin/env Rscript

# Locked ANCOVA v3 Phase 1 analysis entrypoint (Track E violation detection).
# Cutoff fitting / held-out validation are out of scope for Phase 1.
# Track E ΔAUC tooling arrives in Task 5 (`R/track_e.R`).

analyse_lm_ancova_v3_calibration <- function(...) {
  stop(
    paste(
      "Phase 1 Track E analysis is performed by the frozen ΔAUC tooling",
      "in R/track_e.R after production replicates are assembled;",
      "there is no categorical-cutoff fit in lm_ancova_v3 Phase 1."
    ),
    call. = FALSE
  )
}
