#!/usr/bin/env Rscript

# Locked binary-proportion analysis entrypoint: fit on training, freeze, then
# validate held-out once without refitting.  Track A'' (two-band cutoff) and
# Track D' (replication curve) share the same training rows; both candidates
# are frozen before held-out is opened.

analyse_binary_proportion_calibration <- function(training, validation = NULL,
                                                    scenario_manifest_hash,
                                                    training_manifest_hash,
                                                    validation_manifest_hash = NULL,
                                                    cluster_B = 1000L,
                                                    cluster_seed = 20260808L) {
  track_a <- fit_fisher_exact_cutoffs(training)
  track_d <- fit_fisher_exact_replication_curve(training)
  frozen <- freeze_binary_proportion_candidate(
    track_a,
    track_d = track_d,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  if (is.null(validation) || !identical(frozen$status, "candidate")) {
    return(list(
      track_a = track_a,
      track_d = track_d,
      frozen = frozen,
      validation = NULL,
      status = frozen$status,
      validation_refit = FALSE
    ))
  }
  if (is.null(validation_manifest_hash)) {
    stop("validation_manifest_hash is required when opening held-out data",
         call. = FALSE)
  }
  validated <- validate_binary_proportion_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = validation_manifest_hash,
    cluster_B = cluster_B,
    cluster_seed = cluster_seed
  )
  list(
    track_a = track_a,
    track_d = track_d,
    frozen = frozen,
    validation = validated,
    status = validated$status,
    validation_refit = FALSE
  )
}
