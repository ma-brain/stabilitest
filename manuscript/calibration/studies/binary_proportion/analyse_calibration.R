#!/usr/bin/env Rscript

# Locked binary-proportion analysis entrypoint: fit on training, freeze, then
# validate held-out once without refitting.  Track A'' (two-band cutoff) and
# Track D' (replication curve) share the same training rows; both candidates
# are frozen before held-out is opened.

analyse_binary_proportion_calibration <- function(training, validation = NULL,
                                                    scenario_manifest_hash,
                                                    training_manifest_hash,
                                                    validation_manifest_hash = NULL,
                                                    replication_data = NULL,
                                                    cluster_B = 1000L,
                                                    cluster_seed = 20260808L) {
  track_a <- fit_fisher_exact_cutoffs(training)
  # The Track D' replication curve is fit from the dedicated replication draws
  # (one primary-test-only replicate per completed significant row).  When no
  # replication data is supplied, it is fit from training if it carries a
  # replication_significant column, otherwise archived as unavailable.
  track_d <- if (!is.null(replication_data)) {
    fit_fisher_exact_replication_curve(replication_data)
  } else if (!is.null(training) && is.data.frame(training) &&
             "replication_significant" %in% names(training)) {
    fit_fisher_exact_replication_curve(training)
  } else {
    list(status = "unavailable", reason = "no_replication_data")
  }
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
