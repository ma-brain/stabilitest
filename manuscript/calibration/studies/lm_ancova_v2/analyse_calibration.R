#!/usr/bin/env Rscript

# Locked ANCOVA v2 analysis entrypoint: fit single cutoff L on training,
# freeze, then validate held-out once without refitting.

analyse_lm_ancova_v2_calibration <- function(training, validation = NULL,
                                             scenario_manifest_hash,
                                             training_manifest_hash,
                                             validation_manifest_hash = NULL,
                                             cluster_B = 1000L,
                                             cluster_seed = 20260806L) {
  fit <- fit_lm_ancova_v2_cutoffs(training)
  frozen <- freeze_lm_ancova_v2_candidate(
    fit,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash
  )
  if (is.null(validation) || !identical(frozen$status, "candidate")) {
    return(list(
      fit = fit,
      frozen = frozen,
      validation = NULL,
      status = frozen$status,
      validation_refit = FALSE
    ))
  }
  if (is.null(validation_manifest_hash)) {
    stop(
      "validation_manifest_hash is required when opening held-out data",
      call. = FALSE
    )
  }
  validated <- validate_lm_ancova_v2_candidate(
    frozen,
    validation,
    scenario_manifest_hash = scenario_manifest_hash,
    training_manifest_hash = training_manifest_hash,
    validation_manifest_hash = validation_manifest_hash,
    cluster_B = cluster_B,
    cluster_seed = cluster_seed
  )
  list(
    fit = fit,
    frozen = frozen,
    validation = validated,
    status = validated$status,
    validation_refit = FALSE
  )
}
