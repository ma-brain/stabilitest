# Candidate hashing and single-opening validation for the Welch study.

hash_welch_candidate <- function(candidate, context) {
  digest::digest(
    list(candidate = candidate, context = context),
    algo = "sha256",
    serialize = TRUE
  )
}

freeze_welch_candidate <- function(candidate, context) {
  if (!is.list(candidate) || is.null(candidate$status)) {
    stop("candidate must be a Welch fitting result", call. = FALSE)
  }
  payload <- list(
    candidate = candidate,
    context = context,
    held_out_opened = FALSE,
    validation_refit = FALSE
  )
  frozen <- candidate
  frozen$context <- context
  frozen$held_out_opened <- FALSE
  frozen$validation_refit <- FALSE
  frozen$hash_payload <- payload
  frozen$candidate_hash <- digest::digest(
    payload, algo = "sha256", serialize = TRUE
  )
  frozen
}

recompute_welch_candidate_hash <- function(frozen) {
  if (!is.list(frozen) || is.null(frozen$hash_payload)) {
    stop("frozen candidate has no hash payload", call. = FALSE)
  }
  digest::digest(frozen$hash_payload, algo = "sha256", serialize = TRUE)
}

validate_frozen_welch_candidate <- function(frozen, validation,
                                            occupancy_pass = TRUE,
                                            cluster_B = 1000L,
                                            cluster_seed = 202640001L) {
  if (!is.list(frozen) || is.null(frozen$candidate_hash)) {
    stop("validation requires a frozen candidate", call. = FALSE)
  }
  if (!identical(recompute_welch_candidate_hash(frozen), frozen$candidate_hash)) {
    stop("frozen candidate hash mismatch", call. = FALSE)
  }
  if (!identical(frozen$status, "candidate")) {
    return(list(
      status = "uncalibrated",
      reason = frozen$reason,
      held_out_opened = FALSE,
      validation_refit = FALSE,
      candidate_hash = frozen$candidate_hash
    ))
  }
  if (!isTRUE(occupancy_pass)) {
    return(list(
      status = "uncalibrated",
      reason = "validation_occupancy_failure",
      held_out_opened = TRUE,
      validation_refit = FALSE,
      candidate_hash = frozen$candidate_hash
    ))
  }
  metrics <- welch_candidate_metrics(
    validation,
    frozen$mapping_type,
    lower = frozen$cutoffs[[1L]],
    upper = frozen$cutoffs[[2L]],
    cluster_B = cluster_B,
    cluster_seed = cluster_seed
  )
  gate <- welch_candidate_gate(metrics, frozen$mapping_type)
  list(
    status = if (gate$feasible) "validated_method_specific" else "uncalibrated",
    reason = if (gate$feasible) NA_character_ else paste(gate$reasons, collapse = ","),
    mapping_type = frozen$mapping_type,
    cutoffs = frozen$cutoffs,
    metrics = metrics,
    conservative_bounds = list(
      false_reassurance_upper = metrics$false_reassurance_conservative_upper,
      clear_identification_lower = metrics$clear_identification_conservative_lower,
      wilson = list(
        false_reassurance_upper = metrics$false_reassurance_wilson_upper,
        clear_identification_lower = metrics$clear_identification_wilson_lower
      ),
      cluster = list(
        false_reassurance_upper = metrics$false_reassurance_cluster_upper,
        clear_identification_lower = metrics$clear_identification_cluster_lower
      )
    ),
    held_out_opened = TRUE,
    validation_refit = FALSE,
    candidate_hash = frozen$candidate_hash
  )
}
