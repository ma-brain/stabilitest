# Deterministic seed derivation for calibration simulations.

.calibration_seed_abort <- function(message) {
  stop(message, call. = FALSE)
}

.calibration_scalar_integer <- function(value, name, positive = FALSE) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || floor(value) != value ||
      value < -.Machine$integer.max || value > .Machine$integer.max ||
      (positive && value <= 0)) {
    .calibration_seed_abort(sprintf("%s must be one finite integer", name))
  }
  as.integer(value)
}

.calibration_scalar_text <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    .calibration_seed_abort(sprintf("%s must be one non-empty character value", name))
  }
  enc2utf8(value)
}

# A small, platform-stable polynomial hash.  The multiplier keeps each
# intermediate product below 2^53, so all arithmetic is exact in base R's
# doubles while the prime modulus keeps seeds in set.seed's supported range.
.calibration_hash_integer <- function(...) {
  text <- paste(vapply(list(...), as.character, character(1)), collapse = "\u001f")
  bytes <- utf8ToInt(enc2utf8(text))
  modulus <- 2147483647
  hash <- 104729
  if (length(bytes) > 0L) {
    for (byte in bytes) {
      hash <- (hash * 131 + byte) %% modulus
    }
  }
  as.integer(hash + 1)
}

.calibration_set_rng_kind <- function() {
  RNGkind(
    kind = "L'Ecuyer-CMRG",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  invisible(NULL)
}

#' Derive a deterministic seed for a scenario.
scenario_seed <- function(scenario_id, master_seed) {
  scenario_id <- .calibration_scalar_text(scenario_id, "scenario_id")
  master_seed <- .calibration_scalar_integer(master_seed, "master_seed")
  .calibration_set_rng_kind()
  .calibration_hash_integer("scenario", master_seed, scenario_id)
}

#' Derive a deterministic seed for one replicate in a scenario.
replicate_seed <- function(scenario_seed, replicate_id) {
  scenario_seed <- .calibration_scalar_integer(scenario_seed, "scenario_seed")
  replicate_id <- .calibration_scalar_integer(replicate_id, "replicate_id", positive = TRUE)
  .calibration_set_rng_kind()
  .calibration_hash_integer("replicate", scenario_seed, replicate_id)
}

#' Derive a deterministic seed for bootstrap resampling of one replicate.
bootstrap_seed <- function(replicate_seed) {
  replicate_seed <- .calibration_scalar_integer(replicate_seed, "replicate_seed")
  .calibration_set_rng_kind()
  .calibration_hash_integer("bootstrap", replicate_seed)
}
