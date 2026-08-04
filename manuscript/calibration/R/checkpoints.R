# Atomic, manifest-aware calibration checkpoints.

.checkpoint_abort <- function(message) {
  stop(message, call. = FALSE)
}

.checkpoint_scalar_text <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    .checkpoint_abort(sprintf("%s must be one non-empty character value", name))
  }
  value
}

.checkpoint_component <- function(value, name) {
  value <- .checkpoint_scalar_text(value, name)
  if (value %in% c(".", "..") || grepl("[/\\\\]", value)) {
    .checkpoint_abort(sprintf("%s must be one safe path component", name))
  }
  value
}

.checkpoint_scalar_integer <- function(value, name, nonnegative = FALSE) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || floor(value) != value ||
      value < 0 || value > .Machine$integer.max ||
      (!nonnegative && value <= 0)) {
    .checkpoint_abort(sprintf("%s must be one valid integer", name))
  }
  as.integer(value)
}

# Return the canonical checkpoint location below an artifact root.
checkpoint_path <- function(root, scenario_id, stratum) {
  root <- .checkpoint_scalar_text(root, "root")
  scenario_id <- .checkpoint_component(scenario_id, "scenario_id")
  stratum <- .checkpoint_component(stratum, "stratum")
  file.path(root, "checkpoints", scenario_id, paste0(stratum, ".rds"))
}

.checkpoint_target_n <- function(x) {
  if (is.list(x) && !is.data.frame(x) && "target_n" %in% names(x)) {
    target_n <- x$target_n
    if (length(target_n) != 1L || !is.numeric(target_n) || is.na(target_n) ||
        !is.finite(target_n) || floor(target_n) != target_n || target_n < 0 ||
        target_n > .Machine$integer.max) {
      .checkpoint_abort("target_n metadata must be one non-negative integer")
    }
    target_n <- as.integer(target_n)
    if ("replicates" %in% names(x)) {
      replicate_n <- .checkpoint_target_n(x$replicates)
      if (!identical(target_n, replicate_n)) {
        .checkpoint_abort("target_n metadata does not match replicate count")
      }
    }
    return(target_n)
  }
  if (is.data.frame(x)) {
    return(as.integer(nrow(x)))
  }
  if (is.list(x) && !is.null(x$replicates)) {
    return(.checkpoint_target_n(x$replicates))
  }
  as.integer(length(x))
}

.checkpoint_read_envelope <- function(path, manifest_hash) {
  path <- .checkpoint_scalar_text(path, "path")
  manifest_hash <- .checkpoint_scalar_text(manifest_hash, "manifest_hash")
  info <- file.info(path)
  if (!file.exists(path) || is.null(info) || isTRUE(info$isdir)) {
    .checkpoint_abort("checkpoint does not exist")
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  envelope <- tryCatch(
    {
      value <- readRDS(connection)
      repeat {
        trailing <- readBin(connection, what = "raw", n = 65536L)
        if (length(trailing) == 0L) {
          break
        }
        .checkpoint_abort("checkpoint contains trailing bytes")
      }
      value
    },
    error = function(error) {
      .checkpoint_abort(sprintf("invalid or truncated checkpoint: %s", conditionMessage(error)))
    }
  )
  required <- c("version", "manifest_hash", "complete", "target_n", "payload")
  payload_target_n <- if (is.list(envelope) && "payload" %in% names(envelope)) {
    tryCatch(.checkpoint_target_n(envelope$payload), error = function(error) NA_integer_)
  } else {
    NA_integer_
  }
  if (!is.list(envelope) || !all(required %in% names(envelope)) ||
      !identical(envelope$version, 1L) ||
      !is.character(envelope$manifest_hash) || length(envelope$manifest_hash) != 1L ||
      is.na(envelope$manifest_hash) ||
      !identical(envelope$manifest_hash, manifest_hash) ||
      !identical(envelope$complete, TRUE) ||
      !is.numeric(envelope$target_n) || length(envelope$target_n) != 1L ||
      !is.finite(envelope$target_n) || floor(envelope$target_n) != envelope$target_n ||
      envelope$target_n < 0 || is.na(payload_target_n) ||
      !identical(as.integer(envelope$target_n), payload_target_n)) {
    .checkpoint_abort("checkpoint is invalid or has a manifest mismatch")
  }
  envelope
}

#' Write a complete checkpoint using a same-directory temporary file.
write_checkpoint <- function(x, path, manifest_hash) {
  path <- .checkpoint_scalar_text(path, "path")
  manifest_hash <- .checkpoint_scalar_text(manifest_hash, "manifest_hash")
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(parent)) {
    .checkpoint_abort("unable to create checkpoint directory")
  }

  temporary <- tempfile(
    pattern = paste0(".", basename(path), "."),
    tmpdir = parent,
    fileext = ".tmp"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  envelope <- list(
    version = 1L,
    manifest_hash = manifest_hash,
    complete = TRUE,
    target_n = .checkpoint_target_n(x),
    payload = x
  )
  tryCatch(
    saveRDS(envelope, temporary, version = 3, compress = FALSE),
    error = function(error) {
      .checkpoint_abort(sprintf("unable to write checkpoint: %s", conditionMessage(error)))
    }
  )
  renamed <- suppressWarnings(file.rename(temporary, path))
  if (!isTRUE(renamed)) {
    .checkpoint_abort("unable to atomically rename checkpoint")
  }
  invisible(path)
}

#' Read and validate a checkpoint's manifest before returning its payload.
read_checkpoint <- function(path, manifest_hash) {
  .checkpoint_read_envelope(path, manifest_hash)$payload
}

#' Check whether a checkpoint can satisfy a requested completed-row target.
checkpoint_complete <- function(path, manifest_hash, target_n) {
  target_n <- tryCatch(
    .checkpoint_scalar_integer(target_n, "target_n", nonnegative = TRUE),
    error = function(error) NULL
  )
  if (is.null(target_n)) {
    return(FALSE)
  }
  envelope <- tryCatch(
    .checkpoint_read_envelope(path, manifest_hash),
    error = function(error) NULL
  )
  !is.null(envelope) && isTRUE(envelope$complete) && envelope$target_n >= target_n
}
