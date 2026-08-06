#!/usr/bin/env Rscript

# Atomic binary-proportion publication writer with fail-closed destination
# checks.  Validates the frozen candidate on held-out once (no refit, no second
# candidate) and publishes either outcome (validated or fail-closed) identically.

`%||%` <- function(left, right) if (is.null(left)) right else left

.binary_proportion_write_artifact <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "csv")) {
    if (!is.data.frame(value)) value <- as.data.frame(value, stringsAsFactors = FALSE)
    utils::write.csv(value, path, row.names = FALSE)
  } else if (identical(ext, "rds")) {
    saveRDS(value, path, version = 2)
  } else if (identical(ext, "md") || identical(ext, "txt")) {
    writeLines(as.character(value), path)
  } else {
    saveRDS(value, path, version = 2)
  }
  normalizePath(path, mustWork = TRUE)
}

binary_proportion_hash_ledger <- function(paths) {
  if (!length(paths)) stop("hash ledger requires at least one artifact", call. = FALSE)
  missing <- paths[!file.exists(unlist(paths))]
  if (length(missing)) {
    stop(sprintf("missing required hash target: %s",
                 paste(unlist(missing), collapse = ", ")), call. = FALSE)
  }
  setNames(lapply(paths, function(path) {
    path <- normalizePath(path, mustWork = TRUE)
    list(path = path, hash = unname(as.character(tools::md5sum(path))),
         bytes = unname(file.info(path)$size))
  }), names(paths) %||% unlist(paths))
}

binary_proportion_publish_atomic <- function(artifacts, destination,
                                             allow_overwrite = FALSE) {
  destination <- normalizePath(destination, mustWork = FALSE)
  if (dir.exists(destination)) {
    existing <- list.files(destination, all.files = TRUE, no.. = TRUE)
    if (length(existing) && !isTRUE(allow_overwrite)) {
      stop("stale artifact already present at the publication destination",
           call. = FALSE)
    }
  } else if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("unable to create publication destination: %s", destination),
         call. = FALSE)
  }

  staging <- tempfile("prop-publish-", tmpdir = dirname(destination))
  dir.create(staging, recursive = TRUE)
  on.exit({ if (dir.exists(staging)) unlink(staging, recursive = TRUE, force = TRUE) },
          add = TRUE)

  file_map <- list(
    completed_training = "completed_training.rds",
    completed_validation = "completed_validation.rds",
    audit_training = "audit_training.rds",
    audit_validation = "audit_validation.rds",
    occupancy = "training-occupancy.csv",
    candidate = "candidate.rds",
    validation = "validation.rds",
    registry = "registry.rds",
    training_manifest = "training-manifest.rds",
    validation_manifest = "validation-manifest.rds",
    replication_curve = "training-replication-curve.rds",
    candidate_diagnostics = "candidate-diagnostics.json",
    score_pilot_gate = "SCORE_PILOT_GATE.json"
  )

  written <- list()
  for (name in names(file_map)) {
    if (is.null(artifacts[[name]])) next
    written[[file_map[[name]]]] <- .binary_proportion_write_artifact(
      artifacts[[name]], file.path(staging, file_map[[name]])
    )
  }
  if (!is.null(artifacts$registry)) {
    written[["registry.csv"]] <- .binary_proportion_write_artifact(
      if (is.data.frame(artifacts$registry)) artifacts$registry else as.data.frame(artifacts$registry),
      file.path(staging, "registry.csv")
    )
  }

  ledger <- binary_proportion_hash_ledger(written)
  saveRDS(ledger, file.path(staging, "hash_ledger.rds"), version = 2)
  output_hash_lines <- vapply(names(ledger), function(nm) {
    sprintf("%s  %s", ledger[[nm]]$hash, nm)
  }, character(1))
  writeLines(output_hash_lines, file.path(staging, "output-hashes.txt"))

  # Atomic swap.
  if (dir.exists(destination)) {
    old <- list.files(destination, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(old)) unlink(old, recursive = TRUE, force = TRUE)
  }
  for (path in list.files(staging, full.names = TRUE)) {
    file.rename(path, file.path(destination, basename(path)))
  }
  final_paths <- setNames(file.path(destination, names(written)), names(written))
  final_ledger <- binary_proportion_hash_ledger(final_paths)
  saveRDS(final_ledger, file.path(destination, "hash_ledger.rds"), version = 2)
  final_ledger
}
