#!/usr/bin/env Rscript

# Entrypoint for the locked two-stage calibration analysis.  Policy functions
# live in R/thresholds.R so that package tests and downstream reports can
# compose them without invoking a command-line process.

calibration_analysis_from_files <- function(training, validation,
                                            training_manifest = NULL,
                                            validation_manifest = NULL,
                                            output = NULL, ...) {
  read_artifact <- function(value, label) {
    if (is.character(value) && length(value) == 1L) {
      if (!file.exists(value)) stop(sprintf("%s artifact does not exist: %s", label, value), call. = FALSE)
      return(readRDS(value))
    }
    value
  }
  read_manifest <- function(value, label) {
    if (is.null(value)) return(NULL)
    if (is.character(value) && length(value) == 1L) {
      if (!file.exists(value)) stop(sprintf("%s manifest does not exist: %s", label, value), call. = FALSE)
      value <- readRDS(value)
    }
    if (!is.list(value)) stop(sprintf("%s manifest must be a list", label), call. = FALSE)
    value
  }
  training <- read_artifact(training, "training")
  validation <- read_artifact(validation, "validation")
  training_manifest <- read_manifest(training_manifest, "training")
  validation_manifest <- read_manifest(validation_manifest, "validation")
  result <- analyse_calibration(training, validation,
                                training_manifest = training_manifest,
                                validation_manifest = validation_manifest,
                                output = output, ...)

  # Non-significant conclusions use a separate, exploratory estimand.  Keep
  # them out of the significant-result registry and attach diagnostics from
  # the held-out rows only, so this step cannot refit or alter the frozen
  # candidate cutoffs.
  # The active executor contract carries the three-part method identity.  A
  # small number of locked Task 15/reduced-publication fixtures predate that
  # contract; keep those historical artifacts readable without routing them
  # through active non-significant diagnostics (which must never infer a
  # broad-family calibration identity).
  non_sig_required <- c("analysis_engine", "calibration_family", "calibration_unit")
  if (exists("analyse_non_significant", mode = "function", inherits = TRUE) &&
      all(non_sig_required %in% names(validation))) {
    result$non_significant <- analyse_non_significant(validation)
    result$non_significant$split <- "validation"
    result$non_significant_registry <- non_significant_registry(validation)
    if (!is.null(output)) {
      saveRDS(result, file.path(output, "calibration-registry.rds"), version = 2)
      utils::write.csv(result$non_significant_registry,
                       file.path(output, "non-significant-registry.csv"),
                       row.names = FALSE)
    }
  } else if (!is.null(output)) {
    result$non_significant <- list(
      status = "bands_not_applicable", applicable = FALSE, split = "validation",
      reason = "historical artifact lacks method-specific calibration identity"
    )
    result$non_significant_registry <- data.frame()
    utils::write.csv(result$non_significant_registry,
                     file.path(output, "non-significant-registry.csv"),
                     row.names = FALSE)
  }
  result
}

# Explicit helper for report scripts that already hold an analysis result.
# This never mutates the significant-result registry or supplies cutoffs.
attach_non_significant_analysis <- function(result, validation, ...) {
  if (!is.list(result)) stop("result must be a calibration analysis list", call. = FALSE)
  if (!exists("analyse_non_significant", mode = "function", inherits = TRUE)) {
    stop("non-significant analysis helper is not loaded", call. = FALSE)
  }
  result$non_significant <- analyse_non_significant(validation, ...)
  result$non_significant$split <- "validation"
  result$non_significant_registry <- non_significant_registry(validation, ...)
  result
}
