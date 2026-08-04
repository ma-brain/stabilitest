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
  analyse_calibration(training, validation,
                      training_manifest = training_manifest,
                      validation_manifest = validation_manifest,
                      output = output, ...)
}

