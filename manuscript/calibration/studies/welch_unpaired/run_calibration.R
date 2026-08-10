#!/usr/bin/env Rscript

# Command-line entry point for the prospective Welch calibration study.

.welch_runner_script <- function() {
  command <- sub(
    "^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  if (length(command) == 1L && file.exists(command)) {
    return(normalizePath(command, mustWork = TRUE))
  }
  normalizePath(
    file.path(
      "manuscript", "calibration", "studies", "welch_unpaired",
      "run_calibration.R"
    ),
    mustWork = TRUE
  )
}

.welch_runner_study_root <- function(script = .welch_runner_script()) {
  normalizePath(dirname(script), mustWork = TRUE)
}

.welch_runner_project_root <- function(study_root = .welch_runner_study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

parse_welch_cli <- function(args) {
  value <- function(flag, default = NULL) {
    index <- which(args == flag)
    if (!length(index)) return(default)
    if (index[[1L]] == length(args)) {
      stop(sprintf("%s requires a value", flag), call. = FALSE)
    }
    args[[index[[1L]] + 1L]]
  }
  mode <- value("--mode", "smoke")
  layer <- value("--layer", "training")
  if (!mode %in% c("smoke", "production")) {
    stop("--mode must be smoke or production", call. = FALSE)
  }
  if (!layer %in% c("training", "validation")) {
    stop("--layer must be training or validation", call. = FALSE)
  }
  detected_cores <- parallel::detectCores(logical = FALSE)
  default_workers <- if (is.na(detected_cores)) {
    1L
  } else {
    max(1L, min(4L, as.integer(detected_cores)))
  }
  workers <- as.integer(value("--workers", as.character(default_workers)))
  if (is.na(workers) || workers < 1L) stop("--workers must be positive", call. = FALSE)
  list(
    mode = mode,
    layer = layer,
    candidate_hash = value("--candidate-hash", NULL),
    workers = workers,
    resume = !"--no-resume" %in% args
  )
}

run_welch_calibration <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- parse_welch_cli(args)
  study_root <- .welch_runner_study_root()
  project_root <- .welch_runner_project_root(study_root)
  devtools::load_all(project_root, quiet = TRUE)
  env <- environment()
  sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
  env$load_welch_study(project_root = project_root, envir = env)
  env$run_welch_layer(
    study_root = study_root,
    project_root = project_root,
    mode = options$mode,
    layer = options$layer,
    candidate_hash = options$candidate_hash,
    workers = options$workers,
    resume = options$resume
  )
}

if (identical(sys.nframe(), 0L)) run_welch_calibration()
