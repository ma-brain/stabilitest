#!/usr/bin/env Rscript

# Isolated binary-proportion calibration entry point.  Reuses the shared runner
# while keeping scenario and adapter lookup local to this study.

.binary_proportion_runner_script <- function() {
  files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  files <- files[!is.na(files) & basename(files) == "run_calibration.R"]
  study_files <- files[grepl(
    paste0("studies", .Platform$file.sep, "binary_proportion", .Platform$file.sep),
    files,
    fixed = TRUE
  )]
  if (length(study_files)) {
    return(normalizePath(tail(study_files, 1L), mustWork = TRUE))
  }
  command <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(command) == 1L && file.exists(command)) {
    return(normalizePath(command, mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "binary_proportion",
              "run_calibration.R"),
    mustWork = TRUE
  )
}

.binary_proportion_runner_study_root <- function(script = .binary_proportion_runner_script()) {
  normalizePath(dirname(script), mustWork = TRUE)
}

.binary_proportion_runner_project_root <- function(study_root = .binary_proportion_runner_study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

prepare_binary_proportion_runner <- function(project_root = NULL,
                                              study_root = NULL,
                                              envir = parent.frame()) {
  if (is.null(study_root)) study_root <- .binary_proportion_runner_study_root()
  if (is.null(project_root)) project_root <- .binary_proportion_runner_project_root(study_root)
  project_root <- normalizePath(project_root, mustWork = TRUE)
  study_root <- normalizePath(study_root, mustWork = TRUE)

  loader <- file.path(study_root, "R", "load_study.R")
  sys.source(loader, envir = envir)
  envir$load_binary_proportion_study(project_root = project_root, envir = envir)

  shared_runner <- file.path(
    project_root, "manuscript", "calibration", "run_calibration.R"
  )
  sys.source(shared_runner, envir = envir)

  envir$calibration_scenarios <- envir$binary_proportion_scenarios
  envir$.calibration_adapter_for_scenario <- function(scenario, envir = parent.frame()) {
    family <- as.character(scenario$analysis_engine[[1L]])
    if (!identical(family, "proportion")) {
      stop(sprintf(
        "binary_proportion study runner supports only proportion scenarios, got: %s",
        family
      ), call. = FALSE)
    }
    get("binary_proportion_adapter", envir = envir, inherits = TRUE)()
  }

  invisible(list(project_root = project_root, study_root = study_root))
}

run_binary_proportion_calibration <- function(args = commandArgs(trailingOnly = TRUE),
                                                project_root = NULL,
                                                study_root = NULL,
                                                hooks = list()) {
  runner_env <- environment()
  prepared <- prepare_binary_proportion_runner(
    project_root = project_root,
    study_root = study_root,
    envir = runner_env
  )
  script <- .binary_proportion_runner_script()
  command <- c("Rscript", script, args)
  runner_env$run_calibration(
    args = args,
    project_root = prepared$project_root,
    hooks = hooks,
    command = command
  )
}

.binary_proportion_runner_is_direct <- function() {
  identical(sys.nframe(), 0L) ||
    length(grep("^--file=.*studies/binary_proportion/run_calibration[.]R$",
                commandArgs(trailingOnly = FALSE))) > 0L
}

if (isTRUE(.binary_proportion_runner_is_direct()) &&
    identical(sys.nframe(), 0L)) {
  run_binary_proportion_calibration()
}
