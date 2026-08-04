# Load the current stabilitest checkout and calibration helpers.

.calibration_script_path <- function() {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    character(1)
  )
  source_files <- source_files[
    !is.na(source_files) & basename(source_files) == "load_calibration.R"
  ]
  if (length(source_files) > 0L) {
    return(normalizePath(source_files[[length(source_files)]], mustWork = TRUE))
  }

  file_args <- sub(
    "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  file_args <- file_args[basename(file_args) == "load_calibration.R"]
  if (length(file_args) == 1L) {
    return(normalizePath(file_args[[1L]], mustWork = TRUE))
  }

  stop("Unable to locate load_calibration.R", call. = FALSE)
}

.calibration_script_file <- tryCatch(
  .calibration_script_path(),
  error = function(error) NULL
)

.calibration_project_root <- function(script_path = .calibration_script_file, start = getwd()) {
  if (!is.null(script_path)) {
    root <- dirname(dirname(dirname(dirname(normalizePath(script_path, mustWork = TRUE)))))
    markers <- file.path(root, c("DESCRIPTION", "R/robustness_analysis.R"))
    if (!all(file.exists(markers))) {
      stop("Unable to locate the stabilitest project root", call. = FALSE)
    }
    return(root)
  }

  # `sys.source()` does not retain an `ofile` frame.  In that case, walk only
  # the current directory's ancestors and require both repository markers;
  # this avoids guessing among unrelated directories.
  candidate <- normalizePath(start, mustWork = TRUE)
  repeat {
    markers <- file.path(candidate, c("DESCRIPTION", "R/robustness_analysis.R"))
    if (all(file.exists(markers))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      break
    }
    candidate <- parent
  }
  stop("Unable to locate the stabilitest project root", call. = FALSE)
}

load_calibration <- function(project_root = .calibration_project_root(), envir = parent.frame()) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop(
      "The pkgload package is required to load the calibration checkout",
      call. = FALSE
    )
  }

  pkgload::load_all(project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

  calibration_r_dir <- file.path(project_root, "manuscript", "calibration", "R")
  calibration_files <- list.files(calibration_r_dir, pattern = "[.]R$", full.names = TRUE)
  this_file <- if (is.null(.calibration_script_file)) {
    file.path(project_root, "manuscript", "calibration", "R", "load_calibration.R")
  } else {
    normalizePath(.calibration_script_file, mustWork = TRUE)
  }
  calibration_files <- calibration_files[
    normalizePath(calibration_files, mustWork = TRUE) != this_file
  ]
  for (calibration_file in sort(calibration_files)) {
    sys.source(calibration_file, envir = envir)
  }

  scenario_file <- file.path(project_root, "manuscript", "calibration", "config", "scenarios.R")
  if (!file.exists(scenario_file)) {
    stop("Calibration scenario configuration is missing", call. = FALSE)
  }
  sys.source(scenario_file, envir = envir)

  invisible(project_root)
}
