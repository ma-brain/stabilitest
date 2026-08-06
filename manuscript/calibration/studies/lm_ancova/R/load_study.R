# Load shared calibration helpers plus the isolated ANCOVA study sources.

.lm_ancova_study_script_path <- function() {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    character(1)
  )
  source_files <- source_files[
    !is.na(source_files) & basename(source_files) == "load_study.R"
  ]
  if (length(source_files) > 0L) {
    return(normalizePath(source_files[[length(source_files)]], mustWork = TRUE))
  }

  file_args <- sub(
    "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  file_args <- file_args[basename(file_args) == "load_study.R"]
  if (length(file_args) == 1L) {
    return(normalizePath(file_args[[1L]], mustWork = TRUE))
  }

  stop("Unable to locate load_study.R", call. = FALSE)
}

.lm_ancova_study_script_file <- tryCatch(
  .lm_ancova_study_script_path(),
  error = function(error) NULL
)

.lm_ancova_study_root <- function(script_path = .lm_ancova_study_script_file) {
  if (!is.null(script_path)) {
    return(dirname(dirname(normalizePath(script_path, mustWork = TRUE))))
  }

  # Fallback when sourced without ofile: walk from getwd() looking for the
  # study markers.
  candidate <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    markers <- file.path(
      candidate,
      c("R/load_study.R", "config/scenarios.R")
    )
    if (all(file.exists(markers))) {
      return(candidate)
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      break
    }
    candidate <- parent
  }
  stop("Unable to locate the lm_ancova study root", call. = FALSE)
}

.lm_ancova_project_root <- function(study_root = .lm_ancova_study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

load_lm_ancova_study <- function(project_root = NULL, envir = parent.frame()) {
  study_root <- .lm_ancova_study_root()
  if (is.null(project_root)) {
    project_root <- .lm_ancova_project_root(study_root)
  } else {
    project_root <- normalizePath(project_root, mustWork = TRUE)
  }

  shared_loader <- file.path(
    project_root, "manuscript", "calibration", "R", "load_calibration.R"
  )
  if (!file.exists(shared_loader)) {
    stop("Shared calibration loader is missing", call. = FALSE)
  }
  sys.source(shared_loader, envir = envir)
  envir$load_calibration(project_root = project_root, envir = envir)

  study_r_dir <- file.path(study_root, "R")
  study_files <- list.files(study_r_dir, pattern = "[.]R$", full.names = TRUE)
  this_file <- normalizePath(file.path(study_r_dir, "load_study.R"), mustWork = TRUE)
  study_files <- study_files[
    normalizePath(study_files, mustWork = TRUE) != this_file
  ]
  for (study_file in sort(study_files)) {
    sys.source(study_file, envir = envir)
  }

  scenario_file <- file.path(study_root, "config", "scenarios.R")
  if (!file.exists(scenario_file)) {
    stop("ANCOVA study scenario configuration is missing", call. = FALSE)
  }
  sys.source(scenario_file, envir = envir)

  invisible(list(project_root = project_root, study_root = study_root))
}
