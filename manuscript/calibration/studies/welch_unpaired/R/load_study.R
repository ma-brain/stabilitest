# Load all helpers for the prospective Welch calibration study.

.welch_study_script_path <- function() {
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
  if (length(source_files)) {
    return(normalizePath(source_files[[length(source_files)]], mustWork = TRUE))
  }
  stop("Unable to locate Welch load_study.R", call. = FALSE)
}

.welch_study_script_file <- tryCatch(
  .welch_study_script_path(),
  error = function(error) NULL
)

.welch_study_root <- function(script_path = .welch_study_script_file) {
  if (!is.null(script_path) && nzchar(script_path)) {
    return(dirname(dirname(normalizePath(script_path, mustWork = TRUE))))
  }
  candidate <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    markers <- file.path(candidate, c("R/load_study.R", "config/scenarios.R"))
    if (all(file.exists(markers))) return(candidate)
    nested <- file.path(
      candidate, "manuscript", "calibration", "studies", "welch_unpaired"
    )
    nested_markers <- file.path(nested, c("R/load_study.R", "config/scenarios.R"))
    if (all(file.exists(nested_markers))) {
      return(normalizePath(nested, mustWork = TRUE))
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  stop("Unable to locate the welch_unpaired study root", call. = FALSE)
}

.welch_project_root <- function(study_root = .welch_study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

load_welch_study <- function(project_root = NULL, envir = parent.frame()) {
  study_root <- .welch_study_root()
  if (is.null(project_root)) {
    project_root <- .welch_project_root(study_root)
  } else {
    project_root <- normalizePath(project_root, mustWork = TRUE)
  }

  study_r_dir <- file.path(study_root, "R")
  study_files <- list.files(study_r_dir, pattern = "[.]R$", full.names = TRUE)
  this_file <- normalizePath(file.path(study_r_dir, "load_study.R"), mustWork = TRUE)
  study_files <- study_files[
    normalizePath(study_files, mustWork = TRUE) != this_file
  ]
  for (study_file in sort(study_files)) {
    sys.source(study_file, envir = envir)
  }
  sys.source(file.path(study_root, "config", "scenarios.R"), envir = envir)
  invisible(list(project_root = project_root, study_root = study_root))
}
