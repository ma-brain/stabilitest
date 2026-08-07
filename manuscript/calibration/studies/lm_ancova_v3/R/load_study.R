# Load shared calibration helpers plus the isolated ANCOVA v3 Track E study.
# Reuses v1 generator/power helpers without mutating v1 or v2 study files.

.lm_ancova_v3_study_script_path <- function() {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    character(1)
  )
  v3_marker <- paste0(
    "studies", .Platform$file.sep, "lm_ancova_v3", .Platform$file.sep
  )
  source_files <- source_files[
    !is.na(source_files) &
      basename(source_files) == "load_study.R" &
      grepl(v3_marker, source_files, fixed = TRUE)
  ]
  if (length(source_files) > 0L) {
    return(normalizePath(source_files[[length(source_files)]], mustWork = TRUE))
  }

  file_args <- sub(
    "^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  file_args <- file_args[
    basename(file_args) == "load_study.R" &
      grepl(v3_marker, file_args, fixed = TRUE)
  ]
  if (length(file_args) == 1L) {
    return(normalizePath(file_args[[1L]], mustWork = TRUE))
  }

  stop("Unable to locate lm_ancova_v3 load_study.R", call. = FALSE)
}

.lm_ancova_v3_study_script_file <- tryCatch(
  .lm_ancova_v3_study_script_path(),
  error = function(error) NULL
)

.lm_ancova_v3_is_study_root <- function(path) {
  if (!nzchar(path) || !dir.exists(path)) {
    return(FALSE)
  }
  root <- normalizePath(path, mustWork = FALSE)
  if (!identical(basename(root), "lm_ancova_v3")) {
    return(FALSE)
  }
  markers <- file.path(root, c("R/load_study.R", "config/scenarios.R"))
  all(file.exists(markers))
}

.lm_ancova_v3_study_root <- function(script_path = .lm_ancova_v3_study_script_file) {
  if (!is.null(script_path) && nzchar(script_path)) {
    root <- dirname(dirname(normalizePath(script_path, mustWork = TRUE)))
    if (!.lm_ancova_v3_is_study_root(root)) {
      stop(
        "script_path does not resolve to the lm_ancova_v3 study root",
        call. = FALSE
      )
    }
    return(root)
  }

  candidate <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    if (.lm_ancova_v3_is_study_root(candidate)) {
      return(candidate)
    }
    nested <- file.path(
      candidate, "manuscript", "calibration", "studies", "lm_ancova_v3"
    )
    if (.lm_ancova_v3_is_study_root(nested)) {
      return(normalizePath(nested, mustWork = TRUE))
    }
    parent <- dirname(candidate)
    if (identical(parent, candidate)) {
      break
    }
    candidate <- parent
  }
  stop("Unable to locate the lm_ancova_v3 study root", call. = FALSE)
}

.lm_ancova_v3_project_root <- function(study_root = .lm_ancova_v3_study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

.lm_ancova_v3_v1_study_root <- function(project_root) {
  normalizePath(
    file.path(project_root, "manuscript", "calibration", "studies", "lm_ancova"),
    mustWork = TRUE
  )
}

load_lm_ancova_v3_study <- function(project_root = NULL, envir = parent.frame()) {
  study_root <- .lm_ancova_v3_study_root()
  if (is.null(project_root)) {
    project_root <- .lm_ancova_v3_project_root(study_root)
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

  # Reuse immutable v1 analytic helpers; do not source v1/v2 adapters.
  v1_root <- .lm_ancova_v3_v1_study_root(project_root)
  for (helper in c("power.R", "generator.R")) {
    helper_path <- file.path(v1_root, "R", helper)
    if (!file.exists(helper_path)) {
      stop(sprintf("v1 ANCOVA helper missing: %s", helper), call. = FALSE)
    }
    sys.source(helper_path, envir = envir)
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

  scenario_file <- file.path(study_root, "config", "scenarios.R")
  if (!file.exists(scenario_file)) {
    stop("ANCOVA v3 study scenario configuration is missing", call. = FALSE)
  }
  sys.source(scenario_file, envir = envir)

  invisible(list(project_root = project_root, study_root = study_root))
}
