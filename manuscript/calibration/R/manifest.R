# Provenance and integrity manifests for calibration runs.

.calibration_manifest_abort <- function(message) stop(message, call. = FALSE)

calibration_hash_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path) ||
      file.info(path)$isdir) {
    .calibration_manifest_abort(sprintf("cannot hash missing file: %s", path))
  }
  unname(as.character(tools::md5sum(path)))
}

calibration_hash_object <- function(object) {
  path <- tempfile("calibration-object-")
  on.exit(unlink(path), add = TRUE)
  saveRDS(object, path, version = 2)
  calibration_hash_file(path)
}

calibration_scenario_hash <- function(scenarios) {
  if (!is.data.frame(scenarios)) {
    .calibration_manifest_abort("scenarios must be a data frame")
  }
  calibration_hash_object(scenarios)
}

.calibration_git_command <- function(project_root, args) {
  output <- tryCatch(
    system2("git", c("-C", project_root, args), stdout = TRUE, stderr = FALSE),
    warning = function(w) character(), error = function(e) character()
  )
  if (length(output) == 0L) NA_character_ else trimws(output[[1L]])
}

calibration_git_provenance <- function(project_root = getwd()) {
  root <- normalizePath(project_root, mustWork = TRUE)
  commit <- .calibration_git_command(root, c("rev-parse", "HEAD"))
  status <- .calibration_git_command(root, c("status", "--porcelain"))
  list(
    commit = commit,
    dirty = !is.na(status) && nzchar(status),
    status = if (is.na(status)) character() else status
  )
}

.calibration_package_versions <- function() {
  packages <- c("stabilitest", "R", "testthat", "pkgload")
  versions <- vapply(packages, function(package) {
    if (identical(package, "R")) return(as.character(getRversion()))
    if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
    as.character(utils::packageVersion(package))
  }, character(1))
  versions
}

.calibration_seed_ledger <- function(scenarios, master_seed = NA_integer_) {
  if (!is.data.frame(scenarios) || !"scenario_id" %in% names(scenarios)) {
    .calibration_manifest_abort("scenarios must contain scenario_id")
  }
  ids <- as.character(scenarios$scenario_id)
  scenario_seeds <- if ("scenario_seed" %in% names(scenarios)) {
    as.integer(scenarios$scenario_seed)
  } else rep(NA_integer_, length(ids))
  derived <- scenario_seeds
  if (exists("scenario_seed", mode = "function", inherits = TRUE) &&
      is.numeric(master_seed) && length(master_seed) == 1L && !is.na(master_seed)) {
    derived <- vapply(ids, scenario_seed, integer(1), master_seed = as.integer(master_seed))
  }
  setNames(lapply(seq_along(ids), function(index) list(
    scenario_id = ids[[index]],
    configured_seed = scenario_seeds[[index]],
    derived_seed = derived[[index]],
    master_seed = master_seed
  )), ids)
}

#' Build an in-memory audit manifest before a run starts.
new_calibration_manifest <- function(scenarios, options, command = commandArgs(),
                                     project_root = getwd(), start_time = Sys.time()) {
  if (!is.list(options)) .calibration_manifest_abort("options must be a list")
  git <- calibration_git_provenance(project_root)
  workers <- if (is.null(options$workers)) 1L else as.integer(options$workers)
  list(
    manifest_version = "calibration-1",
    scenario_manifest_hash = calibration_scenario_hash(scenarios),
    scenario_count = nrow(scenarios),
    seed_ledger = .calibration_seed_ledger(scenarios, options$master_seed %||% NA_integer_),
    git_commit = git$commit,
    git_dirty = isTRUE(git$dirty),
    git_status = git$status,
    command = as.character(command),
    options = options,
    package_versions = .calibration_package_versions(),
    r_session = capture.output(utils::sessionInfo()),
    start_time = as.POSIXct(start_time, tz = "UTC"),
    end_time = NULL,
    workers = workers,
    output_hashes = list()
  )
}

`%||%` <- function(left, right) if (is.null(left)) right else left

calibration_output_hashes <- function(output_files) {
  if (is.null(output_files) || length(output_files) == 0L) return(list())
  files <- unique(normalizePath(output_files, mustWork = FALSE))
  files <- files[file.exists(files) & !file.info(files)$isdir]
  setNames(lapply(files, function(path) list(
    path = path,
    hash = calibration_hash_file(path),
    bytes = unname(file.info(path)$size)
  )), files)
}

calibration_assert_clean <- function(project_root = getwd(), mode = "full",
                                     allow_dirty = FALSE) {
  git <- calibration_git_provenance(project_root)
  if (identical(mode, "full") && isTRUE(git$dirty) && !isTRUE(allow_dirty)) {
    .calibration_manifest_abort(
      "full calibration runs require a clean checkout; use --allow-dirty to override"
    )
  }
  invisible(git)
}

#' Write both machine-readable and review-friendly copies of a manifest.
write_calibration_manifest <- function(manifest, output_dir, output_files = NULL,
                                       end_time = Sys.time()) {
  if (!is.list(manifest)) .calibration_manifest_abort("manifest must be a list")
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    .calibration_manifest_abort("unable to create manifest output directory")
  }
  manifest$end_time <- as.POSIXct(end_time, tz = "UTC")
  manifest$output_hashes <- calibration_output_hashes(output_files)
  rds_path <- file.path(output_dir, "manifest.rds")
  temporary <- tempfile("manifest-", tmpdir = output_dir)
  saveRDS(manifest, temporary, version = 2)
  if (!file.rename(temporary, rds_path)) {
    unlink(temporary)
    .calibration_manifest_abort("unable to install manifest.rds")
  }
  dput(manifest, file = file.path(output_dir, "manifest.dput"))
  rds_path
}
