#!/usr/bin/env Rscript

# Verify that every R Journal evidence artifact is traceable to the package,
# generation script, runtime environment, and published Welch verdict.

.script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("cannot locate artifact audit script", call. = FALSE)
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

script_path <- .script_path()
project_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."),
                              mustWork = TRUE)
artifact_root <- file.path(project_root, "manuscript", "rjournal", "artifacts")
study_root <- file.path(project_root, "manuscript", "calibration", "studies",
                        "welch_unpaired")
verdict_path <- file.path(study_root, "published", "VERDICT.json")
registry_path <- file.path(project_root, "inst", "extdata",
                           "calibration-registry.csv")

if (!file.exists(verdict_path)) stop("published Welch verdict is missing", call. = FALSE)
verdict_hash <- digest::digest(file = verdict_path, algo = "sha256")
registry <- utils::read.csv(registry_path, stringsAsFactors = FALSE,
                            na.strings = c("", "NA"), check.names = FALSE)
welch <- registry[registry$calibration_unit == "welch_unpaired", , drop = FALSE]
if (nrow(welch) != 1L) stop("active registry has no unique Welch row", call. = FALSE)

required_directories <- c("simulation", "case-study", "welch-calibration",
                          "timing", "testing")
required_fields <- c(
  "artifact_role", "package_version", "git_commit", "r_version",
  "script_path", "script_sha256", "seed", "runtime_seconds",
  "source_study_verdict_hash", "welch_calibration_status",
  "welch_label_emitted"
)
expected_version <- unname(read.dcf(file.path(project_root, "DESCRIPTION"))[
  1L, "Version"
])

violations <- character()
add_violation <- function(...) {
  violations <<- c(violations, sprintf(...))
}

for (directory in required_directories) {
  path <- file.path(artifact_root, directory)
  manifest_path <- file.path(path, "manifest.json")
  if (!dir.exists(path)) {
    add_violation("%s: artifact directory is missing", directory)
    next
  }
  if (!file.exists(manifest_path)) {
    add_violation("%s: manifest.json is missing", directory)
    next
  }
  manifest <- tryCatch(
    jsonlite::read_json(manifest_path, simplifyVector = TRUE),
    error = function(error) {
      add_violation("%s: manifest.json cannot be parsed: %s",
                    directory, conditionMessage(error))
      NULL
    }
  )
  if (is.null(manifest)) next

  missing <- setdiff(required_fields, names(manifest))
  if (length(missing)) {
    add_violation("%s: missing manifest fields: %s", directory,
                  paste(missing, collapse = ", "))
    next
  }
  if (!identical(as.character(manifest$package_version), expected_version)) {
    add_violation("%s: package version does not match DESCRIPTION", directory)
  }
  if (!grepl("^[0-9a-f]{40}$", manifest$git_commit)) {
    add_violation("%s: git_commit is not a full SHA-1", directory)
  }
  if (!grepl("^[0-9a-f]{64}$", manifest$script_sha256)) {
    add_violation("%s: script_sha256 is not SHA-256", directory)
  }
  generation_script <- file.path(project_root, manifest$script_path)
  if (!file.exists(generation_script)) {
    add_violation("%s: generation script is missing: %s",
                  directory, manifest$script_path)
  } else if (!identical(
    digest::digest(file = generation_script, algo = "sha256"),
    manifest$script_sha256
  )) {
    add_violation("%s: generation script hash mismatch", directory)
  }
  if (!is.numeric(manifest$runtime_seconds) ||
      length(manifest$runtime_seconds) != 1L ||
      !is.finite(manifest$runtime_seconds) || manifest$runtime_seconds < 0) {
    add_violation("%s: runtime_seconds is not a finite non-negative number",
                  directory)
  }
  if (!length(manifest$seed) || anyNA(manifest$seed) ||
      !nzchar(paste(manifest$seed, collapse = "/"))) {
    add_violation("%s: seed is missing", directory)
  }
  if (!identical(manifest$source_study_verdict_hash, verdict_hash)) {
    add_violation("%s: source Welch verdict hash mismatch", directory)
  }
  if (!identical(manifest$welch_calibration_status, welch$status)) {
    add_violation("%s: Welch status disagrees with active registry", directory)
  }
  expected_label <- identical(welch$status, "validated_method_specific")
  if (!is.logical(manifest$welch_label_emitted) ||
      length(manifest$welch_label_emitted) != 1L ||
      !identical(manifest$welch_label_emitted, expected_label)) {
    add_violation("%s: Welch label claim disagrees with active registry",
                  directory)
  }
}

case_path <- file.path(artifact_root, "case-study", "welch-case-study.rds")
if (file.exists(case_path)) {
  case_result <- readRDS(case_path)
  if (!identical(welch$status, "validated_method_specific") &&
      !is.na(case_result$robustness_interpretation)) {
    add_violation("case-study: stored Welch result contains a categorical label")
  }
}

if (length(violations)) {
  cat("R Journal artifact contract audit failed:\n")
  cat(paste0("- ", violations, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}

cat("R Journal artifact contract audit passed for ",
    length(required_directories), " evidence directories.\n", sep = "")
