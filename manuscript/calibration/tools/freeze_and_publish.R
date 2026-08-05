#!/usr/bin/env Rscript

# Assemble -> fit -> freeze -> held-out evaluate once -> write published/.
# Helpers are source-safe so tests can exercise provenance in temporary
# directories without invoking a publication run or changing committed files.

`%||%` <- function(left, right) if (is.null(left)) right else left

publication_script_root <- function(fallback = getwd()) {
  file_args <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  script <- file_args[basename(file_args) == "freeze_and_publish.R"]
  if (length(script) == 1L && file.exists(script[[1L]])) {
    return(normalizePath(file.path(dirname(script[[1L]]), "..", ".."), mustWork = TRUE))
  }
  normalizePath(fallback, mustWork = TRUE)
}

publication_assembly_status <- function(status) {
  status <- as.integer(status)
  if (length(status) != 1L || is.na(status) || status != 0L) {
    stop("calibration replicate assembly failed", call. = FALSE)
  }
  invisible(status)
}

run_assembly_subprocess <- function(script, training_path, validation_path,
                                    runner = "Rscript") {
  status <- system2(runner, c(script, training_path, validation_path), stdout = "", stderr = "")
  publication_assembly_status(status)
}

publication_split_scenarios <- function(scenarios, split) {
  if (!is.data.frame(scenarios) || !nrow(scenarios)) return(scenarios)
  if (!"design_layer" %in% names(scenarios)) return(scenarios)
  if (identical(split, "validation")) scenarios[scenarios$design_layer == "validation", , drop = FALSE]
  else scenarios[scenarios$design_layer != "validation", , drop = FALSE]
}

publication_unsupported_reasons <- function(audit, scenarios = NULL, split = "training",
                                            replicates = NULL) {
  values <- character()
  if (is.data.frame(audit) && nrow(audit) && "status" %in% names(audit)) {
    unsupported <- audit[as.character(audit$status) == "unsupported", , drop = FALSE]
    if (nrow(unsupported)) {
      values <- paste0(as.character(unsupported$scenario_id), ": ",
                       as.character(unsupported$failure_message))
    }
  }
  if (is.data.frame(scenarios) && nrow(scenarios) && "scenario_id" %in% names(scenarios)) {
    sc <- publication_split_scenarios(scenarios, split)
    seen <- unique(c(
      if (is.data.frame(audit) && nrow(audit) && "scenario_id" %in% names(audit)) {
        as.character(audit$scenario_id)
      } else character(),
      if (is.data.frame(replicates) && nrow(replicates) && "scenario_id" %in% names(replicates)) {
        as.character(replicates$scenario_id)
      } else character()
    ))
    # Missing checkpoint scenarios are represented by audit rows; this branch
    # is retained for callers that provide a sparse audit fixture.
    missing <- setdiff(as.character(sc$scenario_id), seen)
    if (length(missing)) {
      values <- c(values, paste0(missing, ": no full checkpoint for scenario"))
    }
  }
  unique(values[nzchar(values)])
}

build_publication_manifest <- function(split, validation_only, replicates, audit,
                                       scenarios = NULL, scenario_manifest_hash = NA_character_,
                                       options = list(), n_boot = 1000L,
                                       target_replicates = 500L,
                                       minimum_heldout_stratum = 100L) {
  status <- if (is.data.frame(audit) && "status" %in% names(audit)) as.character(audit$status) else character()
  completed <- if (is.data.frame(replicates)) nrow(replicates) else 0L
  failed <- sum(status == "failed", na.rm = TRUE)
  excluded <- sum(status == "excluded", na.rm = TRUE)
  attempted <- completed + failed + excluded
  sc <- publication_split_scenarios(scenarios, split)
  scenario_ids <- if (is.data.frame(sc) && "scenario_id" %in% names(sc)) {
    sort(unique(as.character(sc$scenario_id)))
  } else sort(unique(c(
    if (is.data.frame(replicates) && "scenario_id" %in% names(replicates)) as.character(replicates$scenario_id) else character(),
    if (is.data.frame(audit) && "scenario_id" %in% names(audit)) as.character(audit$scenario_id) else character()
  )))
  list(
    artifact_kind = "calibration-publication-manifest",
    artifact_version = "calibration-publication-1",
    manifest_version = "calibration-1",
    status = "publication_run", mode = "full", split = split,
    options = utils::modifyList(list(mode = "full", phase = "all", engine = "all",
                                     validation_only = validation_only, split = split,
                                     workers = 6L, resume = TRUE), options),
    scenario_manifest_hash = as.character(scenario_manifest_hash),
    scenario_count = as.integer(length(scenario_ids)), scenario_ids = scenario_ids,
    n_boot = as.integer(n_boot), target_replicates = as.integer(target_replicates),
    minimum_heldout_stratum = as.integer(minimum_heldout_stratum),
    attempted_replicates = as.integer(attempted), completed_replicates = as.integer(completed),
    failed_replicates = as.integer(failed), excluded_replicates = as.integer(excluded),
    unsupported = publication_unsupported_reasons(audit, scenarios, split, replicates),
    reduced_fixture = FALSE
  )
}

# Names retained for callers of the pre-provenance publication helpers.
mk_manifest <- build_publication_manifest

write_failure_summary <- function(training_audit, validation_audit, path) {
  rows <- list()
  audits <- list(training = training_audit, validation = validation_audit)
  for (split_name in names(audits)) {
    audit <- audits[[split_name]]
    if (!is.data.frame(audit) || !nrow(audit)) next
    status <- if ("status" %in% names(audit)) as.character(audit$status) else rep(NA_character_, nrow(audit))
    stage <- if ("failure_stage" %in% names(audit)) as.character(audit$failure_stage) else rep(NA_character_, nrow(audit))
    cls <- if ("failure_class" %in% names(audit)) as.character(audit$failure_class) else rep(NA_character_, nrow(audit))
    rows[[length(rows) + 1L]] <- data.frame(split = split_name, status = status,
                                             failure_stage = stage, failure_class = cls,
                                             stringsAsFactors = FALSE)
  }
  summary <- if (length(rows)) {
    x <- do.call(rbind, rows)
    stats::aggregate(list(count = rep.int(1L, nrow(x))),
                     x[c("split", "status", "failure_stage", "failure_class")], sum)
  } else data.frame(split = character(), status = character(), failure_stage = character(),
                    failure_class = character(), count = integer(), stringsAsFactors = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, path, row.names = FALSE, na = "")
  invisible(summary)
}

production_hash_targets <- function(published) {
  required <- c("training-manifest.dput", "validation-manifest.dput",
                "calibration-registry.csv", "calibration-registry.rds",
                "non-significant-registry.csv", "failure-summary.csv")
  required[file.exists(file.path(published, required))]
}

write_output_hashes <- function(published, path = file.path(published, "output-hashes.txt")) {
  targets <- production_hash_targets(published)
  targets <- targets[!grepl("pilot", targets, ignore.case = TRUE)]
  if (!length(targets)) stop("no production publication artifacts to hash", call. = FALSE)
  lines <- vapply(targets, function(file) {
    sprintf("%s  manuscript/calibration/published/%s",
            unname(tools::md5sum(file.path(published, file))), file)
  }, character(1L))
  writeLines(lines, path)
  invisible(lines)
}

hash_production_artifacts <- write_output_hashes

freeze_and_publish <- function(project_root = NULL, reassemble = FALSE,
                               assembly_runner = "Rscript", envir = NULL) {
  root <- publication_script_root(project_root %||% getwd())
  loader <- file.path(root, "manuscript", "calibration", "R", "load_calibration.R")
  if (is.null(envir)) {
    envir <- new.env(parent = globalenv())
    sys.source(loader, envir = envir)
    envir$load_calibration(project_root = root, envir = envir)
    sys.source(file.path(root, "manuscript", "calibration", "analyse_calibration.R"), envir = envir)
  }
  assembled_dir <- file.path(root, "manuscript", "calibration", "artifacts", "raw", "assembled")
  training_path <- file.path(assembled_dir, "training-replicates.rds")
  validation_path <- file.path(assembled_dir, "validation-replicates.rds")
  training_audit_path <- file.path(assembled_dir, "training-audit.rds")
  validation_audit_path <- file.path(assembled_dir, "validation-audit.rds")
  if (reassemble || !all(file.exists(c(training_path, validation_path,
                                        training_audit_path, validation_audit_path)))) {
    script <- file.path(root, "manuscript/calibration/tools/assemble_replicates.R")
    run_assembly_subprocess(script, training_path, validation_path, runner = assembly_runner)
  }
  training <- readRDS(training_path)
  validation <- readRDS(validation_path)
  training_audit <- if (file.exists(training_audit_path)) readRDS(training_audit_path) else data.frame()
  validation_audit <- if (file.exists(validation_audit_path)) readRDS(validation_audit_path) else data.frame()
  sc <- envir$calibration_scenarios()
  scenario_hash <- envir$calibration_scenario_hash(sc)
  train_manifest <- build_publication_manifest("training", FALSE, training, training_audit,
                                               sc, scenario_hash)
  valid_manifest <- build_publication_manifest("validation", TRUE, validation, validation_audit,
                                               sc, scenario_hash)
  out <- tempfile("calibration-publish-")
  dir.create(out)
  result <- envir$calibration_analysis_from_files(
    training, validation, train_manifest, valid_manifest,
    output = out, minimum_stratum_n = 100L
  )
  train_manifest$candidate_hash <- result$candidate_hash
  train_manifest$registry_hash <- result$registry_hash
  train_manifest$validation_refit <- isTRUE(result$validation$refit)
  valid_manifest$candidate_hash <- result$candidate_hash
  valid_manifest$registry_hash <- result$registry_hash
  valid_manifest$validation_refit <- isTRUE(result$validation$refit)
  published <- file.path(root, "manuscript", "calibration", "published")
  dir.create(published, recursive = TRUE, showWarnings = FALSE)
  for (name in c("calibration-registry.csv", "non-significant-registry.csv", "calibration-registry.rds")) {
    file.copy(file.path(out, name), file.path(published, name), overwrite = TRUE)
  }
  dput(train_manifest, file.path(published, "training-manifest.dput"))
  dput(valid_manifest, file.path(published, "validation-manifest.dput"))
  write_failure_summary(training_audit, validation_audit, file.path(published, "failure-summary.csv"))
  write_output_hashes(published)
  invisible(list(result = result, training_manifest = train_manifest,
                 validation_manifest = valid_manifest, published = published))
}

.freeze_is_direct <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("freeze_and_publish[.]R$", args))
}

if (.freeze_is_direct()) {
  args <- commandArgs(trailingOnly = TRUE)
  freeze_and_publish(reassemble = "--reassemble" %in% args)
}
