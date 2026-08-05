#!/usr/bin/env Rscript

# Assemble per-scenario checkpoints into fitting and audit tables.  The
# functions in this file are deliberately source-safe: tests and future
# publication runners can load them without accidentally touching production
# artifacts.  The command-line wrapper is the only code that uses the default
# repository paths.

`%||%` <- function(left, right) if (is.null(left)) right else left

assembly_project_root <- function(root = NULL) {
  root <- root %||% getwd()
  normalizePath(root, mustWork = TRUE)
}

assembly_checkpoint_dir <- function(split, checkpoint_root = NULL, project_root = getwd()) {
  if (length(split) != 1L || is.na(split) || !split %in% c("training", "validation")) {
    stop("split must be training or validation", call. = FALSE)
  }
  project_root <- assembly_project_root(project_root)
  default <- file.path(project_root, "manuscript", "calibration", "artifacts", "raw",
                       split, "checkpoints", "full", "checkpoints")
  if (is.null(checkpoint_root)) return(default)
  checkpoint_root <- normalizePath(checkpoint_root, mustWork = FALSE)
  candidates <- c(
    file.path(checkpoint_root, split, "checkpoints", "full", "checkpoints"),
    file.path(checkpoint_root, "checkpoints", "full", "checkpoints"),
    file.path(checkpoint_root, split),
    checkpoint_root
  )
  existing <- candidates[dir.exists(candidates)]
  if (!length(existing)) return(candidates[[1L]])
  # A direct directory containing scenario subdirectories is preferred.  The
  # remaining candidates are useful for temporary fixtures and raw roots.
  direct <- existing[vapply(existing, function(path) {
    length(list.dirs(path, full.names = FALSE, recursive = FALSE)) > 0L
  }, logical(1L))]
  normalizePath(if (length(direct)) direct[[1L]] else existing[[1L]], mustWork = FALSE)
}

assembly_audit_row <- function(scenario_id, status = "unsupported", replicate_id = NA_integer_,
                               failure_stage = "assembly", failure_class = "unsupported_checkpoint",
                               failure_message = "checkpoint missing or unreadable", metadata = NULL) {
  row <- data.frame(
    scenario_id = as.character(scenario_id), replicate_id = as.integer(replicate_id),
    status = as.character(status), failure_stage = as.character(failure_stage),
    failure_class = as.character(failure_class), failure_message = as.character(failure_message),
    stringsAsFactors = FALSE
  )
  if (is.data.frame(metadata) && nrow(metadata)) {
    metadata <- metadata[1L, , drop = FALSE]
    for (column in setdiff(names(metadata), names(row))) row[[column]] <- metadata[[column]][[1L]]
  }
  row
}

assembly_scenario_registry <- function(scenarios = NULL, project_root = getwd(), envir = NULL) {
  if (!is.null(scenarios)) return(scenarios)
  if (!is.null(envir) && exists("calibration_scenarios", envir = envir, mode = "function", inherits = TRUE)) {
    return(get("calibration_scenarios", envir = envir, inherits = TRUE)())
  }
  config <- file.path(assembly_project_root(project_root), "manuscript", "calibration",
                      "config", "scenarios.R")
  if (!file.exists(config)) return(NULL)
  env <- new.env(parent = globalenv())
  sys.source(config, envir = env)
  if (exists("calibration_scenarios", envir = env, mode = "function")) env$calibration_scenarios() else NULL
}

assembly_normalise_replicates <- function(replicates, validator = NULL) {
  if (!is.data.frame(replicates) || !nrow(replicates)) return(replicates)
  completed <- !is.na(replicates$status) & replicates$status == "completed"
  if (any(completed) && "original_p" %in% names(replicates)) {
    replicates$original_p[completed] <- pmin(1, pmax(0, as.numeric(replicates$original_p[completed])))
  }
  if (any(completed) && "effective_p" %in% names(replicates)) {
    replicates$effective_p[completed] <- pmin(1, pmax(0, as.numeric(replicates$effective_p[completed])))
  }
  if (is.function(validator)) validator(replicates)
  replicates
}

#' Collect one checkpoint split.
#'
#' Returns completed rows for fitting and every non-completed/unsupported row
#' for audit.  Missing checkpoints are represented by one unsupported audit
#' row per scenario in the frozen scenario registry.
collect_checkpoint_split <- function(split, checkpoint_root = NULL, scenarios = NULL,
                                      project_root = getwd(), validator = NULL) {
  ck_root <- assembly_checkpoint_dir(split, checkpoint_root, project_root)
  scenario_registry <- assembly_scenario_registry(scenarios, project_root)
  if (!dir.exists(ck_root)) {
    stop(sprintf("checkpoint root missing for %s: %s", split, ck_root), call. = FALSE)
  }
  dirs <- list.dirs(ck_root, full.names = TRUE, recursive = FALSE)
  scenario_ids <- if (is.data.frame(scenario_registry) && "scenario_id" %in% names(scenario_registry)) {
    keep <- if ("design_layer" %in% names(scenario_registry) && identical(split, "validation")) {
      scenario_registry$design_layer == "validation"
    } else if ("design_layer" %in% names(scenario_registry)) {
      scenario_registry$design_layer != "validation"
    } else rep(TRUE, nrow(scenario_registry))
    as.character(scenario_registry$scenario_id[keep])
  } else character()
  scenario_ids <- unique(c(scenario_ids, basename(dirs)))
  fitting <- list()
  audit <- list()
  for (scenario_id in sort(scenario_ids)) {
    scenario_meta <- if (is.data.frame(scenario_registry) && "scenario_id" %in% names(scenario_registry)) {
      scenario_registry[scenario_registry$scenario_id == scenario_id, , drop = FALSE]
    } else NULL
    path <- file.path(ck_root, scenario_id, "full.rds")
    if (!file.exists(path)) {
      audit[[length(audit) + 1L]] <- assembly_audit_row(
        scenario_id, metadata = scenario_meta,
        failure_message = "no full checkpoint for scenario"
      )
      next
    }
    payload <- tryCatch(readRDS(path), error = function(error) error)
    if (inherits(payload, "error")) {
      audit[[length(audit) + 1L]] <- assembly_audit_row(
        scenario_id, metadata = scenario_meta,
        failure_class = "checkpoint_read_error",
        failure_message = conditionMessage(payload)
      )
      next
    }
    replicates <- payload$payload$replicates %||% payload$replicates %||% payload
    if (!is.data.frame(replicates) || !nrow(replicates)) {
      audit[[length(audit) + 1L]] <- assembly_audit_row(
        scenario_id, metadata = scenario_meta,
        failure_class = "empty_checkpoint",
        failure_message = "checkpoint contains no replicate rows"
      )
      next
    }
    replicates <- assembly_normalise_replicates(replicates, validator)
    completed <- !is.na(replicates$status) & replicates$status == "completed"
    if (any(completed)) fitting[[length(fitting) + 1L]] <- replicates[completed, , drop = FALSE]
    if (any(!completed)) audit[[length(audit) + 1L]] <- replicates[!completed, , drop = FALSE]
  }
  fitting <- if (length(fitting)) dplyr::bind_rows(fitting) else data.frame()
  audit <- if (length(audit)) dplyr::bind_rows(audit) else data.frame()
  if (nrow(fitting)) {
    ord <- intersect(c("analysis_engine", "calibration_unit", "scenario_id", "replicate_id"), names(fitting))
    fitting <- fitting[do.call(order, fitting[ord]), , drop = FALSE]
    rownames(fitting) <- NULL
  }
  if (nrow(audit)) {
    ord <- intersect(c("analysis_engine", "calibration_unit", "scenario_id", "replicate_id", "status"), names(audit))
    audit <- audit[do.call(order, audit[ord]), , drop = FALSE]
    rownames(audit) <- NULL
  }
  list(fitting = fitting, audit = audit, checkpoint_root = ck_root,
       scenario_registry = scenario_registry)
}

# Backward-compatible helper name used by early calibration tooling.
collect_split <- collect_checkpoint_split

assemble_replicates <- function(training_out, validation_out, training_audit_out = NULL,
                                validation_audit_out = NULL, checkpoint_root = NULL,
                                project_root = getwd(), scenarios = NULL, envir = NULL) {
  project_root <- assembly_project_root(project_root)
  if (is.null(envir)) envir <- new.env(parent = globalenv())
  validator <- if (exists("validate_calibration_replicates", envir = envir,
                           mode = "function", inherits = TRUE)) {
    get("validate_calibration_replicates", envir = envir, inherits = TRUE)
  } else NULL
  sc <- assembly_scenario_registry(scenarios, project_root, envir)
  training <- collect_checkpoint_split("training", checkpoint_root, sc, project_root, validator)
  validation <- collect_checkpoint_split("validation", checkpoint_root, sc, project_root, validator)
  training_audit_out <- training_audit_out %||% sub("replicates[.]rds$", "audit.rds", training_out)
  validation_audit_out <- validation_audit_out %||% sub("replicates[.]rds$", "audit.rds", validation_out)
  paths <- c(training_out, validation_out, training_audit_out, validation_audit_out)
  for (path in paths) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(training$fitting, training_out, version = 2)
  saveRDS(validation$fitting, validation_out, version = 2)
  saveRDS(training$audit, training_audit_out, version = 2)
  saveRDS(validation$audit, validation_audit_out, version = 2)
  invisible(list(training = training$fitting, validation = validation$fitting,
                 training_audit = training$audit, validation_audit = validation$audit,
                 training_path = training_out, validation_path = validation_out,
                 training_audit_path = training_audit_out, validation_audit_path = validation_audit_out))
}

.assemble_is_direct <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("assemble_replicates[.]R$", args))
}

if (.assemble_is_direct()) {
  root <- assembly_project_root()
  loader <- file.path(root, "manuscript", "calibration", "R", "load_calibration.R")
  env <- new.env(parent = globalenv())
  sys.source(loader, envir = env)
  env$load_calibration(project_root = root, envir = env)
  args <- commandArgs(trailingOnly = TRUE)
  training_out <- if (length(args) >= 1L && nzchar(args[[1L]])) args[[1L]] else
    file.path(root, "manuscript/calibration/artifacts/raw/assembled/training-replicates.rds")
  validation_out <- if (length(args) >= 2L && nzchar(args[[2L]])) args[[2L]] else
    file.path(root, "manuscript/calibration/artifacts/raw/assembled/validation-replicates.rds")
  result <- assemble_replicates(training_out, validation_out, project_root = root, envir = env)
  cat("Wrote:\n  ", result$training_path, "\n  ", result$validation_path,
      "\n  ", result$training_audit_path, "\n  ", result$validation_audit_path, "\n", sep = "")
  for (split in c("training", "validation")) {
    x <- result[[split]]
    cat(sprintf("%s: %d completed replicates across %d scenarios / %d families\n",
                split, nrow(x), length(unique(x$scenario_id)),
                length(unique(x$analysis_family))))
  }
}
