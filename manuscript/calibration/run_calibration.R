#!/usr/bin/env Rscript

# Publication calibration runner.  The runner owns CLI/provenance concerns;
# screening and robustness execution are injected when their later modules are
# available, keeping this entry point usable for contract and smoke checks.

.calibration_runner_script <- function() {
  files <- vapply(sys.frames(), function(frame) {
    if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
  }, character(1))
  files <- files[!is.na(files) & basename(files) == "run_calibration.R"]
  if (length(files)) return(normalizePath(tail(files, 1L), mustWork = TRUE))
  command <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
  if (length(command) == 1L && file.exists(command)) normalizePath(command, mustWork = TRUE)
  else normalizePath(file.path("manuscript", "calibration", "run_calibration.R"), mustWork = TRUE)
}

.calibration_runner_root <- function(script = .calibration_runner_script()) {
  candidate <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  if (!all(file.exists(file.path(candidate, c("DESCRIPTION", "R/robustness_analysis.R"))))) {
    stop("Unable to locate the stabilitest project root", call. = FALSE)
  }
  candidate
}

.calibration_select_scenarios <- function(scenarios, options) {
  if (!is.data.frame(scenarios)) stop("calibration_scenarios() must return a data frame", call. = FALSE)
  selected <- scenarios
  if (!is.null(options$scenario)) {
    selected <- selected[selected$scenario_id == options$scenario, , drop = FALSE]
    if (!nrow(selected)) stop(sprintf("unknown calibration scenario: %s", options$scenario), call. = FALSE)
  }
  if (!identical(options$engine, "all")) {
    selected <- selected[selected$analysis_family == options$engine, , drop = FALSE]
  }
  if (isTRUE(options$validation_only)) {
    selected <- selected[selected$design_layer == "validation", , drop = FALSE]
  } else if (identical(options$mode, "full")) {
    selected <- selected[selected$design_layer != "validation", , drop = FALSE]
  }
  if (identical(options$mode, "smoke") && is.null(options$scenario)) {
    # Prefer the frozen *_smoke row and retain one row per analysis family.
    selected <- selected[order(!grepl("_smoke$", selected$scenario_id)), , drop = FALSE]
    selected <- selected[!duplicated(selected$analysis_family), , drop = FALSE]
  }
  if (!nrow(selected)) stop("no calibration scenarios match the requested filters", call. = FALSE)
  selected[order(selected$scenario_id), , drop = FALSE]
}

.calibration_call_hook <- function(hook, scenario, plan, options, project_root) {
  if (!is.function(hook)) return(NULL)
  # Hooks are deliberately passed named arguments so task 8/9 can evolve
  # without changing the command-line contract.
  hook(scenario = scenario, plan = plan, options = options, project_root = project_root)
}

run_calibration <- function(args = commandArgs(trailingOnly = TRUE), project_root = NULL,
                            hooks = list(), command = NULL) {
  script <- .calibration_runner_script()
  root <- if (is.null(project_root)) .calibration_runner_root(script) else normalizePath(project_root, mustWork = TRUE)
  runner_env <- environment()
  loader <- file.path(root, "manuscript", "calibration", "R", "load_calibration.R")
  if (!exists("parse_calibration_cli", envir = runner_env, inherits = TRUE)) {
    loader_env <- new.env(parent = parent.frame())
    sys.source(loader, envir = loader_env)
    loader_env$load_calibration(project_root = root, envir = runner_env)
  }
  options <- parse_calibration_cli(args)
  if (isTRUE(options$help)) {
    cat(calibration_cli_usage(), "\n")
    return(invisible(NULL))
  }
  calibration_assert_clean(root, options$mode, options$allow_dirty)
  scenarios <- calibration_scenarios()
  if (exists("validate_calibration_scenarios", envir = runner_env, inherits = TRUE)) {
    validate_calibration_scenarios(scenarios)
  }
  selected <- .calibration_select_scenarios(scenarios, options)
  plan <- calibration_run_plan(options)
  output <- normalizePath(options$output, mustWork = FALSE)
  if (!dir.exists(output) && !dir.create(output, recursive = TRUE, showWarnings = FALSE)) {
    stop(sprintf("unable to create output directory: %s", output), call. = FALSE)
  }
  start <- Sys.time()
  manifest <- new_calibration_manifest(
    scenarios = selected, options = options,
    command = command %||% c("Rscript", script, args),
    project_root = root, start_time = start
  )
  saveRDS(plan, file.path(output, "run-plan.rds"), version = 2)
  saveRDS(selected, file.path(output, "selected-scenarios.rds"), version = 2)

  screen_hook <- hooks$screen %||% if (exists("screen_scenario", envir = runner_env, inherits = TRUE)) {
    get("screen_scenario", envir = runner_env)
  } else NULL
  analyse_hook <- hooks$analyse %||% hooks$analyze %||%
    if (exists("run_full_scenario", envir = runner_env, inherits = TRUE)) {
      get("run_full_scenario", envir = runner_env)
    } else NULL
  results <- list()
  if (options$phase %in% c("screen", "all")) {
    results$screen <- lapply(seq_len(nrow(selected)), function(index) {
      .calibration_call_hook(screen_hook, selected[index, , drop = FALSE], plan, options, root)
    })
  }
  if (options$phase %in% c("analyse", "all")) {
    results$analyse <- lapply(seq_len(nrow(selected)), function(index) {
      .calibration_call_hook(analyse_hook, selected[index, , drop = FALSE], plan, options, root)
    })
  }
  saveRDS(results, file.path(output, "run-results.rds"), version = 2)
  manifest$start_time <- as.POSIXct(start, tz = "UTC")
  manifest$selected_scenario_ids <- selected$scenario_id
  manifest$mode_plan <- plan
  manifest_path <- write_calibration_manifest(
    manifest, output,
    output_files = list.files(output, full.names = TRUE, recursive = TRUE)
  )
  invisible(readRDS(manifest_path))
}

`%||%` <- function(left, right) if (is.null(left)) right else left

.calibration_is_direct <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("run_calibration[.]R$", args))
}

if (.calibration_is_direct()) run_calibration()
