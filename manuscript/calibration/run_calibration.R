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
  } else if (identical(options$mode, "pilot")) {
    # Pilot occupancy and runtime projections are based on the pre-specified
    # core shape set.  Stress and validation rows are reserved for production
    # or held-out runs and must not silently inflate pilot quotas.
    selected <- selected[selected$design_layer == "core", , drop = FALSE]
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

.calibration_call_hook <- function(hook, scenario, plan, options, project_root,
                                  adapter = NULL, screened = NULL, envir = parent.frame()) {
  if (!is.function(hook)) return(NULL)
  # Hooks are deliberately passed named arguments.  Subsetting by formals
  # keeps this compatible with both the small test doubles and the concrete
  # Task 8/9 APIs, while `...` hooks receive the complete context.
  arguments <- list(
    scenario = scenario, plan = plan, options = options,
    project_root = project_root, adapter = adapter, screened = screened, envir = envir
  )
  arguments$target_by_stratum <- .calibration_target_by_stratum(scenario, plan)
  arguments$max_draws <- if (is.finite(plan$max_screen_draws)) as.integer(plan$max_screen_draws) else as.integer(scenario$n_boot[[1L]])
  arguments$workers <- options$workers
  arguments$checkpoint_root <- file.path(options$output, "checkpoints")
  arguments$resume <- isTRUE(options$resume)
  arguments$n_boot <- plan$n_boot
  arguments$selected <- if (!is.null(screened)) screened$selected %||% screened else NULL
  arguments$replicate_ids <- if (is.data.frame(arguments$selected) && "replicate_id" %in% names(arguments$selected)) arguments$selected$replicate_id else NULL
  formals <- names(formals(hook))
  if ("..." %in% formals) return(do.call(hook, arguments))
  do.call(hook, arguments[intersect(names(arguments), formals)])
}

.calibration_adapter_for_scenario <- function(scenario, envir = parent.frame()) {
  family <- as.character(scenario$analysis_family[[1L]])
  lookup <- function(name) get(name, envir = envir, inherits = TRUE)
  if (family %in% c("two_sample", "proportion")) {
    adapter <- lookup("two_sample_adapter")()
    adapter$generate <- function(scenario, seed = NULL) lookup("generate_two_sample")(scenario, seed = seed)
    adapter$generate_data <- adapter$generate
    return(adapter)
  }
  if (family %in% c("lm", "binomial", "poisson") && exists("calibration_model_adapters", envir = envir, inherits = TRUE)) {
    return(lookup("calibration_model_adapters")()[[family]])
  }
  if (identical(family, "cox")) {
    generate <- lookup("generate_cox")
    screen <- lookup("screen_cox")
    robustness <- lookup("run_cox_adapter")
    return(list(
      generate = function(scenario, seed = NULL) {
        do.call(generate, c(list(seed = seed), scenario$parameters[[1L]]$generator))
      },
      primary_decision = function(data, scenario) {
        analysis <- scenario$parameters[[1L]]$analysis
        do.call(screen, c(list(data = data), analysis))
      },
      run_robustness = function(data, scenario, n_boot = NULL, seed = NULL) {
        analysis <- scenario$parameters[[1L]]$analysis
        args <- c(list(data = data, n_boot = n_boot %||% scenario$n_boot[[1L]], seed = seed), analysis)
        do.call(robustness, args)
      }
    ))
  }
  if (identical(family, "tost")) {
    generate <- lookup("generate_tost")
    screen <- lookup("screen_tost")
    robustness <- lookup("run_tost_adapter")
    return(list(
      generate = function(scenario, seed = NULL) {
        do.call(generate, c(list(seed = seed), scenario$parameters[[1L]]$generator))
      },
      primary_decision = function(data, scenario) {
        do.call(screen, c(list(data = data), scenario$parameters[[1L]]$analysis))
      },
      run_robustness = function(data, scenario, n_boot = NULL, seed = NULL) {
        args <- c(list(data = data, n_boot = n_boot %||% scenario$n_boot[[1L]], seed = seed), scenario$parameters[[1L]]$analysis)
        do.call(robustness, args)
      }
    ))
  }
  stop(sprintf("unsupported calibration analysis family: %s", family), call. = FALSE)
}

.calibration_target_by_stratum <- function(scenario, plan) {
  target <- if (is.null(plan$replicates_per_stratum)) {
    # The publication contract requires at least 500 completed core analyses;
    # use that frozen lower bound when no pilot quota is supplied.
    500L
  } else {
    value <- plan$replicates_per_stratum
    if (length(value) == 1L) value[[1L]] else value[[1L]]
  }
  truth <- as.character(scenario$truth_class[[1L]])
  target_conclusion <- as.character(scenario$target_conclusion[[1L]])
  conclusions <- if (identical(plan$scenario_selector, "one_per_engine")) target_conclusion else switch(
    target_conclusion,
    significant = c("significant", "non_significant"),
    non_significant = c("significant", "non_significant"),
    equivalent = c("equivalent", "not_equivalent"),
    not_equivalent = c("equivalent", "not_equivalent"),
    noninferior = c("noninferior", "inferior"),
    inferior = c("noninferior", "inferior"),
    target_conclusion
  )
  stats::setNames(rep(as.integer(target), length(conclusions)),
                  paste(truth, conclusions, sep = "::"))
}

.calibration_default_screen <- function(scenario, plan, options, project_root,
                                        adapter = NULL, envir = parent.frame(), ...) {
  if (!exists("screen_scenario", envir = envir, mode = "function", inherits = TRUE)) return(NULL)
  screen <- get("screen_scenario", envir = envir, inherits = TRUE)
  adapter <- adapter %||% .calibration_adapter_for_scenario(scenario)
  max_draws <- if (is.finite(plan$max_screen_draws)) plan$max_screen_draws else max(2L, as.integer(scenario$n_boot[[1L]]))
  screen(
    scenario, adapter, .calibration_target_by_stratum(scenario, plan),
    max_draws = as.integer(max_draws), workers = options$workers,
    checkpoint_root = file.path(options$output, "checkpoints", "screen")
  )
}

.calibration_default_analyse <- function(scenario, plan, options, project_root,
                                         adapter = NULL, screened = NULL,
                                         envir = parent.frame(), ...) {
  if (!exists("run_full_scenario", envir = envir, mode = "function", inherits = TRUE)) return(NULL)
  execute <- get("run_full_scenario", envir = envir, inherits = TRUE)
  adapter <- adapter %||% .calibration_adapter_for_scenario(scenario)
  if (is.null(screened) || is.null(screened$selected)) {
    return(NULL)
  }
  selected <- screened$selected
  execute(
    scenario, adapter, selected = selected, master_seed = options$master_seed,
    workers = options$workers, checkpoint_root = file.path(options$output, "checkpoints", "full"),
    resume = isTRUE(options$resume), n_boot = plan$n_boot
  )
}

run_calibration <- function(args = commandArgs(trailingOnly = TRUE), project_root = NULL,
                            hooks = list(), command = NULL) {
  script <- if (is.null(project_root)) {
    .calibration_runner_script()
  } else {
    normalizePath(file.path(project_root, "manuscript", "calibration", "run_calibration.R"), mustWork = TRUE)
  }
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
    .calibration_default_screen
  } else NULL
  analyse_hook <- hooks$analyse %||% hooks$analyze %||%
    if (exists("run_full_scenario", envir = runner_env, inherits = TRUE)) {
      .calibration_default_analyse
  } else NULL
  results <- list()
  if (identical(options$phase, "analyse")) {
    prior_results_path <- file.path(output, "run-results.rds")
    if (!file.exists(prior_results_path)) {
      stop("--phase analyse requires a prior run-results.rds with screening results", call. = FALSE)
    }
    prior_results <- readRDS(prior_results_path)
    if (!is.list(prior_results$screen) || length(prior_results$screen) != nrow(selected)) {
      stop("prior run-results.rds has no matching screening results", call. = FALSE)
    }
    results$screen <- prior_results$screen
  }
  if (options$phase %in% c("screen", "all")) {
    results$screen <- lapply(seq_len(nrow(selected)), function(index) {
      scenario <- selected[index, , drop = FALSE]
      adapter <- if (is.function(screen_hook)) .calibration_adapter_for_scenario(scenario, runner_env) else NULL
      .calibration_call_hook(screen_hook, scenario, plan, options, root,
                             adapter = adapter, envir = runner_env)
    })
  }
  if (options$phase %in% c("analyse", "all")) {
    results$analyse <- lapply(seq_len(nrow(selected)), function(index) {
      scenario <- selected[index, , drop = FALSE]
      adapter <- if (is.function(analyse_hook)) .calibration_adapter_for_scenario(scenario, runner_env) else NULL
      screened <- if (!is.null(results$screen)) results$screen[[index]] else NULL
      .calibration_call_hook(analyse_hook, scenario, plan, options, root,
                             adapter = adapter, screened = screened, envir = runner_env)
    })
  }
  saveRDS(results, file.path(output, "run-results.rds"), version = 2)
  manifest$start_time <- as.POSIXct(start, tz = "UTC")
  manifest$selected_scenario_ids <- selected$scenario_id
  manifest$mode_plan <- plan
  output_files <- list.files(output, full.names = TRUE, recursive = TRUE)
  output_files <- output_files[!basename(output_files) %in% c("manifest.rds", "manifest.dput")]
  manifest_path <- write_calibration_manifest(
    manifest, output,
    output_files = output_files
  )
  invisible(readRDS(manifest_path))
}

`%||%` <- function(left, right) if (is.null(left)) right else left

.calibration_is_direct <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  any(grepl("run_calibration[.]R$", args))
}

if (.calibration_is_direct()) run_calibration()
