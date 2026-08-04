# Strict command-line parsing and run-mode contracts for calibration.

CALIBRATION_ENGINES <- c(
  "all", "two_sample", "proportion", "lm", "binomial", "poisson", "cox", "tost"
)
CALIBRATION_PHASES <- c("screen", "analyse", "all")
CALIBRATION_MODES <- c("smoke", "pilot", "full")

.calibration_cli_error <- function(message) {
  stop(message, call. = FALSE)
}

.calibration_cli_positive_integer <- function(value, option) {
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || !is.finite(parsed) ||
      parsed < 1 || parsed != floor(parsed) || parsed > .Machine$integer.max) {
    .calibration_cli_error(sprintf("%s must be a positive integer", option))
  }
  as.integer(parsed)
}

.calibration_cli_integer <- function(value, option) {
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || !is.finite(parsed) ||
      parsed != floor(parsed) || abs(parsed) > .Machine$integer.max) {
    .calibration_cli_error(sprintf("%s must be an integer", option))
  }
  as.integer(parsed)
}

calibration_cli_usage <- function() {
  paste(
    "Usage: Rscript manuscript/calibration/run_calibration.R [options]", "",
    "Required:",
    "  --mode smoke|pilot|full", "",
    "Options:",
    "  --phase screen|analyse|all    (default: all)",
    "  --engine all|two_sample|proportion|lm|binomial|poisson|cox|tost",
    "                                 (default: all)",
    "  --scenario ID                 restrict to one scenario",
    "  --workers N                   positive integer (default: 1)",
    "  --resume                      reuse valid checkpoints",
    "  --master-seed N               deterministic master seed (default: 20260804)",
    "  --output PATH                 output directory (required)",
    "  --validation-only             run held-out validation rows (full mode)",
    "  --allow-dirty                 permit a dirty checkout (full mode)",
    "  --help                        show this message",
    sep = "\n"
  )
}

#' Parse the frozen calibration command-line interface.
#'
#' Unknown flags, duplicate options, missing values, and invalid combinations
#' are rejected before any output is created.  `--mode` and `--output` are
#' intentionally required so that an accidental invocation cannot start a
#' production run in the current directory.
parse_calibration_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (!is.character(args)) .calibration_cli_error("args must be a character vector")

  value_options <- c("--mode", "--phase", "--engine", "--scenario",
                     "--workers", "--master-seed", "--output")
  flag_options <- c("--resume", "--validation-only", "--allow-dirty", "--help")
  known <- c(value_options, flag_options)
  seen <- character()
  values <- list()
  index <- 1L
  while (index <= length(args)) {
    option <- args[[index]]
    if (!identical(option, as.character(option)) || !option %in% known) {
      .calibration_cli_error(sprintf("Unknown option: %s", option))
    }
    if (option %in% seen) {
      .calibration_cli_error(sprintf("%s may only be supplied once", option))
    }
    seen <- c(seen, option)
    if (option %in% value_options) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        .calibration_cli_error(sprintf("%s requires a value", option))
      }
      values[[option]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      values[[option]] <- TRUE
      index <- index + 1L
    }
  }

  if ("--help" %in% seen) {
    if (length(seen) > 1L) .calibration_cli_error("--help cannot be combined with run options")
    return(list(help = TRUE))
  }
  if (!"--mode" %in% seen) .calibration_cli_error("--mode is required")
  if (!"--output" %in% seen) .calibration_cli_error("--output is required")

  mode <- values[["--mode"]]
  if (!mode %in% CALIBRATION_MODES) {
    .calibration_cli_error("mode must be one of smoke, pilot, or full")
  }
  phase <- if ("--phase" %in% seen) values[["--phase"]] else "all"
  if (!phase %in% CALIBRATION_PHASES) {
    .calibration_cli_error("phase must be one of screen, analyse, or all")
  }
  engine <- if ("--engine" %in% seen) values[["--engine"]] else "all"
  if (!engine %in% CALIBRATION_ENGINES) {
    .calibration_cli_error("engine must be one of all, two_sample, proportion, lm, binomial, poisson, cox, or tost")
  }
  scenario <- if ("--scenario" %in% seen) values[["--scenario"]] else NULL
  if (!is.null(scenario) && (!nzchar(scenario) || length(scenario) != 1L)) {
    .calibration_cli_error("--scenario requires a non-empty value")
  }
  workers <- if ("--workers" %in% seen) {
    .calibration_cli_positive_integer(values[["--workers"]], "--workers")
  } else 1L
  master_seed <- if ("--master-seed" %in% seen) {
    .calibration_cli_integer(values[["--master-seed"]], "--master-seed")
  } else 20260804L
  output <- values[["--output"]]
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    .calibration_cli_error("--output requires a non-empty path")
  }
  resume <- "--resume" %in% seen
  validation_only <- "--validation-only" %in% seen
  allow_dirty <- "--allow-dirty" %in% seen
  if (resume && mode != "full") .calibration_cli_error("--resume is only supported in full mode")
  if (validation_only && mode != "full") {
    .calibration_cli_error("--validation-only is only supported in full mode")
  }
  if (validation_only && phase != "all") {
    .calibration_cli_error("--validation-only requires --phase all")
  }
  if (allow_dirty && mode != "full") {
    .calibration_cli_error("--allow-dirty is only supported in full mode")
  }

  list(
    mode = mode, phase = phase, engine = engine, scenario = scenario,
    workers = workers, resume = resume, master_seed = master_seed,
    output = output, validation_only = validation_only, allow_dirty = allow_dirty,
    help = FALSE
  )
}

#' Resolve mode-specific simulation quotas without executing any analyses.
calibration_run_plan <- function(options) {
  if (!is.list(options) || is.null(options$mode) ||
      !options$mode %in% CALIBRATION_MODES) {
    .calibration_cli_error("options$mode must be smoke, pilot, or full")
  }
  mode <- options$mode
  if (identical(mode, "smoke")) {
    return(utils::modifyList(options, list(
      n_boot = 5L, replicates_per_stratum = 2L,
      scenario_selector = "one_per_engine", max_screen_draws = 20L
    )))
  }
  if (identical(mode, "pilot")) {
    return(utils::modifyList(options, list(
      n_boot = 50L, replicates_per_stratum = c(10L, 25L),
      scenario_selector = "all_core_shapes", max_screen_draws = 250L
    )))
  }
  utils::modifyList(options, list(
    n_boot = 1000L, replicates_per_stratum = NULL,
    scenario_selector = if (isTRUE(options$validation_only)) "validation" else "frozen",
    # Screening is finite at runtime even though the frozen plan does not
    # impose a small production cap; this budget is large enough to fill both
    # conclusion strata while remaining resumable and auditable.
    max_screen_draws = 10000L
  ))
}

# Backward-compatible aliases used by runner tests and scripts.
.parse_calibration_args <- parse_calibration_cli
parse_calibration_args <- parse_calibration_cli
