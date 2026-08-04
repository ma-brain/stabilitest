# ==============================================================================
# Simulation study validating the robustness framework (manuscript Section 3)
#
# Design: 3 effect sizes (d = 0, 0.5, 0.8) x 2 sample sizes (n = 25, 50 per
# group) x 2 contamination levels (0 or 2 outliers at +4 SD injected into the
# treated group), 500 replications per scenario, B = 200 bootstrap iterations.
#
# NOTE ON RUNTIME: each replication performs ~1500-3500 significance tests,
# so the full grid takes roughly 1-2 h single-threaded. For a smoke test use
# nrep = 25. The manuscript results were produced with an algorithmically
# identical implementation (same tests, same greedy rules, same seeds
# structure); Monte Carlo SE for a proportion at nrep = 500 is <= 0.022, and
# reported values are stable to that precision across RNGs.
# ==============================================================================

library(tidyverse)

.simulation_script_path <- function() {
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)
    },
    character(1)
  )
  source_files <- source_files[
    !is.na(source_files) & basename(source_files) == "simulation_study.R"
  ]
  if (length(source_files) > 0L) {
    return(normalizePath(
      source_files[[length(source_files)]],
      mustWork = TRUE
    ))
  }

  file_args <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  file_args <- file_args[basename(file_args) == "simulation_study.R"]
  if (length(file_args) == 1L) {
    return(normalizePath(file_args[[1L]], mustWork = TRUE))
  }

  stop("Unable to locate simulation_study.R", call. = FALSE)
}

.simulation_project_root <- function(script_path = .simulation_script_path()) {
  root <- dirname(dirname(script_path))
  markers <- file.path(root, c("DESCRIPTION", "R/robustness_analysis.R"))
  if (!all(file.exists(markers))) {
    stop("Unable to locate the stabilitest project root", call. = FALSE)
  }
  root
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop(
    "The pkgload package is required to run the simulation from this checkout",
    call. = FALSE
  )
}
pkgload::load_all(
  .simulation_project_root(),
  export_all = FALSE,
  helpers = FALSE,
  quiet = TRUE
)

.simulation_usage <- function() {
  paste(
    "Usage: Rscript manuscript/simulation_study.R [options]",
    "",
    "Options:",
    "  --nrep N       replications per scenario (default: 500)",
    "  --n-boot B     bootstrap iterations (default: 200)",
    "  --output PATH  destination CSV",
    "  --smoke        run nrep=2, n_boot=10 to a separate smoke CSV",
    "  --help         show this message without running",
    sep = "\n"
  )
}

.parse_positive_integer <- function(value, option) {
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || !is.finite(parsed) ||
      parsed < 1 || parsed != floor(parsed)) {
    stop(sprintf("%s must be a positive integer", option), call. = FALSE)
  }
  as.integer(parsed)
}

.parse_simulation_args <- function(args, script_path = .simulation_script_path()) {
  value_options <- c("--nrep", "--n-boot", "--output")
  flag_options <- c("--smoke", "--help")
  known_options <- c(value_options, flag_options)
  seen <- character()
  values <- list()
  index <- 1L

  while (index <= length(args)) {
    option <- args[[index]]
    if (!option %in% known_options) {
      stop(sprintf("Unknown option: %s", option), call. = FALSE)
    }
    if (option %in% seen) {
      stop(sprintf("%s may only be supplied once", option), call. = FALSE)
    }
    seen <- c(seen, option)

    if (option %in% value_options) {
      if (index == length(args) || startsWith(args[[index + 1L]], "--")) {
        stop(sprintf("%s requires a value", option), call. = FALSE)
      }
      values[[option]] <- args[[index + 1L]]
      index <- index + 2L
    } else {
      values[[option]] <- TRUE
      index <- index + 1L
    }
  }

  help <- "--help" %in% seen
  smoke <- "--smoke" %in% seen
  if (help && length(seen) > 1L) {
    stop("--help cannot be combined with run options", call. = FALSE)
  }
  if (smoke && any(c("--nrep", "--n-boot") %in% seen)) {
    stop("--smoke cannot be combined with --nrep or --n-boot", call. = FALSE)
  }

  nrep <- if (smoke) {
    2L
  } else if ("--nrep" %in% seen) {
    .parse_positive_integer(values[["--nrep"]], "--nrep")
  } else {
    500L
  }
  n_boot <- if (smoke) {
    10L
  } else if ("--n-boot" %in% seen) {
    .parse_positive_integer(values[["--n-boot"]], "--n-boot")
  } else {
    200L
  }

  default_name <- if (smoke) {
    "simulation_results_smoke.csv"
  } else {
    "simulation_results.csv"
  }
  if ("--output" %in% seen) {
    output_value <- values[["--output"]]
    if (!nzchar(output_value)) {
      stop("--output requires a non-empty path", call. = FALSE)
    }
    output_directory <- dirname(output_value)
    if (!dir.exists(output_directory)) {
      stop("simulation output directory does not exist", call. = FALSE)
    }
    output <- file.path(
      normalizePath(output_directory, mustWork = TRUE),
      basename(output_value)
    )
  } else {
    output <- file.path(dirname(script_path), default_name)
    output_directory <- dirname(output)
  }
  if (file.access(output_directory, mode = 2L) != 0L) {
    stop("simulation output directory is not writable", call. = FALSE)
  }

  list(
    nrep = nrep,
    n_boot = n_boot,
    output = output,
    smoke = smoke,
    help = help
  )
}

simulate_scenario <- function(d, n_per_group, n_outliers,
                              nrep = 500, n_boot = 200, alpha = 0.05,
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  reps <- map_dfr(seq_len(nrep), \(rep) {
    g_ctrl <- rnorm(n_per_group, 0, 1)
    g_trt  <- rnorm(n_per_group, d, 1)
    if (n_outliers > 0) {
      # contamination inflating the apparent effect
      g_trt[seq_len(n_outliers)] <- d + 4
    }
    res <- robustness_analysis(g_trt, g_ctrl, test_type = "t.test",
                               alpha = alpha, n_boot = n_boot,
                               seed = sample.int(1e6, 1))
    m <- res$robustness_metrics
    tibble(
      rep         = rep,
      significant = res$original_significant,
      score       = m$overall_robustness,
      k_wc        = m$worstcase_fragility_k,
      frag_wc_pct = m$worstcase_fragility_pct,
      k_ex        = m$extreme_fragility_k,
      s_jack      = m$jackknife_conclusion_stability,
      s_boot      = m$bootstrap_reproducibility
    )
  })

  reps |>
    summarise(
      rejection_rate = mean(significant),
      score_all      = mean(score),
      score_all_sd   = stats::sd(score),
      # calibration is defined conditional on the significance outcome:
      score_sig      = mean(score[significant]),
      score_nonsig   = mean(score[!significant]),
      k_wc_med_sig   = median(k_wc[significant]),
      frag_wc_med_sig = median(frag_wc_pct[significant]),
      k_ex_med_sig   = median(k_ex[significant]),
      s_jack_sig     = mean(s_jack[significant]),
      s_boot_sig     = mean(s_boot[significant]),
      .groups = "drop"
    ) |>
    mutate(
      nrep = nrep,
      n_boot = n_boot,
      scenario_seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
      d = d,
      n_per_group = n_per_group,
      n_outliers = n_outliers,
      .before = 1
    )
}

# --- full grid ----------------------------------------------------------------
scenarios <- expand_grid(
  d          = c(0, 0.5, 0.8),
  n_per_group = c(25, 50),
  n_outliers = c(0, 2)
)

run_simulation <- function(nrep = 500, n_boot = 200) {
  scenario_count <- nrow(scenarios)
  scenarios |>
    mutate(scenario = row_number()) |>
    pmap_dfr(\(d, n_per_group, n_outliers, scenario) {
      scenario_seed <- 987000L + scenario
      message(sprintf(
        "Scenario %d/%d: d=%.1f, n=%d, outliers=%d",
        scenario, scenario_count, d, n_per_group, n_outliers
      ))
      simulate_scenario(d, n_per_group, n_outliers,
                        nrep = nrep, n_boot = n_boot, seed = scenario_seed)
    })
}

.simulation_is_direct <- function(script_path = .simulation_script_path()) {
  file_args <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  length(file_args) == 1L &&
    identical(
      normalizePath(file_args[[1L]], mustWork = TRUE),
      normalizePath(script_path, mustWork = TRUE)
    )
}

.write_simulation_results <- function(results, output) {
  temporary <- tempfile(
    pattern = paste0(".", basename(output), "."),
    tmpdir = dirname(output),
    fileext = ".tmp"
  )
  on.exit(unlink(temporary), add = TRUE)
  readr::write_csv(results, temporary, na = "NA")

  backup <- NULL
  if (file.exists(output)) {
    backup <- tempfile(
      pattern = paste0(".", basename(output), ".backup."),
      tmpdir = dirname(output)
    )
    if (!file.rename(output, backup)) {
      stop("Unable to protect the existing simulation output", call. = FALSE)
    }
  }

  if (!file.rename(temporary, output)) {
    if (!is.null(backup)) file.rename(backup, output)
    stop("Unable to install the completed simulation output", call. = FALSE)
  }
  if (!is.null(backup)) unlink(backup)
  invisible(output)
}

.run_simulation_cli <- function(args = commandArgs(trailingOnly = TRUE),
                                script_path = .simulation_script_path()) {
  options <- .parse_simulation_args(args, script_path)
  if (options$help) {
    cat(.simulation_usage(), "\n")
    return(invisible(NULL))
  }

  message(sprintf(
    "Running simulation with nrep=%d, n_boot=%d",
    options$nrep,
    options$n_boot
  ))
  results <- run_simulation(nrep = options$nrep, n_boot = options$n_boot)
  print(results, width = Inf)
  .write_simulation_results(results, options$output)
  message("Simulation results written to ", options$output)
  invisible(results)
}

if (.simulation_is_direct()) {
  .run_simulation_cli()
}
