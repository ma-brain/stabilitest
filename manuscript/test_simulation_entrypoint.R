# Smoke-test the manuscript simulation entry point without running its full grid.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Run this smoke test with Rscript", call. = FALSE)
}

test_file <- normalizePath(
  sub("^--file=", "", file_arg[[1L]]),
  mustWork = TRUE
)
project_root <- dirname(dirname(test_file))
simulation_file <- file.path(
  project_root,
  "manuscript",
  "simulation_study.R"
)
rscript <- file.path(R.home("bin"), "Rscript")

run_rscript <- function(arguments, working_directory) {
  old_directory <- setwd(working_directory)
  on.exit(setwd(old_directory), add = TRUE)

  output <- system2(rscript, arguments, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  if (status != 0L) {
    stop(paste(output, collapse = "\n"), call. = FALSE)
  }
  invisible(output)
}

source(simulation_file)

expect_parse_error <- function(arguments, pattern) {
  message <- tryCatch(
    {
      .parse_simulation_args(arguments, simulation_file)
      NA_character_
    },
    error = conditionMessage
  )
  stopifnot(!is.na(message), grepl(pattern, message))
}

defaults <- .parse_simulation_args(character(), simulation_file)
stopifnot(
  defaults$nrep == 500L,
  defaults$n_boot == 200L,
  identical(
    defaults$output,
    file.path(dirname(simulation_file), "simulation_results.csv")
  ),
  !defaults$smoke,
  !defaults$help
)

smoke <- .parse_simulation_args("--smoke", simulation_file)
stopifnot(
  smoke$nrep == 2L,
  smoke$n_boot == 10L,
  identical(
    smoke$output,
    file.path(dirname(simulation_file), "simulation_results_smoke.csv")
  ),
  smoke$smoke,
  !smoke$help
)

custom_output <- file.path(tempdir(), "custom-simulation.csv")
custom <- .parse_simulation_args(
  c(
    "--nrep", "7",
    "--n-boot", "11",
    "--output", custom_output
  ),
  simulation_file
)
stopifnot(
  custom$nrep == 7L,
  custom$n_boot == 11L,
  identical(
    custom$output,
    file.path(normalizePath(dirname(custom_output)), basename(custom_output))
  ),
  !custom$smoke,
  !custom$help
)

help <- .parse_simulation_args("--help", simulation_file)
stopifnot(help$help)

expect_parse_error("--unknown", "Unknown option")
expect_parse_error("--nrep", "requires a value")
expect_parse_error(c("--nrep", "0"), "positive integer")
expect_parse_error(c("--nrep", "-1"), "positive integer")
expect_parse_error(c("--nrep", "1.5"), "positive integer")
expect_parse_error(c("--n-boot", "NaN"), "positive integer")
expect_parse_error(
  c("--nrep", "2", "--nrep", "3"),
  "may only be supplied once"
)
expect_parse_error(
  c("--smoke", "--nrep", "2"),
  "cannot be combined"
)
expect_parse_error(
  c("--help", "--output", custom_output),
  "cannot be combined"
)
expect_parse_error(
  c("--output", file.path(tempdir(), "missing", "results.csv")),
  "output directory does not exist"
)

test_simulation_schema <- function() {
  simulation_environment <- environment(run_simulation)
  full_scenarios <- get("scenarios", envir = simulation_environment)
  on.exit(
    assign("scenarios", full_scenarios, envir = simulation_environment),
    add = TRUE
  )
  assign(
    "scenarios",
    full_scenarios[seq_len(2L), , drop = FALSE],
    envir = simulation_environment
  )

  result <- run_simulation(nrep = 1L, n_boot = 2L)
  required <- c(
    "nrep", "n_boot", "scenario_seed", "d", "n_per_group", "n_outliers",
    "rejection_rate", "score_all", "score_all_sd", "score_sig",
    "score_nonsig", "k_wc_med_sig", "frag_wc_med_sig", "k_ex_med_sig",
    "s_jack_sig", "s_boot_sig"
  )
  stopifnot(
    identical(names(result), required),
    all(result$nrep == 1L),
    all(result$n_boot == 2L),
    identical(result$scenario_seed, 987001:987002)
  )
}

test_simulation_schema()

run_rscript(shQuote(simulation_file), tempdir())

source_expression <- sprintf(
  paste(
    "source(%s);",
    "stopifnot(is.function(simulate_scenario), is.function(run_simulation))"
  ),
  encodeString(simulation_file, quote = "\"")
)
run_rscript(c("-e", shQuote(source_expression)), project_root)
