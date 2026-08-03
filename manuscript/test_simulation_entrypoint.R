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

run_rscript(shQuote(simulation_file), tempdir())

source_expression <- sprintf(
  paste(
    "source(%s);",
    "stopifnot(is.function(simulate_scenario), is.function(run_simulation))"
  ),
  dQuote(simulation_file)
)
run_rscript(c("-e", shQuote(source_expression)), project_root)
