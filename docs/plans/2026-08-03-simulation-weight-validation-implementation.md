# Simulation Entry Point and Weight Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the manuscript simulation load the exact current checkout from any working directory and make composite-weight validation reject malformed vectors with clear, consistent errors.

**Architecture:** `manuscript/simulation_study.R` will resolve its own location, verify the repository root, and load that checkout with `pkgload::load_all()`. The shared `validate_alpha_weights()` helper will enforce the complete named-vector contract used by two-sample, model, and TOST scoring; focused public-interface tests and a standalone manuscript subprocess smoke test will protect both fixes.

**Tech Stack:** R 4.2+, testthat 3, pkgload, devtools, base `Rscript` subprocesses, Git worktrees.

---

### Task 1: Specify and enforce the composite-weight contract

**Files:**
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `R/robustness_shared.R:21-29`

**Step 1: Write failing public-interface tests**

Extend the existing `robustness_analysis rejects bad alpha, weights, and
test_type` coverage with table-driven malformed inputs:

```r
bad_name_weights <- list(
  unnamed = c(0.4, 0.4, 0.2),
  missing_name = setNames(c(0.4, 0.4, 0.2),
                          c("jackknife", "", "bootstrap")),
  duplicate_name = c(jackknife = 0.4, jackknife = 0.4, bootstrap = 0.2),
  unknown_name = c(jackknife = 0.4, fragility = 0.4, other = 0.2),
  non_numeric = c(jackknife = "0.4", fragility = "0.4", bootstrap = "0.2")
)
for (bad_weights in bad_name_weights) {
  expect_error(
    robustness_analysis(g1, g2, weights = bad_weights, n_boot = 5),
    "weights must be a named numeric vector containing exactly"
  )
}

bad_value_weights <- list(
  missing = c(jackknife = 0.4, fragility = 0.4, bootstrap = NA_real_),
  not_a_number = c(jackknife = 0.4, fragility = 0.4, bootstrap = NaN),
  infinite = c(jackknife = 0.4, fragility = 0.4, bootstrap = Inf),
  negative = c(jackknife = -0.1, fragility = 0.6, bootstrap = 0.5)
)
for (bad_weights in bad_value_weights) {
  expect_error(
    robustness_analysis(g1, g2, weights = bad_weights, n_boot = 5),
    "weights must contain only finite, non-negative values"
  )
}

expect_error(
  robustness_analysis(
    g1, g2,
    weights = c(jackknife = 0.5, fragility = 0.5, bootstrap = 0.5),
    n_boot = 5
  ),
  "weights must sum to 1"
)
```

Add one malformed-vector assertion through `robustness_lm()` and one through
`robustness_tost()` to demonstrate that their shared engine exposes the same
contract. Update the valid custom-weight test to pass the names in non-canonical
order and retain its score-to-jackknife equality assertion.

**Step 2: Run the focused test and verify RED**

Run:

```bash
Rscript -e 'devtools::test(filter = "edge-cases", stop_on_failure = TRUE)'
```

Expected: failures show `subscript out of bounds`, `missing value where
TRUE/FALSE needed`, or the previous generic weight error instead of the new
messages.

**Step 3: Implement the minimal shared validation**

Replace the weight block in `validate_alpha_weights()` with:

```r
required_names <- c("jackknife", "fragility", "bootstrap")
valid_names <- is.numeric(weights) &&
  length(weights) == length(required_names) &&
  !is.null(names(weights)) &&
  !anyNA(names(weights)) &&
  all(nzchar(names(weights))) &&
  !anyDuplicated(names(weights)) &&
  setequal(names(weights), required_names)
if (!valid_names) {
  stop(
    paste(
      "weights must be a named numeric vector containing exactly:",
      paste(required_names, collapse = ", ")
    ),
    call. = FALSE
  )
}
if (any(!is.finite(weights)) || any(weights < 0)) {
  stop("weights must contain only finite, non-negative values", call. = FALSE)
}
if (abs(sum(weights) - 1) > 1e-8) {
  stop("weights must sum to 1", call. = FALSE)
}
```

Do not reorder or normalize the supplied vector; downstream scoring uses named
indexing.

**Step 4: Run focused tests and verify GREEN**

Run:

```bash
Rscript -e 'devtools::test(filter = "edge-cases", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "robustness_tost", stop_on_failure = TRUE)'
```

Expected: both contexts pass with no failures or warnings.

**Step 5: Commit the weight-validation fix**

```bash
git add R/robustness_shared.R tests/testthat/test-edge-cases.R
git commit -m "fix: validate composite weight names and values"
```

### Task 2: Add the failing simulation entry-point smoke test

**Files:**
- Create: `manuscript/test_simulation_entrypoint.R`

**Step 1: Write a standalone subprocess smoke test**

Create a base-R test runner that locates the repository from its own `--file=`
argument, obtains `Rscript` from `R.home("bin")`, and runs the simulation in
both supported modes:

```r
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Run this smoke test with Rscript", call. = FALSE)
}

test_file <- normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
project_root <- dirname(dirname(test_file))
simulation_file <- file.path(project_root, "manuscript", "simulation_study.R")
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
  "source(%s); stopifnot(is.function(simulate_scenario), is.function(run_simulation))",
  dQuote(simulation_file)
)
run_rscript(c("-e", shQuote(source_expression)), project_root)
```

**Step 2: Run the smoke test and verify RED**

Run:

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: the child simulation fails to find `robustness_analysis.R` from the
temporary working directory.

**Step 3: Commit the regression test**

```bash
git add manuscript/test_simulation_entrypoint.R
git commit -m "test: cover simulation checkout loading"
```

### Task 3: Load the current checkout from the simulation script

**Files:**
- Modify: `manuscript/simulation_study.R:15-18`

**Step 1: Add script-location and project-root helpers**

Before simulation definitions, add small internal helpers that:

- Prefer the most recent `sys.frames()` `ofile` named `simulation_study.R`
  when the script is sourced.
- Otherwise use a `--file=` argument whose basename is
  `simulation_study.R` when it is executed directly.
- Normalize the discovered path with `mustWork = TRUE`.
- Verify that the parent repository contains `DESCRIPTION` and
  `R/robustness_analysis.R`.
- Stop with `Unable to locate simulation_study.R` or `Unable to locate the
  stabilitest project root` when those contracts fail.

Use this shape:

```r
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
    return(normalizePath(source_files[[length(source_files)]], mustWork = TRUE))
  }

  file_args <- sub(
    "^--file=", "",
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
```

**Step 2: Replace the broken source call**

Keep `library(tidyverse)`, then load the checkout:

```r
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
```

Remove `source("robustness_analysis.R")`. Do not change the scenarios,
simulation functions, seeds, or interactive execution guard.

**Step 3: Run the smoke test and verify GREEN**

Run:

```bash
Rscript manuscript/test_simulation_entrypoint.R
Rscript manuscript/simulation_study.R
```

Expected: both commands exit with status zero without running the interactive
full grid.

**Step 4: Commit the simulation repair**

```bash
git add manuscript/simulation_study.R
git commit -m "fix: load current checkout in simulation script"
```

### Task 4: Document the repaired contracts

**Files:**
- Modify: `NEWS.md:1-20`
- Modify: `manuscript/robustness_analysis_manuscript.md:233-252`

**Step 1: Add development NEWS entries**

Under `## Correctness`, add entries stating that:

- Composite weights now require the exact three component names, finite
  non-negative values, and a unit sum, with precise validation errors.
- The manuscript simulation locates and loads its current checkout through
  `pkgload` instead of resolving a partial source file from the working
  directory.

**Step 2: Add simulation reproduction instructions**

In Appendix A, state that the Section 3 simulation requires the development
package `pkgload` and the existing tidyverse runtime, and can be launched from
any directory with:

```bash
Rscript /path/to/stabilitest/manuscript/simulation_study.R
```

Explain that the script loads the checkout containing itself, so it cannot
accidentally simulate against a stale installed package.

**Step 3: Review the documentation diff**

Run:

```bash
git diff --check
git diff -- NEWS.md manuscript/robustness_analysis_manuscript.md
```

Expected: no whitespace errors; wording agrees with the implemented behavior.

**Step 4: Commit documentation**

```bash
git add NEWS.md manuscript/robustness_analysis_manuscript.md
git commit -m "docs: explain simulation loading and weight validation"
```

### Task 5: Verify the complete change set

**Files:**
- Verify all modified files

**Step 1: Run focused regression checks**

```bash
Rscript -e 'devtools::test(filter = "edge-cases", stop_on_failure = TRUE)'
Rscript -e 'devtools::test(filter = "robustness_tost", stop_on_failure = TRUE)'
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: all focused tests pass and the smoke test exits zero.

**Step 2: Run the full package tests**

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

Expected: zero failures, warnings, or skips.

**Step 3: Check formatting and repository state**

```bash
git diff --check
git status --short
```

Expected: no whitespace errors and only intentional changes, if any, remain.

**Step 4: Run a clean source-archive check**

Build the package into a fresh temporary directory and run `R CMD check` on
the resulting tarball with `--no-manual --as-cran`. Do not check the live source
directory, because generated artifacts could mask omissions.

Expected: `Status: OK`, with zero errors, warnings, or notes.

**Step 5: Inspect the final branch history**

```bash
git status --short
git log --oneline main..HEAD
```

Expected: a clean worktree and focused design, test, implementation, and
documentation commits ready for review and integration.
