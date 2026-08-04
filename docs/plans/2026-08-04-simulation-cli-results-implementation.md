# Simulation CLI and Canonical Results Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Turn the manuscript simulation into a deterministic command-line entry point, archive the canonical 500-by-200 run, and guarantee that the published simulation tables match the archived CSV.

**Architecture:** Keep all simulation execution logic in `manuscript/simulation_study.R`, with a base-R CLI parser and direct-execution guard layered around the existing simulation functions. Add a separate source-safe checker that validates the canonical CSV and renders the exact Markdown rows expected in Sections 3.2 and 3.3; regenerate the manuscript PDF only after the CSV and Markdown agree.

**Tech Stack:** R 4.2+, tidyverse/readr, pkgload, base `Rscript` subprocesses, testthat package suite, Pandoc/Typst manuscript build, Git worktrees.

---

### Task 1: Specify and implement the CLI parser

**Files:**
- Modify: `manuscript/test_simulation_entrypoint.R`
- Modify: `manuscript/simulation_study.R:71-143`

**Step 1: Write failing parser tests**

Extend `manuscript/test_simulation_entrypoint.R` to source the simulation file
and call a new `.parse_simulation_args()` helper. Cover:

```r
defaults <- .parse_simulation_args(character(), simulation_file)
stopifnot(
  defaults$nrep == 500L,
  defaults$n_boot == 200L,
  identical(defaults$output,
            file.path(dirname(simulation_file), "simulation_results.csv")),
  !defaults$smoke,
  !defaults$help
)

smoke <- .parse_simulation_args("--smoke", simulation_file)
stopifnot(
  smoke$nrep == 2L,
  smoke$n_boot == 10L,
  identical(smoke$output,
            file.path(dirname(simulation_file),
                      "simulation_results_smoke.csv"))
)

custom <- .parse_simulation_args(
  c("--nrep", "7", "--n-boot", "11", "--output", "custom.csv"),
  simulation_file
)
stopifnot(custom$nrep == 7L, custom$n_boot == 11L)
```

Add a small `expect_parse_error(args, pattern)` helper using `tryCatch()` and
cover unknown options, missing values, zero/negative/non-integer counts,
duplicate options, `--smoke` combined with count overrides, and `--help`
combined with any run option.

**Step 2: Run the smoke harness and verify RED**

Run:

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: failure because `.parse_simulation_args()` does not exist.

**Step 3: Implement minimal parsing helpers**

In `manuscript/simulation_study.R`, add:

```r
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
```

Implement `.parse_simulation_args(args, script_path)` as a simple index-based
loop. Track seen option names to reject duplicates. Resolve the default output
against `dirname(script_path)` and explicit relative output paths against the
caller's working directory. Verify the output directory exists and is writable
before returning the options list.

**Step 4: Run the parser tests and verify GREEN**

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: parser assertions pass; the existing source/direct loader checks
continue to pass without starting a full run.

**Step 5: Commit parser behavior**

```bash
git add manuscript/simulation_study.R manuscript/test_simulation_entrypoint.R
git commit -m "feat: add simulation command-line parser"
```

### Task 2: Propagate canonical parameters and complete the result schema

**Files:**
- Modify: `manuscript/test_simulation_entrypoint.R`
- Modify: `manuscript/simulation_study.R:71-133`

**Step 1: Write failing simulation-schema tests**

After sourcing the script, run a deliberately small scenario grid by replacing
`scenarios` temporarily with two rows and call:

```r
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
```

Use `on.exit()` to restore the twelve-row global `scenarios` object.

**Step 2: Run and verify RED**

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: `run_simulation()` has no `n_boot` parameter and result columns are
missing.

**Step 3: Implement the schema and parameter propagation**

- Add `score_all_sd = stats::sd(score)` in `simulate_scenario()`.
- Add `n_boot` and `scenario_seed` to the leading provenance columns.
- Change `run_simulation(nrep = 500, n_boot = 200)`.
- Pass `n_boot` and `seed = 987000 + scenario` into each scenario.
- Derive the progress denominator from `nrow(scenarios)` rather than hard-code
  twelve, so the reduced harness reports accurately.

For `nrep = 1`, `score_all_sd` is legitimately `NA_real_`.

**Step 4: Run and verify GREEN**

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: the reduced schema test passes.

**Step 5: Commit schema completion**

```bash
git add manuscript/simulation_study.R manuscript/test_simulation_entrypoint.R
git commit -m "feat: record complete simulation provenance"
```

### Task 3: Add direct execution and protected CSV output

**Files:**
- Modify: `manuscript/test_simulation_entrypoint.R`
- Modify: `manuscript/simulation_study.R:135-143`

**Step 1: Write failing subprocess tests**

Replace the old bare-script smoke call, which would now start the canonical
run, with these subprocess checks:

1. `--help` exits zero, contains `Usage:`, and creates no CSV.
2. An invalid command such as `--nrep 0` exits nonzero with the parser error.
3. Direct execution with
   `--nrep 1 --n-boot 2 --output <temporary.csv>` exits zero and writes twelve
   rows with the required schema and supplied parameters.
4. Sourcing the file in a clean temporary directory creates no CSV.

Use the existing `run_rscript()` helper, extending it to optionally accept an
expected nonzero status and return captured output.

**Step 2: Run and verify RED**

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: direct execution exits without writing the requested CSV.

**Step 3: Implement the runner**

Add:

```r
.simulation_is_direct <- function(script_path = .simulation_script_path()) {
  file_args <- sub(
    "^--file=", "",
    grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  )
  length(file_args) == 1L &&
    identical(normalizePath(file_args[[1L]], mustWork = TRUE), script_path)
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
```

Add `.run_simulation_cli()` to parse trailing arguments, print usage for
`--help`, call `run_simulation()`, print the twelve-row summary, and install the
CSV. Replace `if (interactive())` with a direct-execution guard calling this
runner.

**Step 4: Run and verify GREEN**

```bash
Rscript manuscript/test_simulation_entrypoint.R
```

Expected: help, invalid-input, source-only, and direct reduced-run checks all
pass; no canonical run starts.

**Step 5: Commit CLI execution**

```bash
git add manuscript/simulation_study.R manuscript/test_simulation_entrypoint.R
git commit -m "feat: execute and archive simulation from Rscript"
```

### Task 4: Add a manuscript/result consistency checker

**Files:**
- Create: `manuscript/check_simulation_results.R`
- Create: `manuscript/test_simulation_results_check.R`

**Step 1: Write a failing checker test**

Create `test_simulation_results_check.R` that:

- locates and sources `simulation_study.R` and the new checker without
  triggering either CLI;
- generates a twelve-row reduced result with `nrep = 1`, `n_boot = 2`;
- calls `.simulation_table_1_rows()` and `.simulation_table_2_rows()` to build
  a temporary manuscript containing both headings and rendered rows;
- verifies `.check_simulation_results(..., expected_nrep = 1,
  expected_n_boot = 2)` succeeds;
- changes one published value and verifies the checker fails with
  `Simulation results and manuscript table 1 differ`;
- duplicates one scenario and verifies the checker fails with
  `exactly 12 unique scenarios`.

**Step 2: Run and verify RED**

```bash
Rscript manuscript/test_simulation_results_check.R
```

Expected: failure because the checker functions do not exist.

**Step 3: Implement source-safe rendering and validation**

In `check_simulation_results.R`:

- locate the script directory from `--file=` or `sys.frames()$ofile`;
- validate exact required columns, twelve unique scenario keys, canonical
  counts, and seeds `987001:987012` in scenario order;
- sort rows by `d`, `n_per_group`, then `n_outliers` to match the manuscript;
- format Table 1 and Table 2 rows at the design-approved precision;
- extract the Markdown table row blocks under `### 3.2` and `### 3.3`;
- compare normalized expected and actual row vectors exactly;
- expose `.check_simulation_results()` for the harness;
- when executed directly, default to `simulation_results.csv` and
  `robustness_analysis_manuscript.md`, print a success message, and exit zero.

Use em dash for non-finite conditional medians. Reject all other non-finite
canonical values with a column-specific error.

**Step 4: Run and verify GREEN**

```bash
Rscript manuscript/test_simulation_results_check.R
```

Expected: the matched temporary manuscript passes and the deliberately stale
and duplicate cases fail for the intended reasons.

**Step 5: Commit the checker**

```bash
git add manuscript/check_simulation_results.R \
  manuscript/test_simulation_results_check.R
git commit -m "test: check simulation results against manuscript"
```

### Task 5: Run and archive the canonical study

**Files:**
- Create: `manuscript/simulation_results.csv`
- Modify: `manuscript/robustness_analysis_manuscript.md:93-139`
- Modify: `manuscript/robustness_analysis_manuscript.md:249-260`
- Modify: `NEWS.md:3-25`

**Step 1: Run the canonical bare command**

```bash
Rscript manuscript/simulation_study.R
```

Expected: all twelve scenario progress messages complete over roughly one to
two hours, the command exits zero, and
`manuscript/simulation_results.csv` contains twelve canonical rows with
`nrep = 500`, `n_boot = 200`, and seeds `987001:987012`.

Do not claim completion from progress output. Inspect the full CSV, row count,
schema, and command exit status.

**Step 2: Prove the old manuscript is stale**

```bash
Rscript manuscript/check_simulation_results.R
```

Expected: fail with a table mismatch unless the deterministic run happens to
match every previously published rounded value.

**Step 3: Update the manuscript with exact canonical values**

Use the checker's rendered row helpers to obtain both expected tables. Apply
the rows to Sections 3.2 and 3.3 with `apply_patch`. Update the design paragraph
and software appendix to state:

- the tracked script generated the results directly;
- the tracked CSV is the numerical source;
- the bare command runs the canonical settings and writes beside the script;
- `--smoke` and explicit count/output flags are available for development.

Update narrative examples when canonical rounded values differ.

**Step 4: Verify manuscript synchronization**

```bash
Rscript manuscript/check_simulation_results.R
```

Expected: `Simulation results match manuscript tables.`

**Step 5: Update NEWS**

Document the executable canonical CLI, safe smoke/custom modes, archived
canonical CSV, full provenance/schema, and machine-checked manuscript tables.

**Step 6: Commit canonical results and Markdown**

```bash
git add NEWS.md manuscript/simulation_results.csv \
  manuscript/robustness_analysis_manuscript.md
git commit -m "docs: archive canonical simulation calibration"
```

### Task 6: Regenerate and inspect the manuscript PDF

**Files:**
- Modify: `manuscript/robustness_analysis_manuscript.pdf`

**Step 1: Invoke the PDF skill**

Read and follow `pdf:pdf` before regenerating or inspecting the tracked PDF.

**Step 2: Rebuild the PDF**

```bash
./manuscript/build_pdf.sh
```

Expected: the build exits zero and updates the tracked PDF from the synchronized
Markdown.

**Step 3: Render and visually inspect affected pages**

Render the PDF pages containing Sections 3.2, 3.3, and Appendix A. Check table
fit, glyphs, pagination, headings, and command-line examples.

**Step 4: Commit the regenerated PDF**

```bash
git add manuscript/robustness_analysis_manuscript.pdf
git commit -m "docs: regenerate calibrated manuscript PDF"
```

### Task 7: Verify the complete change set

**Files:**
- Verify all modified and created files

**Step 1: Run manuscript workflow checks**

```bash
Rscript manuscript/test_simulation_entrypoint.R
Rscript manuscript/test_simulation_results_check.R
Rscript manuscript/check_simulation_results.R
```

Expected: all commands exit zero; the reduced CLI test uses only temporary
output and canonical results match both manuscript tables.

**Step 2: Run the full package suite**

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
```

Expected: zero failures, warnings, and skips.

**Step 3: Check repository formatting and state**

```bash
git diff --check
git status --short
```

Expected: no whitespace errors and no uncommitted generated artifacts.

**Step 4: Build a fresh source archive with vignettes**

From a fresh temporary directory:

```bash
R CMD build --no-manual /absolute/path/to/worktree
```

Expected: vignette generation succeeds and the tarball contains `inst/doc` but
not `.git`, `manuscript`, or worktree artifacts.

**Step 5: Check the fresh source archive**

```bash
_R_CHECK_CRAN_INCOMING_=false _R_CHECK_SYSTEM_CLOCK_=false \
  R CMD check --no-manual --as-cran stabilitest_0.5.0.tar.gz
```

Expected: `Status: OK` with zero errors, warnings, or notes.

**Step 6: Inspect final history and branch state**

```bash
git status --short
git log --oneline main..HEAD
```

Expected: clean worktree with focused design, plan, CLI, checker, canonical
result, manuscript, and PDF commits ready for integration.
