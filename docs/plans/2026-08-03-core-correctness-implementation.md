# Core Robustness Correctness Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix removal-fragility overstatement, align model robustness with fitted analysis rows, and handle non-finite two-sample bootstrap results explicitly.

**Architecture:** Add small shared validation/finalization helpers, then make each public engine supply correct feasibility and analysis-population inputs. Keep normal result schemas compatible while adding bootstrap validity counts to two-sample results.

**Tech Stack:** R 4.2+, testthat 3, dplyr, purrr, base R model APIs.

---

### Task 1: Reject unevaluable or incomplete fragility searches

**Files:**
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `R/robustness_shared.R`
- Modify: `R/robustness_analysis.R`
- Modify: `R/robustness_models.R`

**Step 1: Write failing tests**

Add tests asserting that:

```r
expect_error(
  robustness_analysis(1:4, 5:8, n_boot = 5),
  "Insufficient sample for fragility analysis"
)

tiny_model <- data.frame(y = rnorm(10), x = rnorm(10))
expect_error(
  robustness_lm(y ~ x, tiny_model, term = "x", n_boot = 5),
  "Insufficient sample for fragility analysis"
)
```

Also add an internal-helper test showing that a no-flip table evaluated through `max_k` returns `max_k + 1`, while a table ending before `max_k` errors with `Fragility removal search ended before`.

**Step 2: Run the focused test and verify RED**

Run:

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-edge-cases.R", reporter="summary")'
```

Expected: FAIL because minimum-size calls currently return high fragility scores and incomplete tables are accepted.

**Step 3: Implement minimal shared validation**

In `R/robustness_shared.R`:

- add a helper that errors when `max_k < 1` with `Insufficient sample for fragility analysis: cannot evaluate a removal while retaining the minimum sample size`;
- change `fragility_index_from_removal()` to use finite, non-missing conclusion rows, determine the largest evaluated `k_removed`, and error if a no-flip search ended before `max_k`;
- preserve `max_k + 1` only for a complete no-flip search.

In `R/robustness_analysis.R`:

- compute feasible removals as `length(group1) - 4` for paired tests and `(length(group1) - 4) + (length(group2) - 4)` otherwise;
- cap configured `max_k` at that capacity and validate it;
- make extreme-value candidate selection consider only groups with more than four observations, so it can complete the feasible horizon.

In `R/robustness_models.R`:

- cap configured `max_k` at `n - min_n` and validate it.

**Step 4: Run focused tests and verify GREEN**

Run the Task 1 command again. Expected: PASS.

**Step 5: Commit**

```bash
git add R/robustness_shared.R R/robustness_analysis.R R/robustness_models.R tests/testthat/test-edge-cases.R
git commit -m "fix: reject unevaluable fragility searches"
```

### Task 2: Align model robustness with fitted analysis rows

**Files:**
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `R/robustness_models.R`

**Step 1: Write failing complete-case tests**

Add an LM regression test with one missing predictor and assert:

```r
fit <- lm(y ~ x, data = dat)
res <- robustness_lm(y ~ x, dat, term = "x", n_boot = 10, seed = 1)
expect_equal(res$n, stats::nobs(fit))
expect_equal(nrow(res$jackknife), stats::nobs(fit))
```

Add a weighted-binomial GLM case with a missing predictor and assert that `res$n` equals `nobs(fit)` and `res$original_p` matches the direct weighted complete-case fit.

**Step 2: Run the focused test and verify RED**

Run the edge-case file command. Expected: FAIL because `res$n` currently uses raw rows.

**Step 3: Implement fit-row alignment**

Add an internal helper in `R/robustness_models.R`:

```r
analysis_data_from_fit <- function(data, fit) {
  omitted <- fit$na.action
  if (is.null(omitted)) return(data)
  data[-as.integer(omitted), , drop = FALSE]
}
```

After each full `lm`, `coxph`, or `glm` fit, restrict the data passed to `robustness_engine()` with this helper. Preserve the GLM private row ID so observation weights remain indexed to their original rows.

**Step 4: Run focused tests and verify GREEN**

Run the edge-case file command again. Expected: PASS.

**Step 5: Commit**

```bash
git add R/robustness_models.R tests/testthat/test-edge-cases.R
git commit -m "fix: use fitted model analysis rows"
```

### Task 3: Handle invalid rank bootstrap replicates

**Files:**
- Modify: `tests/testthat/test-robustness_analysis.R`
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `R/robustness_shared.R`
- Modify: `R/robustness_analysis.R`

**Step 1: Write failing bootstrap tests**

Add the reproduced tied-Wilcoxon case:

```r
res <- suppressWarnings(robustness_analysis(
  c(0, 0, 0, 1), c(0, 0, 1, 1),
  test_type = "wilcoxon", n_boot = 500, seed = 9
))
expect_true(is.finite(res$robustness_metrics$overall_robustness))
expect_gt(res$bootstrap$n_failed, 0)
expect_equal(res$bootstrap$n_valid + res$bootstrap$n_failed, 500)
```

Add a direct internal-helper test asserting that an all-non-finite bootstrap table errors with `No valid bootstrap replicates`.

**Step 2: Run focused tests and verify RED**

Run:

```bash
Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test-robustness_analysis.R", reporter="summary"); testthat::test_file("tests/testthat/test-edge-cases.R", reporter="summary")'
```

Expected: FAIL at `quantile()` and because validity fields/helper do not exist.

**Step 3: Implement explicit validity accounting**

In `R/robustness_shared.R`, add a helper returning the finite-p-value mask and valid/failed counts, erroring if no finite replicate exists.

In `R/robustness_analysis.R`:

- wrap each bootstrap `perform_test()` in `tryCatch()` and retain an `NA` row for failed tests;
- annotate all attempts, then calculate reproducibility from finite p-values only;
- use `na.rm = TRUE` for p-value quantiles, mean, and standard deviation;
- add `n_valid` and `n_failed` to the bootstrap result;
- update interpretation/printing text to show the valid denominator when failures occurred.

**Step 4: Run focused tests and verify GREEN**

Run the Task 3 command again. Expected: PASS with finite metrics and explicit failed-replicate accounting.

**Step 5: Commit**

```bash
git add R/robustness_shared.R R/robustness_analysis.R tests/testthat/test-robustness_analysis.R tests/testthat/test-edge-cases.R
git commit -m "fix: handle invalid bootstrap replicates"
```

### Task 4: Regenerate documentation and verify the package

**Files:**
- Modify if generated: `man/*.Rd`
- Modify: `NEWS.md`

**Step 1: Document the corrected behavior**

Add a development NEWS section describing removal-capacity errors, complete-case model alignment, and bootstrap validity counts. Update roxygen return/details text if the public result schema changed.

**Step 2: Regenerate documentation**

Run:

```bash
Rscript -e 'devtools::document()'
```

Expected: documentation regenerates without error.

**Step 3: Run the complete test suite**

```bash
Rscript -e 'testthat::test_local(reporter = "summary")'
```

Expected: all tests pass.

**Step 4: Run a clean source build and check**

Build from a clean Git archive in a temporary directory, then run `R CMD check --no-manual` on the tarball. Expected: `Status: OK`.

**Step 5: Check repository hygiene**

```bash
git diff --check
git status --short
```

Expected: no whitespace errors; only intended source, test, documentation, and plan changes are present.

**Step 6: Commit**

```bash
git add NEWS.md R man tests docs/plans/2026-08-03-core-correctness-implementation.md
git commit -m "docs: describe robustness correctness fixes"
```
