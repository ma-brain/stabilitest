# CI Installation-Safe Checks Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make the R package checks pass from a built/installed package while preserving a source-tree audit for the method-specific calibration policy.

**Architecture:** Package tests will exercise only installed-package assets and will resolve the calibration registry through `system.file()` when the source tree is unavailable. Source-only documentation scans will move to a standalone base-R script invoked by GitHub Actions from the checkout. Package metadata will exclude repository-only files, and the workflow will use the current checkout action.

**Tech Stack:** R, testthat, rcmdcheck, GitHub Actions.

---

### Task 1: Make the registry regression test installation-safe

**Files:**
- Modify: `tests/testthat/test-calibration-documentation.R`

**Steps:**
1. Resolve `inst/extdata/calibration-registry.csv` when running from a source checkout, otherwise resolve `system.file("extdata", ...)`.
2. Assert the resolved path exists before reading it.
3. Remove source-tree-only documentation scans from the package test file; those checks will run through the CI audit script.
4. Run the focused test in the source checkout and confirm it passes.

### Task 2: Add the source-tree calibration policy audit

**Files:**
- Create: `tools/check-calibration-documentation.R`
- Modify: `.github/workflows/R-CMD-check.yaml`

**Steps:**
1. Move the active-policy and stale-claim checks into a base-R script rooted at the script location.
2. Exit nonzero with file/line diagnostics when a policy assertion fails.
3. Add a workflow step after R setup and before dependency installation.
4. Run the script locally and confirm it passes.

### Task 3: Keep repository-only files out of package checks

**Files:**
- Modify: `.Rbuildignore`

**Steps:**
1. Ignore `AGENTS.md` to remove the non-standard top-level NOTE.
2. Ignore the CI-only `tools/` directory from the built package.

### Task 4: Verify from a clean archive and publish

**Steps:**
1. Run the full development test suite.
2. Build a clean source tree with `git archive HEAD` and run a network-enabled `rcmdcheck` from it.
3. Confirm the source audit and package check pass, with no errors or warnings.
4. Commit the changes, push `main`, and confirm the resulting Actions run succeeds.
