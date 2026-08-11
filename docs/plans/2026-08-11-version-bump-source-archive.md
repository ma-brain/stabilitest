# Version-bump source archive implementation plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build and upload the R source package archive and PDF reference manual only when the `Version:` field in `DESCRIPTION` changes on `main`, and document the release and scientific safeguards in `AGENTS.md`.

**Architecture:** A path-filtered GitHub Actions workflow compares the pre-push and current `DESCRIPTION` versions before any R setup or build work. A standalone repository checker protects the trigger, version gate, read-only permissions, archive filename, and absence of release/submission side effects; the normal R CMD check workflow runs that checker.

**Tech Stack:** GitHub Actions YAML, Bash, R, TinyTeX, `yaml`, `R CMD build`, `R CMD Rd2pdf`, r-lib/actions, actions/upload-artifact.

---

### Task 1: Add a failing workflow contract checker

**Files:**
- Create: `tools/check-build-archive-workflow.R`
- Modify: `.github/workflows/R-CMD-check.yaml`

**Step 1: Create the checker**

Write an R script that reads `.github/workflows/build-archive.yaml` and fails
unless all of these contracts are present:

- pushes are limited to `main` and changes including `DESCRIPTION`;
- tag and manual triggers are absent;
- repository permissions remain `read-all`;
- current and pre-push `DESCRIPTION` versions are read and compared;
- R setup, TinyTeX setup, dependency setup, build, and upload are conditional
  on a true version-change output;
- `R CMD build .` runs and the exact
  `stabilitest_<new-version>.tar.gz` file is required;
- `R CMD Rd2pdf .` runs and the exact non-empty
  `stabilitest_<new-version>-manual.pdf` file is required;
- `actions/upload-artifact@v4` uploads both package files; and
- no release, tag, CRAN, R Journal, or other submission command/action exists.

The checker prints every violation and exits nonzero if any are present.

**Step 2: Run the checker against the current workflow**

Run: `Rscript tools/check-build-archive-workflow.R`

Expected: FAIL because the current workflow is tag-triggered and uses
`softprops/action-gh-release`.

**Step 3: Add the checker to normal CI**

Add this step after the calibration documentation audit in
`.github/workflows/R-CMD-check.yaml`:

```yaml
      - name: Audit version-bump archive workflow
        run: Rscript tools/check-build-archive-workflow.R
```

**Step 4: Do not commit yet**

Keep the failing checker and CI hook together with the implementation so the
workflow is never committed in a deliberately failing state.

### Task 2: Implement the version-bump-only archive workflow

**Files:**
- Modify and track: `.github/workflows/build-archive.yaml`

**Step 1: Replace the tag trigger**

Use:

```yaml
on:
  push:
    branches: [main]
    paths:
      - DESCRIPTION
```

Do not add `workflow_dispatch`, tag, schedule, pull-request, or release
triggers.

**Step 2: Detect the version change before R setup**

Check out with `fetch-depth: 0`. Add a Bash step with `id: version` that:

- extracts exactly one non-empty `Version:` from the current `DESCRIPTION`;
- reads the previous file with
  `git show "${{ github.event.before }}:DESCRIPTION"`;
- exits with an error if either value is missing;
- writes `changed=false` and exits successfully when the values are equal; and
- otherwise writes `changed=true`, `current=<new version>`, and
  `previous=<old version>` to `$GITHUB_OUTPUT`.

**Step 3: Make all expensive and output-producing steps conditional**

Apply `if: steps.version.outputs.changed == 'true'` to setup-r,
setup-tinytex, setup-r-dependencies, build, and upload.

**Step 4: Build and validate the exact package files**

Run `R CMD build .`, compute
`stabilitest_${{ steps.version.outputs.current }}.tar.gz`, and require it to
exist. Run `R CMD Rd2pdf . --force` with output
`stabilitest_${{ steps.version.outputs.current }}-manual.pdf`, require it to be
non-empty, and expose both paths from an `id: package_files` step.

**Step 5: Upload only a workflow artifact**

Use `actions/upload-artifact@v4` with a versioned artifact name and both exact
validated paths. Retain `permissions: read-all`. Remove
`softprops/action-gh-release` and release-note generation.

**Step 6: Run the workflow checker**

Run: `Rscript tools/check-build-archive-workflow.R`

Expected: `Version-bump archive workflow audit passed.`

### Task 3: Synchronize `AGENTS.md`

**Files:**
- Modify: `AGENTS.md`

**Step 1: Add scientific invariants**

Document that Fisher exact is the only active categorical mapping; Welch and
ANCOVA remain numeric-only; the approved term is “bootstrap same-decision
rate”; and held-out calibration data may not be opened without explicit human
permission.

**Step 2: Add R Journal layout guidance**

Document `manuscript/rjournal/artifacts/` as frozen evidence,
`manuscript/rjournal/tools/` as generation/audit scripts, and generated figure
directories under `stabilitest_files/`. Explain that local empty `data/`,
`figures/`, or `scripts/` directories are not canonical inputs.

**Step 3: Add the version-bump archive contract**

State that changing `DESCRIPTION`'s `Version:` on `main` is the sole source
archive and PDF-manual build event; unchanged-version metadata edits must not
build; both outputs are in one GitHub Actions artifact; and the workflow must
not create tags, Releases, CRAN submissions, or R Journal submissions. CRAN
publication is not a prerequisite for building these package files.

**Step 4: Add verification commands**

Include the archive workflow checker, documentation audit, package tests, CRAN
style check, and local `R CMD build .` command.

### Task 4: Verify, commit, and push

**Files:**
- Verify all modified files.

**Step 1: Validate YAML and workflow contracts**

Run:

```sh
Rscript -e 'yaml::read_yaml(".github/workflows/build-archive.yaml"); yaml::read_yaml(".github/workflows/R-CMD-check.yaml")'
Rscript tools/check-build-archive-workflow.R
```

Expected: YAML parses and the workflow audit passes.

**Step 2: Run documentation and package verification**

Run:

```sh
Rscript tools/check-calibration-documentation.R
Rscript -e 'devtools::test()'
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
```

Expected: documentation audit passes; 1,018 tests pass; R CMD check reports 0
errors, 0 warnings, and only the accepted `New submission` note.

**Step 3: Rehearse the archive build locally**

Run `R CMD build .` and `R CMD Rd2pdf .` in a temporary output workflow or move
their outputs outside the repository immediately after the check. Require
`stabilitest_0.6.0.tar.gz` and `stabilitest_0.6.0-manual.pdf`, inspect the
archive with `tar -tzf`, verify its `DESCRIPTION` reports version 0.6.0, inspect
the manual with `pdfinfo`, and visually review every manual page.

**Step 4: Review and stage only intended files**

Run:

```sh
git diff --check
git diff -- .github/workflows/build-archive.yaml .github/workflows/R-CMD-check.yaml tools/check-build-archive-workflow.R AGENTS.md
git status --short --untracked-files=all
```

Stage only those four implementation files plus this committed plan if needed.

**Step 5: Commit**

```sh
git commit -m "ci: build source archive on version bumps"
```

**Step 6: Push directly to main**

Push `main` to `origin/main` without creating a pull request, then verify the
remote ref matches local `HEAD`.
