# AGENTS.md

## Cursor Cloud specific instructions

`stabilitest` is a single **R package** (R `>= 4.2`), not a web/service app. There
are **no runtime services** (no server, database, ports, or daemons). "Running
the application" means loading the library into an R session and calling its
functions; "end-to-end" verification is the `testthat` suite and `R CMD check`.

## Project invariants

- Categorical interpretation is fail-closed. The only active categorical
  mapping is the eligible, statistically significant Fisher exact-test profile
  with its validated weights. Welch and ANCOVA outputs are numeric-only; do not
  restore the historical Welch 55/70 bands or infer a label from a score.
- Use **bootstrap same-decision rate** for the resampling component. It is an
  empirical plug-in quantity, not the probability that a future trial will
  replicate the finding. Keep the public field name `bootstrap_reproducibility`
  only for API compatibility.
- Never open held-out calibration data without explicit human permission after
  the training candidate or fail-closed decision has been frozen and committed.
  A no-feasible-candidate result ends the study without opening validation.
- Do not submit to CRAN, the R Journal, or any external service unless the human
  explicitly requests that submission. Building local or CI package files is
  not an external submission.

## R Journal workspace

The canonical article directory is `manuscript/rjournal` (no hyphen).

- `manuscript/rjournal/artifacts/` contains committed frozen evidence read by
  the article.
- `manuscript/rjournal/tools/` contains evidence generators and article audits.
- Rendered figures live under `manuscript/rjournal/stabilitest_files/`, notably
  `figure-html5/` and, during PDF builds, `figure-latex/`.
- Local empty `data/`, `figures/`, or `scripts/` directories are legacy
  scaffolding, not canonical inputs. Do not move evidence out of `artifacts/`
  or generators out of `tools/` to populate them.
- Follow `manuscript/rjournal/REPRODUCE.md` and run both article audit scripts
  before assembling a submission bundle. Keep submission ZIPs outside the
  repository and never include held-out validation output.

## Version-bump package files

Changing the `Version:` field in `DESCRIPTION` on `main` is the sole event that
builds versioned package files. `.github/workflows/build-archive.yaml` may start
when `DESCRIPTION` changes, but its gate must skip R setup, building, and upload
when the old and new `Version:` values are equal.

For a genuine version bump the workflow builds exactly:

- `stabilitest_<version>.tar.gz` with `R CMD build .`; and
- `stabilitest_<version>-manual.pdf` with `R CMD Rd2pdf`.

Both files are uploaded together as a downloadable GitHub Actions artifact.
The workflow must remain read-only: it does not create a Git tag or GitHub
Release and does not submit to CRAN or the R Journal. CRAN publication is not a
prerequisite for this build. Do not add tag, manual, pull-request, schedule, or
other build triggers without explicit human approval.

### Environment (already provisioned by the startup update script)
- R runtime + all package dependencies are installed as **binary `.deb`s** via
  the [r2u](https://eddelbuettel.github.io/r2u/) apt repo (packages named
  `r-cran-*`) plus the CRAN apt repo for `r-base-core`. These live system-wide in
  `/usr/lib/R/site-library`.
- Do **not** use `pak`/`remotes`/`install.packages()` to (re)install the declared
  dependencies: on this box they re-resolve against CRAN/PPM and **compile from
  source** (slow and brittle, e.g. `Matrix`/`survival` take minutes). Prefer the
  `r-cran-*` apt binaries. If you must install an R package interactively, note
  that `/usr/local/lib/R/site-library` is only writable via `sudo`
  (e.g. `sudo Rscript -e '...'`).

### Common commands (run from repo root `/workspace`)
- Load for development: `Rscript -e 'devtools::load_all(".")'`
- Run tests: `Rscript -e 'devtools::test()'` (takes ~40s)
- Audit calibration documentation: `Rscript tools/check-calibration-documentation.R`
- Audit the version-bump workflow: `Rscript tools/check-build-archive-workflow.R`
- Lint/build/check (mirrors the `R-CMD-check` GitHub Action; there is no separate
  linter): `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'`.
  A clean run reports only the benign `New submission` NOTE.
- Rehearse package files locally: `R CMD build .` and
  `R CMD Rd2pdf --no-preview --force --output=stabilitest-manual.pdf .`. Move
  the generated files outside the source tree after inspection.
- Vignette rebuild works because `pandoc` is installed and on `PATH`.

### Quick smoke test of core functionality
```r
Rscript -e 'devtools::load_all("."); print(robustness_analysis(pain_treatment, pain_placebo, test_type="t.test", n_boot=2000, interpret=TRUE))'
```
Bundled datasets `pain_treatment` / `pain_placebo` are available after
`load_all()`. See `README.md` for the full API (e.g. `robustness_analysis`,
`robustness_lm`, `robustness_glm`, `robustness_surv`, `robustness_tost`).

### Regenerating docs
`NAMESPACE` and `man/` are generated; regenerate with
`Rscript -e 'roxygen2::roxygenise()'` after changing roxygen comments.
