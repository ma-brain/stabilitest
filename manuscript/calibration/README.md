# Analysis-specific calibration

This directory freezes the scenario contract and statistical analysis plan for
calibrating `stabilitest` scores separately for the seven supported analysis
families: two-sample, proportion, linear model, binomial, Poisson, Cox, and
TOST.  The contract is defined by `config/scenarios.R`; each row has one nested
`parameters` object containing generator and analysis settings.

The scenario table is deliberately independent of generated data.  Calibration
runners must use the current checkout (via `R/load_calibration.R`), preserve the
scenario seed, and write intermediate files below `artifacts/`.  Only the
designated `artifacts/checkpoints/`, `artifacts/raw/`, and `artifacts/pilot/`
paths are ignored by Git; published and compact summary artifacts are
intentionally tracked.

The `run_calibration.R` and `analyse_calibration.R` commands below are the
canonical CLI entry points for screening, analysis, candidate freezing, and
publication.  Both scripts are present and runnable from the project root.

## Canonical commands

Run these commands from the project root.  The runner CLI is frozen as
`--mode smoke|pilot|full --phase screen|analyse|all --engine
all|two_sample|proportion|lm|binomial|poisson|cox|tost --workers <N>
[--resume] [--validation-only] --output <path>`.  The commands below use the frozen scenario
contract loaded by `R/load_calibration.R`.

```sh
# Smoke: one quick replicate for every smoke scenario.
Rscript manuscript/calibration/run_calibration.R --mode smoke --phase all \
  --engine all --workers 1 --output /tmp/stabilitest-calibration-smoke

# Pilot: check occupancy and Monte Carlo diagnostics.
Rscript manuscript/calibration/run_calibration.R --mode pilot --phase all \
  --engine all --workers 1 --output manuscript/calibration/artifacts/pilot

# Full production training run: B = 1000 for all scenario families.
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine all --workers <N> --resume \
  --output manuscript/calibration/artifacts/raw/training

# Held-out validation run.
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine all --workers <N> --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation

# Analyse and freeze: fit candidates from the training artifacts.
Rscript manuscript/calibration/analyse_calibration.R \
  --training manuscript/calibration/artifacts/raw/training \
  --freeze-candidate manuscript/calibration/artifacts/raw/candidate-registry.rds

# Validate and publish the frozen candidate map; --publish writes compact
# tables, figures, and the audit manifest.
Rscript manuscript/calibration/analyse_calibration.R \
  --candidate manuscript/calibration/artifacts/raw/candidate-registry.rds \
  --validation manuscript/calibration/artifacts/raw/validation \
  --publish manuscript/calibration/published
```

The smoke command is intended for wiring checks only; the pilot command is for
checking stratum occupancy and Monte Carlo precision before committing a full
run.  Only a production `full` run may feed candidate fitting, validation, and
publication.  Full, analysis, and publish commands must be rerun whenever the
scenario contract or package version changes.

Train versus held-out membership is assigned by `design_layer` in the scenario
registry: `validation` rows are held-out; `core`/`stress` rows are training
(pilot further restricts to `core`).  Use `--validation-only` for the held-out
run.  The schema field `training_split` is retained for contract stability but
is unused by the current runner.

## Running one family on your own machine (resumable chunks)

The full multi-family run is expensive, so it is convenient to calibrate one
analysis family at a time and persist a compact summary after each chunk.  This
works on any machine with R >= 4.2 (for example a laptop) and is fully
resumable.

Prerequisites (the runner loads this checkout in place with `pkgload`, so
`stabilitest` itself does not need installing — only its dependencies):

```sh
Rscript -e 'install.packages(c("dplyr","purrr","tibble","ggplot2","pkgload"), repos="https://cloud.r-project.org")'
```

Run a single family (here `tost`) from a clean checkout — `full` mode refuses a
dirty tree unless `--allow-dirty` is passed.  Set `--workers` to your available
performance cores; outputs go under the git-ignored `artifacts/raw/` paths so
the checkout stays clean:

```sh
# Training (core + stress)
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine tost --workers 10 --resume \
  --output manuscript/calibration/artifacts/raw/training

# Held-out validation
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine tost --workers 10 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
```

`--resume` lets you stop and restart safely; it reuses the atomic per-scenario
checkpoints.  Some conclusion strata may finish `incomplete` when a conclusion
is rare and screening exhausts its 10,000-draw budget (for example the opposite
conclusion of a null scenario) — this is expected and recorded as a diagnostic,
not an error.

Persist a compact, committed summary of the chunk (one row per scenario x
screening-conclusion stratum, with occupancy, score distribution, and 55/70
band counts):

```sh
Rscript manuscript/calibration/tools/summarise_run.R tost
git add manuscript/calibration/artifacts/summaries/tost-run-summary.csv
git commit -m "data: persist compact tost calibration run summary"
```

These per-family summaries are diagnostic snapshots that preserve progress
between chunks; they do not replace the locked training -> freeze -> held-out
analysis, which still requires the full replicate compute across every family.

## Files

- `CALIBRATION_SAP.md` is the statistical analysis plan and decision policy.
- `config/scenarios.R` returns the frozen tibble schema.
- `R/load_calibration.R` resolves the repository root, loads this checkout with
  `pkgload`, and sources calibration helpers and scenario configuration.
- `tools/summarise_run.R` writes a compact, committed per-family run summary
  from the `run-results.rds` of a training and (optional) validation run.
- `tests/testthat/test-scenario-schema.R` guards the contract against drift.
