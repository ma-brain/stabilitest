# Analysis-specific calibration

This directory freezes the scenario contract and statistical analysis plan for
calibrating `stabilitest` scores separately for the seven supported analysis
families: two-sample, proportion, linear model, binomial, Poisson, Cox, and
TOST.  The contract is defined by `config/scenarios.R`; each row has one nested
`parameters` object containing generator and analysis settings.

The scenario table is deliberately independent of generated data.  Calibration
runners must use the current checkout (via `R/load_calibration.R`), preserve the
scenario seed, and write large intermediate files below `artifacts/` (which is
ignored by Git).

## Canonical commands

Run these commands from the project root.  The runner CLI is frozen as
`--mode smoke|pilot|full --phase screen|analyse|all --engine
all|two_sample|proportion|lm|binomial|poisson|cox|tost --workers <N>
[--resume] --output <path>`.  The commands below use the frozen scenario
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

## Files

- `CALIBRATION_SAP.md` is the statistical analysis plan and decision policy.
- `config/scenarios.R` returns the frozen tibble schema.
- `R/load_calibration.R` resolves the repository root, loads this checkout with
  `pkgload`, and sources calibration helpers and scenario configuration.
- `tests/testthat/test-scenario-schema.R` guards the contract against drift.
