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

Run these commands from the project root.  They are the reproducible entry
points for the calibration runner and use the frozen scenario IDs and schema.

```sh
# Smoke: one quick replicate for every smoke scenario.
Rscript manuscript/calibration/run_calibration.R --mode smoke \
  --scenarios manuscript/calibration/config/scenarios.R \
  --output manuscript/calibration/artifacts/pilot/smoke.rds

# Pilot: estimate calibration strata and Monte Carlo diagnostics.
Rscript manuscript/calibration/run_calibration.R --mode pilot \
  --scenarios manuscript/calibration/config/scenarios.R \
  --output manuscript/calibration/artifacts/pilot/pilot.rds

# Full: run the preregistered B = 1000 calibration for all scenarios.
Rscript manuscript/calibration/run_calibration.R --mode full \
  --scenarios manuscript/calibration/config/scenarios.R \
  --output manuscript/calibration/artifacts/checkpoints/full.rds

# Resume: continue an interrupted full run from its last checkpoint.
Rscript manuscript/calibration/run_calibration.R --mode full --resume \
  --checkpoint manuscript/calibration/artifacts/checkpoints/full.rds \
  --output manuscript/calibration/artifacts/checkpoints/full.rds

# Analysis: fit the frozen mappings using held-out calibration results.
Rscript manuscript/calibration/analyze_calibration.R \
  --input manuscript/calibration/artifacts/checkpoints/full.rds \
  --output manuscript/calibration/artifacts/analysis

# Artifact generation: render tables, figures, and the audit manifest.
Rscript manuscript/calibration/generate_artifacts.R \
  --input manuscript/calibration/artifacts/analysis \
  --output manuscript/calibration/artifacts/analysis
```

The smoke command is intended for wiring checks only; the pilot command is for
checking stratum occupancy and Monte Carlo precision before committing a full
run.  Full, analysis, and artifact-generation commands must be rerun whenever
the scenario contract or package version changes.

## Files

- `CALIBRATION_SAP.md` is the statistical analysis plan and decision policy.
- `config/scenarios.R` returns the frozen tibble schema.
- `R/load_calibration.R` resolves the repository root, loads this checkout with
  `pkgload`, and sources calibration helpers and scenario configuration.
- `tests/testthat/test-scenario-schema.R` guards the contract against drift.
