# Calibration run summaries (compact, committed)

Compact, human-readable snapshots of per-family calibration runs, produced in
resumable chunks. Unlike the full replicate outputs under
`manuscript/calibration/artifacts/raw/` (git-ignored, large, and not guaranteed
to survive a fresh environment), these summaries are intentionally committed so
partial progress is not lost between chunks.

These are **diagnostic snapshots of partial runs**, not publication artifacts.
They do not replace the locked training → freeze → held-out analysis, which
still requires re-running the full replicate compute for every family.

## `proportion-run-summary.csv`

Full `proportion` family only, at production `n_boot = 1000`, run on a 4-core
machine.

- Command: `run_calibration.R --mode full --phase all --engine proportion --workers 4 --resume` for training, then again with `--validation-only`.
- Wall time: training ~18.6 min (3 scenarios), held-out validation ~5.4 min (1 scenario); ~24 min total.
- Completed full robustness analyses: 3,835 (1 recorded failure in `proportion_smoke`, kept as a diagnostic, never silently dropped).

One row per scenario × screening-conclusion stratum. Columns:

- `split` — `training` (core+stress) or `validation` (held-out).
- `screened` — screening denominator (draws), `selected`/`completed`/`failed` per stratum.
- `score_*` — overall robustness score summary over completed replicates.
- `n_fragile_le55` / `n_moderate_55_70` / `n_robust_gt70` — label counts under the shared 55/70 bands.
- `scenario_failed_total` — total failed replicates for the scenario (a failed replicate has no screening conclusion, so it is not attributed to a stratum).
- `scenario_status` — `complete` or `incomplete` (a stratum that could not be filled within the 10,000-draw screening budget).

### Notes / expected occupancy limits

- `proportion_stress_fisher_null` (a true-null scenario) filled only 336/500 in the `significant` stratum: significant draws are rare under the null, so screening hit its budget. Recorded as `incomplete`, by design.
- The `significant` stratum of the null scenario shows scores skewing high (median ~64, 125 replicates > 70) — this is the false-reassurance signal the calibration study exists to quantify.

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine proportion --workers 4 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine proportion --workers 4 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
```

## `binomial-run-summary.csv`

Full `binomial` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine binomial --workers 6 --resume` for training, then again with `--validation-only`.
- Completed full robustness analyses: 4,000 (500 per conclusion × 4 usable scenarios × 2 splits for the validation scenario).
- `binomial_stress_separation` is recorded as `incomplete` / unsupported: `separation = TRUE` forces perfect separation, so all 10,000 screening draws fail with GLM non-convergence. That is an expected stress diagnostic, not a runner failure. The runner now records empty selections as unsupported instead of aborting the family.

### Notes

- Null false positives (`binomial_core_null` / `significant`) concentrate below the fragile band (median score 54.94; only 6/500 > 70).
- Clear-effect significant strata remain mostly moderate under the shared 55/70 bands (core significant median 59.43; validation significant median 59.71).

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine binomial --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine binomial --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R binomial
```
