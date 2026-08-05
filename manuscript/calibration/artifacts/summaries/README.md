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

## `lm-run-summary.csv`

Full `lm` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine lm --workers 6 --resume` for training, then again with `--validation-only`.
- Wall time: training ~17 min (4 scenarios), held-out validation ~6 min (1 scenario).
- Completed full robustness analyses: 5,000 (all strata filled; 0 scenario failures).

### Notes

- Null false positives (`lm_core_null` / `significant`) sit near the fragile boundary (median **55.49**; only 13/500 > 70).
- Clear-effect significant strata score higher (core ANCOVA significant median **64.28**; held-out multi-df significant median **67.44**, 180/500 > 70).
- Held-out non-significant clear effects are mostly fragile (median **51.97**, 0/500 > 70).

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine lm --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine lm --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R lm
```

## `tost-run-summary.csv`

Full `tost` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine tost --workers 6 --resume` for training, then again with `--validation-only`.
- Wall time: training ~8 min (4 scenarios), held-out validation ~1 min (1 scenario).
- Completed full robustness analyses: 4,009.

### Notes / expected occupancy limits

- `tost_core_outside_margin` (truth outside the equivalence margin) filled only 9/500 in the `equivalent` stratum — false equivalence is rare, so screening hit its budget (`incomplete` by design). The filled `not_equivalent` stratum scores high (median **83.98**, 465/500 > 70).
- Held-out `tost_prop_equivalence` filled only the `not_equivalent` stratum (500/500); the `equivalent` stratum stayed empty within the 10,000-draw budget (`incomplete`). Those not-equivalent replicates all scored 100 under the shared bands — treat as an occupancy/diagnostic signal, not a calibrated equivalence map.
- Near-margin true equivalence (`tost_core_borderline_near_margin` / `equivalent`) concentrates below the fragile cutoff (median **50.71**, 0/500 > 70).

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine tost --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine tost --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R tost
```

## `poisson-run-summary.csv`

Full `poisson` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine poisson --workers 6 --resume` for training, then again with `--validation-only`.
- Wall time: training ~40 min (3 scenarios), held-out validation ~38 min (2 scenarios: `poisson_smoke`, `poisson_validation_multidf`).
- Completed full robustness analyses: 4,500.

### Notes / expected occupancy limits

- Null false positives (`poisson_core_null` / `significant`) concentrate in the fragile band (median **54.16**; only 2/500 > 70).
- Clear-effect offset models (`poisson_core_offset` / `significant`) sit mostly moderate (median **61.16**).
- `poisson_validation_multidf` filled only the `significant` stratum (500/500); non-significant draws were too rare within the 10,000-draw budget (`incomplete`). Those significant replicates all scored > 70 (median **87.91**).

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine poisson --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine poisson --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R poisson
```

## `two_sample-run-summary.csv`

Full `two_sample` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine two_sample --workers 6 --resume` for training, then again with `--validation-only`.
- Wall time: training ~62 min (11 scenarios), held-out validation ~52 min (6 scenarios).
- Completed full robustness analyses: ~13.6k after recovering the imbalanced-binary stress checkpoint.

### Notes / expected occupancy limits

- Several clear-effect and null scenarios are `incomplete` because the opposite screening conclusion is rare within the 10,000-draw budget (e.g. `two_sample_core_clear_n80`, `two_sample_stress_clear_n80`, `two_sample_validation_clear_n100`).
- `two_sample_stress_imbalanced_binary` initially failed post-analysis schema validation: Fisher/prop `original_p` values of 1 landed at `1 + eps` in floating point. Schema/executor now clamp completed p-values into `[0, 1]`; the checkpoint (503 replicates) was recovered into the summary (`incomplete` occupancy: 500 non-significant / 3 significant).
- Null false positives remain near-moderate rather than fragile in several strata (e.g. smoke significant median **57.86**; core null significant median **64.89**).
- Clear significant strata often score high when filled (core clear significant median **77.27**; stress clear significant median **82.38**; validation clear all 500 score 100).

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine two_sample --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine two_sample --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R two_sample
```

## `cox-run-summary.csv`

Full `cox` family at production `n_boot = 1000`, run with `--workers 6`.

- Command: `run_calibration.R --mode full --phase all --engine cox --workers 6 --resume` for training, then again with `--validation-only`.
- Wall time: training ~36 min after executor fix (4 scenarios), held-out validation ~9 min (`cox_smoke`).
- Completed full robustness analyses: 4,552.

### Notes

- First attempt failed every replicate: `run_cox_adapter()` nests the `robustness_surv` payload under `$analysis`, and the executor only read top-level `metrics`. Executor now resolves nested `analysis$metrics` / labels / `n`.
- `cox_core_clear` is `incomplete` on the non-significant stratum (52/500) — false negatives are rare for a clear HR.
- Null false positives (`cox_core_null` / significant) sit in the fragile band (median **54.02**; only 1/500 > 70).
- Clear significant median **69.74** (227/500 > 70); Weibull stress significant median **65.53**.

### Reproduce

```sh
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine cox --workers 6 --resume \
  --output manuscript/calibration/artifacts/raw/training
Rscript manuscript/calibration/run_calibration.R --mode full --phase all \
  --engine cox --workers 6 --resume --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
Rscript manuscript/calibration/tools/summarise_run.R cox
```
