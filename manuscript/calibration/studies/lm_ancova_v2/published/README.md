# Published ANCOVA v2 calibration artifacts

This directory holds compact, immutable publication outputs for the isolated
`lm_ancova_v2` Track A calibration study. Gate B closed **fail-closed** as
`uncalibrated` / `no_feasible_thresholds` (candidate hash
`3dc2a1f840b3eb725bea629dc130f070`). Held-out validation was **not** opened.
No categorical cutoffs were invented; jackknife-light two-band labels stay
suppressed. Welch 55/70 is not an ANCOVA fallback. The v1 `lm_ancova`
provenance row remains the immutable historical uncalibrated record.

Tracked compact artifacts:

- `candidate.rds` / `candidate-diagnostics.json`
- `training-occupancy.csv` / `training-failures.csv`
- `power-verification.csv`
- `training-manifest.rds` / `training-manifest.json`
- method-specific `registry.csv` / `registry.rds`
- `hash_ledger.rds` / `output-hashes.txt`

Large raw checkpoints and pilot outputs remain under `artifacts/` and are
gitignored. Do not overwrite committed publication files unless an explicit
development-only test flag is supplied.
