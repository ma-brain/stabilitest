# Published ANCOVA calibration artifacts

This directory holds compact, immutable publication outputs for the isolated
`lm_ancova` v1 calibration study. Gate B closed **fail-closed** as
`uncalibrated` / `no_feasible_thresholds` (candidate hash
`9ccfc2fca7c0a07c19a3a18838e9a3f2`). Held-out validation was **not** opened.
No categorical cutoffs were invented; Welch 55/70 is not an ANCOVA fallback.

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
