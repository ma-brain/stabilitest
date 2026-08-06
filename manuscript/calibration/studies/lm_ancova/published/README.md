# Published ANCOVA calibration artifacts

This directory holds compact, immutable publication outputs for the isolated
`lm_ancova` calibration study:

- completed training/validation replicate tables
- full audit rows and failure/occupancy summaries
- frozen candidate and held-out validation diagnostics
- method-specific registry CSV/RDS
- training/validation manifests
- output hash ledger

Large raw checkpoints and pilot outputs remain under `artifacts/` and are
gitignored. Do not overwrite committed publication files unless an explicit
development-only test flag is supplied.
