# Prospective `welch_unpaired` Calibration Study

This directory owns the prospective, method-specific calibration attempt for
categorical interpretation of significant independent-groups Welch results.
Its frozen calibration unit uses `alpha = 0.05`, package-default weights
`(0.4, 0.4, 0.2)`, `n_boot = 1000`, and a 30% removal cap.

The historical Welch 55/70 simulation and the historical Task 15 calibration
archive are design inputs and named comparators only. Their rows have already
been inspected and cannot be reused as training evidence or fresh held-out
confirmation. The R Journal 12-scenario simulation is likewise descriptive,
not confirmatory evidence for this study.

## Layout

- `CALIBRATION_SAP.md`: frozen truth definitions, gates, candidate hierarchy,
  and single-opening rule.
- `config/scenarios.R`: explicit training, validation, and diagnostic-stress
  scenario registry with disjoint IDs and seed streams.
- `R/`: deterministic power, generation, adapter, runner, threshold, and
  validation helpers added in later implementation tasks.
- `tools/`: power verification, candidate fitting, and freeze/publication
  commands.
- `artifacts/design/`: resolved scenario and power-verification artifacts.
- `artifacts/training/` and `artifacts/summaries/`: production training data
  and the immutable candidate freeze.
- `artifacts/validation/`: absent until explicit human permission is given.
- `published/`: compact final verdict artifacts.
- `tests/`: study-local contract and behavior tests.

## Decision policy

Training first searches frozen three-band candidates, then frozen two-band
candidates, and otherwise records `no_feasible_thresholds`. A frozen candidate
and hash must be committed before validation can be opened. A no-candidate
training decision keeps validation closed and leads to numeric-only Welch
output after human acknowledgement. A held-out failure also leads to
numeric-only output; gates are never weakened after results are seen.

## Current status

Protocol frozen before any robustness score production. The active package
registry remains `welch-2026-1` until this prospective study publishes its
fail-closed decision.
