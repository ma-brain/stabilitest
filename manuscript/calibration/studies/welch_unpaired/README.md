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

Production training is frozen with `status = "no_feasible_thresholds"` and
candidate hash
`9c45481b952cab7cb9b9086e37924a39d83fe0484628745dbe2e79eb33e8797d`.
All 18 required training cells reached 250 completed significant analyses
(4,500 total) with zero failures. The held-out validation directory was not
created and remains unopened. After human acknowledgement, the deterministic
fail verdict was published without validation: Welch categorical labels are
suppressed while numeric scores and component metrics remain available.

Study 2026-2 recorded `welch-2026-2` as `uncalibrated` with no cutoffs and
provenance pointing to `published/`. The published verdict and registry
proposal both record `held_out_opened = FALSE` and `validation_refit = FALSE`.

Study 2026-3 (relaxed gates) is also closed. Its training fit chose a `Stable`
cutoff of 65, but the p-value-only comparator had no eligible grid point, so
the frozen SAP closed the study with reason `p_comparator_unavailable`
(candidate hash
`b113e704766eb8d0740af9c5f387044b175c0d0b078d971fb867bd928c0b4f98`) and
validation stayed unopened. The active package registry now records
`welch-2026-3` as `uncalibrated`, with provenance pointing to
`published/2026-3/`.

## Study versions

Study 2026-2 (`CALIBRATION_SAP.md`) keeps its original `artifacts/` and
`published/` locations and is closed. Study 2026-3 (relaxed gates,
`CALIBRATION_SAP_2026-3.md`) writes to `artifacts/2026-3/` and
`published/2026-3/`. The runner and each `tools/` script take
`--study-version 2026-2|2026-3` (default `2026-2`):

```sh
Rscript tools/verify_power_gate.R --study-version 2026-3
Rscript run_calibration.R --mode production --layer training --study-version 2026-3
Rscript tools/fit_training_candidate.R --study-version 2026-3
Rscript tools/freeze_candidate.R --study-version 2026-3
# only if the frozen decision is closed at the freeze:
Rscript tools/publish_fail_decision.R --study-version 2026-3
```

Scenario IDs carry the version (`WTR3-`, `WVA3-`, `WST3-`), and a run stops if
IDs and `--study-version` disagree. Slow end-to-end tool tests run with
`WELCH_TOOL_TESTS=1`.
