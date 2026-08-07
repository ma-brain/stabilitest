# Isolated Binary-Proportion Calibration Study (Phase 1: `fisher_exact`)

This directory owns the independent categorical-band calibration for eligible
significant canonical two-arm `fisher_exact` results (`binary_proportion`
family, Phase 1).

## Scope

- Calibrate only the two-band Fragile / Not-fragile cutoff for eligible
  significant canonical two-arm Fisher exact results.
- Truth strata are exact-power-defined: null exact, borderline exact power
  0.60 (diagnostic only), clear exact power 0.95 — solved against the
  *enumerated exact* power of `fisher.test` (no normal approximation).
- The 0.95 clear-power choice is justified up front by the committed analytic
  feasibility projection (`tools/feasibility-projection-power095.R`), not
  chosen after a failed 0.90 run.
- `chi_square_2x2` and `two_sample_prop` are deferred to Phase 2/3 and never
  appear in this study.
- Score weights remain frozen at the jackknife-light policy
  (`fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`); the v1 comparator
  `0.4/0.4/0.2` is archived but never fitted.
- Reuse shared calibration schemas, execution, checkpoint, seed, and
  uncertainty helpers without mutating the historical mixed-family scenario
  registry, the Welch calibration, or any lm_ancova material.

## Layout

- `R/` study helpers (loader; power, generator, adapter, replication, Walsh FI,
  thresholds, validation arrive in their tasks)
- `config/scenarios.R` authoritative binary-proportion scenario contract
- `CALIBRATION_SAP.md` frozen Gate A protocol (Task 4)
- `tests/` study-local contract and end-to-end fixtures
- `tools/` feasibility projection scripts + freeze-and-publish tooling
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `outputs/` pilot outputs (gitignored)
- `published/` compact immutable publication artifacts
- `manuscript.md` methods / calibration results / illustrative case study

## Frozen constants

See `docs/plans/2026-08-06-proportions-calibration.md` (frozen constants
table) and `CALIBRATION_SAP.md`. In particular: training grid
n/arm {25, 50, 100, 200} x p0 {0.10, 0.25, 0.50} x truth {null, borderline,
clear}; held-out grid n/arm {35, 75, 150} x p0 {0.15, 0.40}; seeds training
61001+ / validation 62001+ / stress 63001+; masters (power / replication /
cluster bootstrap) 20260808.

## Illustrative dataset

`onc_response_trial` is a prospectively frozen synthetic two-arm oncology
responder trial (60/arm, generator seed `20260809L`) frozen in Task 7 *before*
any p-value, score, or band is inspected. It never enters training or held-out
evidence. The manuscript case study follows, rather than precedes, calibration
results.

## Status

Phase 1 in progress. The active package registry row for `fisher_exact`
remains `uncalibrated` until Gate B (human-approved) activates it.
