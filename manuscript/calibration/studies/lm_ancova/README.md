# Isolated LM/ANCOVA Calibration Study

This directory owns the independent categorical-band calibration for eligible
significant canonical 1-df ANCOVA treatment effects (`lm_ancova`).

## Scope

- Calibrate only the additive two-arm ANCOVA treatment coefficient.
- Truth strata are power-defined at approximately 60% and 90% nominal power
  (plus null).
- Keep multi-df and noncanonical LM results numeric-only.
- Score weights remain frozen; only the two categorical cutoffs are fit.
- Welch 55/70 is a Welch comparator, not an ANCOVA default or fallback.
- Reuse shared calibration schemas, execution, checkpoint, and uncertainty
  helpers without mutating the historical mixed-family scenario registry.

## Layout

- `R/` study helpers (loader, power, generator, adapter, thresholds, validation)
- `config/scenarios.R` authoritative ANCOVA scenario contract
- `CALIBRATION_SAP.md` frozen Gate A protocol
- `tests/` study-local contract and end-to-end fixtures
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `published/` compact immutable publication artifacts
- `manuscript.md` methods / calibration results / illustrative case study

## Illustrative dataset

`pain_ancova_trial` is a prospectively frozen synthetic illustration
(seed `20260806`) that never enters training or held-out evidence. The
manuscript case study follows, rather than precedes, calibration results.

## Status

Gate B closed fail-closed: the active package registry row for `lm_ancova`
remains `uncalibrated` with reason `no_feasible_thresholds`. Held-out
validation was not opened. Categorical labels stay suppressed; Welch 55/70 is
not an ANCOVA fallback. Compact decision artifacts are under `published/`.
