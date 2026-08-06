# Isolated LM/ANCOVA Calibration Study

This directory owns the independent categorical-band calibration for eligible
significant canonical 1-df ANCOVA treatment effects (`lm_ancova`).

## Scope

- Calibrate only the additive two-arm ANCOVA treatment coefficient.
- Keep multi-df and noncanonical LM results numeric-only.
- Reuse shared calibration schemas, execution, checkpoint, and uncertainty
  helpers without mutating the historical mixed-family scenario registry.

## Layout

- `R/` study helpers (loader, power, generator, adapter, thresholds, validation)
- `config/scenarios.R` authoritative ANCOVA scenario contract
- `tests/` study-local contract and end-to-end fixtures
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `published/` compact immutable publication artifacts

## Status

Gate A infrastructure only. The active package registry row for `lm_ancova`
remains `uncalibrated` until a frozen candidate passes held-out review.
