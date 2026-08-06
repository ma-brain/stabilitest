# Isolated LM/ANCOVA v2 Calibration Study

This directory owns the Track A two-band (Fragile / Not fragile) calibration
for eligible significant canonical 1-df ANCOVA on a jackknife-light score
(`lm_ancova_v2`).

## Scope

- New calibration unit `lm_ancova_v2`; v1 `lm_ancova` provenance stays immutable.
- Primary fitting strata: null and clear only.
- Borderline (~60% power) is diagnostic-only — not a fitting or acceptance target.
- Clear default target power is `0.90`; SAP may freeze `0.95` after the score
  pilot if separation is weak.
- Score weights and cutoffs are frozen in later tasks; this tree is the study
  scaffold and scenario contract only.
- Reuse v1 generator/power helpers without mutating v1 study files.
- Scenario IDs, seeds, and manifests are isolated from v1.

## Layout

- `R/` study helpers (loader first; adapter/thresholds arrive in later tasks)
- `config/scenarios.R` authoritative v2 scenario contract
- `CALIBRATION_SAP.md` frozen Gate A protocol (Task 2)
- `tests/` study-local contract tests
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `published/` compact immutable publication artifacts
- `outputs/` local runner outputs (gitignored)

## Status

Scaffold only. Adapter, thresholds, SAP freeze, and production compute are
out of scope for this tree until subsequent plan tasks.
