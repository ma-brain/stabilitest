# Isolated LM/ANCOVA v3 Phase 1 study (Track E violation detection)

This directory owns the pre-registered violation-detection study for eligible
significant canonical 1-df ANCOVA (`lm_ancova_v3`). Phase 1 executes Tracks F
(manuscript) and E only. Track D (replication-probability curve) is **parked**
and must not be scaffolded here; seed ranges `51001+` / `52001+` / `53001+`
remain reserved.

## Scope

- New calibration unit `lm_ancova_v3`; v1 `lm_ancova` and v2 `lm_ancova_v2`
  publications stay immutable (uncalibrated; held-out not opened).
- Score weights: `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`
  (v1 `0.4/0.4/0.2` archived as comparator only).
- Track E cells: 3 clean clear (`n ∈ {40,80,160}`, `R² = 0.40`, power 0.90) +
  15 matched violations (allocation 2:1, heteroscedasticity, heavy tails,
  missing baseline, treatment-by-baseline interaction) + 6 diagnostic null
  pairs at `n = 80` (no gate).
- Seeds: `54001+` only. Ranges `41001+`–`53999` (v2 and parked Track D) and
  v1 ledgers are absent.
- Workers maximum `4`; logs to `/tmp/stabilitest-lm-ancova-v3-logs/`.
- Reuse v1 generator/power helpers without mutating v1 or v2 study files.

## Layout

- `R/` study helpers (loader; adapter / Track E metrics in later tasks)
- `config/scenarios.R` authoritative Track E scenario contract
- `CALIBRATION_SAP.md` frozen Phase 1 protocol (Task 4)
- `tests/` study-local contract tests
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `published/` compact immutable publication artifacts
- `outputs/` local runner outputs (gitignored)

## Status

Scaffolded for Phase 1 Track E. SAP freeze, adapter, and production run follow
in later tasks. No package/registry/runtime behavior changes in this plan.
