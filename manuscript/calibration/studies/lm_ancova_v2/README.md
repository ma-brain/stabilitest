# Isolated LM/ANCOVA v2 Calibration Study

This directory owns the Track A two-band (Fragile / Not fragile) calibration
for eligible significant canonical 1-df ANCOVA on a jackknife-light score
(`lm_ancova_v2`).

## Scope

- New calibration unit `lm_ancova_v2`; v1 `lm_ancova` provenance stays immutable
  (uncalibrated / `no_feasible_thresholds`; held-out not opened).
- Frozen Gate A SAP: `CALIBRATION_SAP.md` (Track A).
- Score weights: `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`.
- Bands: Fragile if score ≤ L; else Not fragile.
- Primary fitting strata: null and clear only.
- Borderline (~60% power) is diagnostic-only — not a fitting or acceptance target.
- Clear target power is **frozen at `0.90`** after a recorded score-pilot go
  (`artifacts/summaries/SCORE_PILOT_GATE.json`); escalate to `0.95` only if a
  future re-pilot records marginal separation under the SAP formulas.
- Workers default `4`, `n_boot = 1000`, `max_screen_draws = 10000`, quotas
  ≥100 significant per required scenario.
- v1 validation seeds/IDs are absent from v2 ledgers; Welch 55/70 is not an
  ANCOVA fallback.
- Reuse v1 generator/power helpers without mutating v1 study files.
- Scenario IDs, seeds, and manifests are isolated from v1.

## Layout

- `R/` study helpers (loader first; adapter/thresholds arrive in later tasks)
- `config/scenarios.R` authoritative v2 scenario contract
- `CALIBRATION_SAP.md` frozen Gate A / Track A protocol
- `tests/` study-local contract tests
- `artifacts/` large raw/pilot/checkpoint outputs (gitignored)
- `published/` compact immutable publication artifacts
- `outputs/` local runner outputs (gitignored)

## Status

Gate A SAP frozen (Track A). Score-only pilot **passed** (clear power frozen at
`0.90`). Production power gate **passed** (45/45); execution freeze recorded.
Production training occupancy assembled (33/33 checkpoints; validation
closed). Cutoff search and held-out acceptance remain subsequent plan tasks. Package labels stay suppressed until a
later Gate B integration.
