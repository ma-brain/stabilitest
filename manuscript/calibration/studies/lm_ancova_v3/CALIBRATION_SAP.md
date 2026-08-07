# LM/ANCOVA v3 Phase 1 Statistical Analysis Plan (Tracks F + E; Track D parked)

**Calibration unit:** `lm_ancova_v3`  
**Study path:** `manuscript/calibration/studies/lm_ancova_v3/`  
**Design:** `docs/plans/2026-08-06-lm-ancova-v3-design.md`  
**Plan:** `docs/plans/2026-08-06-lm-ancova-v3-calibration.md`  
**Status:** Phase 1 protocol frozen. Track D is **parked** and must not be
executed under this SAP. v1 (`lm_ancova`) and v2 (`lm_ancova_v2`) publications
remain immutable fail-closed records; this study does not reopen their
held-out ledgers or alter their registry rows.

## Scope

Phase 1 covers:

1. **Track F** (unconditional): negative-result manuscript section explaining
   why categorical truth-class bands are closed (sufficiency, required vs
   delivered AUC, false-GO pilot defect). No compute gate.
2. **Track E** (co-primary): pre-registered test that, among significant
   canonical-looking ANCOVA results, the robustness score discriminates
   assumption-violated data from clean data better than the p-value does.

Bands, cutoffs, and categorical labels are **out of scope**. Welch 55/70 is a
Welch comparator, not an ANCOVA fallback. No package, registry, or runtime
behavior changes are authorized by this SAP; a confirmed Track E result
motivates at most a documented proposal for a diagnostic flag under separate
human approval.

### Track D parking (binding)

Track D (replication-probability curve) is **parked**. Seed ranges
`51001+`, `52001+`, and `53001+` remain **RESERVED** and must not appear in
any v3 ledger under this Phase 1 SAP. Un-parking requires **explicit human
instruction** after the binary-proportion study’s replication-curve outcome
is reviewed. Do not scaffold, seed, or execute Track D scenarios here.

## Score definition (frozen)

Production Track E weights are frozen as `fragility = 0.5`,
`bootstrap = 0.5`, `jackknife = 0`:

| Component | Weight |
| --- | --- |
| fragility | 0.5 |
| bootstrap | 0.5 |
| jackknife | 0 |

The locked v1 composite (`jackknife = 0.4`, `fragility = 0.4`,
`bootstrap = 0.2`) is always archived as a comparator column and is **never**
used for the Track E gate. Weights are not re-optimized against Track E
metrics.

Analysis parameters (frozen): `alpha = 0.05`, `n_boot = 1000`,
`max_removal_pct = 0.30`. Screening is significant-only. Workers maximum
`4`. Logs to `/tmp/stabilitest-lm-ancova-v3-logs/` (never inside the repo).

## Track E scenario contract

### Primary cells (gated)

- Clean clear: `n ∈ {40, 80, 160}` × baseline `R² = 0.40` × truth `clear`,
  with the treatment effect solved for exact nominal power **0.90 under the
  CLEAN model**.
- Violated clear: each clean cell × **5** violations at **identical nominal
  parameters**, violation applied on top via the frozen v1 stress generators:
  - `allocation_2to1` (2:1 allocation)
  - `heteroscedastic`
  - `heavy_tails`
  - `missing_baseline`
  - `interaction` (treatment-by-baseline)
- Quotas: ≥ **100** significant completed replicates per primary cell;
  failures ≤ **5%**.
- Violated rows carry `violation_type` and `matched_clean_id`.

### Diagnostic null pairs (no gate)

- Clean null + 5 violated null cells at `n = 80` only.
- Quota **50** significant each.
- Secondary / descriptive only — **no gate**.

### Seeds and provenance

- Track E scenario seeds begin at **`54001+`** only.
- Master seeds for the power check and the scenario-cluster bootstrap:
  **`20260807`**.
- Ranges `41001+` / `42001+` / `43001+` (v2) and `51001+` / `52001+` /
  `53001+` (parked Track D) must not appear.
- **Assertion:** v1 and v2 **validation** seeds/IDs appear in **no** v3
  ledger. v1/v2 published artifacts and registry rows are never modified.

## Power check (pre-production)

- Clean cells only: primary-test-only Monte Carlo, draws **10000**, master
  seed **20260807**, tolerance **0.02**.
- Violated cells: record empirical significance rates as descriptive output;
  no tolerance applies (deviation from nominal power is the phenomenon under
  study).
- Stop on a clean-cell miss; do not adjust the design.

## Track E primary metric and gate (frozen)

### Metric

Among significant **clear** primary rows, discriminate violated vs clean:

\[
\Delta\mathrm{AUC} = \mathrm{AUC}_{\mathrm{score}} - \mathrm{AUC}_{p}
\]

- **Score orientation (pre-specified):** clean > violated (higher score
  favors clean).
- **p orientation:** use \(-\log_{10} p\), with the direction chosen
  **empirically** as whichever favors p (conservative for the claim that the
  score adds information).
- Report pooled ΔAUC and per-violation ΔAUC (per-violation descriptive
  either way).

### Scenario-cluster bootstrap CI

- Resample whole scenario cells (matched clean + violation clusters as
  defined in the analysis tooling).
- Seed **20260807**, \(B = 1000\).
- Report the 95% CI for pooled ΔAUC.

### Gate (do not soften)

Track E is **confirmed** if and only if:

1. pooled ΔAUC ≥ **0.10**, **and**
2. the bootstrap 95% CI **lower bound > 0**.

A failed gate is a publishable negative outcome: publish exactly, commit,
report. Do **not** re-run with adjusted thresholds.

## Fail-closed / publication rules

- Outcome either way is publishable.
- Commit only code, SAPs, compact summaries, manifests, and hash ledgers.
  Raw / checkpoint / pilot outputs stay gitignored.
- No second candidate, no post-hoc threshold search, no package registry
  mutation in Phase 1.

## Case study boundary

`pain_ancova_trial` stays frozen and never enters Track E evidence.
