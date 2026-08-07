# LM/ANCOVA Calibration Design Addendum (v2)

**Parent design:** `docs/plans/2026-08-06-lm-ancova-calibration-design.md`  
**Parent plan:** `docs/plans/2026-08-06-lm-ancova-calibration.md`  
**Date:** 2026-08-06  
**Status:** approved — Track A locked  
**Calibration unit (proposed):** `lm_ancova_v2`  
**v1 outcome:** training fail-closed (`no_feasible_thresholds`); active
`lm_ancova` remains uncalibrated / numeric-only until Gate B integration of
that decision.

## Context

Gate A v1 froze the Welch-era composite weights
(`jackknife = 0.4`, `fragility = 0.4`, `bootstrap = 0.2`) and sought three
bands (Fragile / Moderately Robust / Robust) on power-defined null / ~60% /
~90% strata for eligible significant canonical 1-df ANCOVA.

Training (2700 core completed replicates; candidate hash
`9ccfc2fca7c0a07c19a3a18838e9a3f2`) found **zero** feasible `(L, U)` pairs
among 5050 candidates. Best balanced accuracy was ≈ 0.56 against a gate of
≥ 0.70. Empirical diagnostics showed:

- medians ordered (~55 / ~62 / ~70) but heavy three-class overlap;
- jackknife saturated near 100 in all truth classes, diluting the composite;
- fragility and bootstrap carried essentially all separation;
- composite levels drifted down with sample size;
- Welch 55/70 was a poor comparator and must not become an ANCOVA fallback.

v1 held-out validation was correctly **not** opened. v1 provenance stays
immutable. A categorical ANCOVA claim now requires a **new** study surface.

## Decision (locked)

**Track A — Two-band claim on a jackknife-light ANCOVA score.**

| Element | v2 choice |
| --- | --- |
| Claim | Fragile vs Not fragile for eligible significant canonical ANCOVA only |
| Score | Jackknife-light composite, frozen before training |
| Default weights | `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0` |
| Alternate (pilot only) | `0.45 / 0.45 / 0.10` if jackknife retention is needed for continuity |
| Truth targets | **null** vs **clear** (≥90% power; prefer 95% if pilot separation is weak) |
| Borderline (~60%) | Diagnostic stratum only — no label target, no acceptance gate |
| Cutoffs | Single threshold \(L\): score ≤ \(L\) ⇒ Fragile; else Not fragile |
| Comparator | Locked v1 composite (and Welch 55/70) reported, never fitted |
| Fail closed | If training or held-out gates fail, remain uncalibrated |
| Registry | New unit `lm_ancova_v2`; do not overwrite v1 provenance |

Rejected for v2 start: more bands; three-band-first; softening v1 accuracy
gates to force labels; quiet reuse of v1 validation rows; adopting Welch
cutoffs.

## Goals

- Offer a weaker but reviewable categorical ANCOVA claim, or confirm again
  that none is justified.
- Keep the same canonical model and runtime eligibility gate as v1.
- Separate score redesign from cutoff fitting via a sealed pilot go/no-go.
- Preserve fail-closed publication and non-calibrating
  `pain_ancova_trial` illustration policy.

## Non-Goals

- Calibrating multi-df or noncanonical LM terms.
- More than two calibrated bands on the v2 score.
- Reweighting the v1 composite in place without a new unit/SAP.
- Changing `pain_ancova_trial` based on scores or bands.
- Using v1 held-out replicates as confirmatory data for v2 cutoffs.

## Score policy

Before any production robustness training:

1. Freeze v2 weights in the SAP (`0.5 / 0.5 / 0` unless the alternate is
   selected after a named pilot comparison).
2. Keep `n_boot = 1000` and `max_removal_pct = 0.30` unless a pre-registered
   pilot documents a saturation or runtime defect.
3. Always compute and archive the v1-weight composite as a comparator column
   or parallel summary; never optimize cutoffs on it in v2.

Runtime packaging: either pass explicit weights into `robustness_lm()` for
the study adapter, or define a study-local score assembler from components.
Package defaults for interactive users need not change until Gate B for v2.

## Truth and scenario policy

- Retain canonical generator, allocation, and stress diagnostics from v1.
- Primary fitting strata: null and clear only.
- Clear target power: freeze **0.95** if the score-only pilot’s null/clear
  overlap still looks marginal at 0.90; otherwise freeze **0.90**.
- Borderline (~0.60) scenarios may still be generated for diagnostics and
  manuscript figures but are excluded from cutoff search and acceptance.
- Screening remains significant-only for band-fitting rows (false reassurance
  is defined on significant nulls).

## Pilot-before-production gate

Run a **score-only pilot** (no categorical fitting, no held-out opening):

1. Fix weights and clear-power choice in the SAP.
2. Collect class-wise component and composite quantiles by \(n\).
3. Pre-registered go/no-go metrics (freeze exact formulas in the SAP), e.g.:
   - median(clear) − median(null) on the v2 score;
   - overlap index such as \(P(\text{null} > \text{median(clear)})\);
   - optional ROC/AUC for null vs clear on the sealed pilot grid.
4. **Go** only if separation is plausibly compatible with Track A operating
   characteristics; else stop at numeric-only and do not burn held-out data.

## Cutoff search and acceptance

Training:

- search integer \(L ∈ \{0,…,100\}\);
- Fragile if score ≤ \(L\), else Not fragile;
- require false reassurance ≤ 0.05 with one-sided 95% Wilson upper ≤ 0.10;
- require Not-fragile identification on clear ≥ 0.70 with Wilson lower ≥ 0.60;
- deterministic tie-breaks (highest clear identification, then greatest FR
  safety margin, then smallest \(L\)).

Held-out:

- evaluate the frozen \(L\) once without refit;
- use the more conservative of Wilson and scenario-cluster bootstrap bounds;
- fail closed on miss; no second candidate.

Stress rows never fit or accept.

## Package / provenance rules

- Finish v1 Gate B as an explicit **uncalibrated** publish path for
  `lm_ancova` (Task 16 of the parent plan) before or beside v2 work; do not
  leave v1’s fail-closed decision unpublished.
- v2 artifacts live under a distinct study path or manifest version
  (recommended: `manuscript/calibration/studies/lm_ancova_v2/` or a clearly
  versioned subdirectory/SAP), with independent hashes.
- Successful v2 updates a **new** registry row / profile; v1 row stays as the
  historical uncalibrated record.
- `pain_ancova_trial` remains prospectively frozen and non-calibrating.

## Alternatives considered (summary)

| Track | Summary | Verdict |
| --- | --- | --- |
| A | Two-band + jackknife-light score | **Selected** |
| B | Three-band after pilot proves separation + \(n\)-strata | Deferred; not the v2 start |
| C | Numeric-only, no new compute | Fallback if pilot go/no-go fails |

## Success criteria

- Sealed pilot go/no-go executed and recorded before production training.
- Either a validated two-band `lm_ancova_v2` publication with full ledger, or
  an explicit fail-closed uncalibrated v2 result.
- No contamination of v1 held-out evidence into v2 fitting.
- User-facing docs state clearly that ANCOVA bands (if any) are two-class and
  method-specific, not Welch 55/70.
