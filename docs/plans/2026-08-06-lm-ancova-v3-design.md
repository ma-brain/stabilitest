# LM/ANCOVA Calibration Design Addendum (v3)

**Parent design:** `docs/plans/2026-08-06-lm-ancova-calibration-design.md`
**Parent plan:** `docs/plans/2026-08-06-lm-ancova-calibration.md`
**v2 addendum:** `docs/plans/2026-08-06-lm-ancova-v2-design.md`
**v2 plan:** `docs/plans/2026-08-06-lm-ancova-v2-calibration.md`
**Date:** 2026-08-06
**Status:** proposed — pending review
**Calibration unit (proposed):** `lm_ancova_v3`
**v1 outcome:** training fail-closed (`no_feasible_thresholds`); held-out never
opened; active `lm_ancova` registry row is the immutable uncalibrated record.
**v2 outcome (updated 2026-08-06, post-execution):** v2 WAS executed and
closed **fail-closed** (`no_feasible_thresholds`, candidate hash
`3dc2a1f840b3eb725bea629dc130f070`, published under
`studies/lm_ancova_v2/published/`, registry row `lm-ancova-v2-2026-1`,
held-out never opened). The execution **empirically confirmed both
predictions of this addendum**: the sealed pilot gate returned a false GO
(Δ = 24.4, overlap = 0.034, AUC = 0.892 — all "go") and the training cutoff
search then found zero feasible cutoffs, with best RI at the FR-safe L of
**0.554** against the ≥ 0.70 gate — versus 0.542 predicted from v1 training
rows and 0.54–0.58 from the analytic projection. The v2 publication is
immutable; it now serves as the empirical demonstration of the pilot-gate
defect that the corrected feasibility-projection metric fixes.

## Context: reanalysis of v1 training evidence

All findings below come from the 2,700 completed significant v1 training
replicates (`manuscript/calibration/studies/lm_ancova/artifacts/raw/training/
completed_training_core.rds`), which record the three component metrics per
replicate and therefore answer every reweighting question without new
simulation. v1 held-out data remain closed; nothing here touches them.
Reproduction scripts are committed at
`manuscript/calibration/studies/lm_ancova/tools/reanalysis/`
(`weight-grid-feasibility.R`, `feasibility-projection.R`).

### Finding 1 — no weighting admits three bands

An exhaustive search of the weight simplex (231 combinations of
`(jackknife, fragility, bootstrap)` at 0.05 steps) crossed with all 5,050
integer cutoff pairs found **zero** feasible `(L, U)` under the frozen v1
gates — pooled and within every `n` stratum. The best *unconstrained*
balanced accuracy anywhere on the simplex is **0.573** (gate ≥ 0.70;
per-stratum best 0.549–0.600). The LDA-optimal linear projection, even with
`log n` added as a feature, reaches borderline-vs-clear AUC of only **0.702**.
The middle class is not separable by any linear score on these components.

A four-band scheme requires three cutoffs and finer class separation on the
same one-dimensional score; it is ruled out a fortiori by the same bound.

### Finding 2 — the composite score is a monotone re-expression of p

| Evidence | Value |
| --- | --- |
| Spearman(bootstrap component, −log₁₀ p) | **0.994** |
| Spearman(v2 composite 0.5F/0.5B, −log₁₀ p) | 0.971 |
| AUC null-vs-clear, p alone | 0.8953 |
| AUC null-vs-clear, p + all three components | 0.8955 (**+0.0002**) |

In the canonical Gaussian ANCOVA design the t-statistic is (essentially)
sufficient for the treatment effect, and bootstrap significance rates and
k-to-flip fragility are near-deterministic functions of `(t, n)`. Therefore
**no resampling composite can discriminate power-defined truth classes better
than banding p itself.** The v1/v2 infeasibility is information-theoretic,
not a weighting defect; further weight optimization is closed as a track.

### Finding 3 — the gates demand AUC ≈ 0.94; the truth definition supplies ≈ 0.89

False reassurance ≤ 0.05 plus Not-fragile identification ≥ 0.70 requires the
null 95th percentile to fall below the clear 30th percentile — under a
binormal approximation, **AUC ≥ 0.937**. Truth classes defined at 90% power
deliver ≈ 0.88–0.91. Exact noncentral-t projection of the best achievable
(p-equivalent) two-band rule, `RI at the FR-safe cutoff` (FR-safe means
`P(S > L | significant null) ≤ 0.05`, i.e. effective α′ = 0.0025):

| Clear power target | Projected RI (n = 40 … 240) | Verdict |
| --- | --- | --- |
| 0.90 (v2 default) | 0.540 – 0.579 | fails everywhere |
| 0.95 (v2 escalation) | 0.674 – 0.713 | knife-edge; fails n ≤ 80 |
| 0.99 | 0.865 – 0.892 | passes, but claim is vacuous |

Empirical v1 values (0.53–0.67 across `n`) confirm the projection. The
failure is a property of the truth definition, not of execution quality.
Redefining "clear" at ~0.99 power would make bands feasible but reduce the
claim to "overwhelming effects are not fragile", which we reject.

### Finding 4 — the v2 sealed pilot gate returns a false GO (empirically confirmed)

Evaluated on v1 training rows, every frozen v2 pilot metric passes:
Δ = 26.4 (go ≥ 20), overlap = 0.034 (go ≤ 0.10), AUC = 0.885 (go ≥ 0.75) —
yet the cutoff search on the same rows is infeasible (best RI at the FR-safe
`L` is 0.542 pooled). The pilot measures *location* separation while
feasibility is governed by *tail* separation at the null 95th percentile.

**Empirical confirmation:** the v2 execution reproduced this exactly. Its
pilot GO'd (Δ = 24.4, overlap = 0.034, AUC = 0.892;
`studies/lm_ancova_v2/artifacts/summaries/SCORE_PILOT_GATE.json`), production
training was spent, and the cutoff search closed fail-closed with best RI at
the FR-safe L of 0.554 (`training-cutoff-grid.csv`) — within 0.012 of the
prediction. The prediction-then-confirmation triple (analytic 0.54–0.58,
v1-preview 0.542, v2-empirical 0.554) validates the corrected
feasibility-projection metric below as an accurate, cheap substitute for a
production run. This is the procedural defect v3 corrects.

## Decision (proposed for lock)

**Re-aim the calibration target; keep truth-class bands closed.**

| Element | v3 choice |
| --- | --- |
| v2 disposition | Executed fail-closed and published (immutable); its false-GO pilot + infeasible training are cited as the empirical validation of Finding 4 |
| Band claims (3-band, 4-band, 2-band on truth classes) | Closed at powers ≤ 0.95; 0.99 rejected as vacuous |
| Weight optimization | Closed (Finding 1–2); no further search |
| Track D (primary) | Calibrate the score to **replication probability** as a validated continuous curve |
| Track E (co-primary) | Pre-registered test that the score detects **assumption violations** that p cannot |
| Track F (unconditional) | Publish the negative result with its mechanism |
| Pilot gate (any future band attempt) | Replace location metrics with the **feasibility-projection metric** below |
| Registry | New unit `lm_ancova_v3`; v1/v2 rows untouched; labels stay suppressed until a Gate B for the new claim type |

## Goals

- Give the score a validated, operational meaning users actually need,
  instead of a truth-class band it provably cannot support.
- Demonstrate (or refute, pre-registered either way) that the score adds
  information beyond p exactly where model assumptions fail.
- Convert two fail-closed calibrations into an explained, publishable
  negative result.
- Repair the pilot-gate methodology so no future band attempt can false-GO.

## Non-Goals

- Any 3-band or 4-band claim on truth classes.
- Re-searching composite weights (closed by sufficiency, not by fiat).
- Softening v1/v2 acceptance gates to force labels.
- Reusing v1 held-out replicates for any v3 fitting or acceptance.
- Modifying `pain_ancova_trial`, which stays prospectively frozen and
  non-calibrating.
- Changing the v1 `lm_ancova` or (once recorded) v2 registry provenance.

## Corrected pilot gate — feasibility-projection metric (sealed)

Mandatory primary metric for any future categorical attempt; recorded here
so the defect in Finding 4 cannot recur.

1. On sealed pilot rows, find the smallest integer `L*` with
   `FR(L) = P(S > L | significant null) ≤ 0.05` and one-sided 95% Wilson
   upper ≤ 0.10. No such `L*` ⇒ hard no-go.
2. Projected RI = `P(S > L* | significant clear)`.
3. **Go** ≥ 0.72 (margin above the 0.70 acceptance gate); marginal
   0.70–0.72 (single recorded escalation allowed); hard no-go < 0.70.

Applied retrospectively this metric returns no-go for both v1 and v2,
matching their known outcomes. Location-gap, overlap, and AUC metrics may be
archived as diagnostics but are no longer gate-forming.

## Track D — replication-probability calibration (primary)

**Claim.** For an eligible significant canonical ANCOVA result with score
`s`, report a validated estimate `r(s)` of the probability that an exact
replicate study (same design, same `n`, same true parameters) is again
significant at α = 0.05 — with a confidence band, under the frozen reference
scenario population.

**Design.**

- Score: the v2 jackknife-light composite (`fragility = 0.5`,
  `bootstrap = 0.5`, `jackknife = 0`), for continuity with the frozen v2 SAP;
  v1 composite archived as comparator. Given Finding 2 the choice is not
  load-bearing and must not be re-optimized against Track D metrics.
- Generation: new seeds throughout (v3 ranges, disjoint from `41001`/`42001`/
  `43001` and from v1 ledgers). For each completed significant training
  replicate, draw one independent replicate dataset from the same scenario
  generator and record only the primary-test conclusion (cheap; no
  resampling on the replicate draw).
- Fit: monotone mapping `r(s)` on training rows (logistic in `s` as default;
  isotonic archived as sensitivity), pooled over the core grid. Borderline
  scenarios are **included** — Track D needs the full score range, and the
  truth-class exclusion logic of v2 does not apply to a curve claim.
- Freeze: `r(s)` coefficients + hash before held-out generation, exactly as
  the v1/v2 freeze-then-validate discipline.
- Held-out: evaluate once, no refit, on the held-out grid
  (`n ∈ {60, 120, 240}`, R² ∈ {0.25, 0.55}) whose scenario mix differs from
  training — transportability across the design grid is the point of the
  validation.

**Acceptance gates (exact numbers frozen in the v3 SAP; proposed):**

- held-out calibration intercept |logit| ≤ 0.20 and slope in [0.85, 1.15];
- max absolute bin error (10 equal-count bins) ≤ 0.10 with the more
  conservative of Wilson and scenario-cluster bootstrap bounds;
- Brier score non-inferior to the p-value-only reference mapping
  (margin frozen in SAP) — honesty check that the curve claims no
  discrimination the p-value does not already carry.

**Disclosure.** `r(s)` is defined relative to the frozen calibration
reference population (the scenario grid), like any diagnostic-test
calibration under a case mix. The SAP, manuscript, and package docs must
state this; per-scenario conditional curves are archived as diagnostics.

**Fail-closed.** If held-out gates miss, `lm_ancova_v3` publishes
`uncalibrated` with full diagnostics; no second candidate, no refit.

## Track E — violation-detection value-add (co-primary)

**Claim.** Among significant canonical-looking ANCOVA results, the score
discriminates assumption-violated data from clean data better than the
p-value does. This is the regime where Finding 2's sufficiency argument
breaks and resampling can genuinely add information.

**Design.**

- Paired scenarios: each clean core scenario matched with violated variants
  reusing the existing stress generators (heteroscedasticity, heavy tails,
  missing baseline, nonlinear baseline, treatment-by-baseline interaction)
  at matched `n` and nominal power.
- Pre-registered metric: `AUC_score − AUC_p` for discriminating violated
  from clean among significant results, per violation type and pooled,
  with cluster-bootstrap CIs.
- Go/success threshold frozen in the v3 SAP before any score inspection
  (proposed: pooled ΔAUC ≥ 0.10 with CI lower bound > 0).
- Outcome either way is publishable: a confirmed margin motivates a runtime
  *diagnostic flag* (not a truth band); a null result bounds the score's
  added value honestly.

## Track F — negative-result publication (unconditional)

Manuscript section (ordered after methods, before the case study) reporting:

- v1 and v2 dispositions with hashes and the fail-closed reasoning;
- the sufficiency argument and the +0.0002 incremental-AUC result;
- the required-AUC bound (0.937) versus delivered separation (≈ 0.89);
- the noncentral-t projection table and the corrected pilot gate;
- committed reanalysis scripts as the reproducibility appendix.

This track has no gate; it proceeds regardless of Track D/E outcomes.

## Registry, package, and provenance rules

- New unit `lm_ancova_v3` with its own study path
  (`manuscript/calibration/studies/lm_ancova_v3/`), SAP, manifests, seed
  ranges, and hash ledger. v1 provenance immutable; v2 SAP updated only to
  record the analytic no-go disposition.
- Categorical labels remain suppressed for all LM/ANCOVA profiles. Track D
  success would introduce a **new registry claim type** (e.g.
  `validated_replication_curve`) storing the frozen mapping; runtime
  `robustness_lm()` could then report an estimated replication probability
  with CI for eligible canonical profiles. That runtime change is its own
  Gate B with tests, docs, and review — never a silent side effect.
- Track E success introduces at most a documented diagnostic flag, gated
  the same way.
- `pain_ancova_trial` stays frozen; the case-study vignette reports whatever
  the validated claim type honestly supports (numeric-only if v3 fails).
- Exploratory use of v1 *training* rows (this addendum) is recorded here;
  v1 held-out rows remain closed to every v3 activity.

## Alternatives considered

| Track | Summary | Verdict |
| --- | --- | --- |
| v2 as frozen | Two-band on truth classes, pilot then production | **Executed before this addendum landed; failed exactly as predicted** (pilot false-GO, training infeasible, fail-closed publish) |
| Weight re-optimization / 4-band | Search more scores or bands | **Rejected** — closed by Findings 1–2 |
| Clear power 0.99 bands | Feasible per projection | **Rejected** — vacuous claim |
| n-stratified cutoffs | Per-n bands | **Rejected** — zero feasible combos in every stratum; held-out n values differ from training |
| D: replication curve | Continuous operational claim | **Selected (primary)** |
| E: violation detection | Score value beyond p under model failure | **Selected (co-primary)** |
| C: numeric-only, publish negative result | No new compute beyond replicate draws | Fallback if D and E both fail; Track F happens regardless |

## Success criteria

- v2's published fail-closed decision is verified intact (hashes, held-out
  unopened, registry row) and cross-referenced from this addendum and the
  Track F manuscript section as the empirical pilot-gate-defect demonstration.
- The feasibility-projection pilot metric is frozen as mandatory for any
  future categorical attempt.
- Track D yields either a validated held-out replication curve with full
  ledger, or an explicit fail-closed uncalibrated v3 result.
- Track E's pre-registered ΔAUC test is executed and reported either way.
- Track F's negative-result section lands in the manuscript with committed,
  reproducible scripts.
- No contamination: v1 held-out unopened; v3 seeds disjoint from v1/v2
  ledgers; `pain_ancova_trial` untouched.
