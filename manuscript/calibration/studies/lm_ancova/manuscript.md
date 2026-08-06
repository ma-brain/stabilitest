# ANCOVA Categorical-Band Calibration Manuscript

## Methods

This manuscript reports an independent calibration of categorical Fragile /
Moderately Robust / Robust bands for eligible significant canonical 1-df
ANCOVA treatment effects (`outcome ~ treatment + baseline`). Multi-df and
noncanonical linear-model results remain numeric-only. The composite score,
default weights, removal budget, and bootstrap configuration are frozen; only
the two categorical cutoffs are estimated. Training, candidate freezing, and
held-out evaluation follow the irreversible study flow in
`CALIBRATION_SAP.md`.

## Negative result: categorical truth-class bands are closed

Gate B for `lm_ancova` (v1) closed **fail-closed**. Training found no
feasible `(L, U)` pair (`no_feasible_thresholds`; candidate hash
`9ccfc2fca7c0a07c19a3a18838e9a3f2`). Held-out validation was **not** opened.
The active package registry row for `lm_ancova` remains `uncalibrated`
(version `lm-ancova-2026-1`); categorical labels are suppressed. Compact
decision artifacts live under `published/`.

Gate B for Track A unit `lm_ancova_v2` was then **executed** and also closed
fail-closed (`no_feasible_thresholds`; candidate hash
`3dc2a1f840b3eb725bea629dc130f070`; version `lm-ancova-v2-2026-1`; held-out
never opened). Its sealed pilot returned GO on location metrics
(Δ = 24.4, overlap = 0.034, AUC = 0.892;
`studies/lm_ancova_v2/artifacts/summaries/SCORE_PILOT_GATE.json`), after which
production training found no feasible cutoff: best RI at the FR-safe L was
0.554 against the ≥ 0.70 gate
(`studies/lm_ancova_v2/artifacts/summaries/training-cutoff-grid.csv`). Welch
55/70 cutoffs remain a Welch comparator only and are never an ANCOVA
fallback.

Reanalysis of the 2,700 completed significant v1 training replicates
explains both failures and closes further weight search. Reproduction
scripts are committed at
`studies/lm_ancova/tools/reanalysis/`
(`weight-grid-feasibility.R`, `feasibility-projection.R`); the scientific
summary is locked in `docs/plans/2026-08-06-lm-ancova-v3-design.md`.

**Sufficiency.** In the canonical Gaussian ANCOVA design the t-statistic is
essentially sufficient for the treatment effect, so bootstrap significance
rates and k-to-flip fragility are near-deterministic functions of `(t, n)`.
On v1 training rows, Spearman(bootstrap component, −log₁₀ p) = 0.994 and
Spearman(v2 composite 0.5F/0.5B, −log₁₀ p) = 0.971. Null-versus-clear AUC
for p alone is 0.8953; adding all three component metrics raises it only to
0.8955 (**+0.0002**). No resampling composite can discriminate power-defined
truth classes better than banding p itself.

**Required versus delivered separation.** False reassurance ≤ 0.05 plus
Not-fragile identification ≥ 0.70 requires the null 95th percentile to fall
below the clear 30th percentile — under a binormal approximation,
AUC ≥ 0.937. Truth classes defined at 90% power deliver ≈ 0.88–0.91.
Exact noncentral-t projection of the best achievable (p-equivalent)
two-band rule, RI at the FR-safe cutoff
(`P(S > L | significant null) ≤ 0.05`, i.e. effective α′ = 0.0025):

| Clear power target | Projected RI (n = 40 … 240) | Verdict |
| --- | --- | --- |
| 0.90 (v2 default) | 0.540 – 0.579 | fails everywhere |
| 0.95 (v2 escalation) | 0.674 – 0.713 | knife-edge; fails n ≤ 80 |
| 0.99 | 0.865 – 0.892 | passes, but claim is vacuous |

Empirical v1 values (0.53–0.67 across `n`) confirm the projection. The
failure is a property of the truth definition, not of execution quality.
Redefining “clear” at ~0.99 power would make bands feasible but reduce the
claim to “overwhelming effects are not fragile”, which we reject. An
exhaustive weight-simplex search (231 combinations × 5,050 integer cutoff
pairs) found zero feasible `(L, U)` under the frozen v1 gates.

**Corrected pilot-gate metric (prediction then confirmation).** The v2
sealed pilot measured *location* separation while feasibility is governed
by *tail* separation at the null 95th percentile. Evaluated on v1 training
rows, every frozen v2 pilot metric would have passed (Δ = 26.4, overlap =
0.034, AUC = 0.885) while the same rows were already infeasible (best RI at
the FR-safe L = 0.542 pooled). The executed v2 run confirmed the defect:
analytic projection 0.54–0.58 → v1-preview 0.542 → v2-empirical 0.554, while
the frozen location metrics all said GO. Any future categorical attempt must
use a feasibility-projection pilot metric rather than location GO/NO-GO.

**Track E (violation detection) — not confirmed.** The pre-registered Phase 1
study `lm_ancova_v3` tested whether, among significant clear ANCOVA rows, the
jackknife-light score (`fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`)
discriminates assumption-violated data from clean data better than the
p-value. Quotas were met (2,100 completed, 0 failed; ≥100 significant per
primary cell; ≤5% failures). The frozen gate required pooled
ΔAUC = AUC_score − AUC_p ≥ 0.10 **and** scenario-cluster bootstrap 95% CI
lower bound > 0 (seed `20260807`, B = 1000). Observed pooled values:
AUC_score = 0.5569, AUC_p = 0.5516, **ΔAUC = 0.0053**, CI
**[−0.1109, 0.1289]**. Verdict: **not confirmed**
(`studies/lm_ancova_v3/published/TRACK_E_VERDICT.json`, md5
`8fe6c66f28b5c788a637eff0cb8a3029`). Per-violation ΔAUC was descriptive only
(largest +0.060 for 2:1 allocation; others near zero or slightly negative;
`track-e-per-violation.csv`). This is an honest bound on the score’s added
value beyond p under the frozen violation suite: the score does not clear the
pre-registered discrimination gate. Compact publication artifacts and the
hash ledger are under `studies/lm_ancova_v3/published/`. No package registry
or runtime behavior change follows.

**What remains.** The replication-probability curve target (Track D) stays
parked pending the binary-proportion study’s replication-curve outcome and
explicit human un-parking; seed ranges `51001+` / `52001+` / `53001+` remain
reserved. Categorical labels stay suppressed for all ANCOVA units.

## Illustrative synthetic case study

The package ships a prospectively frozen synthetic dataset,
`pain_ancova_trial`, generated from `data-raw/pain_ancova_trial.R` with seed
`20260806` before production calibration scores were inspected. This example
did **not** contribute calibration evidence: it was never screened for
significance for threshold fitting, never entered training or held-out rows,
and must not be regenerated to improve a p-value, score, or band.

The primary worked analysis uses the exact calibrated configuration:

```r
case_result <- robustness_lm(
  week12_pain ~ arm + baseline_pain,
  pain_ancova_trial,
  term = "armActive",
  alpha = 0.05,
  n_boot = 1000,
  max_removal_pct = 0.30,
  weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
  seed = 1408
)
```

At Gate A this section documents the intended outputs only: adjusted treatment
estimate and p-value, calibration-profile eligibility, jackknife stability,
worst-case removal fragility, bootstrap reproducibility, composite score,
applicable label (if any), and deletion/influence plots. Because Gate B closed
fail-closed (`uncalibrated` / `no_feasible_thresholds`; held-out not opened),
the published case-study outcome is numeric scores and component metrics with
the categorical label suppressed.

## Discussion

Discussion of operating characteristics, applicability limits, and the
illustrative case study will follow the locked calibration decision.
