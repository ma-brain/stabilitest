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

**What remains.** Track E (pre-registered violation-detection ΔAUC among
significant results) will be appended to this section when published. The
replication-probability curve target (Track D) is parked pending the
binary-proportion study’s replication-curve outcome and is not executed in
this Phase 1 plan. Categorical labels stay suppressed for all ANCOVA units.

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
