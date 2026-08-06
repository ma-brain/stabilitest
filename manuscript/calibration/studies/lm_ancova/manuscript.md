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

## Calibration results

Calibration results will be populated after the held-out gate is opened once
and reviewed. Until Gate B, the active package registry row for `lm_ancova`
remains `uncalibrated`. Welch 55/70 cutoffs are reported only as a comparator
and are never used as an ANCOVA fallback.

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
applicable label (if any), and deletion/influence plots. Final numeric claims
and the categorical label status are deferred until after the calibration
decision is published. The vignette must render honestly for either outcome: a
validated label or an uncalibrated numeric-only result.

## Discussion

Discussion of operating characteristics, applicability limits, and the
illustrative case study will follow the locked calibration decision.
