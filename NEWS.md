# stabilitest 0.1.0

Experimental initial release. The API may change.

## Features

* Added `robustness_analysis()` for two-sample location tests (Welch and
  paired t, Wilcoxon), combining jackknife leave-one-out stability, greedy
  worst-case removal fragility, and bootstrap reproducibility into component
  metrics and a composite 0–100 score.
* Added `robustness_lm()` and `robustness_surv()` for the same scoring
  pipeline on linear model / ANCOVA terms and Cox proportional hazards terms.
* Added print and plot methods for analysis objects, optional narrative
  interpretation (`interpret = TRUE`), and simulation-calibrated score bands
  (Robust / Moderately Robust / Fragile) for significant primary results.
* Shipped synthetic pain trial datasets (`pain_treatment`, `pain_placebo`)
  used in the manuscript case study.

## Documentation and packaging

* Added roxygen2 documentation (`man/`, `NAMESPACE`) for the package and
  exported functions.
* Added vignette *Pain Trial Case Study: Interpreting Robustness Scores*,
  covering the case-study walkthrough and interpretation bands.

## Testing and development

* Expanded testthat coverage for survival analyses, plotting, non-significant
  and borderline cases, and other edge conditions.
* Added GitHub Actions `R-CMD-check` workflow.
