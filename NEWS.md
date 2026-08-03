# Unreleased

## Features

* Extended `robustness_analysis()` with two-group binary proportion tests:
  `test_type = "fisher"` (Fisher's exact), `"chisq"` (`stats::chisq.test`),
  and `"prop"` (`stats::prop.test`). Inputs are individual-level binary 0/1
  or logical vectors so jackknife, worst-case removal, and bootstrap reuse the
  existing two-sample machinery. Continuity correction for `"chisq"` /
  `"prop"` is controlled by `correct` (default `TRUE`, matching base R).
  Barnard's exact test is not included (no light dependency / pure
  implementation in this release).

# stabilitest 0.3.0

## Features

* Added `robustness_glm()` for binomial logistic (`logit`) GLM terms, with
  optional observation weights via `obs_weights` (distinct from composite
  score `weights`).
* Extended `print.robustness_model()` to report odds ratios for binomial GLM
  fits.

## Validation and edge cases

* Validate GLM family/link (binomial logit only; redirect Gaussian to
  `robustness_lm()`; reject quasi-families and non-logit links).
* Cover GLM edges in tests: small *n*, bad score/`obs_weights`, missing terms,
  separation, collinearity, and all-zero binary outcomes.

## Documentation and manuscript

* Documented `robustness_glm()` in README, package DESCRIPTION, vignette, and
  roxygen/`man/` pages.
* Manuscript Typst Springer-like PDF styling, pandoc template, `build_pdf.sh`,
  and tighter heading spacing for the rebuilt PDF.

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

## Bug fixes

* Aligned `robustness_engine()` weight and alpha validation with
  `robustness_analysis()`, and reject all-censored Cox fits cleanly when the
  model returns an NA p-value instead of crashing downstream.

## Testing and development

* Expanded testthat coverage for survival analyses, plotting, non-significant
  and borderline cases, and other edge conditions.
* Added dedicated edge-case tests covering invalid weights/alpha and
  all-censored Cox input.
* Added GitHub Actions `R-CMD-check` workflow.
