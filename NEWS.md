# stabilitest (development version)

## Correctness

* Fragility scoring now requires at least one feasible deletion and only reports
  the right-censored `max_k + 1` index after completing the configured removal
  horizon. Analyses fail clearly when the sample cannot support a deletion or
  when candidate fitting ends the search prematurely.
* `robustness_lm()`, `robustness_glm()`, and `robustness_surv()` now run
  jackknife, removal, and bootstrap calculations on exactly the rows retained
  by the full fitted model. Reported sample sizes and percentages therefore
  exclude observations omitted for missing model variables; GLM observation
  weights remain aligned to their original rows.
* Two-sample bootstrap runs now retain and count non-finite or failed
  replicates. Summaries use finite p-values, result objects expose `n_valid`
  and `n_failed`, and an entirely degenerate bootstrap fails with a clear
  error instead of producing a missing or misleading score.
* Composite weights must now be a named numeric vector containing exactly
  `jackknife`, `fragility`, and `bootstrap`, with finite non-negative values
  summing to one. Malformed names, values, and sums produce specific validation
  errors before score calculation begins.
* `manuscript/simulation_study.R` now locates and loads the package checkout
  containing the script through `pkgload`, so it runs from any working
  directory without relying on a partial or stale package source.

# stabilitest 0.5.0

## Internal

* Shared helpers in `R/robustness_shared.R` for validation, composite
  scoring, score bands, jackknife/bootstrap annotation, and fragility
  summaries across the two-sample and model/TOST engines.
* Result objects keep historical field names and add cross-class aliases
  (`metrics` / `robustness_metrics`, `interpretation_label` /
  `robustness_interpretation`, `original_estimate` /
  `original_mean_diff`) plus aligned shared metric columns.
* Related test and documentation updates for schema alignment.

# stabilitest 0.4.2

## CRAN check cleanup

* `.Rbuildignore` now excludes `.github/` (`^\.github$`).
* Import `stats::formula` (used by model engines) and declare
  `.__obs_w__` via `globalVariables()` to clear R CMD check NOTES.

## Documentation

* Added `\value` sections and runnable examples for
  `robustness_analysis()`, `robustness_lm()`, `robustness_glm()`,
  `robustness_surv()`, and `robustness_tost()`.

## Validation

* Continuous `group1` / `group2` inputs reject missing values with a clear
  error (including paired and TOST paths).
* Shared checks: `n_boot` must be a single positive integer;
  `max_removal_pct` must be in `(0, 1]` (across two-sample and model
  engines). Covered by edge-case tests.

# stabilitest 0.4.1

## Features

* Extended `robustness_tost()` with `endpoint = c("mean", "prop", "or")`
  (alias `test`): **Wald risk-difference** TOST/NI for binary 0/1 outcomes
  and **Wald log(OR)** TOST/NI with OR-scale margins
  (`[1/margin, margin]` when `margin > 1`; Haldane–Anscombe 0.5 continuity
  for zero cells). Mean difference (Welch / paired t) remains the default.
  Same `p_eff` adapter into the robustness engine. Score bands are still
  **not** separately calibrated for equivalence/NI. Not Farrington–Manning
  / exact unconditional; Wald can be anticonservative for sparse tables.
  Rank TOST remains deferred.

* `robustness_glm()` now supports **Poisson log-link** models
  (`family = poisson(link = "log")`) alongside binomial logit. Print reports
  `IRR = exp(estimate)` for Poisson (parallel to OR for binomial). Offsets via
  `offset(...)` in the formula work as in `stats::glm`. Gaussian still
  redirects to `robustness_lm()`; quasi-families and non-canonical links for
  these families remain rejected.

* `robustness_glm()` and `robustness_surv()` support **multi-df terms** via the
  same `resolve_model_term()` rules as `robustness_lm()`: pass a term label
  (e.g. `"arm"`) for a joint likelihood-ratio test
  (`drop1(..., test = "Chisq")`), or a coefficient row name for the previous
  single-coef Wald behaviour. Print reports "joint LRT" with ndf / statistic;
  `estimate` is `NA` for joint terms.

# stabilitest 0.4.0

## Features

* `robustness_lm()` now supports **multi-df terms** (e.g. a 3-level factor
  ANCOVA term): pass the term label (`"arm"`) for a joint `drop1(..., test =
  "F")` test, or a single coefficient row name (`"armActive"`) for the
  previous Wald/t behaviour. Multi-df fits store `estimate = NA` and
  `term_info` / `sample_info` with joint F, ndf, and ddf; print reports
  "joint F" rather than a single beta.

* Added `robustness_tost()` for **equivalence (Schuirmann TOST)** and
  **non-inferiority** on continuous mean differences (Welch or paired t).
  Equivalence concludes when both one-sided tests reject at `alpha`
  (equivalently, the `(1 - 2 * alpha)` CI lies inside
  `[delta_L, delta_U]` / `[-margin, margin]`). Non-inferiority uses a
  one-sided test vs `margin`, with `higher_is_better` controlling direction.
  Robustness reuses the model-engine composite score via an effective
  p-value (`p_eff = max(p_lower, p_upper)` for TOST; the NI p-value for
  non-inferiority) so jackknife / worst-case / bootstrap track the
  TOST/NI **decision**. Score bands are not separately calibrated for
  equivalence/NI. Binary RD / OR TOST added in 0.4.1; rank TOST remains
  deferred.

* Extended `robustness_analysis()` with two-group binary proportion tests:
  `test_type = "fisher"` (Fisher's exact), `"chisq"` (`stats::chisq.test`),
  and `"prop"` (`stats::prop.test`). Inputs are individual-level binary 0/1
  or logical vectors so jackknife, worst-case removal, and bootstrap reuse the
  existing two-sample machinery. Continuity correction for `"chisq"` /
  `"prop"` is controlled by `correct` (default `TRUE`, matching base R).
  Barnard's exact test is not included (no light dependency / pure
  implementation in this release).

* Added unpaired `test_type = "brunner_munzel"` (base-R Brunner–Munzel /
  nonparametric Behrens–Fisher; Neubert & Brunner 2007 formulas). Prefer it
  over `"wilcoxon"` when unequal variances are plausible; paired designs are
  rejected (use `"wilcoxon.paired"`).

* Rank tests (`"wilcoxon"`, `"wilcoxon.paired"`, `"brunner_munzel"`) now report
  the Hodges–Lehmann location shift in `original_mean_diff` (unpaired: median
  of all pairwise differences; paired: median of Walsh averages). Brunner–Munzel
  also stores stochastic superiority
  `P(X < Y) + 0.5 P(X = Y)` in `sample_info$stochastic_superiority`.

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
