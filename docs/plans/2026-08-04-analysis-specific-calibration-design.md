# Analysis-Specific Calibration Design

## Goal

Build a publication-grade, reproducible simulation program that tests whether
the existing robustness-score bands transfer across every public analysis
family. Retain one shared set of bands when predefined validation criteria are
met, introduce family-specific bands only when transferability materially
fails, and avoid categorical interpretation where calibration is unsupported.

## Scope

The calibration program covers:

- Welch and paired t-tests;
- Wilcoxon rank-sum and signed-rank tests;
- Brunner-Munzel tests;
- Fisher exact, chi-square, and proportion tests;
- linear models and ANCOVA, including multi-df terms;
- binomial-logit and Poisson-log GLMs, including multi-df terms and offsets;
- Cox proportional-hazards models, including multi-df terms;
- equivalence and non-inferiority for means, risk differences, and odds ratios;
- the interpretation policy for non-significant or unsuccessful conclusions.

The first phase builds and freezes the simulation infrastructure and analysis
plan. Package cutoffs and interpretation behavior do not change until the
held-out validation analysis is complete.

## Calibration Strategy

The current cutoffs are a frozen candidate:

- score greater than 70: Robust;
- score in (55, 70]: Moderately Robust;
- score at or below 55: Fragile.

The study first evaluates these shared bands across all analysis families.
Family-specific cutoffs are considered only when the shared bands fail
predefined held-out criteria. A family-specific policy is adopted only if it
improves held-out balanced ordinal accuracy by at least 0.05 and differs from
the shared policy by at least five score points. Otherwise that family remains
explicitly uncalibrated rather than receiving unstable thresholds.

For significant superiority conclusions and successful equivalence or
non-inferiority conclusions, reference strata are:

- **Fragile:** null false positives, contamination-driven positives, false
  equivalence, or false non-inferiority;
- **Moderate:** genuine but boundary-adjacent or moderate effects;
- **Robust:** clear true effects or conclusions comfortably within the target
  margin.

The shared bands pass for a family only if held-out validation shows:

- false reassurance point estimate at or below 5%, with its one-sided 95%
  upper confidence bound at or below 10%;
- robust identification at or above 70%, with its one-sided 95% lower bound at
  or above 60%;
- correctly ordered median scores in every core validation stratum;
- no clinically material reversal of ordering in a stress scenario.

The calibration report also presents balanced ordinal accuracy, score and
component distributions, failure rates, and sensitivity to sample size,
`n_boot`, weights, and `max_removal_pct`.

## Non-Significant Conclusions

Non-significant results are analyzed separately. The study evaluates whether
the score distinguishes true nulls from false negatives, but does not assume
that the existing Fragile/Moderate/Robust labels are meaningful. Stable
non-rejection may reflect low power rather than evidence for no effect, while
the greedy removal path asks how easily significance can be manufactured.

Unless held-out discrimination and ordering are consistently strong across
families, the package will suppress the categorical label for non-significant
results and return the component metrics with a `bands_not_applicable` status.
Any future non-significant categories must use distinct terminology and require
a separate scientific justification.

## Architecture

Create a modular pipeline under `manuscript/calibration/` rather than extending
the existing `manuscript/simulation_study.R` monolith.

The pipeline contains:

- a versioned scenario registry;
- common scenario and result-schema validation;
- thin analysis-family adapters;
- deterministic screening and full-analysis runners;
- serial and parallel execution with atomic checkpoints and resume support;
- training and held-out validation analysis;
- table, figure, registry, manifest, and audit-report generation.

Adapters expose two operations:

1. `primary_decision(data, scenario)` performs only the inexpensive primary
   analysis used for screening;
2. `robustness_analysis(data, scenario)` calls the corresponding exported
   `stabilitest` function and returns the full result.

Parity tests verify that the screening p-value and conclusion match the public
function's full-data p-value and conclusion for representative datasets. The
calibration operates through public functions rather than duplicating the
robustness score internally.

## Scenario Design

The design avoids an unmanageable full factorial by using three layers:

1. **Core calibration scenarios:** balanced, common settings used to evaluate
   the shared bands.
2. **Stress scenarios:** sparse outcomes, heavy tails, heteroscedasticity,
   separation, high censoring, non-proportional hazards, directional
   contamination, and decision-boundary effects.
3. **Held-out validation scenarios:** parameter combinations and seed ranges
   excluded from threshold selection.

Every engine covers at least three sample-size levels and relevant truth and
conclusion strata. Superiority engines include null, moderate, and large
effects. Equivalence includes effects comfortably inside, near, and outside
the margin. Non-inferiority includes comfortably NI, boundary-adjacent, and
truly inferior effects. Model engines include single-coefficient and multi-df
joint tests.

Family-specific factors include:

- **Welch/rank:** normal and heavy-tailed distributions, equal and unequal
  variances, paired correlation, and directional contamination.
- **Proportions:** baseline risk, risk difference, allocation imbalance, and
  sparse tables.
- **LM/ANCOVA:** prognostic strength, treatment-covariate imbalance,
  heteroscedasticity, missing analysis rows, and multi-level factors.
- **Binomial GLM:** baseline prevalence, odds ratio, covariate strength,
  imbalance, and separation stress.
- **Poisson GLM:** baseline rate, incidence-rate ratio, exposure offsets,
  exposure variability, and overdispersion stress.
- **Cox:** event prevalence, censoring, hazard ratio, baseline hazard shape,
  and proportional-hazards violations.
- **TOST/NI:** mean, risk-difference, and odds-ratio endpoints; equivalence and
  NI; paired and unpaired mean designs; margin size and distance to boundary;
  sparse binary outcomes.

## Two-Stage Sampling

The pipeline first screens generated datasets using only the primary test and
records the screening denominator. It then randomly retains a predefined
number of datasets in each truth-by-conclusion stratum for the expensive
jackknife, removal, and bootstrap analysis.

This produces adequate samples of null false positives without running the
full robustness engine on the roughly 95% of null datasets that do not reject.
Selection is deterministic from the scenario seed ledger. Screening counts are
retained so rejection, equivalence, and NI rates remain estimable.

Core strata start with at least 500 completed full robustness analyses and may
increase toward 1,000 when the predefined Monte Carlo precision target is not
met. Stress strata may use smaller quotas when they are sensitivity analyses
rather than threshold-defining scenarios. Final calibration uses production
settings, primarily `n_boot = 1000`; smoke and pilot runs use smaller values.

## Data Flow and Artifacts

The pipeline proceeds in this order:

1. Validate and freeze the scenario manifest and seed ledger.
2. Screen simulated datasets and record denominators and selection strata.
3. Run selected datasets through the public robustness APIs.
4. Write one atomic checkpoint per scenario and stratum.
5. Validate and combine raw replicate files.
6. Analyze training scenarios and lock the candidate calibration registry.
7. Evaluate the locked policy once on held-out scenarios.
8. Generate publication tables, figures, diagnostics, and audit reports.

Large raw results and checkpoints remain outside version control. Checked-in
artifacts include scenario definitions, seed and run manifests, file hashes,
compact summary tables, figures, calibration registry, statistical analysis
report, Git commit, commands, session information, and package versions.

The replicate schema records scenario and replicate IDs, engine and endpoint,
truth stratum, conclusion, original p-value/effective p-value, component
metrics, score, label, sample size, failure/convergence information, runtime,
and seed identifiers.

## Uncertainty and Failure Handling

- Use Wilson intervals for classification and failure rates.
- Use scenario-clustered bootstrap intervals for pooled operating
  characteristics and fitted cutoffs.
- Define Monte Carlo precision and minimum completed-stratum targets before
  the full run.
- Retain full-fit, subset-fit, convergence, degenerate-bootstrap, and scenario
  failures in an audit table rather than silently dropping them.
- Mark scenarios that cannot meet completion quotas as unsupported and exclude
  them from threshold fitting while retaining them in feasibility reporting.
- Use atomic writes and manifest validation so interrupted runs resume without
  corrupting completed results.
- Use deterministic per-scenario and per-replicate seeds so worker count and
  execution order do not change the selected datasets or results.

## Calibration Registry and Package Policy

After held-out validation, produce a machine-readable registry containing:

- calibration version;
- analysis family and conclusion type;
- supported conditions;
- shared or family-specific cutoffs;
- validation status;
- training and validation manifest hashes;
- operating characteristics and uncertainty summaries.

Package behavior changes only from this frozen registry:

- validated shared bands remain the default where transportability passes;
- validated family-specific bands are selected when required;
- unsupported combinations return the numeric score and components but mark
  categorical interpretation as uncalibrated;
- non-significant results return no categorical band unless the dedicated
  analysis supports a scientifically defensible alternative;
- result objects record calibration version, family, applicability, source,
  and status.

## Vignette Structure

Replace the single overloaded teaching surface with five focused vignettes:

1. **Getting Started: Pain Trial Case Study** — core workflow, components, and
   introductory interpretation.
2. **Two-Sample and Binary Comparisons** — Welch, paired t, Wilcoxon,
   Brunner-Munzel, Fisher, chi-square, and proportion tests.
3. **Regression and Time-to-Event Models** — LM/ANCOVA, binomial and Poisson
   GLM, Cox, single versus multi-df terms, missing rows, weights, convergence,
   and censoring.
4. **Equivalence and Non-Inferiority** — mean, risk-difference, and odds-ratio
   endpoints, margins, direction, effective p-values, and limitations.
5. **Calibration and Interpretation** — score formula, registry, validation
   status, significant versus non-significant conclusions, simulation design,
   thresholds, uncertainty, and reporting guidance.

Each function has one canonical vignette. Expensive calibration results are
read from frozen compact artifacts rather than recomputed during vignette
builds. Cross-links and a calibration-status table connect the vignettes while
avoiding duplicated caveats.

## Testing and Release Workflow

Tests include:

- scenario-manifest and replicate-schema validation;
- adapter primary-decision parity for every engine;
- deterministic seed behavior across worker counts and resume boundaries;
- atomic checkpoint and corruption-recovery behavior;
- failure and convergence accounting;
- tiny end-to-end smoke scenarios for every adapter;
- frozen synthetic fixtures for threshold fitting and validation;
- registry selection and interpretation-applicability behavior;
- vignette build and documentation-link checks;
- full package tests and R CMD check.

Ordinary CI runs only smoke scenarios. The publication-grade run is a manual or
release workflow with retained manifests and hashes because it is too expensive
for every commit.
