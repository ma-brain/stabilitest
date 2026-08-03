# Core Robustness Correctness Design

## Goal

Correct three high-impact failure modes in the robustness metrics: unearned fragility scores when no deletion can be evaluated, model resampling over rows omitted by the fitted model, and crashes caused by non-finite rank-test bootstrap results.

## Removal fragility

The composite score requires at least one valid deletion candidate. Each engine will compute the maximum number of deletions that can be made while retaining its documented minimum analysis size. The configured `max_k` will be capped at that feasible capacity. If the resulting budget is zero, the public call will fail with a clear `insufficient sample for fragility analysis` error.

The removal result will also be checked against the largest successfully evaluated `k`. A no-flip result is right-censored as `max_k + 1` only after every configured, feasible removal step was evaluated. If candidate fitting stops the search before that horizon, the call will fail rather than award a perfect fragility component.

Normal analyses that complete their configured search retain the current result schema and score calculation.

## Model analysis rows

The full `lm`, `glm`, or `coxph` fit defines the analysis population. After that fit, the engine input will be restricted using the fit's `na.action`, so `n`, jackknife rows, removal percentages, and bootstrap samples all refer to observations actually used by the model.

For GLMs with observation weights, the existing private row identifier remains attached before fitting. Subsetting the analysis data therefore preserves the mapping back to the original weight vector, including bootstrap duplicates.

## Non-finite bootstrap results

The two-sample bootstrap will retain all requested attempts and identify finite versus invalid p-values. Reproducibility and p-value summaries will use valid replicates only. The result object will expose valid and failed replicate counts so callers can judge bootstrap reliability. If every replicate is invalid, the call will fail with a clear degeneracy error.

This behavior matches the model engine's successful-fit denominator while making the denominator explicit. Existing successful datasets retain the same bootstrap metric.

## Error handling

- Insufficient legal deletion capacity: fail before expensive resampling.
- Premature removal-search termination: fail rather than manufacture a censored bound.
- No finite bootstrap p-values: fail after the bootstrap with an actionable message.
- Partially invalid bootstrap: return the result with explicit valid/failed counts.

## Testing

Regression tests will cover:

- minimum-size two-sample and model inputs producing the new fragility-capacity error;
- a completed no-flip search retaining the `max_k + 1` censored result;
- incomplete model rows being excluded consistently from `n`, jackknife, and bootstrap;
- weighted GLM row identifiers remaining aligned after complete-case restriction;
- the reproduced tied Wilcoxon bootstrap completing with finite metrics and nonzero failure accounting;
- an all-invalid bootstrap producing a clear error;
- the complete package test suite and a clean source-package `R CMD check`.
