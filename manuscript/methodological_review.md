# Methodological Review: Robustness Analysis Framework (January 2026 version)

**Scope:** Critical review of the January 2026 `robustness_analysis.R` and manuscript as of 2026-01-22. Issues are ordered by severity. Each issue states the problem, why it matters, and the resolution implemented in the July 2026 revision (manuscript v2.0).

**Current layout (this repository):** the revised two-sample engine lives in `R/robustness_analysis.R`; model-based extensions (ANCOVA / Cox) in `R/robustness_models.R`; the revised manuscript in `manuscript/robustness_analysis_manuscript.md`. Simulation design for Section 3 is intended as `manuscript/simulation_study.R` (or a package vignette equivalent). The Task 15 review recorded the historical score bands (> 70 robust; (55, 70] moderately robust; ≤ 55 fragile); the active registry now applies them only to an applicable significant Welch result. Other methods retain numeric scores/components with labels suppressed. Case-study bootstrap figures use `n_boot = 2000` and `seed = 14` (package default seed remains 123).

---

## 1. Major methodological issues

### 1.1 Bootstrap "conclusion stability" measures reproducibility, not robustness

The bootstrap component resamples each group with replacement and reports the proportion of resamples reaching the same significance conclusion. Because resampling preserves the observed effect, this proportion is essentially a bootstrap estimate of the *reproducibility probability* — approximately the power of the test at the observed effect size (Goodman, 1992; Shao & Chow, 2002). It is driven primarily by how far p₀ sits from α, not by contamination or influential observations.

Consequences: a perfectly clean dataset with p₀ = 0.04 will show bootstrap "stability" near 50–60% and be penalized in the composite score, while a heavily contaminated dataset with p₀ = 0.0001 will show stability near 100%. The metric therefore conflates *strength of evidence* with *robustness to perturbation* — two distinct concepts the framework claims to separate.

**Resolution:** The metric is retained but relabeled *bootstrap reproducibility probability* and reported as a separate axis of evidence ("would a replicate sample likely reach the same conclusion?"), with the conflation documented in the interpretation guidelines. Its weight in the composite score is reduced (see 1.4) and the manuscript now explains what it does and does not measure. The "95% CI for the p-value" label is corrected to *percentile interval of the bootstrap p-value distribution* — a p-value is not a parameter, and the interval is descriptive, not inferential.

### 1.2 Grand-mean strategic removal does not probe the worst case, and is confounded with true effects

Ranking observations by distance from the *grand mean* has three problems. First, under a genuine treatment effect the most extreme observations are disproportionately the best responders; removing them shrinks the effect estimate toward zero by construction, so a low fragility index can simply reflect a real effect rather than fragility. Second, the procedure preferentially removes observations from whichever group has the larger variance, unbalancing the comparison. Third, distance from the grand mean is not the direction of maximum damage: the removal set that most easily overturns the conclusion is generally *not* the set of grand-mean outliers.

The result is a fragility index that is neither a worst-case bound (it can be far too optimistic) nor a clean outlier-sensitivity measure (it is confounded with effect size).

**Resolution:** The revision adds a *worst-case removal analysis* in the spirit of the Approximate Maximum Influence Perturbation of Broderick, Giordano & Meager (2023): a greedy algorithm that at each step removes the observation whose deletion moves the p-value most strongly toward overturning the conclusion. The resulting *worst-case fragility index* is an upper bound on the size of the smallest overturning subset (the greedy set suffices to flip the conclusion; a smaller one may exist) — in practice a far tighter bound than symmetric outlier trimming — and is used in the composite score. The original grand-mean removal is retained as a descriptive supplementary analysis (relabeled *extreme-value removal*), with its limitations documented.

### 1.3 The extended fragility index is not the Walsh fragility index

The manuscript borrows the term "fragility index" from Walsh et al. (2014), where it counts *event-status flips* in 2×2 tables. Removing observations is a materially different operation: it changes the sample size and discards information rather than perturbing outcomes. Reviewers and regulators familiar with the Walsh FI will be misled if the distinction is not explicit.

**Resolution:** The metric is renamed *removal fragility index* throughout, with an explicit paragraph distinguishing it from the event-flip FI and citing the relevant critique literature (e.g., Potter, 2020, on FI limitations).

### 1.4 The composite score is miscalibrated and its weights are arbitrary

Two scaling problems inflate the score. Because removal is capped at 30% of the sample, the fragility percentage f% cannot exceed ~30, so the term (100 − f%) is bounded below by ~70: the "worst possible" analysis still contributes 21 of a possible 30 points from this component. Jackknife stability has the opposite problem at large n: a single observation carries O(1/n) influence, so leave-one-out stability tends to 100% as n grows regardless of contamination — the component measures less and less as trials get bigger. Averaging three non-commensurate percentages with weights (0.4, 0.3, 0.3) chosen "by expert judgment" then produces a number whose scale (>80 = robust) has no calibration argument behind it.

**Resolution:** (i) The fragility component is rescaled to use the full 0–100 range: 100 × min(k_frag / (k_max + 1), 1), where k_max is the removal cap. (ii) Worst-case fragility (1.2) replaces grand-mean fragility in the score. (iii) Weights become a documented function argument (default 0.4/0.4/0.2, down-weighting the bootstrap per 1.1) so users can pre-specify their own in a SAP. (iv) The manuscript now presents the composite as a *heuristic communication device* and directs primary interpretation to the component metrics. Simulation-based calibration of the thresholds is reported (Section 3 of the revised manuscript).

### 1.5 Simulation results were not backed by code

Section 3 of the manuscript reports simulation results (Type I error, power, robustness score distributions) for which no simulation code exists in the project. The numbers cannot be reproduced or checked.

**Resolution:** A runnable simulation study (`simulation_study.R`, now expected alongside the manuscript under `manuscript/`) generates every number in Section 3; results in the revised manuscript come from an actual run of the identical algorithm, with Monte Carlo precision stated.

---

## 2. Defects in the R implementation

### 2.1 `generate_interpretation()` is broken (would error whenever `interpret = TRUE` fires the fragility branch)

Three defects, confirmed by code inspection:

1. `if (frag_k > max(strategic_p_range))` compares the fragility index (an integer count) against the maximum *p-value* (≤ 1). The condition is almost always true, so the "high robustness" text is selected even for fragile results.
2. That branch then references `max_removal_pct`, which is not an argument of `generate_interpretation()` and is not in scope — R's lexical scoping resolves it in the global environment, producing `object 'max_removal_pct' not found` at run time.
3. The bootstrap paragraph references `bootstrap$results` and `bootstrap$p_ci`, neither of which is passed in nor exists in the function's enclosing environment (and inside `robustness_analysis()`, `bootstrap` is a plain tibble with no `$results` element).

These indicate the `interpret = TRUE` path was never executed end-to-end. **Resolution:** the interpretation generator is rewritten with an explicit argument list and is exercised by the verification runs.

### 2.2 Sign of the reported mean difference

`diff(test$estimate)` returns mean₂ − mean₁, the negative of the conventional group-1-minus-group-2 difference. In the pain-trial case study this silently flips the sign of the treatment effect. **Resolution:** the revision computes `estimate[1] - estimate[2]` and labels the direction explicitly.

### 2.3 Reported p-value at the fragility point

The interpretation text reports `strategic_p_range[2]` (the maximum p over all removal steps) as "the p-value after removing k_frag observations". These generally differ. **Resolution:** the p-value at k = k_frag is extracted and reported.

### 2.4 Case study is not reproducible

The manuscript's case study reports p = 0.0055, t = −2.89, df = 53 (a pooled-variance df, although the code runs Welch's test), while the appendix code simulates *different* data from `rnorm()` and overwrites two values — data which do not yield the reported statistics. **Resolution:** the case-study dataset is now a fixed, listed dataset shipped with the code; every number in Section 4 is computed from it and is exactly reproducible.

### 2.5 Minor points

The coefficient of variation of p-values is retained only as a descriptive quantity — p-values are not on a ratio scale, so the CV has no inferential meaning. The influential-observation threshold |Δp| > 0.05 is arbitrary and position-dependent (a Δp of 0.05 means something entirely different at p₀ = 0.5 than at p₀ = 0.049); the revision adds a significance-flip criterion alongside it. Bootstrap resamples of the Wilcoxon test contain ties by construction, so `exact = FALSE` (normal approximation) is required and now documented.

---

## 3. Framework-level limitations acknowledged but not fixed

Binary significance framing: all conclusion-stability metrics inherit the α = 0.05 dichotomy the manuscript itself criticizes; a p = 0.053 vs p = 0.047 "flip" is a knife-edge distinction. This is intrinsic to the fragility concept and is now stated plainly in the limitations. Independence assumption: unchanged; clustered/longitudinal extensions remain future work. Multiplicity: robustness metrics are descriptive and require no multiplicity adjustment, but this is now stated rather than left implicit.

---

## References added in the revision

Broderick T, Giordano R, Meager R. An Automatic Finite-Sample Robustness Metric: When Can Dropping a Little Data Make a Big Difference? (2023). Goodman SN. A comment on replication, p-values and evidence. Stat Med. 1992;11(7):875–879. Shao J, Chow SC. Reproducibility probability in clinical trials. Stat Med. 2002;21(12):1727–1742. Potter GE. Dismantling the Fragility Index. Stat Med. 2020;39(26):3720–3731.
