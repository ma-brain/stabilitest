---
title: "How Easily Could This Conclusion Be Overturned? A Framework for Robustness and Fragility Analysis of Statistical Tests in Clinical Trials"
short-title: "Robustness and fragility analysis of clinical trial tests"
subtitle: "Version 2.0 — July 2026. Major revision following methodological review. Software: stabilitest R package v0.5.0 (Ally, 2026)."
author:
  - name: Marius Ally
    email: marally@gmail.com
correspondence: "Correspondence: Marius Ally · marally@gmail.com"
date: July 2026
keywords:
  - robustness analysis
  - sensitivity analysis
  - fragility index
  - jackknife
  - bootstrap
  - maximum influence perturbation
  - clinical trials
abstract: |
  **Background:** The reliability of statistical conclusions in clinical trials depends not only on p-values but on the stability of those conclusions under perturbation of the data. Regulatory guidance (ICH E9(R1)) calls for sensitivity analyses, yet standardized, quantitative frameworks for conclusion robustness remain limited.

  **Objective:** To develop, validate, and calibrate a framework for assessing the robustness of statistical test conclusions through combined jackknife, worst-case observation removal, and bootstrap resampling.

  **Methods:** The framework comprises: (1) jackknife leave-one-out analysis identifying influential observations; (2) a *worst-case removal analysis* — greedy adversarial deletion in the spirit of the maximum influence perturbation (Broderick, Giordano & Meager, 2023) — yielding a *removal fragility index*, a greedy upper bound on the size of a minimal overturning subset; (3) bootstrap resampling estimating the *reproducibility probability* (Goodman, 1992). A composite 0–100 score combines the components with pre-specifiable weights (default 0.4/0.4/0.2). The Task 15 simulation study is retained as historical evidence for the original Welch calibration; it does not transfer interpretation bands to other method families. The framework extends to linear-model (ANCOVA), GLM, and Cox terms via a common case-deletion engine; the accompanying software (v0.5.0) also supports proportion tests, Brunner–Munzel, and TOST equivalence/non-inferiority.

  **Results:** Type I error was preserved in clean data (0.046–0.052) and inflated by contamination (up to 0.092), which the historical Task 15 simulation correctly flagged. Conditional on statistical significance, chance findings under the null averaged a robustness score of 52 with a median worst-case fragility of 1–2 observations (2% of the sample), whereas true large effects (d = 0.8, n = 50) averaged 75 with median fragility of 12–14 observations. Grand-mean-ranked removal — the usual "remove the outliers" heuristic — overstated robustness dramatically (median flipping set 13–31 observations vs 2–6 under adversarial removal in the same data). These 55/70 bands are retained only for the narrowly documented significant Welch configuration; labels are suppressed for uncalibrated methods and conclusions.

  **Conclusions:** The framework provides transparent, quantitative robustness assessment suitable for regulatory submissions, operationalizing ICH E9(R1) sensitivity-analysis principles with numeric component metrics available across methods. Categorical interpretation is intentionally limited to the applicable significant Welch configuration until independent method-specific calibration is completed.
papersize: a4
fontsize: 10pt
page-numbering: "1"
---

## 1. Introduction

### 1.1 Background

Statistical hypothesis testing in clinical trials typically reduces evidence to a binary decision at p < α. This framework conveys nothing about the *stability* of the decision: a p-value of 0.048 obtained because of a single extreme responder and a p-value of 0.048 that survives any plausible perturbation of the data are treated identically. The ICH E9(R1) addendum makes robustness to assumptions and data anomalies a regulatory expectation, but provides no specific quantitative machinery for it.

Three questions, each requiring a different tool, jointly characterize the stability of a test conclusion:

1. *Which individual observations drive the result?* (influence)
2. *What is the smallest set of observations whose removal would overturn the conclusion?* (fragility)
3. *Would a replicate sample likely reach the same conclusion?* (reproducibility)

### 1.2 Relation to existing approaches

The fragility index of Walsh et al. (2014) counts outcome-event flips in 2×2 tables; it does not apply to continuous endpoints and has known limitations (Potter, 2020). Classical influence diagnostics (Cook, 1977; Belsley et al., 1980) quantify influence on *estimates*, not on *conclusions*. Broderick, Giordano and Meager (2023) formalized the question of the smallest data subset whose removal reverses a conclusion (the maximum influence perturbation) and showed that in prominent published analyses removing under 1% of observations can flip signs of estimated effects. Reproducibility probability was developed by Goodman (1992) and Shao & Chow (2002). The contribution of the present framework is the integration of all three perspectives into a single pre-specifiable analysis with clinically interpretable outputs.

Terminology: throughout, *removal fragility index* denotes the number of removed observations at which the conclusion first changes. It is related to but distinct from the Walsh fragility index, which flips event status without changing n.

### 1.3 What changed in version 2

The January 2026 version had four methodological weaknesses, documented in the accompanying review: its bootstrap "stability" metric conflated strength of evidence with robustness; its removal analysis ranked observations by distance from the grand mean, which under a genuine treatment effect preferentially removes true responders and is far from the worst case; its composite score was mis-scaled (the fragility term could not fall below 70); and its interpretation thresholds were uncalibrated. Version 2 addresses each point and validates the result by simulation.

### 1.4 Current calibration policy

The runtime registry is keyed by the resolved method, endpoint, and observed
conclusion (`welch_unpaired`, `paired_t`, `fisher_exact`, `lm_ancova`, and so
on); it does not use the generic `two_sample` identity. Every analysis returns
the numeric composite score and component metrics. Categorical labels are
suppressed (`NA`) unless the result is a significant, applicable
`welch_unpaired` analysis using the documented default score definition,
weights, and independent-groups conditions. The public
`robustness_analysis()` dispatcher and its existing test choices are unchanged.
Task 15's broad-family tables are archived as historical evidence under
`manuscript/calibration/published/`; they are not active calibration inputs.
The next independent calibration target is `lm_ancova`.

---

## 2. Methods

### 2.1 Framework overview

Given two samples (or a model term; Section 2.6) and a test with significance level α, four analyses are performed:

**Component 1 — Jackknife (leave-one-out).** Each observation is deleted in turn and the test repeated (n tests), following the classical leave-one-out framework (Shao & Tu, 1995). Outputs: conclusion stability S_jack (% of deletions preserving the conclusion), the set of influential observations (deletion flips the conclusion or shifts p by more than δ = 0.05), and the leave-one-out p-value range. Because single-deletion influence shrinks as ~1/n, high jackknife stability in large samples is *weak* evidence of robustness; the component's chief value is identifying specific observations for data-quality review.

**Component 2 — Worst-case removal (primary fragility analysis).** A greedy algorithm deletes, at each step, the observation whose removal moves the p-value furthest toward overturning the conclusion, up to k_max = ⌊0.30 n⌋ deletions. The *worst-case removal fragility index* k_frag is the step at which the conclusion first flips (k_max + 1 if it never does). The greedy set is *sufficient* to overturn the conclusion; a smaller set may exist, so k_frag is an upper bound on the size of the minimal overturning subset — in practice a much tighter bound than any symmetric outlier-trimming rule provides.

**Component 3 — Extreme-value removal (descriptive).** The v1 procedure — cumulative deletion of observations most distant from the (recomputed) grand mean — is retained for continuity and because the *gap* between its fragility index and the worst-case index is itself informative: it measures how much protection the "obvious outliers" narrative provides relative to an adversarial reading of the data. It does not enter the composite score.

**Component 4 — Bootstrap reproducibility.** B resamples (default 1000) drawn with replacement within groups (Efron & Tibshirani, 1993); S_boot is the proportion reaching the original conclusion. Because resampling preserves the observed effect, S_boot estimates the *reproducibility probability* — approximately the power at the observed effect size — and reflects strength of evidence, not resistance to contamination. A marginal p-value yields S_boot near 50–60% even in perfectly clean data. It is reported with that explicit framing, together with the mean, SD, and 2.5th–97.5th percentile interval of the bootstrap p-value distribution (a descriptive interval; p-values are not parameters and this is not a confidence interval).

### 2.2 Composite robustness score

R = w₁·S_jack + w₂·F + w₃·S_boot, where F = 100·min(k_frag/(k_max+1), 1) rescales the worst-case fragility index to the full 0–100 range (in v1 this term was bounded below by ~70, inflating all scores). Default weights w = (0.4, 0.4, 0.2): influence and fragility carry equal weight; the reproducibility term is down-weighted because it largely restates the p-value. Weights are an explicit argument of the software and should be pre-specified in the SAP.

### 2.3 Interpretation policy and active Welch calibration

Scores and component metrics are descriptive outputs for every supported
method. The only active categorical mapping is the significant
`welch_unpaired` row with the documented default score definition and weights:
score > 70 is **Robust**, (55, 70] is **Moderately Robust**, and ≤ 55 is
**Fragile**. Exact 70 is moderately robust; exact 55 is fragile. The mapping
does not apply to paired t, rank, binary, model, Cox, or TOST methods, to
non-significant results, or to custom weights/unsupported conditions; those
results retain scores and components but have a suppressed (`NA`) label.

### 2.4 Statistical implementation

Implemented in the `stabilitest` R package (v0.5.0; this repository). `robustness_analysis()` covers two-sample location tests (Welch and paired t; Wilcoxon rank-sum/signed-rank; Brunner–Munzel) and two-group binary proportion tests (`fisher`, `chisq`, `prop`) without changing the public dispatcher. Rank-based location analyses report the Hodges–Lehmann shift as the effect summary. Model engines: `robustness_lm()` (linear/ANCOVA), `robustness_glm()` (binomial logit / Poisson log), and `robustness_surv()` (Cox); each accepts a single coefficient or a multi-df factor term tested jointly (F for lm; LRT for glm/surv). Equivalence and non-inferiority use `robustness_tost()` (TOST / one-sided margin tests) for mean, risk-difference, and odds-ratio endpoints. Numeric scores and components are retained across all these engines; categorical labels are suppressed until each exact calibration unit is validated. Computational cost is dominated by the greedy search: O(n·k_max) test evaluations, about 3,400 tests for n = 55 — under a second for closed-form tests, minutes for model refits at n of a few hundred.

### 2.5 Application context

Primary use: two-arm comparisons of continuous endpoints, n ≈ 20–200 per group, Phase I–III — the setting in which sensitivity of conclusions to individual patients is a recurring concern in drug development and trial reporting (Senn, 2007; Pocock, McMurray & Collier, 2015). The row-deletion engine (Section 2.6) extends the same logic to covariate-adjusted analyses. Observations are assumed independent; clustered and longitudinal extensions are future work (Section 5).

### 2.6 Model-based extensions

For ANCOVA (`change ~ arm + baseline`), GLM (`y ~ arm + covariates`; binomial logit or Poisson log), and Cox regression (`Surv(time, event) ~ arm + covariates`), all components operate on *rows* (subjects) of the analysis dataset, with the conclusion defined by the p-value of a pre-specified model term — a single coefficient, or a multi-df factor term via a joint test (`drop1` F for lm; LRT for glm/surv). For Cox with a single binary arm the score test is asymptotically the log-rank test. Deleting whole subjects — with their covariates and follow-up — preserves the adjustment structure and handles censoring naturally. Model fits that fail after deletion (e.g., a factor level vanishing) are skipped and reported.

---

## 3. Task 15 historical simulation evidence (inactive)

The following simulation and case-study tables are retained to document the
original broad-family experiment. They are historical evidence for the narrow
Welch calibration only and must not be read as active calibration for other
methods. The current runtime registry and label policy are defined in Section
2.3 and the package calibration registry.

### 3.1 Design

Full factorial: effect size d ∈ {0, 0.5, 0.8} (data N(0,1) vs N(d,1)); n ∈ {25, 50} per group; contamination ∈ {none, 2 outliers at +4 SD injected into the treated group, inflating the apparent effect}. 500 replications per scenario; Welch t-test at α = 0.05; B = 200; k_max = 30% of the pooled sample; default weights. Monte Carlo SE ≤ 0.022 for proportions. (Runtime note: `simulation_study.R` reproduces the design; results below were generated with an algorithmically identical implementation of the same components — differences across RNGs are within Monte Carlo error.)

### 3.2 Rejection rates and overall score (all replications)

| d | n/group | Outliers | Rejection rate | Mean score (SD) |
|---|---|---|---|---|
| 0 | 25 | 0 | 0.052 | 67.7 (8.6) |
| 0 | 25 | 2 | 0.092 | 65.6 (10.8) |
| 0 | 50 | 0 | 0.046 | 64.2 (6.7) |
| 0 | 50 | 2 | 0.068 | 63.4 (6.7) |
| 0.5 | 25 | 0 | 0.424 | 61.5 (11.3) |
| 0.5 | 25 | 2 | 0.638 | 61.5 (11.7) |
| 0.5 | 50 | 0 | 0.728 | 60.9 (10.7) |
| 0.5 | 50 | 2 | 0.844 | 63.3 (10.5) |
| 0.8 | 25 | 0 | 0.774 | 67.8 (14.0) |
| 0.8 | 25 | 2 | 0.912 | 71.4 (13.3) |
| 0.8 | 50 | 0 | 0.976 | 74.9 (10.6) |
| 0.8 | 50 | 2 | 0.988 | 77.0 (10.1) |

Type I error is preserved without contamination and inflated by directional outliers (0.092 at n = 25), exactly the situation in which a robustness assessment is needed.

### 3.3 Calibration conditional on significance (Table 2)

Among *significant* replications:

| d | n/group | Outliers | Mean score | Median k_frag (worst-case) | Median fragility % | Median k (extreme-value) | S_jack | S_boot |
|---|---|---|---|---|---|---|---|---|
| 0 | 25 | 0 | 52.2 | 1 | 2.0 | — | 84.5 | 65.5 |
| 0 | 25 | 2 | 51.6 | 1 | 2.0 | — | 84.3 | 65.6 |
| 0 | 50 | 0 | 51.6 | 2 | 2.0 | — | 87.2 | 64.2 |
| 0 | 50 | 2 | 54.8 | 2 | 2.0 | — | 93.9 | 68.2 |
| 0.5 | 25 | 0 | 62.7 | 3 | 6.0 | 13 | 94.7 | 75.9 |
| 0.5 | 25 | 2 | 63.2 | 3 | 6.0 | 7 | 95.3 | 78.1 |
| 0.5 | 50 | 0 | 63.1 | 5 | 5.0 | 14 | 97.3 | 80.1 |
| 0.5 | 50 | 2 | 65.3 | 6 | 6.0 | 12 | 98.7 | 84.2 |
| 0.8 | 25 | 0 | 70.8 | 5 | 10.0 | 10 | 97.5 | 84.5 |
| 0.8 | 25 | 2 | 73.0 | 6 | 12.0 | 8 | 98.5 | 88.2 |
| 0.8 | 50 | 0 | 75.4 | 12 | 12.0 | 26 | 99.7 | 93.9 |
| 0.8 | 50 | 2 | 77.3 | 14 | 14.0 | 28 | 99.8 | 96.0 |

Three findings. **First, the score separates chance findings from real effects.** False positives under the null average 52; true large effects average 71–77. The calibrated bands in Section 2.3 follow directly. **Second, a significant result that took only 1–2 adversarial removals to overturn is the signature of a chance finding** — median worst-case fragility of false positives was 2% of the sample, versus 10–14% for large true effects. This mirrors, at trial scale, the finding of Broderick et al. that fragile conclusions can hinge on a fraction of a percent of observations. **Third, symmetric outlier-trimming grossly overstates robustness**: median flipping sets under grand-mean removal were 2–5 times larger than under adversarial removal in the same data (e.g., 14 vs 5 at d = 0.5, n = 50). A result that "survives outlier removal" may still be one clever deletion away from reversal.

Jackknife stability exceeded 84% everywhere and 97% for all n = 50 scenarios with true effects, confirming that leave-one-out stability saturates with n and cannot serve as the primary fragility measure (Section 2.1).

---

## 4. Task 15 historical case example: Phase II Analgesic Trial (inactive)

This case study predates the method-specific registry. Its Welch result remains
useful as a reproducible historical example, but the broad-family interpretation
claims below are not runtime evidence for other methods.

### 4.1 Study and primary analysis

Randomized Phase II trial of a novel analgesic vs placebo in chronic pain; primary endpoint: change from baseline in pain score (0–100) at Week 12. The analysis dataset is shipped with the software (`pain_treatment`, n = 28; `pain_placebo`, n = 27) so that every number below is exactly reproducible (bootstrap component: B = 2000, seed = 14; see Appendix A).

Treatment: mean change −19.80 (SD 13.80). Placebo: −8.40 (SD 11.88). Welch t-test: difference −11.40 (95% CI −18.35 to −4.44), t = −3.286, df = 52.3, **p = 0.0018**. The treatment group includes one extreme responder (−52.0, subject 14); the placebo group includes a small-magnitude positive change (+3.0, subject 22), distinct from a larger placebo worsening (+23.0, subject 16).

### 4.2 Robustness analysis

**Overall score: 72.5/100 — Robust** (calibrated band > 70), with weights 0.4/0.4/0.2, B = 2000, and seed = 14.

*Jackknife:* 100% conclusion stability across all 55 leave-one-out tests; p-value range 0.0006–0.0034; no observation met the influence criterion. As anticipated by the simulation, this near-perfect stability mostly reflects n = 55, not invulnerability.

*Worst-case removal:* fragility index **k_frag = 6** (10.9% of the sample). Greedy trajectory: p = 0.0018 → 0.0034 → 0.0062 → 0.0114 → 0.0201 → 0.0349 → **0.0602** after the sixth deletion. Six specific patients — led by the extreme responder — jointly carry the conclusion; their removal is sufficient (though not necessarily minimal) to lose significance. For context, the simulated median for true large effects at this sample size is 5–6, versus 1–2 for chance findings.

*Extreme-value removal (descriptive):* also flips at k = 6 in this dataset — here the grand-mean extremes coincide with the most damaging observations, which is not guaranteed in general (Section 3.3).

*Bootstrap:* reproducibility probability ≈ 92% (B = 2000, seed = 14; mean bootstrap p = 0.019, SD 0.065; percentile interval < 0.001 to 0.175). (Bootstrap figures vary by ±~1 point across RNG streams; all other numbers above are deterministic.)

### 4.3 Interpretation and reporting

The result is classified robust: strong primary evidence (p = 0.0018), perfect leave-one-out stability, worst-case fragility comparable to simulated true large effects, and high reproducibility. Two actions are still warranted for the CSR: clinical review of subject 14 (protocol adherence, concomitant medication) since this patient heads the worst-case removal set; and a supplementary rank-based analysis, which is less leveraged by extreme responders.

Suggested reporting text (Results): "Robustness analysis (stabilitest v0.5.0; B = 2000, seed = 14) yielded an overall score of 72.5/100 (robust; calibrated bands from simulation). All 55 leave-one-out analyses preserved statistical significance (p ≤ 0.0034). Worst-case removal analysis identified a set of 6 patients (10.9% of the sample) whose exclusion would raise the p-value to 0.060; the corresponding median for chance-significant findings in simulation is 1–2 patients. Bootstrap reproducibility probability was 92%."

---

## 5. Discussion

### 5.1 Principal findings

The framework separates three questions usually blurred together — who drives the result, how small a deletion overturns it, and would it replicate — and, in the historical Task 15 Welch simulation, the summary score had empirical anchors: chance-significant findings scored ~52 and true large effects ~75. Those anchors are not transferable to other method-specific units, which retain numeric scores and components with categorical labels suppressed until independently calibrated. The starkest practical lesson from the simulation is the gap between adversarial and symmetric removal: robustness claims based on "we removed the outliers and the result held" can overstate stability several-fold.

### 5.2 Interpretation guidance

For an applicable, significant `welch_unpaired` result using the documented default configuration, score > 70 supports confirmatory-grade reporting; (55, 70] calls for transparent component reporting, clinical review of the worst-case removal set, and a rank-based supplementary analysis; ≤ 55 indicates the profile typical of chance findings — treat as hypothesis-generating. Other methods and conclusions retain numeric scores and components but have suppressed categorical labels until independently calibrated. Worst-case fragility below ~5% of the sample should always trigger review of the removed subjects, whatever the score. For non-significant results, use the components descriptively; the score bands do not apply (Section 2.3).

### 5.3 Regulatory alignment

The framework operationalizes ICH guidance on conclusion robustness. ICH E9 states that it is important to evaluate "the robustness of the results and primary conclusions of the trial," defining robustness as "the sensitivity of the overall conclusions to various limitations of the data, assumptions, and analytic approaches to data analysis." The E9(R1) addendum elaborates this under sensitivity analysis: inferences for an estimand should be robust to data limitations and deviations from the main estimator's assumptions, assessed through pre-specified sensitivity analyses. Accordingly, pre-specify components and weights in the SAP (template, Appendix B), and pre-specify the 55/70 bands only when the primary analysis is the applicable significant Welch configuration; for other methods, report numeric scores/components and the suppressed label. Report component metrics in the SAR and give clinical context for the worst-case removal set in the CSR. Because all metrics are descriptive functions of one pre-specified primary analysis, no multiplicity adjustment is implied.

### 5.4 Limitations

(1) The binary significance framing is inherited from the fragility concept itself; a flip from p = 0.047 to 0.053 is a knife-edge event, and the p-value trajectory should always accompany the index. (2) The greedy fragility index is an upper bound on the minimal overturning subset; exact minimal sets are combinatorially hard, though greedy search is near-optimal for monotone single-deletion influence. (3) Adversarial removal answers "could the data support the opposite conclusion?", which is deliberately pessimistic; it should complement, not replace, assumption-based sensitivity analyses (missing data, model form). (4) Independence of observations is assumed. (5) Calibration is retained only for the Welch test under normality with directional contamination. Bands for model-based, proportion, rank, Brunner–Munzel, Cox, and TOST engines are not transferable and their labels remain suppressed pending independent calibration. (6) Weight choice remains a convention, now explicit and pre-specifiable; each method-specific calibration must be earned independently.

### 5.5 Future work

 Independent calibration for `lm_ancova` (the next priority), followed by GLM/Cox, proportion, rank, and TOST units; clustered and longitudinal data; exact or certified bounds on minimal overturning subsets (integer-programming formulations); Bayesian analogues (prior-sensitivity and posterior-probability fragility); CDISC ADaM integration; interactive reporting.

---

## 6. Conclusions

Statistical significance answers "is there an effect?" Robustness analysis answers "how much of the data does that answer rest on?" The revised framework quantifies both fragility (worst-case removal) and reproducibility (bootstrap), identifies the specific patients who carry the conclusion, and retains numeric scores/components across methods. The historical Welch calibration distinguishes chance findings from true effects only for its applicable significant configuration; other methods have suppressed labels pending independent calibration. We recommend pre-specified application to primary endpoints of confirmatory trials, with the component metrics — not the composite alone — as the substance of reporting.

---

## References

Ally M. *stabilitest: Robustness and Fragility Analysis of Statistical Test Conclusions*. R package version 0.5.0; 2026. Available from: https://github.com/ma-brain/stabilitest

Belsley DA, Kuh E, Welsch RE. *Regression Diagnostics: Identifying Influential Data and Sources of Collinearity*. John Wiley & Sons; 1980.

Broderick T, Giordano R, Meager R. An Automatic Finite-Sample Robustness Metric: When Can Dropping a Little Data Make a Big Difference? arXiv:2011.14999; 2023. doi:10.48550/arXiv.2011.14999. Revised versions also appear as Giordano, Meager & Broderick, *Phil. Trans. R. Soc. A*. 2026;384(2321), Parts I–II (doi:10.1098/rsta.2025.0001; doi:10.1098/rsta.2024.0614).

Cook RD. Detection of influential observation in linear regression. *Technometrics*. 1977;19(1):15–18. doi:10.1080/00401706.1977.10489493.

Efron B, Tibshirani RJ. *An Introduction to the Bootstrap*. Chapman & Hall/CRC; 1993.

Goodman SN. A comment on replication, p-values and evidence. *Stat Med*. 1992;11(7):875–879. doi:10.1002/sim.4780110705.

International Council for Harmonisation. ICH E9: Statistical Principles for Clinical Trials. 1998. Available from: https://database.ich.org/sites/default/files/E9_Guideline.pdf

International Council for Harmonisation. ICH E9(R1): Addendum on Estimands and Sensitivity Analysis in Clinical Trials. 2019. Available from: https://database.ich.org/sites/default/files/E9-R1_Step4_Guideline_2019_1203.pdf

Pocock SJ, McMurray JJV, Collier TJ. Statistical controversies in reporting of clinical trials. *J Am Coll Cardiol*. 2015;66(23):2648–2662. doi:10.1016/j.jacc.2015.10.023.

Potter GE. Dismantling the Fragility Index: a demonstration of statistical reasoning. *Stat Med*. 2020;39(26):3720–3731. doi:10.1002/sim.8689.

Senn S. *Statistical Issues in Drug Development*. 2nd ed. John Wiley & Sons; 2007.

Shao J, Chow SC. Reproducibility probability in clinical trials. *Stat Med*. 2002;21(12):1727–1742. doi:10.1002/sim.1177.

Shao J, Tu D. *The Jackknife and Bootstrap*. Springer; 1995.

Walsh M, Srinathan SK, McAuley DF, et al. The statistical significance of randomized controlled trial results is frequently fragile: a case for a Fragility Index. *J Clin Epidemiol*. 2014;67(6):622–628. doi:10.1016/j.jclinepi.2013.10.019.

---

## Appendix A: Software

Complete implementation in this repository (package v0.5.0): `robustness_analysis.R` (two-sample framework — including proportion tests, Brunner–Munzel, and Hodges–Lehmann reporting — and case-study data), `robustness_models.R` (lm/ANCOVA, GLM, Cox; multi-df joint tests), `robustness_tost.R` (equivalence / non-inferiority for mean, prop, and OR endpoints), `simulation_study.R` (Section 3 design), and the `stabilitest/` R package (functions, bundled case-study data, unit tests). The Section 4 case-study numbers (overall score ≈ 72.5; bootstrap reproducibility ≈ 92%) use `n_boot = 2000` and `seed = 14` (the package default seed is 123 and yields a slightly different bootstrap component). Reproduce the analysis by:

```r
library(stabilitest)
res <- robustness_analysis(pain_treatment, pain_placebo,
                           test_type = "t.test", n_boot = 2000,
                           seed = 14, interpret = TRUE)
print(res)
plot(res)
# Also: test_type = "wilcoxon" | "brunner_munzel" | "fisher" | "chisq" | "prop"
# Models: robustness_lm(), robustness_glm(), robustness_surv()
# Equivalence / NI: robustness_tost(..., endpoint = "mean" | "prop" | "or")
```

To reproduce the Section 3 simulation against the exact checkout containing
the manuscript, install the development packages `pkgload` and `tidyverse`,
then run the script from any working directory:

```sh
Rscript /path/to/stabilitest/manuscript/simulation_study.R
```

The script resolves the repository from its own location and loads that
checkout with `pkgload`; it does not use a potentially stale installed copy of
`stabilitest`.

## Appendix B: SAP/SAR template (updated)

**SAP — Sensitivity and Robustness Analyses.** Robustness of the primary efficacy analysis will be assessed with the stabilitest framework: (1) jackknife leave-one-out analysis (influence criterion: significance flip or |Δp| > 0.05); (2) worst-case greedy removal up to 30% of the sample, yielding the removal fragility index; (3) bootstrap resampling (B = [1000] iterations) estimating the reproducibility probability. The numeric composite score and component metrics will be reported for every method. Categorical bands (> 70 robust; (55, 70] moderately robust; ≤ 55 fragile) will be reported only for an applicable significant Welch result under the documented default configuration; labels will be suppressed for uncalibrated methods and conclusions. If the worst-case fragility index is below 5% of the sample, the subjects in the removal set will be reviewed for data quality and a rank-based supplementary analysis performed.

**SAR — Results skeleton.** Overall score [XX]/100 ([band]). Jackknife: [XX]% stability; influential subjects [IDs]; leave-one-out p-range [[X], [X]]. Worst-case removal: fragility index [k] ([X]% of sample); p-value trajectory [Table/Figure]; removed subjects [IDs]. Extreme-value removal (descriptive): index [k]. Bootstrap: reproducibility [XX]% (B = [X]); bootstrap p mean [X], percentile interval [[X], [X]]. Clinical review note: [context for removal-set subjects].

*End of manuscript.*
