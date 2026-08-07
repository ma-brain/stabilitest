# stabilitest

Robustness and fragility analysis of statistical test conclusions.

`stabilitest` asks a question p-values cannot answer: **how easily could this
conclusion be overturned?** It integrates three complementary sensitivity
analyses into interpretable metrics:

- **Jackknife leave-one-out** — which individual observations drive the result?
- **Worst-case removal** (greedy, in the spirit of the maximum influence
  perturbation of Broderick, Giordano & Meager 2023) — what is the smallest
  set of observations whose removal flips the conclusion? (*removal fragility
  index*)
- **Bootstrap reproducibility probability** (Goodman 1992) — would a replicate
  sample likely reach the same conclusion?

A data-dependent composite score from 0–100 summarises the components. The
numeric score and its component metrics are returned for every supported
method. Categorical interpretation labels are deliberately conservative: the
current release assigns **Robust**, **Moderately Robust**, or **Fragile** only
to an applicable, statistically significant `welch_unpaired` result using the
documented default score definition and weights. Labels are `NA` for
uncalibrated methods or conclusions, while scores and components remain
available for descriptive review. The 55/70 thresholds are retained only as
the narrow Welch calibration; the broader Task 15 simulation is historical
evidence, not a runtime calibration claim.

Calibration is keyed by the resolved method (`welch_unpaired`, `paired_t`,
`wilcoxon_rank_sum`, `wilcoxon_signed_rank`, `brunner_munzel`,
`fisher_exact`, `chi_square_2x2`, `two_sample_prop`, `lm_ancova`,
`lm_ancova_v2`, `glm_binomial`, `glm_poisson`, `cox_ph`, and the three TOST
endpoints). The public `robustness_analysis()` dispatcher and its existing
`test_type` values are unchanged. Gate B for the isolated `lm_ancova` v1 study
closed fail-closed: the registry row remains `uncalibrated`
(`no_feasible_thresholds`; held-out not opened; version `lm-ancova-2026-1`).
Gate B for Track A `lm_ancova_v2` likewise closed fail-closed
(`no_feasible_thresholds`; candidate hash `3dc2a1f840b3eb725bea629dc130f070`;
held-out not opened; version `lm-ancova-v2-2026-1`), so model labels remain
suppressed for both ANCOVA units. Welch 55/70 is a Welch comparator, not an
ANCOVA fallback. Gate A froze the isolated ANCOVA study for eligible significant
canonical 1-df treatment effects with 60%/90% power-defined truth strata.
Multi-df labels remain suppressed and score weights remain frozen. The Track A
`lm_ancova_v2` SAP froze a two-band (Fragile / Not fragile) jackknife-light
protocol (`fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`); training found
no feasible L, so labels stay suppressed. The prospectively frozen
`pain_ancova_trial` dataset is an illustration only and never enters training
or held-out evidence.

A method-specific calibration study for `fisher_exact` (binary-proportion
Phase 1) has validated a two-band Fragile / Not-fragile decision at cutoff
`L = 58` on a fresh held-out grid; its published artifacts live in
`manuscript/calibration/studies/binary_proportion/`. Runtime activation in the
package registry is pending separate review (Gate B); until then, proportion
results retain numeric scores and component metrics with labels suppressed.

## Installation

```r
# from a local checkout
# install.packages(c("dplyr", "purrr", "tibble", "ggplot2"))
devtools::install_local("stabilitest")
```

## Usage

```r
library(stabilitest)

# Two-sample comparison (Welch t-test) — bundled case-study data
res <- robustness_analysis(pain_treatment, pain_placebo,
                           test_type = "t.test", n_boot = 2000,
                           interpret = TRUE)
print(res)
plot(res)

# Rank-based two-sample (Wilcoxon or Brunner–Munzel)
# Effect size is the Hodges–Lehmann location shift (g1 - g2).
# Prefer brunner_munzel when unequal variances are plausible (unpaired only).
res_w <- robustness_analysis(pain_treatment, pain_placebo,
                             test_type = "wilcoxon", n_boot = 500)
res_bm <- robustness_analysis(pain_treatment, pain_placebo,
                              test_type = "brunner_munzel", n_boot = 500)

# Two-group binary proportions (Fisher / chi-square / prop.test)
# Individual-level 0/1 (or logical) outcomes — same jackknife / fragility /
# bootstrap machinery as the continuous API
g1 <- c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0)   # 6/10 responders
g2 <- c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0)   # 2/10 responders
res_prop <- robustness_analysis(g1, g2, test_type = "fisher", n_boot = 500)
# Also: test_type = "chisq" or "prop"; correct = TRUE/FALSE for chisq/prop

# ANCOVA term (single coefficient or multi-df factor term label)
res_lm <- robustness_lm(change ~ arm + baseline, dat, term = "armActive")
# 3-level factor: joint F via drop1(..., test = "F")
# res_lm_joint <- robustness_lm(change ~ arm + baseline, dat, term = "arm")

# GLM term: binomial logit (OR) or Poisson log (IRR); multi-df via term label
res_glm <- robustness_glm(y ~ arm + x, dat, term = "armActive",
                          family = binomial())
# res_pois <- robustness_glm(count ~ arm + offset(log(exptime)), dat,
#                            term = "arm", family = poisson())

# Cox proportional hazards term (single coef or multi-df joint LRT)
res_cox <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                           term = "armActive")
# res_cox_joint <- robustness_surv(survival::Surv(time, event) ~ arm + age, dat,
#                                  term = "arm")

# TOST equivalence / non-inferiority
# endpoint = "mean" (Welch/paired t), "prop" (Wald RD), or "or" (Wald log OR).
# Conclusion is equivalence (both one-sided tests reject) or NI (one-sided
# vs margin); robustness uses p_eff = max(p_lower, p_upper) for TOST so the
# existing jackknife / fragility / bootstrap machinery applies unchanged.
# Score bands are not separately calibrated for equivalence/NI.
set.seed(1)
g1 <- rnorm(40, 0, 1); g2 <- rnorm(40, 0.05, 1)
res_eq <- robustness_tost(g1, g2, type = "equivalence", margin = 0.5,
                          n_boot = 500, seed = 1)
res_ni <- robustness_tost(g1, g2, type = "noninferiority", margin = 0.3,
                          higher_is_better = TRUE, n_boot = 500, seed = 1)

# Binary risk difference / odds ratio (individual-level 0/1 or logical)
set.seed(2)
b1 <- rbinom(60, 1, 0.45); b2 <- rbinom(60, 1, 0.48)
res_rd <- robustness_tost(b1, b2, type = "equivalence", endpoint = "prop",
                          margin = 0.15, n_boot = 200, seed = 2)
res_or <- robustness_tost(b1, b2, type = "noninferiority", endpoint = "or",
                          margin = 1.25, higher_is_better = TRUE,
                          n_boot = 200, seed = 2)
```

## Status

Experimental (v0.5.1). API may change. See [NEWS.md](NEWS.md) for release
notes. Cite with `citation("stabilitest")`. Browse the vignette with
`browseVignettes("stabilitest")` or
`vignette("pain-case-study", package = "stabilitest")`. `NAMESPACE` and `man/`
should be regenerated with `roxygen2::roxygenise()`; run `devtools::check()`
before any submission. Not affiliated with the CRAN packages `robust`,
`robustbase`, or `stabs`.
