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
- **Bootstrap same-decision rate** (Goodman 1992) — how often do resamples
  drawn from the observed data preserve the original significance decision?
  This is an empirical plug-in quantity, not the probability that a new trial
  will replicate the finding.

A data-dependent composite score from 0–100 summarises the components. The
numeric score and its component metrics are returned for every supported
method.

For analysis types without validated cutoffs, stabilitest reports the numeric
score and all stress-test details but leaves the categorical label blank. A
blank label does not mean the analysis failed or that the score is missing. It
means there is not enough calibration evidence to call that score **Robust**,
**Moderately robust**, **Fragile**, or **Not fragile**. This deliberate
fail-closed behavior avoids presenting an unvalidated reference range as a
clinical interpretation.

The current release prints a categorical label only for an applicable,
statistically significant Fisher exact test. Under its validated profile and
explicit weights (`fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`), a
score of 58 or below is **Fragile** and a score above 58 is **Not fragile**.
There is no Robust tier for Fisher. Default score weights and results outside
the validated profile remain numeric-only. See the
[`fisher_exact` calibration study](manuscript/calibration/studies/binary_proportion/)
for the exact eligibility rules and validation results.

Calibration is keyed by the resolved method (`welch_unpaired`, `paired_t`,
`wilcoxon_rank_sum`, `wilcoxon_signed_rank`, `brunner_munzel`,
`fisher_exact`, `chi_square_2x2`, `two_sample_prop`, `lm_ancova`,
`lm_ancova_v2`, `glm_binomial`, `glm_poisson`, `cox_ph`, and the three TOST
endpoints). The public `robustness_analysis()` dispatcher and its existing
`test_type` values are unchanged.

The prospective Welch study (`welch-2026-2`) completed 4,500 significant
training analyses. It could not find cutoffs that were both safe against false
reassurance and useful for identifying clear effects. The historical 55/70
rule would have falsely reassured 45.8% of null results and identified only
49.9% of clear-effect results in training. Because no candidate met the frozen
requirements, the study stopped and the held-out validation data were not
opened. Welch results therefore retain their numeric scores and stress-test
details but have no categorical label. The
[`welch_unpaired` study archive](manuscript/calibration/studies/welch_unpaired/)
contains the protocol, exact audit identifiers, and published negative result.

Two prospective ANCOVA calibration attempts also found no safe and useful
categorical cutoff, and a later study did not confirm that the score detected
assumption violations better than the p-value alone. ANCOVA results therefore
remain numeric-only. The `pain_ancova_trial` dataset is an illustration and was
never used as calibration evidence. See the
[`lm_ancova` study archive](manuscript/calibration/studies/lm_ancova/) and
[`lm_ancova_v2` study archive](manuscript/calibration/studies/lm_ancova_v2/)
for the full protocols and audit record.

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

Experimental (v0.6.0). API may change. See [NEWS.md](NEWS.md) for release
notes. Cite with `citation("stabilitest")`. Browse the vignette with
`browseVignettes("stabilitest")` or
`vignette("pain-case-study", package = "stabilitest")`. `NAMESPACE` and `man/`
should be regenerated with `roxygen2::roxygenise()`; run `devtools::check()`
before any submission. Not affiliated with the CRAN packages `robust`,
`robustbase`, or `stabs`.
