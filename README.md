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

A composite 0–100 score summarises the components; interpretation bands were
calibrated by simulation (chance-significant findings under H0 average ~52,
true large effects ~75; see the accompanying manuscript).

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

# ANCOVA term
res_lm <- robustness_lm(change ~ arm + baseline, dat, term = "armActive")

# Binomial logistic GLM term
res_glm <- robustness_glm(y ~ arm + x, dat, term = "armActive",
                          family = binomial())

# Cox proportional hazards term
res_cox <- robustness_surv(survival::Surv(time, event) ~ arm, dat,
                           term = "armActive")
```

## Status

Experimental (v0.3.0). API may change. See [NEWS.md](NEWS.md) for release
notes. Cite with `citation("stabilitest")`. Browse the vignette with
`browseVignettes("stabilitest")` or
`vignette("pain-case-study", package = "stabilitest")`. `NAMESPACE` and `man/`
should be regenerated with `roxygen2::roxygenise()`; run `devtools::check()`
before any submission. Not affiliated with the CRAN packages `robust`,
`robustbase`, or `stabs`.
