# Related R packages

Source: Task 4 of `docs/plans/2026-08-06-r-journal-submission-plan.md`. Every
claim below was checked against the package's live CRAN page (description,
version, publication date) or, for GitHub-only software, its repository
README, on 2026-08-07. No package is cited without that check; none was
dropped.

## Comparison table

| Package | CRAN status (checked 2026-08-07) | Question it answers | Endpoint / model types | How `stabilitest` differs |
| --- | --- | --- | --- | --- |
| **fragility** | On CRAN, v1.6.1 (2025-01-23) | What is the smallest number of outcome-status flips (e.g., non-event → event) that reverses statistical significance, for a single study or a meta-analysis? | Binary outcomes only; individual studies, pairwise and network meta-analysis | `fragility` only flips *outcome labels*, only for binary endpoints, and reports one number (the fragility index/quotient). `stabilitest` additionally deletes *observations* (with their full covariate/censoring structure) for continuous, binary, model-based (ANCOVA/GLM/Cox), and equivalence endpoints; adds a jackknife stability view and a bootstrap reproducibility probability; and — where calibrated — reports a validated Fragile/Robust label rather than a raw count. |
| **fragilityindex** | **Archived** on CRAN 2020-07-08 ("check problems were not corrected in time"); source still installable from the CRAN Archive or from GitHub (`kippjohnson/fragilityindex`) | Same fragility-index question as `fragility`, for binary-outcome RCTs (the original Walsh et al. 2014 clinical definition) | Binary outcomes, individual RCTs | Predates and is superseded on CRAN by `fragility`; not a maintained comparator. Listed here for completeness because it is the package most frequently cited in the clinical fragility-index literature `stabilitest`'s worst-case-removal component is often compared against. |
| **sensemakr** | On CRAN, v0.1.6 (2024-07-22) | How large would an unmeasured confounder have to be (in partial-R² terms) to explain away, or meaningfully change, a regression coefficient? | Linear regression coefficients; omitted-variable bias (Cinelli & Hazlett 2020) | Answers a *confounding* question (an unmeasured variable you never observed), not a *data-perturbation* question (what happens if specific observed rows are removed or resampled). The two are complementary, not competing: a result can be simultaneously insensitive to plausible confounding (`sensemakr`) and fragile to a handful of observed patients (`stabilitest`), or vice versa. |
| **konfound** | On CRAN, v1.0.3 (2025-05-28) | How much bias (correlated omitted variable, or replaced/discrepant cases) would it take to invalidate an inference, for a user's own fitted model, a single published statistic, or a set of published studies (`konfound()`, `pkonfound()`, `mkonfound()`)? | General regression-style inferences (mostly education/social-science applications); works from a fitted model object or from reported summary statistics | Overlaps most closely with `stabilitest`'s worst-case-removal idea via its "percent of cases that would need to be replaced" statement, but that number comes from a closed-form bias formula (Frank 2000; Frank et al. 2013), not from actually refitting the model after removing specific, named observations. `stabilitest`'s greedy removal is empirical and identifies *which* patients matter; it also adds jackknife and bootstrap views the Frank-style formula does not provide, and is restricted to the two-sample/model-term designs common in trial reporting rather than arbitrary regression inferences. |
| **EValue** | On CRAN, v4.1.4 (2025-08-28) | How strong would unmeasured confounding, selection bias, or measurement error have to be to explain away an observed association? | Observational-study effect estimates (risk ratios, odds ratios, etc.); single studies or meta-analyses | Like `sensemakr` and `konfound`, this is a *bias-magnitude* sensitivity analysis for unobserved threats, computed analytically from the point estimate and its CI. It says nothing about whether the *observed* sample already contains an unusually influential handful of patients — the question `stabilitest`'s jackknife and worst-case components target. |
| **boot** | On CRAN (recommended-adjacent, ubiquitous bootstrap infrastructure) | How do I bootstrap-resample an arbitrary statistic and get standard errors / CIs? | General-purpose; any user-supplied statistic function | `boot` is a general resampling *engine*, not a robustness framework — it has no notion of a trial conclusion, a fragility index, or a calibrated verdict. `stabilitest`'s bootstrap reproducibility component is a purpose-built application of the same idea (resample, recompute the *conclusion*, not just the estimate) to a fixed catalogue of trial-relevant tests, wired into the composite score. |
| **influence.ME** | On CRAN | Which grouping units (not individual rows) are influential in a fitted `lme4` generalized linear/linear mixed-effects model, via a one-group-deleted refit and Cook's-distance-style diagnostics? | Mixed-effects models fit with `lme4`; influence is assessed at the level of a random-effects grouping factor (e.g., site, cluster), not the individual observation | Nearest in spirit to `stabilitest`'s jackknife component (leave-one-unit-out, refit, compare), but scoped to mixed models and to *grouping factors* rather than patients, with no worst-case/adversarial search, no bootstrap reproducibility view, and no composite score or calibrated verdict. `stabilitest` does not currently support mixed-effects models; the two packages are complementary for a clustered trial. |
| **car** | On CRAN, actively maintained (companion to Fox & Weisberg, *An R Companion to Applied Regression*) | Standard regression diagnostics: which observations are outliers, high-leverage, or influential on a fitted `lm`/`glm`, via `influencePlot()`, `infIndexPlot()`, `outlierTest()`, and `dfbeta`-style measures | Any `lm`/`glm` fit; diagnostics are single-observation deletion measures (Cook's distance, hat values, studentized residuals) | `car`'s influence measures are the classical single-deletion regression diagnostics literature that `stabilitest`'s worst-case removal explicitly argues *against relying on alone* (Section "The determined skeptic"): a result can pass every `car` outlier/leverage flag and still be one adversarially-chosen small subset away from losing significance. `stabilitest` reports both views' conclusion (not just the estimate) after deletion, searches adversarially rather than by residual/leverage rank, and adds bootstrap and worst-case-percentage views `car` does not provide. |
| **zaminfluence** (GitHub only, `rgiordan/zaminfluence`; not on CRAN) | Not on CRAN; research software accompanying Broderick, Giordano & Meager (2023), *"An Automatic Finite-Sample Robustness Metric: When Can Dropping a Little Data Change Conclusions?"* | What is the smallest fraction of the sample whose removal changes a Z-estimator's sign or significance, computed via an efficient linearized approximation to the true leave-*k*-out refit? | Z-estimators generally; the paper's applications are econometric regression coefficients | This is the paper `stabilitest`'s worst-case-removal component explicitly credits as its methodological ancestor. The relationship is direct: `zaminfluence` computes an approximate influence-function-based estimate of the minimal removal fraction (fast, but a linear approximation), while `stabilitest` performs the actual greedy leave-*k*-out refit (exact for the tests it supports, more expensive, and reported as an upper bound on the true minimal set rather than an approximation to it) and packages the result alongside jackknife and bootstrap views plus a calibrated verdict, for the two-sample and model-term trial designs common in clinical reporting rather than general Z-estimators. |

## Positioning

Every package above answers a version of "how much would have to change for this
conclusion to flip?" — but they partition into two families. `sensemakr`,
`konfound`, and `EValue` ask the question about *unmeasured* threats: a
confounder, a selection mechanism, or a batch of unobserved cases that the
analyst never had in hand. Their answers are analytic bounds computed from
the reported estimate and its uncertainty, and none of them touch the actual
rows of the analysis dataset. `fragility`, `fragilityindex`, `influence.ME`,
`car`, `zaminfluence`, and `stabilitest` instead ask the question about the
*observed* sample: which specific patients, if perturbed or removed, would
change the conclusion? Within that second family, the packages differ in
scope and rigor rather than in kind. `fragility` is restricted to flipping
binary outcome labels; `car` and `influence.ME` restrict themselves to
classical single-deletion diagnostics that this manuscript's own simulations
(Section "The determined skeptic") show can overstate robustness by a factor
of two to five relative to adversarial removal; `zaminfluence` approximates
the adversarial leave-*k*-out answer efficiently but only for Z-estimators in
general and does not target the two-sample and model-term designs of trial
reporting.

`stabilitest`'s distinguishing choices, relative to all eight packages, are:
(i) it evaluates deletion, jackknife, and bootstrap sensitivity together as
three complementary views of the same conclusion rather than offering one
diagnostic in isolation; (ii) worst-case removal is an actual adversarial
refit search across the trial-relevant test catalogue (two-sample tests,
ANCOVA/GLM/Cox model terms, TOST equivalence), not a residual/leverage
heuristic or a linear approximation; (iii) the resulting composite score
carries a categorical verdict only where an independent, pre-registered
calibration study has validated it for that exact analysis configuration,
and stays numeric-only — never silently defaulting to a borrowed threshold —
everywhere else, including two named negative calibration results reported
as evidence of that discipline rather than omitted. No other package in this
table ships a calibration registry or a fail-closed labelling policy.

## Packages considered but not included

None. All nine candidates named in the plan (`fragility`, `fragilityindex`,
`sensemakr`, `konfound`, `EValue`, `boot`, `influence.ME`, `car`,
`zaminfluence`) were verified against a live CRAN page or GitHub repository
and are included above.
