# R Journal Welch Recalibration and Submission Design

**Date:** 2026-08-10

**Status:** Approved

**Submission assumption:** `stabilitest` is already published on CRAN before
the R Journal submission is assembled.

## Purpose

Prepare the R Journal article for submission without relying on the historical
Welch 55/70 bands as if they were a prospectively frozen, method-specific,
held-out validation. The central scientific gate is a new calibration study for
the significant, unpaired Welch configuration. The study either earns a new
runtime mapping or closes fail-closed and makes Welch numeric-only.

The article, registry, documentation, and submission package must all describe
the same outcome. No manuscript wording may anticipate a positive result.

## Why a new study is necessary

The active registry row for `welch_unpaired` points to a historical manuscript
commit rather than to a self-contained calibration study. The repository's
historical broad-family calibration archive explicitly says that its inspected
validation rows cannot be reused as fresh method-specific confirmation.

The R Journal simulation is informative but cannot itself validate the current
bands. Across its 12 scenarios, 132 null replicates reached significance. Of
those, 47.7% scored above 55 and would receive a Moderate or Robust verdict;
among the 58 significant clean-null replicates, the corresponding rate was
48.3%. That is incompatible with the repository's current false-reassurance
definition and gate if the study is treated as confirmatory evidence.

The replacement study therefore starts from a new written protocol and new
random streams. The existing simulation remains a historical comparator and a
design input, never training or validation evidence.

## Scientific design

### Calibration unit

The unit is the exact runtime configuration:

- resolved method: `welch_unpaired`;
- endpoint: mean difference;
- conclusion: statistically significant superiority at `alpha = 0.05`;
- weights: package defaults `(0.4, 0.4, 0.2)`;
- removal budget: `max_removal_pct = 0.30`;
- bootstrap size: `n_boot = 1000`;
- independent groups only.

The eligibility profile must be expressible from information available at
runtime. It may include group-size range and allocation ratio, but not latent
distributional facts that the package cannot verify for a user.

### Truth and scenario layers

Core calibration scenarios use clean data and define truth independently of
the observed p-value:

- **Null:** true mean difference is zero.
- **Borderline:** effect chosen to give approximately 60% Welch-test power;
  retained for ordinal diagnostics and not used to make a binary claim look
  easier.
- **Clear:** effect chosen to give approximately 95% Welch-test power.

The core grid crosses pre-specified group sizes, allocation ratios, variance
ratios, and distribution shapes that the final eligibility claim is intended
to cover. Training and validation use disjoint scenario rows and disjoint
random streams.

Contamination belongs to a separate stress layer. Its truth refers to the
latent uncontaminated mean contrast, while injected values model gross data
error or distributional contamination. These rows must not be described as
ordinary Type I-error experiments. They test degradation and ordering, not the
primary clean-data false-reassurance estimand.

### Candidate fitting and held-out confirmation

Training may compare:

1. the historical three-band 55/70 mapping;
2. a newly fitted three-band mapping;
3. a two-band Fragile/Not-fragile mapping;
4. no categorical mapping.

Candidate selection must obey frozen error gates, minimum occupancy, monotone
truth ordering, and scenario-cluster uncertainty. The candidate and its hash
are frozen before validation data are opened. Validation is evaluated once;
there is no threshold refit, gate change, weight search, or scenario removal
after opening.

The acceptance gates are frozen in the SAP. At minimum they retain the current
project standard:

- false reassurance point estimate at most 5%, with one-sided 95% upper bound
  at most 10%;
- clear-effect identification point estimate at least 70%, with one-sided 95%
  lower bound at least 60%;
- balanced ordinal accuracy at least 0.70 for a three-band candidate;
- no material reversal across required core scenarios;
- failures and unfilled quotas reported explicitly.

The SAP must define precisely whether "not fragile" is an assurance claim. If
it is emitted as a categorical interpretation, it counts as reassurance for
the false-reassurance gate.

### Fail-closed outcomes

There are only two permissible outcomes:

- **Pass:** publish `welch-2026-2`, update the registry with the frozen mapping
  and an enforceable eligibility profile, and report the held-out estimates and
  bounds in the article.
- **Fail:** change the active Welch registry row to `uncalibrated`, suppress
  Welch labels, retain numeric metrics, and report the negative calibration
  result. Fisher remains the only active categorical mapping.

The article may be submitted under either outcome. It may not be submitted
with the historical mapping still described as independently validated.

## Manuscript revision design

The paper remains a package article, not a calibration report. Calibration
supports the label policy but does not displace API, implementation, examples,
and comparisons with related R software.

The revision will:

- replace the Welch section with the new study design and outcome;
- give scenario counts, significant-result denominators, failure counts,
  confidence bounds, and eligibility limits;
- replace internal project labels such as "Task 15", "Gate B", "v3 Track E",
  hashes, and raw reason strings with reader-facing study names, moving audit
  identifiers to supplementary material;
- call the bootstrap quantity a bootstrap same-decision rate and explain its
  plug-in interpretation;
- explain that the greedy removal result is an upper bound and that a no-flip
  result is right-censored at the removal cap;
- remove claims that the case study identifies a "genuine effect", that greedy
  search is generally near-optimal, or that empirical ANCOVA infeasibility is
  information-theoretic;
- separate the observed extreme-value-removal comparison from claims about
  `car` and `influence.ME`;
- position observation deletion as an adjunct to, not a substitute for,
  estimand-based regulatory sensitivity analysis;
- distinguish total sample size from per-group sample size consistently.

## Production and submission design

The production pass fixes known R Journal issues:

- title case and CRAN-package markup;
- citations for compared R packages and missing stable reference links;
- the clipped related-packages table;
- the case-study figure float that separates code from output;
- the overfull source-path line;
- explicit figure alt text;
- the incorrect test-evaluation count at `n = 55`;
- the claim that `n_boot = 2000` is a package default;
- dependency-install order and the incomplete `_Rpackages.txt` list;
- removal of `.log`, `.aux`, and `.out` files from the submission archive.

Heavy calibration remains a documented optional regeneration. The article
loads frozen published artifacts and must knit to PDF and HTML in less than ten
minutes. The submitted archive must be reproducible from a clean checkout and
contain only requested source, article, reproduction, data, package-list, and
motivation-letter files.

## Verification and release gate

Before packaging:

1. Run the calibration study tests and validate the freeze/hash sequence.
2. Confirm the registry behavior for both the pass and fail-closed paths.
3. Run the package `testthat` suite and `R CMD check --as-cran`.
4. Render PDF and HTML from a clean temporary checkout.
5. Run all applicable `rjtools` checks individually and through
   `initial_check_article()`.
6. Inspect every PDF page and the HTML output visually.
7. Verify page count, abstract length, archive size, runtime, citations, table
   widths, figure placement, and source/output consistency.

Submission is a human action. The implementation workflow prepares and audits
the archive but does not upload to CRAN, the R Journal, or any external service.
