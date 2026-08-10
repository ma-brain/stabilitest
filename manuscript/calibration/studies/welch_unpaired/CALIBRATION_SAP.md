# Prospective Welch Calibration Statistical Analysis Plan

**Study version:** prospective Welch study 2026-2  
**Calibration unit:** `welch_unpaired`  
**Status:** frozen before score production  
**Study path:** `manuscript/calibration/studies/welch_unpaired/`

This plan governs one prospective, method-specific calibration attempt. The
historical 55/70 bands and all previously inspected simulations are design
inputs and comparators only. They are not training or validation evidence.
After this document and `config/scenarios.R` are committed, no score-dependent
change to a gate, truth definition, scenario, weight, seed, or candidate rule
is permitted.

## Calibration unit and runtime claim

The unit is an independent-groups Welch two-sample test resolved by
`stabilitest` as `welch_unpaired`, with endpoint mean difference and a
statistically significant superiority conclusion at `alpha = 0.05`. Every
scored analysis uses package-default weights `(jackknife = 0.4, fragility =
0.4, bootstrap = 0.2)`, `n_boot = 1000`, and `max_removal_pct = 0.30`.

A passing mapping is eligible only for complete-case independent groups with
per-group sizes from 25 through 200 and allocation ratio `n_group1/n_group2`
from 2/3 through 3/2, under the exact conclusion, alpha, bootstrap, removal,
and weight profile above. Runtime eligibility never relies on unobservable
distribution shape or true variance ratio.

## Truth and scenario registry

Truth is defined independently of the observed p-value:

- `null`: true mean difference zero;
- `borderline`: positive mean difference solving the frozen two-sided Welch
  power approximation to 0.60;
- `clear`: positive mean difference solving it to 0.95.

The core registry contains these truth strata for each of six archetypes in
both training and validation:

| Archetype | n1 | n2 | sd1 | sd2 |
| --- | ---: | ---: | ---: | ---: |
| W01 | 25 | 25 | 1 | 1 |
| W02 | 50 | 50 | 1 | 2 |
| W03 | 100 | 100 | 2 | 1 |
| W04 | 200 | 200 | 1 | 1 |
| W05 | 40 | 60 | 1 | 2 |
| W06 | 60 | 40 | 2 | 1 |

Training IDs begin `WTR-` and seeds begin at `202610001`; validation IDs begin
`WVA-` and seeds begin at `202620001`. Scenario rows and random streams are
disjoint. The resolved non-null deltas are produced by the committed power
solver without changing scenario identities.

Six diagnostic stress rows use independent `WST-` IDs and seeds beginning at
`202630001`: null and clear variants of Student-t5 at 50/50, centered
log-normal at 100/100, and gross-error treatment contamination at 50/50. The
gross-error generator injects a `+8` offset into a frozen 5% of treatment
observations while preserving the latent clean value and contrast. Its truth
always refers to the uncontaminated contrast. Stress rows measure degradation
and ordering; they never enter fitting or acceptance gates and are not
described as ordinary Type I-error experiments.

## Conditional sampling, quotas, and failures

Categorical interpretation applies only to significant conclusions. The
runner therefore screens generated draws with the same two-sided Welch test
and performs the full robustness analysis only when `p < 0.05`. Each required
scenario must yield 250 completed significant analyses. The frozen draw cap is
20,000 attempts per scenario.

Every scenario manifest records attempted, screened, completed, failed, and
excluded counts. A required cell fails occupancy if it has fewer than 250
completed rows at the draw cap or if full-analysis failures exceed 5% of
screened-significant draws. Failures retain scenario ID, replicate ID, seed,
stage, and message; omissions are never silent. Smoke mode has separate paths,
20 bootstraps, and five completed rows per cell and is not study evidence.

## Candidate mappings

Training compares, in this order:

1. a three-band mapping (`score <= L`: Fragile; `L < score <= U`: Moderate;
   `score > U`: Robust) searched over integer `0 <= L < U <= 100`;
2. a two-band mapping (`score <= L`: Fragile; `score > L`: Not fragile)
   searched over integer `0 <= L <= 100`;
3. no categorical mapping.

The historical 55/70 three-band mapping is reported as a named comparator and
receives no preference. For either categorical mapping, every emitted verdict
other than Fragile counts as reassurance. Thus Moderate, Robust, and Not
fragile all count in the null false-reassurance numerator.

For a three-band candidate, ordinal correctness maps null to Fragile,
borderline to Moderate, and clear to Robust. Balanced ordinal accuracy is the
unweighted mean of the three truth-stratum accuracies. For a two-band
candidate, the diagnostic balanced accuracy is the unweighted mean of null
Fragile accuracy and clear Not-fragile accuracy; borderline rows are reported
but do not make the binary claim easier.

## Frozen gates

All of the following are required on training:

- false reassurance among completed significant null rows: point estimate at
  most 0.05 and one-sided 95% upper bound at most 0.10;
- clear identification: Robust for three bands or Not fragile for two bands,
  point estimate at least 0.70 and one-sided 95% lower bound at least 0.60;
- balanced ordinal accuracy at least 0.70 for a three-band candidate;
- every emitted band occupies at least 5% of all fitting rows;
- pooled medians are ordered `null <= borderline <= clear`;
- within each core archetype, a reversal is material and disqualifying when
  the null median exceeds the borderline median by more than 5 score points,
  or the borderline median exceeds the clear median by more than 5 points;
- every required scenario meets quota and has failure rate at most 5%.

Point estimates use pooled completed rows. One-sided 95% Wilson bounds are
computed for binomial proportions. Scenario-cluster bootstrap bounds resample
whole scenario cells with replacement using seed `202640001` and 1,000 draws.
For upper-limit gates the larger of Wilson and cluster bounds governs; for
lower-limit gates the smaller governs. Balanced accuracy uses a two-sided
scenario-cluster percentile interval for reporting, with its point gate frozen
above.

Within a mapping class, passing candidates are ordered by highest balanced
accuracy, then highest minimum standardized distance from every applicable
gate, then the more conservative threshold (higher `L`, followed by higher
`U` for three bands), then lexicographic cutoff order. A passing three-band
mapping is preferred over a passing two-band mapping because that hierarchy is
frozen before training. If no candidate passes, the result is exactly
`status = "no_feasible_thresholds"`; a nearest failure is never substituted.

## Freeze and held-out confirmation

The selected candidate or no-candidate decision is serialized and hashed with
SHA-256 together with this SAP, resolved scenarios, generator, adapter,
threshold code, training data, and package version. Training summaries,
occupancy, failures, candidate diagnostics, the candidate hash, and the
training manifest are committed before validation can be selected or opened.

Validation requires the committed candidate hash and is evaluated once.
Only the frozen candidate is evaluated: no threshold refit, weight search,
gate change, scenario removal, or repeated validation is permitted. The same
occupancy, failure, false-reassurance, clear-identification, ordering, and
applicable accuracy gates govern held-out confirmation using the conservative
bounds defined above.

## Published outcomes

There are only two outcomes:

- **Pass:** publish version `welch-2026-2`, its frozen cutoffs, the enforceable
  runtime eligibility profile, held-out estimates and bounds, failure counts,
  and provenance.
- **Fail:** publish an `uncalibrated` Welch registry proposal with no cutoffs,
  suppress Welch labels, retain numeric scores and components, and report the
  negative calibration result.

A training result of `no_feasible_thresholds` is already final for this
attempt: validation remains unopened and the fail outcome proceeds only after
human acknowledgement. No failed gate starts another attempt inside this
study.
