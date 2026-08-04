# Calibration statistical analysis plan

## Scope and estimand

Calibration is analysis-specific.  Each scenario in `config/scenarios.R` fixes
the data-generating truth, endpoint, adapter, sample size, removal budget,
training split, seed, and `n_boot = 1000`.  The estimand is the probability that
the reported robustness score leads to the correct pre-specified conclusion for
an independent held-out replicate from the same scenario family.

## Frozen truth classes and score bands

Truth classes are `null`, `borderline`, and `clear`.  A scenario's
`target_conclusion` is frozen before simulation and is never changed in response
to observed scores.  Shared score cutoffs are frozen at 55 and 70:

- score ≤ 55: fragile;
- 55 < score ≤ 70: moderate;
- score > 70: robust.

Scores are interpreted as evidence about the target conclusion, not as a
replacement for the primary hypothesis test.

## Training and held-out evaluation

The `training_split` column determines the deterministic split within each
scenario and seed.  Training replicates are used only to fit calibration maps,
choose any family-specific intercept/slope, and estimate uncertainty.  Held-out
replicates are untouched until final evaluation.  No seed, replicate, or
scenario may occur in both sets; resume checkpoints must preserve this split.

For each score band, report the held-out calibration rate, Monte Carlo standard
error, and a binomial confidence interval.  Production calibration of each core
scenario starts with at least 500 completed full robustness analyses and targets
Monte Carlo standard error ≤ 0.02 for each reported proportion.  Attempted,
failed, and excluded replicates are reported separately.  The approved held-out
stratum minimum is 100 replicates; a stratum below that minimum is diagnostic
only and cannot support a definitive mapping.  Pilot runs are for wiring,
occupancy, and precision checks and never freeze candidates or publish a map.

## Pilot review and frozen execution plan

The Task 14 pilot was run with the frozen scenario registry at commit
`bfc3816`, workers `1`, `n_boot = 50`, a target of 10 completed replicates per
truth/conclusion stratum, and a maximum of 250 screening draws per scenario.
The run selected all 13 core scenario rows (the pilot selection is recorded in
`published/pilot-runtime-summary.csv`).  It completed 257 of 257 selected
analyses with no analysis failures.  One occupancy limit was observed:
`two_sample_core_clear_n80` supplied only 7 of the 10 requested
`clear::non_significant` screening rows, so 17 rather than 20 replicates were
selected.  This is recorded as a quota-incomplete screening condition, not as
an exclusion or a score-based decision.

The pilot recorded per-scenario runtime, screening completion, selected counts,
and failure classes only; it was not used to inspect score distributions,
change truth classes, tune cutoffs, or fit a calibration map.  The compact
failure record is `published/pilot-failure-summary.csv`.  The pilot therefore
freezes these operational rules for the publication run:

- retain the scenario parameters, truth labels, cutoffs, and `n_boot = 1000`;
- retain a 10-replicate-per-stratum pilot target and 250-draw pilot budget for
  future wiring checks;
- use the full-run screening budget of 10,000 draws, with resume checkpoints,
  when filling the publication quotas;
- report any unfilled stratum explicitly and do not substitute score-based
  sampling or alter a scenario to improve occupancy;
- treat computational failures above 5% (or any failure that changes a held-out
  rate by more than 0.05) as a review trigger before publishing.

The full scenario manifest hash after this pilot review is
`f543a90c41a342497f07f1287503eb5b`.  Any subsequent scenario, adapter, or RNG
change requires a new hash and a new pilot before full execution.

Use one-sided 95% Wilson bounds for the acceptance decisions below.  When rates
are aggregated across scenarios, retain scenario identity and report
scenario-cluster uncertainty (a scenario-cluster bootstrap or equivalent
cluster-robust interval); do not treat replicates from one scenario as
independent evidence about another scenario.

## Decision criteria

False reassurance is a held-out false-positive conclusion: the calibrated score
is moderate/robust while the truth class is `null` or the target conclusion is
not supported.  Robust identification is a held-out correct conclusion in the
direction specified by `target_conclusion`, with the score band consistent with
that conclusion.  Both rates are reported by family and truth class.  The
frozen decision thresholds are:

- false reassurance is acceptable only when its point estimate is ≤ 5% **and**
  its one-sided 95% Wilson upper bound is ≤ 10%;
- robust identification is acceptable only when its point estimate is ≥ 70%
  **and** its one-sided 95% Wilson lower bound is ≥ 60%;
- shared mapping acceptance also requires held-out balanced ordinal accuracy
  ≥ 0.70.  This blocks policies that satisfy FR/RI and median ordering while
  dumping every `borderline` replicate into `robust` or `fragile` (accuracy
  2/3) and thereby suppressing family-specific alternatives.

In addition, the median score ordering must agree with the truth ordering in
core scenarios (`null` ≤ `borderline` ≤ `clear`).  Stress scenarios must show no
material reversal of that ordering.  A failed threshold, ordinal-accuracy, or
ordering criterion marks the candidate indeterminate, even when the shared
score band looks favorable on the extremes alone.

The family-specific rule is frozen as a five-point and 0.05 rule.  A mapping is
accepted only when its held-out conclusion rate is at least five percentage
points better than the competing mapping (absolute difference ≥ 0.05), and the
direction is replicated in every required truth stratum.  If either condition
fails, the mapping is marked indeterminate and the shared 55/70 bands remain the
reported sensitivity analysis.

The five-point/0.05 rule is applied to the relevant estimand for each family:

- two-sample and proportion: direction and significance of the group contrast;
- lm, binomial, and Poisson: the named model term (including its link-scale
  direction);
- Cox: the named hazard ratio direction;
- TOST: equivalence/non-inferiority success in both component tests.

## Non-significant results

Non-significant primary tests are not treated as evidence of equivalence or no
effect.  They are reported as inconclusive unless the scenario is a TOST
equivalence/non-inferiority target and both pre-specified component tests pass.
The robustness score may describe stability of a non-significant result, but it
cannot change this policy.

## Primary and sensitivity analyses

The primary analysis uses the frozen adapters, score cutoffs (55, 70), training
split, removal budget, and `n_boot = 1000` in the scenario table.  Sensitivity
analyses vary one item at a time: the shared cutoffs, the training split, the
removal budget, and the bootstrap seed.  Family-specific adapter substitutions
are sensitivity analyses, never replacements for the primary adapter.

## Exclusions and failure reporting

Replicates are excluded only for a pre-specified computational or estimand
failure (for example, a singular model matrix, zero events for a required Cox
stratum, or an adapter error).  A failed replicate is never silently dropped:
record scenario ID, replicate ID, seed, failure class, error message, and the
stage at which it occurred.  Report attempted, successful, excluded, and failed
counts for every stratum.  If failures exceed 5% or alter a held-out rate by
more than 0.05, the scenario is flagged for review and no definitive mapping is
declared.

All changes to scenarios, adapters, package code, or random-number handling
require a new calibration run and an updated audit manifest.

## Task 15 execution record

The publication-grade command was exercised through the locked training →
freeze → held-out flow using the deterministic reduced fixtures under
`tests/fixtures/`.  A full 35-scenario run with `n_boot = 1000` and the frozen
500-completed-replicate core quotas was not computationally practical in this
environment, so it is explicitly unsupported for publication claims here.
The reduced run retained the frozen scenario manifest hash
`f543a90c41a342497f07f1287503eb5b`, recorded 600 completed and 300 failed
training rows, and evaluated 900 held-out rows once.  The frozen candidate hash
is `a0a017fcb1bd8e2a6f11dcde16b4aea2`; held-out validation recorded
`validation_refit = FALSE` and produced registry hash
`690467ca331d162feea16386b5921a3a`.

Compact manifests, registry tables, and their MD5 ledger are tracked under
`published/`.  Failed rows and any unfilled production strata remain explicit
diagnostics; a complete full run is required before these artifacts can support
package interpretation metadata or release claims.
