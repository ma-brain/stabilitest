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
error, and a binomial confidence interval.  The minimum accepted stratum size is
100 held-out replicates.  A reported proportion must have Monte Carlo standard
error ≤ 0.02 (otherwise the stratum is flagged as underpowered and is not used
for a definitive mapping).

## Decision criteria

False reassurance is a held-out false-positive conclusion: the calibrated score
is moderate/robust while the truth class is `null` or the target conclusion is
not supported.  Robust identification is a held-out correct conclusion in the
direction specified by `target_conclusion`, with the score band consistent with
that conclusion.  Both rates are reported by family and truth class.

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
