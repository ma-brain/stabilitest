# LM/ANCOVA Categorical-Band Calibration Design

## Context

`stabilitest` currently publishes calibrated Fragile / Moderately Robust /
Robust bands only for eligible significant `welch_unpaired` results. The
active calibration registry identifies `lm_ancova` as the next independent
target, but its row remains uncalibrated and `robustness_lm()` suppresses the
categorical label.

The historical mixed-family calibration run cannot establish ANCOVA bands. Its
LM grid mixed single-coefficient and multi-df behavior, did not contain a
complete held-out null/borderline/clear design, and produced no feasible
cutoffs under the locked policy. A new claim therefore requires an independent,
method-specific study with fresh scenarios and held-out evidence.

Current FDA and EMA guidance supports the selected canonical setting:
prespecified prognostic baseline-covariate adjustment in randomized,
parallel-group confirmatory trials. The calibration does not claim validity for
arbitrary linear-model coefficients.

## Decisions

- Calibrate only the 1-df adjusted treatment coefficient.
- Keep multi-df joint terms explicitly uncalibrated.
- Target a canonical two-arm randomized ANCOVA with a continuous outcome, one
  prespecified continuous baseline covariate, an additive treatment effect,
  balanced allocation, complete data, and ordinary least squares inference.
- Treat imbalance, heteroscedasticity, heavy tails, missingness, and nonlinearity
  as stress diagnostics rather than supported conditions.
- Define borderline and clear alternatives by prespecified operating power,
  not fixed standardized effects.
- Freeze the current composite score, default weights, removal budget, and
  bootstrap configuration. Fit only the two categorical cutoffs.
- Fail closed: if no candidate passes the frozen held-out criteria,
  `lm_ancova` remains uncalibrated.

## Goals

- Produce an independently reviewable ANCOVA calibration study.
- Estimate ANCOVA-specific categorical cutoffs for eligible significant
  canonical treatment effects.
- Preserve `robustness_lm()` and existing numeric result fields.
- Enforce every mechanically observable supported condition at runtime.
- Keep training, candidate freezing, held-out evaluation, failures, and
  publication provenance auditable.

## Non-Goals

- Calibrating multi-df treatment tests.
- Calibrating arbitrary continuous covariates or other 1-df LM coefficients.
- Reweighting or redesigning the composite robustness score.
- Validating missing-data methods, heteroscedasticity-robust standard errors,
  interactions, nonlinear models, or nonrandomized analyses.
- Treating Welch's 55/70 cutoffs as an ANCOVA default or fallback.
- Reusing previously inspected Task 15 validation results as confirmatory data.

## Alternatives Considered

### 1. Dedicated ANCOVA study using shared infrastructure (selected)

Create an isolated ANCOVA scenario registry, SAP, manifests, runner, cutoff
analysis, and publication artifacts while reusing the stable calibration
schema, executor, checkpoints, seed handling, uncertainty, and threshold
utilities. This gives the new claim its own provenance without duplicating the
entire harness.

### 2. Extend the historical mixed-family registry in place

This would require fewer initial file changes, but every ANCOVA edit would
change the global scenario manifest and would keep the new claim entangled with
the historical seven-family experiment.

### 3. Refactor the complete harness into a generic study framework first

This could improve long-term reuse, but it would materially expand scope before
producing ANCOVA evidence. The dedicated study should expose the abstractions
actually needed by the next method-specific calibration.

## Architecture

The study lives under an ANCOVA-specific area within
`manuscript/calibration/`. It owns:

- an ANCOVA scenario contract;
- a power-target solver and validation routine;
- a study runner and CLI configuration;
- an ANCOVA SAP;
- training and validation manifests;
- frozen candidate and registry artifacts;
- a publication hash ledger;
- reduced fixtures for automated end-to-end tests.

It sources or calls existing shared helpers for replicate schemas, deterministic
seeds, screening, execution, checkpoints, failure records, Wilson intervals,
and cluster-aware summaries. Historical Task 15 inputs and published artifacts
remain immutable.

Package integration has two gates:

1. **Study gate:** build and verify the complete calibration study while the
   active `lm_ancova` registry row stays `uncalibrated`.
2. **Runtime gate:** update the active registry only after a full held-out run
   passes, its artifacts and hashes are committed, and the result receives
   statistical and code review.

## Runtime Applicability Profile

The public calibration identity remains `lm_ancova`. `robustness_lm()` builds a
machine-readable analysis profile from the original fit, resolved term, and
score configuration. A validated row is applicable only when the observed
result has all of these properties:

- the primary conclusion is significant;
- the selected term is a single 1-df coefficient;
- the coefficient belongs to a two-level categorical treatment term;
- the response is continuous;
- the model contains the treatment term and exactly one continuous baseline
  covariate;
- the formula is additive and contains no interaction, transformation, offset,
  or additional adjustment term;
- the fitted analysis omitted no rows;
- total analysis sample size is within 40 to 240;
- `alpha = 0.05`;
- `n_boot = 1000`;
- weights are `jackknife = 0.4`, `fragility = 0.4`, and `bootstrap = 0.2`;
- `max_removal_pct = 0.30`.

A result outside this profile retains its score and component metrics but has
no categorical label. This includes all multi-df joint tests. Legacy serialized
results without the required profile fields also fail closed.

Randomization, prospective covariate specification, the population baseline
prognostic strength, and distributional assumptions cannot be proven from a
fitted R model. They are explicit user responsibilities in
`supported_conditions`; runtime checks enforce only observable facts.

## Canonical Data-Generating Model

Core data follow

```text
Y_i = beta * T_i + gamma * X_i + epsilon_i
```

where `T` is a balanced randomized binary treatment, `X` is a standard-normal
baseline covariate independent of treatment, and `epsilon` is a normal,
homoscedastic residual. The analysis is
`outcome ~ treatment + baseline` using the ordinary coefficient t-test.

Truth classes are:

- **null:** `beta = 0`; an eligible significant false positive should be
  Fragile;
- **borderline:** `beta` gives approximately 60% nominal two-sided ANCOVA
  power; an eligible significant result should be Moderately Robust;
- **clear:** `beta` gives approximately 90% nominal power; an eligible
  significant result should be Robust.

The effect for each scenario is solved from the noncentral t distribution. A
separate primary-test-only Monte Carlo run verifies the achieved power before
any robustness scores are generated. Power validation uses independent seeds
and does not inspect or tune score distributions.

## Scenario Grid

### Training

Cross:

- total sample size: 40, 80, 160;
- baseline prognostic strength: `R^2 = 0.10, 0.40, 0.70`;
- truth: null, borderline, clear.

### Held-out validation

Use unseen combinations:

- total sample size: 60, 120, 240;
- baseline prognostic strength: `R^2 = 0.25, 0.55`;
- truth: null, borderline, clear;
- independent scenario and replicate seed blocks.

A small negative-effect block checks two-sided directional symmetry without
doubling the entire primary grid.

### Stress diagnostics

Separate, non-calibrating scenarios cover:

- 2:1 allocation;
- residual heteroscedasticity;
- heavy-tailed residuals;
- missing baseline observations;
- nonlinear baseline response;
- treatment-by-baseline interaction.

Stress scenarios do not fit cutoffs, do not enter held-out acceptance, and
cannot expand `supported_conditions`. A serious reversal or false-reassurance
signal triggers methodological review before runtime integration.

## Screening and Replicate Quotas

Categorical labels apply only to significant superiority conclusions. The
study therefore screens generated datasets using the same coefficient test as
`robustness_lm()` and runs the expensive robustness engine only for eligible
significant draws.

This yields the calibration populations directly:

- significant null draws estimate false reassurance;
- significant borderline draws estimate moderate-band discrimination;
- significant clear draws estimate robust identification.

Every held-out scenario contributes at least 100 completed eligible results.
Aggregate held-out truth-stratum counts target at least 600, giving
approximately 0.02 worst-case binomial Monte Carlo error before scenario-level
clustering is accounted for. Screening attempts, eligible draws, completed
analyses, exclusions, and failures remain separately countable.

## Irreversible Study Flow

1. Freeze scenario definitions, power targets, seeds, quotas, score settings,
   failure rules, and the scenario-manifest hash.
2. Run a wiring/runtime/occupancy pilot without inspecting scores.
3. Verify power targets with primary-test-only simulation.
4. Generate the training robustness results.
5. Search the frozen cutoff grid and select one candidate.
6. Freeze the candidate, its diagnostics, and its hash.
7. Open the held-out seed/scenario block once.
8. Evaluate the frozen candidate without refitting.
9. Publish a validated or explicit uncalibrated ANCOVA registry artifact.
10. Update the package registry only after independent review.

If training produces no feasible candidate, the held-out block remains
unopened so it can support a genuinely new, versioned future design.

## Cutoff Fitting

Search every ordered integer cutoff pair `(L, U)` on the 0--100 score scale:

- score `<= L`: Fragile;
- `L < score <= U`: Moderately Robust;
- score `> U`: Robust.

A training pair is feasible only when:

- false reassurance among eligible null results is at most 5%, and its
  one-sided 95% upper bound is at most 10%;
- robust identification among eligible clear results is at least 70%, and its
  one-sided 95% lower bound is at least 60%;
- balanced three-class accuracy is at least 0.70;
- accuracy within each truth class is at least 0.60;
- median scores are ordered null < borderline < clear.

Select among feasible pairs by:

1. highest balanced accuracy;
2. highest minimum truth-class accuracy;
3. greatest safety margin from the false-reassurance and
   robust-identification limits;
4. deterministic lexicographic order.

The Welch 55/70 mapping is reported as a comparator only. It receives no
preference and is never a fallback.

## Held-Out Acceptance

The frozen candidate must satisfy the same point-estimate criteria on held-out
data. Acceptance uses the more conservative of:

- one-sided Wilson bounds over eligible replicates;
- scenario-cluster bootstrap bounds preserving scenario identity.

Additionally:

- held-out balanced accuracy must be at least 0.70;
- its one-sided cluster lower bound must be at least 0.65;
- every truth-class accuracy must be at least 0.60;
- median ordering must hold within matched sample-size/prognostic-strength
  blocks.

A held-out failure leaves `lm_ancova` uncalibrated. No second candidate,
threshold adjustment, scenario removal, or validation refit is allowed. A
future attempt requires a new calibration version and a fresh held-out block.

## Failure and Exclusion Policy

- Record every attempted replicate and its final stage/status.
- Never exclude a replicate because of its score or band.
- Preserve scenario ID, replicate ID, deterministic seed, failure stage,
  failure class, and message.
- Block publication when an eligible scenario misses its quota.
- Block publication when analysis failures exceed 5% in any required scenario.
- Reject checkpoint resume when scenario, code, option, or manifest hashes do
  not match.
- Fail commands on nonzero child-process status rather than publishing stale
  assembled output.
- Include failure and exclusion summaries in the publication hash ledger.

## Verification Strategy

### Statistical and scenario tests

- Verify analytic power solutions against independent Monte Carlo power.
- Validate scenario IDs, supported values, quotas, and design layers.
- Prove training and held-out scenario/seed disjointness.
- Verify positive/negative effect symmetry.
- Verify every required truth/scenario quota before fitting or publishing.

### Adapter parity tests

- Compare the screening coefficient p-value, estimate, and conclusion with a
  reduced `robustness_lm()` run.
- Verify complete-case row identity and term resolution.
- Reject aliased, singular, malformed, and noncanonical model profiles.

### Cutoff and freeze tests

- Use synthetic fixtures with known feasible and infeasible cutoff results.
- Test each hard constraint and deterministic tie-break independently.
- Verify Wilson and scenario-cluster interval calculations.
- Hash the frozen candidate and reject held-out evaluation after any mutation.
- Prove held-out analysis never calls the fitting path.

### Runtime applicability tests

Exercise both eligible and rejected profiles, including:

- valid canonical significant 1-df treatment effects;
- non-significant conclusions;
- multi-df joint terms;
- continuous target covariates;
- three-level treatment factors;
- extra covariates and interactions;
- transformations and offsets;
- omitted rows;
- nondefault alpha, bootstrap count, weights, or removal budget;
- samples outside the validated range;
- legacy objects without an analysis profile.

### End-to-end and package verification

- Run a deterministic reduced training -> freeze -> held-out fixture.
- Check manifests, failure accounting, artifact hashes, and no-refit metadata.
- Run the calibration-specific `testthat` suite.
- Run `devtools::test()` and the calibration documentation audit.
- Run `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))` before
  runtime integration.

## Documentation and Publication

The ANCOVA SAP, published registry artifact, README, help pages, NEWS, and
calibration documentation must state that:

- bands apply only to eligible significant canonical 1-df ANCOVA treatment
  effects;
- randomization and prospective covariate choice are user responsibilities;
- multi-df and noncanonical LM results remain numeric-only;
- 55/70 is a Welch comparator, not an ANCOVA default;
- an unsuccessful calibration remains a valid, publishable uncalibrated result.

Validated provenance includes the calibration version, source commit/artifact,
scenario manifest hash, candidate hash, registry hash, execution options,
software versions, and hashes for all compact publication outputs.

## Primary Guidance Sources

- FDA, *Adjusting for Covariates in Randomized Clinical Trials for Drugs and
  Biological Products*, May 2023:
  https://www.fda.gov/regulatory-information/search-fda-guidance-documents/adjusting-covariates-randomized-clinical-trials-drugs-and-biological-products
- EMA, *Guideline on adjustment for baseline covariates in clinical trials*,
  EMA/CHMP/295050/2013:
  https://www.ema.europa.eu/en/adjustment-baseline-covariates-clinical-trials-scientific-guideline
