# Method-Specific Calibration and Interpretation Design

## Context

The Task 15 production calibration run grouped heterogeneous analyses into
seven broad families. The `two_sample` family was especially problematic: it
combined Welch and paired t-tests, Wilcoxon rank-sum and signed-rank tests,
Brunner-Munzel, and binary Fisher, chi-square, and proportion tests. A separate
`proportion` family duplicated some of those binary methods.

That taxonomy is too broad for calibration. The methods have different
estimands, sampling distributions, discreteness, pairing structures, and
failure modes. A cutoff validated for one cannot be inferred to apply to the
others merely because they share the `robustness_analysis()` dispatcher.

The public dispatcher remains useful and backward compatible. This design
therefore removes `two_sample` only as a calibration identity, not as a public
API concept or implementation engine.

## Goals

- Replace broad calibration-family lookup with explicit method-level keys.
- Preserve the public `robustness_analysis()` API and its current `test_type`
  values.
- Retain the original Welch calibration only under its documented conditions.
- Suppress categorical Robust / Moderately Robust / Fragile labels whenever a
  result has no applicable validated calibration.
- Continue returning numeric scores and component metrics for every supported
  analysis.
- Calibrate and validate one method at a time in small, independently reviewed
  projects.
- Preserve Task 15 artifacts as historical evidence without treating their
  all-family registry as active calibration.

## Non-Goals

- Removing or splitting the exported `robustness_analysis()` function.
- Recalibrating another method as part of the taxonomy release.
- Claiming that the Welch cutoffs transfer to paired, rank, binary, model,
  survival, or equivalence/non-inferiority analyses.
- Redesigning the composite score in the taxonomy release.
- Reusing already inspected Task 15 validation data as fresh confirmatory
  evidence for a future calibration.

## Public API Policy

`robustness_analysis()` remains the public entry point for the currently
supported two-vector analyses. Its `test_type` argument continues to accept:

- `t.test`;
- `paired.t.test`;
- `wilcoxon`;
- `wilcoxon.paired`;
- `brunner_munzel`;
- `fisher`;
- `chisq`;
- `prop`.

The selected `test_type` maps internally to one explicit calibration key. No
result is assigned the generic calibration key `two_sample`.

Existing numeric result fields and component metrics remain stable. The
interpretation label becomes conditional on the resolved registry entry. An
uncalibrated result retains its score but returns no categorical label.

## Calibration Taxonomy

The user-facing family groups methods for navigation and documentation. The
calibration unit is the narrower key used for registry lookup and statistical
validation.

| Family | Calibration unit | Current status |
|---|---|---|
| Continuous parametric | `welch_unpaired` | Valid only under the original narrow Welch calibration |
| Continuous parametric | `paired_t` | Uncalibrated |
| Rank/nonparametric | `wilcoxon_rank_sum` | Uncalibrated |
| Rank/nonparametric | `wilcoxon_signed_rank` | Uncalibrated |
| Rank/nonparametric | `brunner_munzel` | Uncalibrated |
| Binary proportions | `fisher_exact` | Uncalibrated |
| Binary proportions | `chi_square_2x2` | Uncalibrated |
| Binary proportions | `two_sample_prop` | Uncalibrated |
| Linear models | `lm_ancova` | Uncalibrated |
| Generalized linear models | `glm_binomial` | Uncalibrated |
| Generalized linear models | `glm_poisson` | Uncalibrated |
| Survival | `cox_ph` | Uncalibrated |
| Equivalence/non-inferiority | `tost_mean` | Uncalibrated |
| Equivalence/non-inferiority | `tost_risk_difference` | Uncalibrated |
| Equivalence/non-inferiority | `tost_odds_ratio` | Uncalibrated |

Equivalence and non-inferiority are additionally distinguished by conclusion
type in the registry because they are different decision problems even when
they share an endpoint implementation.

The registry key is:

```text
calibration_unit + endpoint + conclusion_type + supported_conditions
```

The broad family is metadata and must never be sufficient to select cutoffs.

## Welch Calibration Scope

The existing 55 and 70 cutoffs remain available only for `welch_unpaired`
results that satisfy the conditions supported by the original simulation:

- independent continuous two-group comparison;
- Welch t-test;
- a statistically significant superiority conclusion;
- the normal-data and directional-contamination setting documented by the
  original simulation and manuscript;
- the calibrated score definition, weights, removal budget, and bootstrap
  interpretation used by that simulation.

Results outside those conditions are `uncalibrated`, even if they were produced
by `test_type = "t.test"`. Documentation must describe this as narrow empirical
support, not general validation under all Welch-test assumptions.

The active registry records provenance as an immutable Git object reference to
the committed manuscript section that reports this calibration. The earlier
`manuscript/simulation_results.csv` name is not present in the repository and
must not be used as a dangling validated source.

The Welch registry entry should use a status that explicitly communicates its
scope, such as `validated_method_specific`, and record its calibration version
and source artifact.

## Interpretation Policy

Every result receives calibration metadata containing at least:

```text
version
family
calibration_unit
endpoint
conclusion_type
status
applicable
cutoff_fragile
cutoff_robust
source
supported_conditions
```

Categorical interpretation is emitted only when `applicable` is true and the
status is validated. Otherwise:

- the numeric composite score remains available;
- jackknife, worst-case-removal, bootstrap, and other component metrics remain
  available;
- the categorical label is `NA` or absent according to the existing result
  schema contract;
- print and narrative methods say `categorical bands not calibrated for this
  method`;
- no fallback to Welch thresholds occurs.

Non-significant superiority results remain band-inapplicable. Unsuccessful
equivalence/non-inferiority conclusions also remain band-inapplicable unless a
future dedicated calibration establishes a separate interpretation.

## Registry and Dispatch Flow

The result flow is:

1. The public analysis function resolves the actual method and endpoint.
2. A shared helper maps that information to a calibration unit.
3. The helper derives the conclusion type from the observed analysis result,
   not only from scenario or caller intent.
4. Registry lookup requires an exact method-level match.
5. Supported-condition predicates determine applicability.
6. Validated applicable entries produce cutoffs and a label.
7. Every other path returns explicit uncalibrated metadata and no label.

Unknown or malformed calibration keys fail closed: they never inherit a
neighboring method's cutoffs.

## Task 15 Artifact Policy

Task 15 raw and compact artifacts remain immutable historical evidence of the
broad-family experiment. They document that the initial transferability
hypothesis failed and identify problems in the scenario and estimand design.

They must not drive active lookup after the taxonomy migration. In particular:

- the `two_sample` registry row is archived, not translated into a validated
  method entry;
- the broad-family all-uncalibrated registry is retained for auditability;
- new method-specific registries use new calibration versions and manifests;
- already inspected held-out data are development evidence, not a fresh
  validation set.

## Single-Method Calibration Workflow

Each future calibration is a bounded project for exactly one calibration unit:

1. Define the estimand, applicable conclusion, supported conditions, and
   failure rules.
2. Freeze core, stress, and genuinely held-out scenarios for that method.
3. Verify screening and public-API parity.
4. Run a pilot using runtime and failure outputs only.
5. Freeze sample quotas, seeds, and the analysis plan.
6. Run training simulations and fit candidate cutoffs.
7. Freeze the candidate and its hash.
8. Run a new held-out seed/scenario block once, without refitting.
9. Review statistical validity, failures, exclusions, provenance, and code.
10. Add or update only that calibration unit's registry entry.

The calibration estimand includes only band-applicable conclusions. For a
superiority method, false reassurance is evaluated among null datasets that
produce a false-positive superiority conclusion; robust identification is
evaluated among true-effect datasets that produce the correct significant
conclusion. Stable non-significant results do not enter those rates.

## Priority Sequence

The next package version ships the taxonomy and interpretation-policy cleanup
without waiting for a new calibration.

Subsequent calibration work proceeds in this order:

1. `lm_ancova` — highest priority because adjusted continuous-outcome models
   are common confirmatory-trial primary analyses and Task 15 showed plausible
   but insufficient separation.
2. Binary proportions — start with `two_sample_prop`, then calibrate
   `fisher_exact` and `chi_square_2x2` separately.
3. `glm_binomial`.
4. `cox_ph`.
5. `paired_t`, followed by the rank/nonparametric methods one at a time.
6. `glm_poisson`.
7. TOST/equivalence/non-inferiority as a separate methodological program;
   Task 15 suggests that it may require score or component redesign rather
   than new thresholds alone.

Priority may change when product use data or a concrete regulatory use case
shows greater demand, but calibration units must not be bundled merely to save
simulation time.

## Migration and Compatibility

- Keep all exported function names and current `test_type` values.
- Add a deterministic mapping test for every supported method.
- Preserve numeric scores and component fields.
- Treat categorical-label suppression as an intentional correctness change.
- Update print, summary, narrative, README, help, vignette, manuscript, and NEWS
  wording together.
- If serialized result compatibility matters, tolerate old objects that lack
  calibration metadata and print an explicit legacy-status message rather
  than silently assigning cutoffs.
- Do not delete historical simulation artifacts or rewrite their hashes.

## Failure Handling

- Missing registry entries resolve to `uncalibrated`.
- Duplicate method-level keys are a registry validation error.
- Cutoffs on an unvalidated or inapplicable row are a registry validation
  error.
- Unknown public `test_type` values continue to fail through normal argument
  validation.
- Unsupported-condition evaluation failures suppress labels and attach a
  diagnostic reason; they never default to Welch.
- Publication tooling must retain attempted, completed, failed, and excluded
  counts and fail when assembly or hashing commands fail.

## Testing and Verification

Tests must cover:

- exact mapping from every public method to its calibration unit;
- absence of the generic `two_sample` calibration key;
- narrow positive and negative applicability cases for Welch;
- label suppression for every uncalibrated calibration unit;
- numeric score and component preservation after suppression;
- no Welch fallback for paired, rank, binary, model, survival, or TOST paths;
- print, summary, and narrative behavior for calibrated, uncalibrated,
  inapplicable, and legacy result objects;
- registry uniqueness, status/cutoff consistency, version, provenance, and
  supported-condition validation;
- preservation and clear archival status of Task 15 artifacts;
- refreshed package documentation and checks for stale broad-family claims.

Final verification includes focused calibration tests, the full `testthat`
suite, regenerated roxygen documentation, vignette build, source-package build,
and `R CMD check --as-cran`.

## Release Boundary

The taxonomy release is complete when:

- `two_sample` is absent as an active calibration method or registry key;
- every public analysis resolves to an explicit calibration unit;
- only applicable `welch_unpaired` results can receive the existing bands;
- all other combinations retain scores and components but suppress labels;
- Task 15 artifacts are preserved as historical, non-active evidence;
- documentation states the scope accurately;
- package tests and checks pass.

No new method needs to become calibrated for this release. The first follow-up
project is the independent `lm_ancova` calibration.
