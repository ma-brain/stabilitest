# Prospective Welch Calibration Statistical Analysis Plan, Relaxed-Gates Study

**Study version:** prospective Welch study 2026-3 (`welch-2026-3`)  
**Calibration unit:** `welch_unpaired`  
**Status:** FROZEN as of the commit that introduces this status line, on
2026-10-25. It is binding together with `config/scenarios_2026_3.R` and
`R/thresholds_2026_3.R` as merged in `a961efd`, and it precedes any 2026-3
score. After this commit no score-dependent change to a gate, truth
definition, scenario, weight, seed, or candidate rule is permitted.  
**Design document:** `docs/plans/2026-10-25-welch-relaxed-gates-design.md`  
**Predecessor:** `CALIBRATION_SAP.md` (study 2026-2). That plan and its
artifacts are frozen and must not be edited. This plan amends it by
replacement for the 2026-3 study only.

This plan governs one prospective, method-specific calibration attempt with a
weaker, explicitly disclosed claim than study 2026-2. Study 2026-2 ended
`no_feasible_thresholds` with validation never opened. Its training rows were
re-examined to inform the gates below; that is disclosed as an analyst degree
of freedom. Those rows are design inputs and comparators only and are not
training or validation evidence here. After this document is committed, no
score-dependent change to a gate, truth definition, scenario, weight, seed, or
candidate rule is permitted.

## Differences from study 2026-2

Everything in `CALIBRATION_SAP.md` applies unchanged unless a section below
replaces it. The replaced sections are: study version, seeds, candidate
mappings, frozen gates, selection rule, the p-value baseline (new), and
published outcomes. Calibration unit, runtime eligibility profile, truth
definitions, the six-archetype core registry, stress rows, conditional
sampling, quotas, and failure reporting are carried over verbatim.

## Calibration unit and runtime claim

Unchanged from 2026-2: `welch_unpaired`, mean-difference endpoint, significant
superiority conclusion at `alpha = 0.05`, weights `(0.4, 0.4, 0.2)`,
`n_boot = 1000`, `max_removal_pct = 0.30`, complete-case independent groups,
per-group sizes 25 through 200, allocation ratio 2/3 through 3/2.

The claim is a single upper-band label, `Stable`, for scores at or above a
cutoff `U`. Scores below `U` receive no categorical label. There is no
`Fragile`, `Moderate`, `Robust`, or `Not fragile` label in this study. `Stable`
counts as reassurance for the false-reassurance gate. It is not a probability
of replication and may not be described as certifying the finding.

## Seeds and identities

All 2026-3 streams are disjoint from 2026-2 and from each other.

| Stream | ID prefix | First seed |
| --- | --- | ---: |
| Power verification master | n/a | 202750001 |
| Training core | `WTR3-` | 202710001 |
| Validation core | `WVA3-` | 202720001 |
| Stress diagnostics | `WST3-` | 202730001 |
| Cluster bootstrap | n/a | 202740001 |
| p-only baseline bootstrap | n/a | 202760001 |

Scenario identities, order, sizes, standard deviations, and truth definitions
are those of 2026-2 (archetypes W01-W06, null / borderline 0.60 power / clear
0.95 power). Non-null deltas are solved by the committed power solver, with
verification tolerance 0.03 and 2,000 verification draws per core row.

## Conditional sampling and quotas

Unchanged: only significant draws (two-sided Welch `p < 0.05`) get the full
robustness analysis. Each required scenario must reach 250 completed
significant analyses within the 20,000-draw cap, and its full-analysis failure
rate must be at most 5% of screened-significant draws.

## Candidate mappings

Training compares exactly:

1. a single-cutoff mapping (`score >= U`: `Stable`; otherwise unlabelled),
   searched over integer `U` from 50 through 90;
2. no categorical mapping.

Three-band and two-band Fragile mappings are not searched. The historical
55/70 mapping is reported as a named comparator on the same rows and receives
no preference.

## Frozen gates

A cutoff `U` is **gate-passing** on a data set when all of the following hold
there:

- false reassurance among completed significant null rows, the share with
  `score >= U`: point estimate at most 0.10, and conservative upper bound at
  most 0.15;
- clear identification among completed significant clear rows, the share with
  `score >= U`: point estimate at least 0.60, and conservative lower bound at
  least 0.50;
- pooled medians ordered `null <= borderline <= clear`;
- within each core archetype, no material reversal (null median above
  borderline by more than 5 points, or borderline above clear by more than
  5 points);
- every required scenario meets quota and has failure rate at most 5%;
- FR at most 0.25 in every core archetype (guard below);
- the `Stable` band holds at least 5% of all fitting rows.

Balanced ordinal accuracy is not applicable to a single-band mapping.

Point estimates use pooled completed rows. Wilson bounds are one-sided 95%.
Scenario-cluster bounds resample whole scenario cells with replacement using
seed 202740001 and 1,000 draws. For upper-limit gates the larger of the Wilson
and cluster bounds governs; for lower-limit gates the smaller governs.

### Per-archetype false-reassurance guard

Pooled FR can hide a small-sample archetype. On 2026-2 training rows at
`U = 65`, FR by scenario was 0.188 (W01, n = 25/25), 0.084, 0.016, 0.008,
0.048, 0.056. Because the runtime eligibility profile covers n = 25 per group,
a cutoff is gate-passing only if its FR is at most 0.25 in every core
archetype (null rows only). Per-archetype FR and RI are also published with
the verdict. The 0.25 limit is a coarse guard against a single archetype
dominating, not a calibration target.

## Cutoff selection rule

On training rows:

1. A cutoff `U` is **eligible** only if every integer in `[U - 2, U + 2]` is
   gate-passing on training, using the training-only gates above. Cutoffs with
   neighbours outside 50-90 are ineligible.
2. Among eligible cutoffs, choose the one with the lowest training false
   reassurance. Ties go to higher clear identification, then to the lower `U`.
3. If no cutoff is eligible, the result is exactly
   `status = "no_feasible_thresholds"`. No nearest failure is substituted.
   Validation stays unopened.

## p-value baseline (new gate)

On the same training rows, fit a p-only comparator: label `p <= q`, with `q`
searched over the fixed grid {0.04, 0.03, 0.02, 0.01, 0.005, 0.002, 0.001,
0.0005, 0.0002, 0.0001}. The comparator uses the same gates, the per-archetype
guard, and the tie rules as the score candidate, with the neighbourhood taken
as two adjacent grid steps. Among eligible grid points choose the one whose
training FR is closest to the selected score candidate's training FR (ties go
to the smaller `q`). If the score candidate has no eligible cutoff the study is
already fail-closed and no comparator is needed.

Both frozen rules are evaluated once on validation. The score candidate
**beats the baseline** when its validation clear identification exceeds the
comparator's by at least 0.02 and the one-sided 95% lower bound of that
difference is at least 0.00. The bound comes from a paired scenario-cluster
bootstrap (seed 202760001, 1,000 draws). The comparison is made at the frozen
cutoffs; no rematching is done on validation. If the comparator's validation FR
exceeds the score's by more than 0.02, the comparison is reported but the
score is not penalised for it.

If the score candidate is eligible but the comparator has no eligible grid
point (status `no_eligible_comparator`), the baseline gate cannot be met and
the study closes fail-closed with reason `p_comparator_unavailable`. This is
known from training alone, so it is final at the freeze: validation stays
unopened.

If the score fails to beat the baseline, the study closes fail-closed. The
label is **not** published as a p-value restatement.

## Freeze and held-out confirmation

The selected candidate, the p-only comparator, or the no-candidate decision is
serialized and hashed with SHA-256 together with this SAP, resolved scenarios,
generator, adapter, threshold code, training data, and package version. Training
summaries, occupancy, failures, diagnostics, hashes, and manifest are committed
before validation can be selected or opened.

Opening validation requires explicit human permission recorded in the commit
history after the freeze commit. Validation is evaluated once. No threshold
refit, weight search, gate change, scenario removal, or repeat is permitted.
Held-out acceptance applies every gate above to validation rows using the
conservative bounds, plus the p-value baseline comparison.

## Power and precision check for the bound gates

Pre-registered, training-data-free design check. With 6 archetypes the
scenario-cluster bootstrap resamples only 6 clusters, so cluster bounds are
wide and can bind. As a feasibility plausibility check only (not part of any
gate), the 2026-2 training rows give, at the following cutoffs:

| `U` | FR point | FR cluster upper | RI point | RI cluster lower |
| ---: | ---: | ---: | ---: | ---: |
| 63 | 0.096 | 0.145 | 0.774 | 0.701 |
| 65 | 0.067 | 0.112 | 0.713 | 0.615 |
| 67 | 0.041 | 0.070 | 0.631 | 0.505 |

Re-running the frozen fitting code on those rows (cluster B = 200) shows the
neighbourhood rule is the binding constraint: cutoffs 64-67 pass every gate,
63 fails the FR upper bound (0.155 against 0.15), and 68 fails the RI point
gate. Four consecutive passing cutoffs are one short of the five that
`[U - 2, U + 2]` requires, so the 2026-2 training rows would give
`no_feasible_thresholds`. Fresh data may differ either way. This is a
pre-registered acceptable outcome, not a reason to change the rule. The core registry and 250-rows-per-cell quota are kept at
2026-2 values so results stay comparable. Increasing archetypes would tighten
the cluster bounds but changes the registry and is deferred.

## Published outcomes

- **Pass:** held-out passes every gate and beats the p-value baseline. Publish
  version `welch-2026-3` with the cutoff `U`, the runtime eligibility profile,
  held-out estimates and bounds, per-archetype results, failure counts, and
  provenance. Runtime activation is a separate, human-approved step.
- **Fail:** any other result, including a training `no_feasible_thresholds`.
  Publish an `uncalibrated` registry proposal with no cutoffs, suppress Welch
  labels, retain numeric scores and components, and report the negative result.

A training `no_feasible_thresholds` is final for this attempt: validation stays
unopened, and the fail outcome proceeds after human acknowledgement. No failed
gate starts another attempt inside this study.

## Implementation consequences (not part of the freeze)

These need code changes before training can start and are done only after this
plan is approved:

- add a 2026-3 gate-constants set (FR 0.10 / 0.15, RI 0.60 / 0.50,
  neighbourhood 2) beside, not replacing, `WELCH_GATE_CONSTANTS`;
- add a single-cutoff search with the neighbourhood rule and the p-only
  comparator to `R/thresholds.R`;
- add the 2026-3 scenario config (IDs `WTR3-`, `WVA3-`, `WST3-`, seeds above);
- write outputs to a versioned subtree so 2026-2 artifacts are never
  overwritten;
- update `tools/check-calibration-documentation.R` expectations and tests.
