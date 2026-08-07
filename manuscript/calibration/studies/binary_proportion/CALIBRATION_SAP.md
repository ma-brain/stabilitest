# Binary-Proportion Calibration Statistical Analysis Plan (Phase 1: `fisher_exact`)

**Calibration unit:** `fisher_exact`
**Family:** `binary_proportion`
**Study path:** `manuscript/calibration/studies/binary_proportion/`
**Status:** Phase 1, Gate A pre-production. The active package registry row for
`fisher_exact` remains `uncalibrated` until Gate B (human-approved) activates
it. Numeric scores and all component metrics are retained for every
proportion result; categorical labels are suppressed until this exact unit is
validated.

This plan is frozen verbatim from
`docs/plans/2026-08-06-proportions-calibration.md` (frozen constants table) and
`docs/plans/2026-08-06-proportions-calibration-design.md` (authoritative
design). No constant, gate, seed, or weight may be altered after freeze. A
failed gate means: publish the fail-closed artifact exactly as this plan says,
commit, STOP, report.

## Scope

Bands apply only to eligible significant canonical two-arm `fisher_exact`
results: individual-level 0/1 binary input, two arms, complete cases, a
significant conclusion, `alpha = 0.05`, `n_boot = 1000`, `max_removal_pct =
0.30`, and the frozen jackknife-light weights `jackknife = 0`,
`fragility = 0.5`, `bootstrap = 0.5`. Phase 2/3 units (`chi_square_2x2`,
`two_sample_prop`) are deferred to their own studies and never appear here.

The primary claim (Track A'') is a two-band Fragile / Not-fragile decision at
a single integer cutoff L. Track D' (replication-probability curve) is the
fallback; its draws are collected during training regardless of whether
Track A'' proceeds.

## Justification for the clear-power 0.95 truth target

The 0.95 clear-power choice is justified up front by committed analytic
evidence, not chosen after a failed 0.90 run:
`manuscript/calibration/studies/binary_proportion/tools/feasibility-projection-power090.R`
(projected Not-fragile identification RI 0.59-0.70 at the FR-safe cutoff for a
p-monotone score at clear power 0.90 - **infeasible**, repeating the ANCOVA v1
failure) and `...-power095.R` (RI 0.70-0.89, all 12 cells pass; typical ~0.77;
only n=25, p0=0.25 knife-edge at 0.702 - **feasible with margin**). The
projection is exact (no simulation): the entire Fisher p-value distribution is
enumerable over the 2x2 table grid.

## Frozen constants

| Constant | Value |
| --- | --- |
| Unit (Phase 1) | `fisher_exact`; study path `manuscript/calibration/studies/binary_proportion/` |
| Score weights | `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`; v1 comparator `0.4/0.4/0.2` archived, never fitted |
| Training grid | n/arm {25, 50, 100, 200} x p0 {0.10, 0.25, 0.50} x truth {null, borderline, clear} = 36 scenarios |
| Held-out grid | n/arm {35, 75, 150} x p0 {0.15, 0.40} x same truth classes = 18 scenarios |
| Truth targets | null exact; borderline exact power 0.60 (diagnostic only); clear exact power 0.95 - solved against the *enumerated exact* power of `fisher.test`, not an approximation |
| Stress rows | 2:1 allocation; p0 = 0.03; 5% misclassification; beta-binomial overdispersion (rho 0.10); missing outcomes (10%) - diagnostic only, never fit/accept |
| Seeds | training 61001+, validation 62001+, stress 63001+; masters (power / replication / cluster bootstrap) 20260808 |
| Screening | significant-only; >= 100 significant completed per required scenario; failures <= 5%; `n_boot = 1000`; `max_removal_pct = 0.30`; alpha = 0.05 |

Effect direction is active-benefit (active arm has the higher event rate);
`effect_direction = 1` is frozen. The active-arm probability p1 is solved so
the *enumerated exact* Fisher power equals the frozen target (tolerance 1e-6 on
the achieved power), reusing the projection-script enumeration. Per-arm n is the
nominal 1:1 arm size; allocation fractions the total into
`n_active = round(2 * n_per_arm * allocation)` (matching the lm_ancova generator
semantics).

## Adapter and comparator conventions

The adapter screening decision equals a direct `stats::fisher.test` on the
generated 2x2 table (p, conclusion to 1e-12). The robustness path calls
`stabilitest::robustness_analysis(..., test_type = "fisher", weights =
c(jackknife = 0, fragility = 0.5, bootstrap = 0.5))`. A v1-weight
(`0.4/0.4/0.2`) comparator score is recomputed from each result's component
metrics and archived; it is never the fitted score and never emits a label.

**Walsh event-flip fragility index (frozen convention):** the smaller-event arm
is identified; non-events in that arm are flipped `0 -> 1` one at a time
(adding events, shrinking the between-arm disparity) until the Fisher exact
p-value reaches `>= alpha`. The index is the count of flips required; `0` if
already non-significant. This matches the literature Walsh FI event-flip
definition for 2x2 tables. It is archived per replicate for comparison with the
removal-based fragility component; it never enters the score or any gate.

## Replication draws (Track D')

One primary-test-only replicate per completed significant row: regenerate the
scenario's data at the same frozen truth and record the Fisher p-value and
conclusion (no robustness score). The seed stream is dedicated (master
20260808) and disjoint from the screening replicate and bootstrap seed columns.
Replication draws are collected during training regardless of whether Track A''
proceeds, so the Track D' curve can be fitted on the same training rows.

## Pilot gate (sealed, score-only)

Before any production compute, a sealed score-only pilot computes the
feasibility-projection metric on pilot scores: the smallest FR-safe integer L
(FR <= 0.05, Wilson upper <= 0.10), and the projected
RI = P(score > L | clear, significant).

| Outcome | Rule |
| --- | --- |
| Go | projected RI >= 0.72 |
| Marginal | 0.70 <= projected RI < 0.72 -> STOP for human decision |
| Hard no-go | projected RI < 0.70 -> Track A'' ends; await approval to proceed with Track D' only |

A pilot no-go does not loosen any gate. Location, overlap, and AUC are archived
as diagnostics only. The pilot gate outcome is committed (machine-readable
`SCORE_PILOT_GATE.json` + diagnostics) before production begins.

## Track A'' cutoff search and acceptance

Integer L in {0...100}; Fragile iff score <= L; Not-fragile iff score > L.

Training gates (all required):

- FR <= 0.05 with Wilson upper <= 0.10
- RI >= 0.70 with Wilson lower >= 0.60
- Deterministic tie-breaks: highest RI, then FR safety margin, then smallest L

The candidate hash is frozen and committed before held-out is opened.

Held-out acceptance: the same operating characteristics, taking the more
conservative of the Wilson and scenario-cluster bootstrap bounds (seed 20260808,
B = 1000). Held-out is evaluated once, with no refit and no second candidate.

## Track D' (fallback) gates

Logistic `replication ~ score`; held-out calibration:

- intercept |logit| <= 0.20
- slope in [0.85, 1.15]
- max abs error over 10 equal-count bins <= 0.10 (conservative bounds)
- Brier(score) - Brier(p-only reference) <= 0.01

If Track A'' fails its pilot, Tasks 9-10 still run; Task 9's fitting step then
fits only the replication curve.

## Runtime eligibility (fail-closed profile)

A `prop_calibration_profile()` (`version = "prop-profile-1"`) is recorded on
every two-sample result. A label is returned **only** when all of the following
hold (exact numbers frozen above):

- resolved unit `fisher_exact`; two-arm individual-level binary; complete cases; significant conclusion;
- per-arm n within [25, 200]; allocation ratio within [0.8, 1.25];
- observed control-arm event rate within [0.08, 0.55], with >= 3 events and >= 3 non-events in the control arm (transportability guard for the calibrated p0 range);
- alpha = 0.05, `n_boot = 1000`, `max_removal_pct = 0.30`, and the frozen jackknife-light weights explicitly supplied.

Everything else: numeric score + components, label `NA`.

**Note for Gate B:** because the calibrated weights (`0/0.5/0.5`) differ from
the current package default (`0.4/0.4/0.2`), label emission requires the
explicit v2-style weights. Whether to introduce per-method default weights is a
Gate B decision, not a study decision.

## Case-study dataset freeze discipline

`onc_response_trial` is a prospectively frozen synthetic two-arm oncology
responder trial (60/arm, binary objective response, generator seed `20260809L`,
generator rates control response 0.20 / active 0.45). It is frozen in Task 7
**before** any p-value, score, or band is inspected, with plausibility contract
tests first and a single unconditional draw committed before analysis. It is
never regenerated for nicer results. Its seed `20260809L` and IDs appear in no
calibration ledger.

The manuscript case study follows, rather than precedes, the calibration
results (methods -> calibration results -> case study).

## Phase 2/3 sequencing

`chi_square_2x2` is calibrated by the same recipe only after the Phase 1
`fisher_exact` decision publishes; `two_sample_prop` is third. One unit at a
time; no shared cutoff.

## Fail closed

Any failed gate => publish `uncalibrated`, held-out untouched/unrepeated,
labels stay `NA`. No frozen number, gate, seed, or weight is changed to make a
gate pass.

## Provenance

New study tree with independent SAP, manifests, seed ledgers, hash ledger. No
writes to Welch artifacts, lm_ancova v1/v2/v3 material, `pain_ancova_trial`,
or the active registry (until Gate B of this study, and then only
`fisher_exact` with human approval).
