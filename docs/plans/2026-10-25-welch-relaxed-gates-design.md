# Welch Relaxed-Gates Calibration Protocol (welch-2026-3)

**Date:** 2026-10-25

**Status:** Draft, awaiting human approval. Nothing in this document authorizes
generating data, opening held-out data, or changing the runtime registry.

**Supersedes:** nothing. `welch-2026-2` stays closed and fail-closed
(`no_feasible_thresholds`, held-out never opened). This is a new study with a
new version label, new random streams, and a deliberately weaker claim.

## Purpose

`welch-2026-2` found no cutoff meeting the project's standard gates
(false reassurance, FR, at most 5% and clear-effect identification, RI, at
least 70%). This protocol asks a different, narrower question: is there a
single score cutoff that earns a *weaker, explicitly disclosed* label for the
significant, unpaired Welch configuration? If not, Welch stays numeric-only
and the negative result is reported twice (2026-2, 2026-3).

## What the 2026-2 training data show (design input only)

Exploratory re-analysis of the 4,500 training rows, held-out untouched:

| Quantity | Value |
|---|---|
| AUC, clear vs null (significant results) | 0.911 |
| Smallest cutoff with FR <= 5% | 67 (RI = 0.63; bootstrap range of cutoff 66-67) |
| Standard gates (FR <= 5%, RI >= 70%) | no feasible cutoff |
| FR <= 10% and RI >= 60% | cutoffs 63-67 |
| FR / RI at 65 | 0.067 / 0.713 |
| FR / RI at 60 | 0.191 / 0.859 |

The infeasibility is a property of the score, not of cutoff precision: the
component scores are largely a re-expression of the p-value (see the ANCOVA
negative result). A tolerance on the cutoff cannot close the RI gap at FR 5%.
Only a weaker claim can be feasible.

These figures informed the gates below. That is an analyst degree of freedom and
is disclosed in the article. It is mitigated, not removed, by fresh data, fresh
seeds, and gates frozen before any new row exists.

## Rationale for relaxing the gates

The relaxation must be justified by what the label means to a user, not by what
the data allow. The wording below is the only justification the article may use:

- The label does **not** certify a finding. It marks a significant Welch result
  whose score is high enough that, in calibration, at most about 1 in 10
  significant true-null results reached it.
- Users still see the numeric score, all components, and the p-value. The label
  only summarizes the upper end.
- Labelling only the upper band is the conservative direction. Scores below the
  cutoff are *unlabelled*, not "Fragile".

## Claim and label semantics

- **One active band.** Score >= U gets the label `Stable` (final wording is
  fixed in the SAP amendment). Score < U gets no categorical label.
- **No "Fragile" claim.** A lower label would be a second assurance-type claim
  needing its own gate. Out of scope.
- **Eligibility profile.** Exact runtime configuration of 2026-2: resolved
  method `welch_unpaired`, mean difference, significant at `alpha = 0.05`,
  weights `(0.4, 0.4, 0.2)`, `max_removal_pct = 0.30`, `n_boot = 1000`,
  independent groups, plus a group-size / allocation-ratio range computed from
  runtime-observable quantities only.
- **Fail-closed otherwise.** Non-significant, ineligible, or non-default
  configurations remain numeric-only.
- `Stable` counts as reassurance for the FR gate. The documentation must state
  the calibrated FR bound next to the label and must not describe it as a
  probability of replication.

## Frozen acceptance gates

All evaluated on held-out significant replicates, once.

| Gate | Standard (2026-2 and Fisher) | This protocol |
|---|---|---|
| FR point estimate | <= 5% | <= 10% |
| FR conservative upper bound | <= 10% | <= 15% |
| RI point estimate | >= 70% | >= 60% |
| RI conservative lower bound | >= 60% | >= 50% |
| Balanced ordinal accuracy | >= 0.70 (3-band) | not applicable (single band) |
| Median ordering null <= borderline <= clear | required | required |
| Material reversal in stress layer | none | none |

"Conservative" is the larger of the Wilson bound and the scenario-cluster
bound, as in the existing SAP. Failures and unfilled quotas are reported.

Added gate not present before: **the label must beat the p-value baseline.**
Fit the best p-only cutoff on training with the same selection rule and
evaluate it once on held-out. If the score's held-out FR/RI does not exceed the
p-only rule's by at least 0.02 in RI at matched FR, the active label is
documented as a p-value threshold restatement or the study closes fail-closed.
Which of those two is decided and frozen in the SAP before any data exist (see
Open decisions).

## Cutoff selection rule (frozen before training)

1. Candidate cutoffs are integers 50-90.
2. A cutoff U is *eligible* only if training FR <= 10% and RI >= 60% hold at
   every integer in [U-2, U+2]. This is the neighbourhood-robustness
   requirement: it prefers cutoffs that survive Monte Carlo noise.
3. Among eligible cutoffs choose the one that minimizes training FR; ties go to
   the higher RI, then the lower U.
4. If no cutoff is eligible, the study ends fail-closed and validation is never
   opened.
5. The selected cutoff, candidate hash, and training summary are committed
   before the held-out streams are generated.

Note: on the 2026-2 data, rule 2 leaves roughly {65}, so U near 65 is the
expected outcome. That is a prediction, not a commitment.

## Scenario design

Reuse the 2026-2 core grid structure (36 core rows, sizes, allocation ratios,
variance ratios, distribution shapes) with these changes:

- New study version `welch-2026-3`; new master seeds in disjoint ranges for
  power verification, training, and validation (values reserved in Task 1 and
  recorded in the manifest).
- Training and validation scenario rows remain disjoint by `design_layer`.
- Truth classes unchanged: null, borderline (~60% power), clear (~95% power),
  verified by the same noncentral-t power check with tolerance 0.03.
- Target at least 1,500 significant replicates per truth class in training and
  in validation, so the Wilson/cluster bounds are not the binding constraint.
- Stress layer (contamination) kept, for ordering and reversal checks only.
- No scenario may be added, removed, or re-weighted after training starts.

An alternative lever, raising clear power from 95% to 99%, is **not** adopted.
It changes the truth definition rather than the claim and would make the
result incomparable to 2026-2 and Fisher.

## Verdict paths

| Outcome | Registry effect |
|---|---|
| Training: no eligible cutoff | `uncalibrated`, `no_feasible_thresholds`, held-out never opened |
| Held-out passes all gates | candidate for `welch-2026-3`, one active band |
| Held-out fails any gate | `uncalibrated`, numeric-only, reported as negative result |

Passing the held-out gates does not by itself change the runtime. Activation is
a separate, human-approved step (Gate B), including the label wording and the
documentation of the FR bound.

## Governance (from AGENTS.md invariants)

- Held-out data are not opened without explicit human permission, given only
  after the training candidate or fail-closed decision is frozen and committed.
- No threshold refit, gate change, weight search, or scenario removal after
  opening. Validation runs once.
- Welch and ANCOVA outputs remain numeric-only until the registry is
  explicitly activated. Historical 55/70 bands are not restored.
- No submission to CRAN, the R Journal, or any external service as part of this
  study.
- The article reports all three Welch outcomes honestly: the historical
  simulation, 2026-2 (fail-closed), and 2026-3.

## Implementation outline

1. SAP amendment: gates table, label wording, selection rule, p-only baseline
   comparison, seed ranges. Commit and hash before generating anything.
2. Scenario manifest and power verification for `welch-2026-3`.
3. Training run (reuse 2026-2 runner with version and seed changes).
4. Candidate fit with the frozen selection rule; commit hash or fail-closed.
5. Stop. Request human permission to open held-out data.
6. Validation, verdict, published artifacts under
   `manuscript/calibration/studies/welch_unpaired/` (new subfolder or version
   tag; do not overwrite 2026-2 artifacts).
7. Update `tools/check-calibration-documentation.R` expectations, tests, and
   `NEWS.md` only if activation is approved.

## Risks

- **Weaker label may be seen as goalpost moving.** Mitigation: disclose the
  history, keep 2026-2 published, use fresh data.
- **Score is p-monotone.** The p-only baseline gate makes this explicit.
- **FR of up to 10% is not small.** Documentation must say so next to the
  label, and the label must not read as assurance.
- **Cluster bound may bind.** The 2026-2 55/70 evaluation had a cluster upper
  bound about 3 points above Wilson. With true FR near 7-9%, the 15% upper gate
  could be tight; the scenario count should be checked in a pre-registered
  power calculation (SAP step 1).

## Open decisions for the human

1. Label wording for the single band (`Stable`, `Moderately stable`, other).
2. Gate numbers: confirm FR <= 10% / upper <= 15% and RI >= 60% / lower >= 50%,
   or choose others. They must be fixed before step 2.
3. If the score fails the p-only baseline: publish the label as a p-value
   restatement, or close fail-closed? Recommended: close fail-closed.
4. Whether to apply the same neighbourhood-robustness rule to the Fisher
   cutoff as a documented amendment (separate decision, not part of this
   study).
