# Binary-Proportion Family Calibration Design (`fisher_exact` first)

**Date:** 2026-08-06
**Status:** proposed — pending review
**Family:** `binary_proportion` (registry units `fisher_exact`,
`chi_square_2x2`, `two_sample_prop` — all currently `uncalibrated`)
**Study path:** `manuscript/calibration/studies/binary_proportion/`
**Precedents:** Welch calibration (active);
`lm_ancova` v1 fail-closed + v2 analytic no-go + v3 re-aim
(`docs/plans/2026-08-06-lm-ancova-*.md`, worktree branch
`codex/lm-ancova-calibration`).

## Why this family can succeed where ANCOVA v1 failed

The ANCOVA post-mortem established the failure mechanism: the composite score
is a near-monotone transform of the p-value, and the band gates
(FR ≤ 0.05, RI ≥ 0.70) implicitly require null/clear separation that
power-0.90 truth classes cannot supply (required AUC ≈ 0.94 vs ≈ 0.89
delivered). The corrected discipline is to compute the **feasibility
projection analytically before designing any study**.

For 2×2 tables this projection is *exact* — the entire p-value distribution is
enumerable with no simulation. It was computed on 2026-08-06 (committed
scripts: `manuscript/calibration/studies/binary_proportion/tools/
feasibility-projection-power090.R` and `feasibility-projection-power095.R`),
projecting the Not-fragile identification rate (RI) at the FR-safe threshold
for a p-monotone score, balanced arms, α = 0.05:

| Clear power | Fisher exact | χ² (corrected) | Verdict |
| --- | --- | --- | --- |
| 0.90 | RI 0.59–0.70 across n ∈ {25…200}/arm, p₀ ∈ {0.10, 0.25, 0.50} (passes only two small-n/rare-event corners) | RI 0.64–0.77 (mostly fails) | **Infeasible** — a 0.90 attempt would repeat the ANCOVA v1 failure |
| **0.95** | **RI 0.70–0.89, all 12 cells pass** (typical ≈ 0.77; only n = 25, p₀ = 0.25 is knife-edge at 0.702) | **RI 0.74–0.84, all cells pass** | **Feasible with margin** |

Two structural reasons the proportions family differs from ANCOVA: Fisher's
conservatism (exact type-I error 0.009–0.040, so the significant-null stratum
is thinner and less overlapping) and the discreteness of the p-value atoms.

**Therefore the truth definition is frozen up front at clear power 0.95,
justified by committed analytic evidence — not chosen after a failed 0.90
run.** At 0.95 the "clear" effects are large but not vacuous (e.g.,
p₀ = 0.25 → p₁ ≈ 0.50 at n = 100/arm), unlike the 0.99 escalation that was
rejected for ANCOVA.

### Honest caveat carried into the pilot gate

The projection assumes the score is perfectly monotone in p. Empirically
(ANCOVA v1 data) score-based RI ran a few points below the p-banding
projection at small n, and binary-data ties broken by resampling noise may
loosen the coupling further. The margin (~0.07 typical) is real but must be
confirmed on actual scores by the **sealed feasibility-projection pilot
metric** (v3 design's corrected gate: projected RI at the FR-safe cutoff,
go ≥ 0.72) before any production compute. A pilot no-go falls back to
Track D′ below; it does not loosen gates.

## Decision (proposed for lock)

| Element | Choice |
| --- | --- |
| Primary claim (Track A″) | **Fragile vs Not fragile** (two bands, single cutoff L) for eligible significant canonical two-arm `fisher_exact` results |
| Second unit (Phase 2) | `chi_square_2x2` by the same recipe after the Fisher decision publishes; `two_sample_prop` third. One unit at a time; no shared cutoff |
| Score | Jackknife-light composite, `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0` (v2/v3 policy); v1 weights `0.4/0.4/0.2` archived as comparator, never fitted |
| Truth strata | null (p₁ = p₀) vs clear (exact power 0.95); borderline (0.60) diagnostic only |
| Pilot gate | Sealed score-only pilot; primary metric = projected RI at FR-safe L; go ≥ 0.72, hard no-go < 0.70; location/overlap/AUC archived as diagnostics only |
| Fallback (Track D′) | Replication-probability curve with the v3 Track D gates, fitted on the same production rows — replication draws are collected during training either way |
| Extra comparator | Classic Walsh event-flip fragility index archived per replicate (literature + `fragility` CRAN package linkage); never in the score |
| Fail closed | Any failed gate ⇒ publish `uncalibrated`, held-out untouched/unrepeated, labels stay `NA` |
| Vignette | Separate `vignettes/proportions-case-study.Rmd` on a prospectively frozen synthetic responder-trial dataset (below) |

## Scenario and generator policy

- Generator: individual-level binary 0/1 vectors per arm (matching the
  engine's input contract), exact `rbinom` draws; effect solved per cell so
  the *exact enumerated* power of the target test equals the frozen target
  (no normal approximation).
- Training grid: n/arm ∈ {25, 50, 100, 200} × control rate p₀ ∈
  {0.10, 0.25, 0.50} × truth {null, borderline, clear} = 36 scenarios.
- Held-out grid: n/arm ∈ {35, 75, 150} × p₀ ∈ {0.15, 0.40} × same truth
  classes = 18 scenarios.
- Stress (diagnostic only, never fit/accept): 2:1 allocation, rare events
  (p₀ = 0.03), 5% outcome misclassification, beta-binomial overdispersion,
  missing outcomes.
- Screening significant-only; quota ≥ 100 significant completed per required
  scenario; failure limit ≤ 5%; `n_boot = 1000`; `max_removal_pct = 0.30`;
  workers ≤ 4.
- Seeds: training `61001+`, validation `62001+`, stress `63001+`;
  power/replication/cluster-bootstrap master `20260808`. All disjoint from
  Welch, lm_ancova v1/v2/v3, and `pain_ancova_trial` ledgers.

## Cutoff search and acceptance (Track A″)

Identical machinery to the v2/v3 lineage: integer L ∈ {0…100}; Fragile iff
score ≤ L; training requires FR ≤ 0.05 (Wilson upper ≤ 0.10) and RI ≥ 0.70
(Wilson lower ≥ 0.60); deterministic tie-breaks (highest RI, then FR safety
margin, then smallest L); freeze candidate hash before held-out; evaluate
held-out once, no refit, more conservative of Wilson and scenario-cluster
bootstrap bounds (seed 20260808, B = 1000); no second candidate.

## Runtime eligibility (fail-closed profile)

Mirror the `lm_calibration_profile()` pattern: a
`prop_calibration_profile()` recorded on every proportions result, and a
resolver that returns a label **only** when all hold (exact numbers frozen in
the SAP):

- resolved unit `fisher_exact`; two-arm individual-level binary; complete
  cases; significant conclusion;
- per-arm n within [25, 200]; allocation ratio within [0.8, 1.25];
- observed control-arm event rate within [0.08, 0.55] and ≥ 3 events and
  ≥ 3 non-events in the control arm (transportability guard for the
  calibrated p₀ range);
- α = 0.05, `n_boot = 1000`, `max_removal_pct = 0.30`, and the frozen
  jackknife-light weights explicitly supplied.

Everything else: numeric score + components, label `NA`. Note for Gate B:
because the calibrated weights differ from the current package default
(0.4/0.4/0.2), label emission requires the explicit v2-style weights; whether
to introduce per-method default weights is a Gate B decision, not a study
decision.

## Vignette and frozen case-study dataset

- New packaged dataset `onc_response_trial`: synthetic two-arm oncology
  responder trial, 60 patients/arm, binary objective response, generator and
  seed frozen in `data-raw/onc_response_trial.R` **before any p-value, score,
  or band is inspected** (exact `pain_ancova_trial` discipline: plausibility
  contract tests first, single unconditional draw, committed before analysis;
  never regenerated for nicer results; ID/seed excluded from every
  calibration ledger).
- Separate vignette `vignettes/proportions-case-study.Rmd`: primary Fisher
  analysis, full component walk-through, Walsh event-flip FI comparison,
  and honest label rendering — validated band, replication estimate, or
  numeric-only, whatever the published decision supports.
- Manuscript section ordering follows the ANCOVA rule: methods → calibration
  results → case study.

## Provenance rules

- New study tree with independent SAP, manifests, seed ledgers, hash ledger.
- No writes to: Welch artifacts, lm_ancova v1/v2/v3 material,
  `pain_ancova_trial`, or the active registry (until Gate B of this study).
- Registry updates at Gate B touch only the `fisher_exact` row (then
  `chi_square_2x2`, `two_sample_prop` in their own phases).

## Success criteria

- Committed analytic projection cited as the pre-registered basis for the
  0.95 clear-power choice.
- Sealed pilot go/no-go recorded before production; corrected metric only.
- Either a validated two-band `fisher_exact` publication with full ledger, or
  an explicit fail-closed result plus the Track D′ replication-curve decision.
- Replication draws collected during training regardless of track.
- `onc_response_trial` frozen before inspection; vignette renders honestly
  under the published policy.
- Phase 2/3 units proceed only after the Phase 1 decision publishes.
