# Binary-Proportion (`fisher_exact`) Calibration — Manuscript Section

**Unit:** `fisher_exact` (Phase 1 of the `binary_proportion` family)
**Study path:** `manuscript/calibration/studies/binary_proportion/`
**Decision:** validated two-band Fragile / Not-fragile at cutoff `L = 58`
(candidate hash `cc334493161406ab6c23c0c457445722`).

## Methods

The calibration delivers a two-band Fragile / Not-fragile decision for eligible
significant canonical two-arm Fisher exact results (Fragile iff score ≤ L,
Not-fragile iff score > L). The composite score uses the jackknife-light
weights `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`; the v1
`0.4/0.4/0.2` weights are archived as a comparator and never fitted.

Truth strata are exact-power-defined against the *enumerated exact* power of
Fisher's test (no normal approximation): null exact, borderline exact power
0.60 (diagnostic only), and **clear exact power 0.95**. The 0.95 target was
chosen up front on the basis of committed analytic evidence
(`tools/feasibility-projection-power095.R`): at clear power 0.90 the projected
Not-fragile identification rate is 0.59–0.70 (infeasible — only 2 of 12 cells
pass), whereas at 0.95 all 12 cells pass (RI 0.70–0.89). The truth definition
was frozen before any failed 0.90 run.

Training grid: n/arm {25, 50, 100, 200} × control rate p₀ {0.10, 0.25, 0.50} ×
truth {null, borderline, clear}. Held-out grid: n/arm {35, 75, 150} ×
p₀ {0.15, 0.40} × same truth classes. Stress rows (2:1 allocation, rare events
p₀ = 0.03, 5% misclassification, beta-binomial overdispersion, missing
outcomes) are diagnostic only and never enter fitting or acceptance. Seeds:
training 61001+, validation 62001+, stress 63001+; power / replication /
cluster-bootstrap masters 20260808.

A sealed score-only pilot computed the feasibility-projection metric before
production: the smallest FR-safe integer L (FR ≤ 0.05, Wilson upper ≤ 0.10) and
projected RI = P(score > L | clear, significant). The pilot returned a **go**
(projected RI 0.7250, lower 0.7033) before any production compute.

Track A″ acceptance required FR ≤ 0.05 (Wilson upper ≤ 0.10) and RI ≥ 0.70
(Wilson lower ≥ 0.60); ties broken by highest RI, then FR safety margin, then
smallest L. Held-out acceptance took the more conservative of the Wilson and
scenario-cluster bootstrap bounds (seed 20260808, B = 1000), evaluated once
with no refit. A classic Walsh event-flip fragility index was archived per
replicate as a literature comparator and never entered the score.

## Calibration results

Training (clean checkout) collected 3596 significant completed replicates
across the 36 core scenarios (borderline 1200, clear 1200, null 1196); the
null stratum, sparse under Fisher's exact conservatism (enumerated type-I
0.009–0.040), reached 1196 with the production draw budget. Replication draws
(one primary-test-only replicate per completed significant row) were collected
for the Track D′ replication-probability curve regardless of the Track A″
outcome.

The Track A″ fitter selected cutoff **L = 58** on training: FR 0.0493 (Wilson
upper 0.0607), RI 0.7608 (Wilson lower 0.74). The frozen candidate hash was
committed before held-out was opened.

Held-out validation (18 scenarios, 600 significant completed per truth class)
confirmed the decision under the conservative-bound rule: FR conservative upper
0.0617 (≤ 0.10, the cluster bootstrap is the binding bound) and RI conservative
lower 0.6767 (≥ 0.60). The published decision is
`validated_method_specific`.

## Case study

The illustrative case study uses the prospectively frozen synthetic oncology
responder trial `onc_response_trial` (60/arm, seed `20260809`), analysed once
with the frozen configuration after the calibration was published. The primary
Fisher exact test is **not significant** (p = 0.137; active response rate 0.48
vs control 0.33). The published calibration correctly fail-closes on this
non-significant conclusion: `status = bands_not_applicable`, label `NA`, while
numeric scores (overall 39.1; jackknife 100, removal fragility k = 4,
bootstrap 67.4) and a Walsh event-flip FI of 0 remain fully reported. The case
study is reported exactly as the single frozen analysis produced it; the
dataset was never regenerated or reselected.
