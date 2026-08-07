# Binary-Proportion (`fisher_exact`) Calibration — Final Report (Task 12)

**Branch:** `codex/proportions-calibration`
**Decision:** validated two-band Fragile / Not-fragile at cutoff **L = 58**
**Candidate hash:** `cc334493161406ab6c23c0c457445722`
**Published:** `manuscript/calibration/studies/binary_proportion/published/`
(14 atomic artifacts + hash ledger)

## Outcome summary

| Gate | Result |
| --- | --- |
| Analytic projection (Task 0) | 0.90 infeasible / 0.95 all-pass — clear power 0.95 frozen up front |
| Sealed pilot gate (Task 8) | **GO** — projected RI 0.7250 (lower 0.7033) at FR-safe L=60 |
| Power gate (Task 8) | PASS — achieved 0.946–0.954 vs target 0.95 |
| Track A″ training (Task 9) | **candidate** L=58; FR 0.0493 (upper 0.0607), RI 0.7608 (lower 0.74) |
| Held-out validation (Task 10) | **validated_method_specific**; FR conservative upper 0.0617, RI conservative lower 0.6767 |
| Active runtime registry | **untouched** — `fisher_exact` stays `uncalibrated` (Gate B is separate human-approved work) |

## Occupancy

- Training: 3596 significant completed (borderline 1200, clear 1200, null 1196)
- Held-out: 1800 significant completed (600 per truth class)
- Replication draws: 3596 collected (Track D′ curve fit)

## Commit log (14 commits, `main..codex/proportions-calibration`)

```
e35ca17 docs: add proportions case study vignette
65c1a56 data: publish binary proportion calibration decision
34c87b0 data: freeze binary proportion training candidates
36ba211 docs: record binary proportion pilot gate
486d290 feat: add binary proportion score pilot gate
3c882c2 fix: wire proportion runner and adapter to shared harness
3343df5 data: freeze synthetic oncology response trial
567fe96 feat: run isolated binary proportion calibration
85c154e feat: fit two-band and replication-curve proportion calibrations
c5ef7a7 docs: freeze binary proportion calibration sap
078ff2d feat: generate exact-power proportion scenarios
3040e7f feat: define binary proportion calibration study
7d49548 feat: gate proportion bands on canonical profiles
b1ee4f8 docs: add binary proportion calibration design and projection evidence
```

## Artifact paths

- Study root: `manuscript/calibration/studies/binary_proportion/`
- SAP: `CALIBRATION_SAP.md`
- Pilot gate: `artifacts/summaries/SCORE_PILOT_GATE.json`, `SCORE_PILOT_REVIEW.md`
- Candidate + training: `artifacts/summaries/{candidate,training-fit,training-manifest,training-replication-curve,candidate-diagnostics}.{rds,json}`, `training-occupancy.csv`
- Published decision (atomic): `published/{candidate,validation,registry,completed_training,completed_validation,training-manifest,validation-manifest,training-replication-curve,training-occupancy,hash_ledger,output-hashes,...}.{rds,csv,json,txt}`
- Case study: `vignettes/proportions-case-study.Rmd`, `manuscript.md`
- Frozen dataset: `data/onc_response_trial.rda`, `data-raw/onc_response_trial.R`

## Protected artifacts (verified untouched)

No commits on this branch modified: the active runtime registry
(`inst/extdata/calibration-registry.csv`), Welch calibration artifacts,
`lm_ancova` v1/v2/v3 study material, `R/robustness_models.R`, or
`pain_ancova_trial`. Raw training/validation outputs and pilot outputs stay
gitignored.

## Deviations and notes

1. **Walsh FI flip direction** — frozen as 0→1 in the smaller-event arm
   (shrinking the disparity), documented in the SAP. Approved implicitly by the
   SAP freeze approval.
2. **Null-cell sparsity** — Fisher's exact conservatism (type-I 0.009–0.040)
   makes null cells structurally sparse; the SAP justifies this as the reason
   for the 0.95 clear-power choice. Null quota is not hard-enforced (clear/
   borderline are); null occupancy is recorded honestly. The 10000-draw
   production budget collected 1196 null-significant in training, 600 in
   held-out — ample for the FR-safe cutoff.
3. **Shared-runner port** — two shared `manuscript/calibration/` fixes (study-
   runner exclusion in `.calibration_is_direct`; resume-mismatch-fatal in the
   executor) were ported from the unmerged lm_ancova branch because they are
   required to execute the study runner; the active runtime registry was not
   touched.
4. **Track D′ curve** — fit on dedicated replication draws (one per completed
   significant row); archived. Track A″ passed, so Track D′ is the documented
   fallback, not the active decision.

## What is NOT done (separate human-approved work)

Registry/runtime integration (Gate B: activating `fisher_exact` labels in the
runtime registry, weights-default policy, Phase 2 `chi_square_2x2`) is separate
human-approved work and was not begun.
