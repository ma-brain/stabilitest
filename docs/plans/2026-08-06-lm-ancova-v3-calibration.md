# LM/ANCOVA v3 Implementation Plan — Phase 1 "v3-lite" (Tracks F + E); Track D parked

> **For the executing agent:** Work strictly task-by-task, in order. Use
> test-driven development for every code task. After each task, run the stated
> verification commands and commit with the stated message. If anything
> unexpected happens (a failing gate, a missing file, an ambiguous
> instruction), STOP and report; do not improvise a workaround. You do not
> have authority to change any frozen number, gate, seed, or policy in this
> plan or the SAP. **Phase 2 (Track D) is PARKED: do not scaffold, implement,
> or execute anything in it under any circumstances.**

**Goal (Phase 1):** Verify the v2 fail-closed publication (v2 was executed on
2026-08-06 and failed exactly as the design predicted — false-GO pilot, then
`no_feasible_thresholds`), draft the negative-result manuscript section
(Track F), and execute the pre-registered violation-detection study (Track E):
does the robustness score discriminate assumption-violated data from clean
data better than the p-value does, among significant results? This is the one
regime where the score can add information beyond p (the sufficiency argument
breaks under model violations).

**Explicitly out of scope (parked as Phase 2):** Track D — the
replication-probability curve. Rationale: the score is p-monotone, so the
curve is a recalibrated p-value; the replication-curve recipe will be built
and validated first in the binary-proportion study (its plan already collects
replication draws during training). Track D may only be un-parked by explicit
human instruction after that outcome is reviewed.

**Design (authoritative):** `docs/plans/2026-08-06-lm-ancova-v3-design.md`
(read in full before Task 0; its Findings 1–4 and the corrected pilot-gate
metric are the scientific basis). Where this plan and the design conflict on
Track E/F content, STOP and report. Where the design describes Track D
execution, Phase 2 parking in this plan takes precedence.

**Workspace:** worktree `.worktrees/lm-ancova-calibration`, branch
`codex/lm-ancova-calibration`. All paths below are relative to the worktree
root.

---

## Non-negotiable rules

1. NEVER modify: v1 published artifacts (`studies/lm_ancova/published/`),
   v2 published artifacts (`studies/lm_ancova_v2/published/`), the v1
   `lm_ancova` or v2 `lm_ancova_v2` registry rows, or `pain_ancova_trial`
   (data, generator, or seed). Both prior studies are closed and immutable.
2. NEVER reuse v1 or v2 *validation* scenarios, seeds, or replicates. v1/v2
   training rows are usable only as the already-recorded exploratory evidence
   cited in the design.
3. NEVER soften a frozen gate. A failed Track E gate is a publishable
   negative outcome — publish it, commit, report. Do not re-run with
   adjusted thresholds.
4. Do not begin Phase 2 / Track D. Do not create its files, scenarios, or
   seeds. If any task seems to require it, STOP and report.
5. Production compute: `--workers 4` maximum. Logs to
   `/tmp/stabilitest-lm-ancova-v3-logs/`, never inside the repo.
6. Commit only code, SAPs, compact summaries, manifests, registries, and
   hash ledgers. Raw/checkpoint outputs stay gitignored.
7. MANDATORY STOP POINTS (halt, output a summary, wait for explicit human
   approval): end of Task 4 (SAP freeze), inside Task 6 before the
   production run, end of Task 7 (final report).
8. No package/registry/runtime behavior changes in this plan. A confirmed
   Track E result motivates at most a *documented proposal* for a diagnostic
   flag — Gate B work under separate human approval.

## Frozen Phase 1 constants (do not alter)

| Constant | Value |
| --- | --- |
| Study unit / path | `lm_ancova_v3` under `manuscript/calibration/studies/lm_ancova_v3/` |
| Score | `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`; v1 composite `0.4/0.4/0.2` archived as comparator column, never gated on |
| Track E clean cells | `n ∈ {40, 80, 160}` × baseline `R² = 0.40` × truth `clear` (effect solved for exact nominal power 0.90 under the CLEAN model) |
| Track E violated cells | each clean cell × 5 violations at IDENTICAL nominal parameters, violation applied on top: 2:1 allocation, heteroscedasticity, heavy tails, missing baseline, treatment-by-baseline interaction (reuse the frozen v1 stress generators) |
| Diagnostic null pairs | clean + 5 violated null cells at `n = 80` only, quota 50 significant each (secondary; no gate) |
| Quotas | ≥ 100 significant completed per primary cell; failures ≤ 5%; screening significant-only |
| Analysis params | `alpha = 0.05`, `n_boot = 1000`, `max_removal_pct = 0.30` |
| Seeds | Track E scenarios `54001+`; masters (power check / cluster bootstrap) `20260807`. Ranges `51001+`/`52001+`/`53001+` remain RESERVED for parked Track D — do not use |
| Power check | clean cells only: primary-test-only Monte Carlo, draws 10000, master 20260807, tolerance 0.02 (violated cells are exempt — deviation from nominal power is the phenomenon under study, not an error) |
| Track E primary metric | pooled ΔAUC = AUC_score − AUC_p for discriminating violated vs clean among significant clear rows. Score orientation PRE-SPECIFIED: clean > violated. p orientation (−log₁₀ p) chosen empirically, whichever favors p (conservative for the claim). Scenario-cluster bootstrap CI: seed 20260807, B = 1000, resample whole scenario cells |
| Track E gate (frozen) | confirmed if pooled ΔAUC ≥ 0.10 AND bootstrap 95% CI lower bound > 0; per-violation ΔAUC reported descriptively either way |
| Estimated Phase 1 compute | ~1,800 primary + ~300 diagnostic scored replicates. Measured basis: v2 scored 3,300 replicates in ~80 min wall on this host (freeze 12:59 → occupancy 14:19, 2026-08-06); v1 ledger mean runtime 0.87/1.86/1.78 s at n = 40/80/160 → ≈ 55 core-min pure scoring. Expected wall time ≈ 1–2 h at 4 workers (≤ 3 h if violated cells depress significance rates) |

---

### Task 0: Preconditions

1. Confirm branch `codex/lm-ancova-calibration`; report any untracked files.
2. Run and confirm green:
   ```bash
   Rscript -e 'devtools::test()'
   Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary")'
   Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
   ```
3. Confirm `docs/plans/2026-08-06-lm-ancova-v3-design.md` and the two
   reanalysis scripts under
   `manuscript/calibration/studies/lm_ancova/tools/reanalysis/` exist; commit
   any of them still untracked:
   `docs: add lm ancova v3 design and reanalysis evidence`

### Task 1: Verify the v2 fail-closed publication (read-only on v2)

**Files (writes only outside v2):** `NEWS.md`,
`manuscript/calibration/README.md`, `tools/check-calibration-documentation.R`,
`tests/testthat/test-calibration-documentation.R`.

1. Verify, without modifying anything under `studies/lm_ancova_v2/`:
   published candidate hash `3dc2a1f840b3eb725bea629dc130f070`;
   `held_out_opened = false` and `validation_refit = false` in
   `published/candidate-diagnostics.json`; registry row `lm_ancova_v2`
   uncalibrated (version `lm-ancova-v2-2026-1`); output hash ledger checks
   out. STOP and report on any mismatch.
2. TDD: extend the documentation audit to require that calibration docs
   describe v2 as *executed fail-closed* (pilot GO at Δ = 24.4 /
   overlap = 0.034 / AUC = 0.892; training `no_feasible_thresholds`; best RI
   at FR-safe L = 0.554 vs gate 0.70; held-out never opened) and
   cross-reference the v3 design's Finding 4. Update NEWS / calibration
   README wording accordingly.
3. Verify: documentation audit + full package tests green.
4. Commit: `docs: cross-reference ancova v2 empirical fail-closed outcome`

### Task 2: Track F — negative-result manuscript section

**Files:** `manuscript/calibration/studies/lm_ancova/manuscript.md`.

1. Draft the section (order: after methods, before the case study),
   covering: v1 and v2 dispositions with hashes; the sufficiency argument;
   the +0.0002 incremental-AUC result; the required-AUC bound (0.937 vs
   ≈ 0.89 delivered); the noncentral-t projection table (RI 0.54–0.58 at
   power 0.90; 0.67–0.71 at 0.95; 0.87–0.89 at 0.99); the corrected
   pilot-gate metric with its prediction-then-confirmation record (analytic
   0.54–0.58 → v1-preview 0.542 → v2-empirical 0.554, while the frozen v2
   pilot location metrics all said GO); pointers to the committed reanalysis
   scripts. State that Track E results will be appended when published
   (Task 7) and that the replication-curve target is parked pending the
   binary-proportion study.
2. Every number must be traceable to a committed artifact or script output —
   no invented values.
3. Commit: `docs: add ancova negative-result manuscript section`

### Task 3: Scaffold the Track E study tree

**Files:** `manuscript/calibration/studies/lm_ancova_v3/{README.md,
run_calibration.R, analyse_calibration.R, R/load_study.R, config/scenarios.R,
tests/testthat/test-scenarios.R}`; `.gitignore`.

1. TDD scenario tests: unit `lm_ancova_v3`; exactly the frozen Track E cells
   (3 clean clear + 15 violated clear primary; 6 diagnostic null cells at
   n = 80); violated cells carry `violation_type` and
   `matched_clean_id`; seeds `54001+` only; seed ranges `41001+`, `42001+`,
   `43001+`, `51001+`, `52001+`, `53001+` and the v1/v2 ledger seeds all
   absent; `validate_calibration_scenarios()` passes.
2. Loader per the v2 pattern (private env, study scenarios authoritative,
   shared harness + frozen v1 stress generators reused).
3. Gitignore the study's `artifacts/raw`, `artifacts/pilot`, `outputs`.
4. Verify: study test dir green.
5. Commit: `feat: scaffold lm ancova v3 violation study`

### Task 4: Freeze the Phase 1 SAP — STOP POINT after commit

**Files:** `manuscript/calibration/studies/lm_ancova_v3/CALIBRATION_SAP.md`,
`tools/check-calibration-documentation.R`,
`tests/testthat/test-calibration-documentation.R`.

1. TDD documentation audit, then write the SAP freezing verbatim: every
   constant in the table above; the ΔAUC formula and orientation rules; the
   gate; the cluster-bootstrap procedure; the null-pair diagnostic role (no
   gate); the statement that Track D is parked, its seed ranges reserved,
   and its un-parking requires explicit human instruction after the
   binary-proportion replication-curve outcome is reviewed; the assertion
   that v1/v2 validation seeds/IDs appear in no v3 ledger.
2. Verify: audit + package tests green.
3. Commit: `docs: freeze lm ancova v3 phase 1 sap`
4. **STOP.** Present the frozen SAP for human review. Wait for approval.

### Task 5: Adapter and ΔAUC tooling

**Files:** `manuscript/calibration/studies/lm_ancova_v3/R/adapter.R`,
`.../R/track_e.R`, `.../tests/testthat/test-adapter.R`,
`.../tests/testthat/test-track-e.R`.

1. TDD adapter: `robustness_lm(..., weights = c(jackknife = 0,
   fragility = 0.5, bootstrap = 0.5))` drives `overall_score`; v1-weight
   comparator archived as a column; screening/robustness parity tests as in
   the v2 adapter precedent; violation switches route through the frozen v1
   stress generators with matched nominal parameters (test that a violated
   cell and its `matched_clean_id` share n, R², and solved effect).
2. TDD Track E metrics on synthetic fixtures: AUC with the pre-specified
   score orientation; empirical (p-favoring) orientation for −log₁₀ p;
   pooled and per-violation ΔAUC; deterministic cluster-bootstrap CI
   (seed 20260807, B = 1000); machine-readable verdict against the frozen
   gate; a fixture where score and p are identical yields ΔAUC = 0 and
   verdict "not confirmed".
3. Verify: study tests green.
4. Commit: `feat: add ancova v3 violation-detection tooling`

### Task 6: Smoke, pilot, power check, production run — STOP before production

1. Smoke to `/tmp/stabilitest-lm-ancova-v3-smoke`; pilot with `--workers 4`
   to `studies/lm_ancova_v3/outputs/pilot`. Inspect ONLY wiring, runtime,
   occupancy, and failure summaries — no score distributions.
2. Power check on the three CLEAN cells (draws 10000, seed 20260807,
   tolerance 0.02). Violated cells: record their empirical significance
   rates as descriptive output; no tolerance applies. STOP on a clean-cell
   miss; do not adjust the design.
3. Commit compact summaries + execution freeze (scenario hash, code commit,
   runtime projection, worker plan):
   `docs: freeze ancova v3 phase 1 execution`
4. **STOP.** Report and wait for approval to spend production compute.
5. On approval: production run (`--mode full --workers 4 --resume`) to
   `studies/lm_ancova_v3/artifacts/raw/track-e`; verify quotas and the ≤ 5%
   failure limit; compute the pre-registered ΔAUC verdict ONCE; publish
   compact artifacts (occupancy, failures, per-violation and pooled ΔAUC
   with CIs, verdict JSON, manifests, hash ledger).
6. Commit: `data: publish ancova v3 violation-detection result`

### Task 7: Append the Track E outcome and final report — STOP POINT (end)

1. Append the published Track E result to the Task 2 manuscript section:
   confirmed (score detects violations p cannot — first positive evidence of
   added value beyond p) or not confirmed (honest bound on the score's added
   value). Either way, cite the verdict artifact and hashes.
2. Update NEWS / calibration README with the published outcome only —
   no runtime behavior claims.
3. Run: full package tests, both calibration test dirs, documentation audit.
4. Commit: `docs: report ancova v3 violation-detection outcome`
5. **STOP.** Final report: every commit, every gate outcome, every artifact
   path, any deviation from this plan. Do not begin Phase 2 / Track D or any
   Gate B integration.

---

## Phase 2 (PARKED — do not execute): Track D replication curve

Preserved for a future explicitly-authorized plan revision. Un-parking
requires BOTH: (a) the binary-proportion study's replication-curve outcome
reviewed by a human, and (b) an explicit human instruction naming this
section. Reserved constants (do not reuse elsewhere): seeds `51001+`
(training), `52001+` (validation); replication-draw master `20260807`;
Track D gates as recorded in the v3 design (logistic `replication ~ score`;
held-out intercept |logit| ≤ 0.20; slope ∈ [0.85, 1.15]; max 10-bin error
≤ 0.10 with conservative bounds; Brier margin ≤ 0.01 vs the p-only
reference; cluster bootstrap seed 20260807, B = 1000; freeze-then-validate,
no refit, fail closed).
