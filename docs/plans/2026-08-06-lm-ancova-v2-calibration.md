# LM/ANCOVA v2 (Track A) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Deliver a fail-closed two-band (Fragile / Not fragile) calibration for eligible significant canonical ANCOVA on a jackknife-light score (`lm_ancova_v2`), after publishing v1’s uncalibrated decision and passing a sealed score-only pilot gate.

**Architecture:** Keep v1 study provenance immutable. Add an isolated `lm_ancova_v2` study (new path + SAP + manifests) that reuses shared calibration harness helpers and most v1 generators/adapters, but freezes new score weights, a single cutoff \(L\), null-vs-clear fitting strata, and borderline-as-diagnostic-only. Do not activate package labels until held-out acceptance passes.

**Tech Stack:** R >= 4.2, `stats::lm`, existing `stabilitest` calibration harness, `testthat`, `devtools`, `roxygen2`, `rcmdcheck`.

**Design:** `docs/plans/2026-08-06-lm-ancova-v2-design.md` (Track A locked).

---

## Execution rules

- Use `@superpowers:test-driven-development` for code tasks.
- Use `@superpowers:systematic-debugging` on unexpected failures.
- Use `@superpowers:verification-before-completion` before each gate.
- Cap production workers at **4** on this machine (M4 Pro 48 GB); log outside the repo so `full` mode stays clean.
- Do not open v2 held-out scores before the candidate hash is frozen.
- Do not reuse v1 validation replicates for v2 fitting or acceptance.
- Do not overwrite the v1 `lm_ancova` registry provenance row.
- Do not inspect or regenerate `pain_ancova_trial` for nicer scores/bands.
- Commit only code, SAPs, compact summaries, manifests, registries, and ledgers.

## Prerequisite: close v1 Gate B (uncalibrated)

### Task 0: Publish v1 uncalibrated decision into package policy

**Files:**
- Modify: `inst/extdata/calibration-registry.csv`
- Modify: `R/calibration_registry.R` (as needed for explicit uncalibrated reason)
- Modify: `NEWS.md`, `README.md`, `manuscript/calibration/README.md`
- Modify: `manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md`
- Modify: `manuscript/calibration/studies/lm_ancova/published/` (compact v1 decision artifacts)
- Test: `tests/testthat/test-calibration-registry.R`
- Test: `tests/testthat/test-calibration-documentation.R`

**Steps:**
1. Write failing tests that the active `lm_ancova` row remains uncalibrated with an auditable reason such as `no_feasible_thresholds`, and that labels stay suppressed.
2. Publish compact v1 artifacts (candidate/diagnostics/occupancy/power/hash ledger) under `studies/lm_ancova/published/` without inventing cutoffs.
3. Update docs/SAP status to record training fail-closed and “held-out not opened”.
4. Run package + documentation audits.
5. Commit: `data: publish ancova v1 uncalibrated decision`

---

## v2 study scaffold

### Task 1: Scaffold `lm_ancova_v2` study tree

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova_v2/README.md`
- Create: `manuscript/calibration/studies/lm_ancova_v2/R/load_study.R`
- Create: `manuscript/calibration/studies/lm_ancova_v2/run_calibration.R`
- Create: `manuscript/calibration/studies/lm_ancova_v2/config/scenarios.R`
- Modify: `.gitignore` (ignore `lm_ancova_v2` raw/pilot/outputs)
- Test: `manuscript/calibration/studies/lm_ancova_v2/tests/testthat/test-scenarios.R`

**Steps:**
1. Write failing tests: scenarios use `calibration_unit = "lm_ancova_v2"`; core/validation grids exist; borderline rows are marked `diagnostic_only = TRUE` (or equivalent parameter flag); clear default target power is `0.90` with SAP ability to freeze `0.95`.
2. Implement loader that reuses v1 generator/power helpers where possible but keeps manifests/hashes isolated.
3. Run study scenario tests.
4. Commit: `feat: scaffold lm ancova v2 study`

### Task 2: Freeze the v2 SAP (Track A)

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova_v2/CALIBRATION_SAP.md`
- Modify: `tools/check-calibration-documentation.R`
- Test: `tests/testthat/test-calibration-documentation.R`

**Freeze explicitly:**
- weights `fragility=0.5`, `bootstrap=0.5`, `jackknife=0`
- bands: Fragile if score ≤ \(L\), else Not fragile
- fitting strata: null + clear only
- borderline: diagnostic only
- pilot go/no-go formulas and thresholds
- clear power choice rule (`0.90` default; escalate to `0.95` only via recorded pilot decision)
- workers default `4`, `n_boot=1000`, `max_screen_draws=10000`, quotas ≥100 significant per required scenario
- acceptance gates from the v2 design
- assertion that v1 validation seeds/IDs are absent from v2 ledgers

**Steps:** TDD documentation audit → write SAP → update READMEs/NEWS Gate wording → commit `docs: freeze lm ancova v2 track a sap`

### Task 3: Adapter emits v2 score + v1 comparator

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova_v2/R/adapter.R`
- Test: `manuscript/calibration/studies/lm_ancova_v2/tests/testthat/test-adapter.R`
- Optionally thin wrappers in study `R/` for score assembly

**Steps:**
1. Failing tests: `robustness_lm(..., weights = c(jackknife=0, fragility=0.5, bootstrap=0.5))` drives `overall_score`; a comparator score with v1 weights is archived (column or parallel summary) and **not** used for fitting.
2. Implement adapter parity with v1 screening/`robustness_lm` term tests.
3. Commit: `feat: add ancova v2 weighted score adapter`

### Task 4: Two-band cutoff fitter and validation (no refit)

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova_v2/R/thresholds.R`
- Create: `manuscript/calibration/studies/lm_ancova_v2/R/validation.R`
- Create: `manuscript/calibration/studies/lm_ancova_v2/analyse_calibration.R`
- Test: `.../tests/testthat/test-thresholds.R`
- Test: `.../tests/testthat/test-validation.R`

**Steps:**
1. Failing tests for integer \(L\) search, FR/RI gates, deterministic ties, freeze hash, validate-once without calling fit.
2. Exclude borderline/`diagnostic_only` rows from fitting and acceptance.
3. Commit: `feat: fit two-band ancova v2 cutoffs`

### Task 5: Score-only pilot tooling and go/no-go recorder

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova_v2/tools/score_pilot_gate.R`
- Create: `manuscript/calibration/studies/lm_ancova_v2/artifacts/summaries/` placeholders as needed
- Test: focused unit tests for metric helpers

**Steps:**
1. Implement sealed metrics: median(clear)−median(null); \(P(\text{null}>\text{median(clear)})\); optional AUC; by-\(n\) quantiles for v2 score and components.
2. Write machine-readable `SCORE_PILOT_GATE.json` with pass/fail and frozen clear-power decision.
3. Do **not** search \(L\) in this task.
4. Commit tooling: `feat: add ancova v2 score pilot gate`

---

## Execution gates

### Task 6: Run score-only pilot (4 workers) and record go/no-go

**Commands (indicative):**
```bash
Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode pilot --phase all --engine lm --workers 4 \
  --output manuscript/calibration/studies/lm_ancova_v2/outputs/score-pilot

Rscript manuscript/calibration/studies/lm_ancova_v2/tools/score_pilot_gate.R
```

**Rules:** Inspect only pre-registered separation summaries. If gate fails → publish numeric-only v2 stop decision and end plan. If gate passes → freeze clear power + weights in SAP and commit `docs: pass ancova v2 score pilot gate`.

### Task 7: Power gate + production training (4 workers)

Reuse v1 power verifier patterns with v2 scenarios (null/clear required; borderline optional diagnostic). Then:

```bash
Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode full --phase all --engine lm --workers 4 --resume \
  --output manuscript/calibration/studies/lm_ancova_v2/artifacts/raw/training
```

Assemble from per-scenario checkpoints if the parent process dies after a large `run-results.rds` (as in v1). Commit compact occupancy + execution freeze: `docs: freeze ancova v2 production execution` then training summaries when complete.

### Task 8: Fit/freeze \(L\) on training only

**Files:**
- Create/modify: `manuscript/calibration/studies/lm_ancova_v2/tools/fit_training_candidate.R`
- Summaries under `artifacts/summaries/`

**Steps:**
1. Fit on core null+clear significant completed rows only.
2. If `no_feasible_threshold` → publish uncalibrated v2 artifacts; **skip held-out**; jump to Task 10 uncalibrated path.
3. If candidate exists → commit compact diagnostics/hash before any validation: `data: freeze ancova v2 two-band candidate`

### Task 9: Open held-out once (only if candidate exists)

```bash
Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode full --phase all --engine lm --workers 4 --resume \
  --validation-only \
  --output manuscript/calibration/studies/lm_ancova_v2/artifacts/raw/validation

Rscript manuscript/calibration/studies/lm_ancova_v2/tools/freeze_and_publish.R
```

Require `validation_refit = FALSE`, quotas, failure limits, hash ledger. Commit: `data: publish ancova v2 calibration decision`

### Task 10: Integrate reviewed v2 outcome into package policy

**Files:**
- Modify: `inst/extdata/calibration-registry.csv` (add/update `lm_ancova_v2` only)
- Modify: `R/calibration_registry.R`, `R/robustness_models.R` as needed for v2 profile/weights/label mapping
- Tests + NEWS/README/vignette honesty

**Steps:**
1. If validated: enable Fragile/Not fragile labels only for eligible canonical profiles under the frozen v2 weights and \(L\).
2. If uncalibrated: registry row records fail-closed reason; labels remain suppressed.
3. Keep v1 `lm_ancova` row as historical uncalibrated record.
4. `devtools::test()`, study tests, docs audit, `rcmdcheck`.
5. Commit: `feat: integrate ancova v2 calibration policy` (or `data:`/`docs:` as appropriate)

---

## Verification checklist (final)

- [ ] v1 uncalibrated decision published; held-out never opened for v1
- [ ] v2 SAP freezes Track A weights, two-band rule, pilot metrics
- [ ] Score pilot go/no-go recorded before production cutoff search
- [ ] Borderline excluded from fitting/acceptance
- [ ] v1 composite comparator archived, not fitted
- [ ] Workers ≤ 4 for production on this host
- [ ] No v1 validation contamination
- [ ] Registry does not silently overwrite v1 provenance
- [ ] `pain_ancova_trial` untouched as calibration evidence

---

## Out of scope

- Track B three-band work
- Softening acceptance gates to force a label
- Welch 55/70 as ANCOVA bands
- Harness-wide refactor beyond what v2 isolation needs
