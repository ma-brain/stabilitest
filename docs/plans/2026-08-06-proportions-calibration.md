# Binary-Proportion Calibration Implementation Plan (Phase 1: `fisher_exact`)

> **For the executing agent:** Work strictly task-by-task, in order. Use
> test-driven development for every code task: write the failing test, verify
> it fails, implement, verify it passes, run the stated verification commands,
> commit with the exact message given. If anything unexpected happens, STOP
> and report; do not improvise. You may not change any frozen number, gate,
> seed, or policy.

**Goal:** Deliver a fail-closed calibration for eligible significant canonical
`fisher_exact` results — two-band Fragile/Not-fragile at clear power 0.95
(Track A″), with the replication-curve fallback (Track D′) — plus the runtime
eligibility profile, a prospectively frozen case-study dataset, and a separate
vignette.

**Design (authoritative):** `docs/plans/2026-08-06-proportions-calibration-design.md`.
Read it in full first. On any conflict with this plan, STOP and report.

**Workspace:** create worktree + branch `codex/proportions-calibration` from
`main`. All paths relative to the worktree root.

---

## Non-negotiable rules

1. NEVER open held-out outputs before the Track A″ candidate hash is frozen
   and committed. A pilot no-go means Track A″ ends; held-out stays closed
   for it.
2. NEVER modify Welch calibration artifacts, lm_ancova v1/v2/v3 material,
   `pain_ancova_trial`, or any active registry row (until the Gate B stop
   point, and then only `fisher_exact` with human approval).
3. NEVER soften a frozen gate. Failed gate ⇒ publish fail-closed, commit,
   STOP, report.
4. Freeze `onc_response_trial` (generator, seed, committed data) BEFORE
   inspecting its p-value, score, components, or band. Never regenerate it.
5. Production compute: ≤ 4 workers; logs to
   `/tmp/stabilitest-proportions-logs/`, never inside the repo.
6. Commit only code, SAPs, compact summaries, manifests, registries, ledgers.
   Raw/checkpoint outputs stay gitignored.
7. MANDATORY STOP POINTS: end of Task 4 (SAP freeze), end of Task 8 (pilot
   verdict), end of Task 9 (before held-out), end of Task 12 (before any
   registry/runtime change).

## Frozen constants (do not alter)

| Constant | Value |
| --- | --- |
| Unit (Phase 1) | `fisher_exact`; study path `manuscript/calibration/studies/binary_proportion/` |
| Score weights | `fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`; v1 comparator `0.4/0.4/0.2` archived, never fitted |
| Training grid | n/arm {25, 50, 100, 200} × p₀ {0.10, 0.25, 0.50} × truth {null, borderline, clear} |
| Held-out grid | n/arm {35, 75, 150} × p₀ {0.15, 0.40} × same truth classes |
| Truth targets | null exact; borderline exact power 0.60 (diagnostic only); clear exact power 0.95 — solved against the *enumerated exact* power of `fisher.test`, not an approximation |
| Stress rows | 2:1 allocation; p₀ = 0.03; 5% misclassification; beta-binomial overdispersion; missing outcomes — diagnostic only |
| Seeds | training 61001+, validation 62001+, stress 63001+; masters (power / replication / cluster bootstrap) 20260808 |
| Screening | significant-only; ≥ 100 significant completed per required scenario; failures ≤ 5%; `n_boot = 1000`; `max_removal_pct = 0.30`; α = 0.05 |
| Pilot gate (sealed) | smallest FR-safe integer L (FR ≤ 0.05, Wilson upper ≤ 0.10); projected RI = P(score > L | clear, significant): go ≥ 0.72; hard no-go < 0.70; 0.70–0.72 marginal ⇒ STOP for human decision |
| Track A″ training gates | FR ≤ 0.05 with Wilson upper ≤ 0.10; RI ≥ 0.70 with Wilson lower ≥ 0.60; ties: highest RI, then FR safety margin, then smallest L |
| Held-out acceptance | same operating characteristics, conservative of Wilson and scenario-cluster bootstrap (seed 20260808, B = 1000); once, no refit, no second candidate |
| Track D′ (fallback) gates | logistic `replication ~ score`; held-out calibration intercept \|logit\| ≤ 0.20; slope ∈ [0.85, 1.15]; max abs error over 10 equal-count bins ≤ 0.10 (conservative bounds); Brier(score) − Brier(p-only reference) ≤ 0.01 |
| Runtime profile bounds | per-arm n ∈ [25, 200]; allocation ratio ∈ [0.8, 1.25]; observed control-arm rate ∈ [0.08, 0.55] with ≥ 3 events and ≥ 3 non-events in control |
| Case study | dataset `onc_response_trial`, 60/arm, generator seed `20260809L`, generator rates: control response 0.20, active 0.45 (clinical plausibility choice, frozen before inspection) |
| Walsh FI comparator | event-flip fragility index archived per replicate; never in the score or gates |

---

### Task 0: Worktree, preconditions, and evidence commit

1. From `main`: create worktree/branch `codex/proportions-calibration`.
2. Run `Rscript -e 'devtools::test()'` and the shared calibration suite;
   expect green. STOP if not.
3. Verify the committed projection scripts exist at
   `manuscript/calibration/studies/binary_proportion/tools/
   feasibility-projection-power090.R` and `...-power095.R`; run both and
   confirm the design-doc table (0.90 infeasible; 0.95 all-pass). Commit
   scripts + design/plan docs if untracked:
   `docs: add binary proportion calibration design and projection evidence`

### Task 1: Runtime analysis profile for proportion tests

**Files:** `R/robustness_analysis.R` (or `R/robustness_shared.R`),
`tests/testthat/test-robustness_analysis.R`.

1. TDD `prop_calibration_profile()` attached to every proportions result:
   unit, per-arm n, allocation ratio, observed event counts/rates per arm,
   complete-case flag, alpha, `n_boot`, weights, `max_removal_pct`, `correct`
   flag, version tag `prop-profile-1`. Canonical fixtures TRUE; fixtures
   violating each bound FALSE (n out of range, unbalanced 2:1, control rate
   0.02, < 3 events, nondefault alpha/weights/budget).
2. Extend the registry resolver exactly as `.is_supported_lm_ancova_profile`
   was added for lm: fail-closed predicate keyed to the frozen bounds; the
   active `fisher_exact` row stays uncalibrated at this stage; Welch behavior
   unchanged (test this).
3. Verify: `Rscript -e 'devtools::test(filter = "robustness_analysis|calibration-registry")'`.
4. Commit: `feat: gate proportion bands on canonical profiles`

### Task 2: Study scaffold and scenario contract

**Files:** `manuscript/calibration/studies/binary_proportion/{README.md,
run_calibration.R, analyse_calibration.R, R/load_study.R, config/scenarios.R,
tests/testthat/test-scenarios.R}`; `.gitignore`.

1. TDD: unit `fisher_exact` only in Phase 1 rows; frozen grids/seeds;
   borderline `diagnostic_only = TRUE`; stress rows `design_layer = "stress"`;
   `validate_calibration_scenarios()` passes; seed ranges of every earlier
   study absent.
2. Loader per the lm_ancova/v2 pattern (private env, study scenarios
   authoritative, shared harness reused).
3. Gitignore the study's `artifacts/raw`, `artifacts/pilot`, `outputs`.
4. Verify: study test dir green.
5. Commit: `feat: define binary proportion calibration study`

### Task 3: Exact-power generator and verifiers

**Files:** study `R/power.R`, `R/generator.R`, matching tests.

1. TDD power: `solve_prop_effect(n, p0, target)` returns p₁ such that the
   *enumerated exact* Fisher power equals the target (tolerance 1e-6);
   null returns p₀; monotonicity tests. Enumeration = binomial-weighted sum
   over the 2×2 table grid (reuse the committed projection-script approach).
2. TDD generator: individual-level 0/1 vectors per arm; exact `rbinom`;
   stress switches (allocation, misclassification, overdispersion via
   beta-binomial, missingness); identical seeds ⇒ identical data; row-ID
   structure matching existing generators.
3. Independent Monte Carlo verifier using only `fisher.test` p-values
   (production: draws 10000, master 20260808, tolerance 0.02; unit tests:
   2000 draws, tolerance 0.04).
4. Verify: study tests green.
5. Commit: `feat: generate exact-power proportion scenarios`

### Task 4: Adapter, comparators, replication draws — then SAP freeze; STOP

**Files:** study `R/adapter.R`, `R/replication.R`, `R/walsh_fi.R`, tests;
`CALIBRATION_SAP.md`; `tools/check-calibration-documentation.R` +
`tests/testthat/test-calibration-documentation.R`.

1. TDD adapter: screening decision from `fisher.test` parity with
   `robustness_analysis(..., test_type = "fisher", weights = c(jackknife = 0,
   fragility = 0.5, bootstrap = 0.5))` (p, estimate, conclusion to 1e-12);
   v1-weight comparator column archived; per-row Walsh event-flip FI
   computed and archived (flip events in the smaller-event arm until
   significance changes; document the exact convention in the SAP).
2. TDD replication: one primary-test-only replicate draw per completed
   significant row, dedicated seed stream (master 20260808), recorded seed,
   deterministic reproduction; stream collision tests against all other seed
   columns.
3. Write the SAP freezing every constant in the table above verbatim, the
   pilot decision rule, both track gate sets, the profile bounds, the
   case-study freeze discipline (including that its seed `20260809L` and IDs
   appear in no calibration ledger), and Phase 2/3 sequencing. Extend the
   documentation audit to enforce the key sentences; run it.
4. Verify: full package tests + study tests + audit green.
5. Commit: `docs: freeze binary proportion calibration sap`
6. **STOP.** Present the SAP for human review. Wait for approval.

### Task 5: Two-band fitter and validation modules

**Files:** study `R/thresholds.R`, `R/validation.R`, tests.

1. TDD with synthetic fixtures (one uniquely-feasible L, one infeasible):
   FR/RI definitions, Wilson bounds, deterministic ties, full-grid
   diagnostics returned, `no_feasible_threshold` path; freeze hash on
   candidate + scenario manifest; validation callable only on a frozen
   candidate, no call path to the fitter, `validation_refit = FALSE`,
   cluster-bootstrap determinism (seed 20260808), conservative-bound rule.
2. TDD Track D′ modules (logistic + isotonic sensitivity, p-only reference
   map, four gates) — same structure as the v3 plan's Task 5.
3. Verify: study tests green.
4. Commit: `feat: fit two-band and replication-curve proportion calibrations`

### Task 6: Isolated runner, manifests, resume safety

Reuse the lm_ancova Task 6 pattern: study wrapper over the shared runner,
manifest hashes over study scenarios only, resume-mismatch fatal, smoke
selection tests. Verify and commit:
`feat: run isolated binary proportion calibration`

### Task 7: Freeze the case-study dataset BEFORE any analysis

**Files:** `data-raw/onc_response_trial.R`, `data/onc_response_trial.rda`,
`R/data_onc_response.R`, `tests/testthat/test-data-onc-response.R`.

1. TDD dataset contract: 120 rows; columns `subject_id`, `arm`
   (factor Placebo/Active), `response` (0/1 integer); 60 per arm; unique
   IDs; no missing values; regenerating from the committed script with seed
   `20260809L` reproduces the packaged object exactly; the seed absent from
   `binary_proportion` scenario seeds and ledgers.
2. Single unconditional draw with the frozen generator (control 0.20,
   active 0.45); commit data + docs BEFORE running any test, score, or band
   on it: `data: freeze synthetic oncology response trial`
3. Do not open its Fisher p-value or robustness output until Task 11.

### Task 8: Smoke, score-only pilot, power gate — STOP POINT

1. Smoke to `/tmp/stabilitest-proportions-smoke`; pilot (`--workers 4`) to
   the study's `outputs/score-pilot`. Inspect only wiring/occupancy/failure
   summaries first.
2. Run the power gate (Task 3 verifier, production settings). STOP on any
   miss.
3. Run the sealed pilot gate: compute the feasibility-projection metric
   (FR-safe L, projected RI) on pilot scores; write machine-readable
   `SCORE_PILOT_GATE.json`; archive diagnostics.
4. Commit summaries + execution freeze:
   `docs: record binary proportion pilot gate`
5. **STOP.** Report the verdict. Go (≥ 0.72) ⇒ await approval for Task 9
   production. Marginal (0.70–0.72) ⇒ human decides. Hard no-go ⇒ Track A″
   ends; await approval to proceed with Track D′ only (Tasks 9–10 still run;
   Task 9's fitting step then fits only the replication curve).

### Task 9: Production training and candidate freeze — STOP POINT

1. Training run (`--mode full --workers 4 --resume`) to
   `artifacts/raw/training`; quotas and ≤ 5% failure limit verified;
   replication draws collected for every completed significant row.
2. Fit on training only: Track A″ cutoff (if pilot passed) and the Track D′
   curve (always). Freeze candidate(s) + hashes; commit compact diagnostics:
   `data: freeze binary proportion training candidates`
3. **STOP.** Report. Held-out untouched. Await approval.

### Task 10: Held-out once; publish the decision

1. Verify hashes/manifests/empty validation dir; run held-out
   (`--validation-only`) once; validate frozen candidate(s) once, no refit.
2. Publish atomically (freeze-and-publish tooling in the study's `tools/`,
   lm_ancova pattern): summaries, occupancy, failures, power artifacts,
   candidate + validation diagnostics, `fisher_exact` registry CSV/RDS,
   manifests, hash ledger; `validation_refit = FALSE` everywhere. Either
   outcome publishes identically.
3. Commit: `data: publish binary proportion calibration decision`

### Task 11: Vignette and manuscript section

**Files:** `vignettes/proportions-case-study.Rmd`, study `manuscript.md`
section, `NEWS.md`, `README.md`, documentation audit.

1. Run the frozen case-study analysis ONCE (`test_type = "fisher"`, frozen
   weights, `n_boot = 1000`, a fixed documented seed) and report whatever it
   shows: estimate, p, components, Walsh FI comparison, and the honest label
   state under the published decision (band, replication estimate, or
   numeric-only). Never regenerate or reselect the dataset.
2. Vignette structure: clinical setup → primary analysis → three components
   walk-through → Walsh FI vs removal fragility contrast → what the published
   calibration does and does not license. Render clean:
   `Rscript -e 'rmarkdown::render("vignettes/proportions-case-study.Rmd", output_dir = tempdir())'`
3. Manuscript section ordered methods → calibration results → case study.
   Update README/NEWS wording to describe actual published status only.
4. Verify: full package tests, audit, `rcmdcheck` (0 err / 0 warn / accepted
   NOTE).
5. Commit: `docs: add proportions case study vignette`

### Task 12: Final report — STOP POINT (end of plan)

1. `git status` clean; task-scoped commit log; every gate outcome and
   artifact path listed; any deviation flagged.
2. **STOP.** Registry/runtime integration (Gate B: activating `fisher_exact`
   labels or replication estimates, weights-default policy, Phase 2
   `chi_square_2x2`) is separate human-approved work. Do not begin it.
