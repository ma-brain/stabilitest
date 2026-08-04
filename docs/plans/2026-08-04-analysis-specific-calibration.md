# Analysis-Specific Calibration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build, run, validate, and publish a reproducible calibration program for every public analysis family, retaining shared score bands where transportability passes and making unsupported interpretation explicit.

**Architecture:** Add a standalone, modular simulation pipeline under `manuscript/calibration/` with a frozen scenario registry, public-API adapters, deterministic two-stage sampling, resumable checkpoints, and separate training/held-out analysis. After publication-grade results are frozen, generate a machine-readable calibration registry that drives package interpretation metadata and five focused vignettes.

**Tech Stack:** R 4.2+, base R, `stats`, `parallel`, `dplyr`, `purrr`, `tibble`, `ggplot2`, `survival`, `testthat`, `pkgload`, `knitr`, and `rmarkdown`.

---

## Phase 1: Freeze the Statistical Contract

### Task 1: Add the calibration statistical analysis plan and directory contract

**Files:**
- Create: `manuscript/calibration/README.md`
- Create: `manuscript/calibration/CALIBRATION_SAP.md`
- Create: `manuscript/calibration/config/scenarios.R`
- Create: `manuscript/calibration/R/load_calibration.R`
- Create: `manuscript/calibration/tests/testthat/test-scenario-schema.R`
- Modify: `.gitignore`

**Step 1: Write a failing scenario-schema test**

Create a test that sources `R/load_calibration.R`, constructs the scenario
registry, and requires these columns:

```r
required_scenario_columns <- c(
  "scenario_id", "analysis_family", "endpoint", "design_layer",
  "data_generator", "primary_adapter", "robustness_adapter",
  "truth_class", "target_conclusion", "sample_size",
  "n_boot", "max_removal_pct", "training_split", "scenario_seed"
)

testthat::test_that("scenario registry has the frozen schema", {
  scenarios <- calibration_scenarios()
  testthat::expect_named(
    scenarios,
    c(required_scenario_columns, "parameters"),
    ignore.order = TRUE
  )
  testthat::expect_true(all(!duplicated(scenarios$scenario_id)))
  testthat::expect_setequal(
    unique(scenarios$design_layer),
    c("core", "stress", "validation")
  )
  testthat::expect_true(all(scenarios$n_boot == 1000L))
})
```

**Step 2: Run the test and verify RED**

Run:

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: failure because the loader and registry do not exist.

**Step 3: Create the loader and initial registry contract**

`load_calibration.R` must locate its own project root, load the current checkout
with `pkgload::load_all(export_all = FALSE, helpers = FALSE)`, and source every
file under `manuscript/calibration/R/` except itself plus the scenario config.

Implement `calibration_scenarios()` as a tibble-producing function. Start with
one smoke scenario per analysis family; later tasks expand it. Store nested
generator parameters in the list-column `parameters` rather than creating a
wide, family-specific table.

**Step 4: Document the frozen statistical rules**

`CALIBRATION_SAP.md` must specify before simulation:

- reference truth classes;
- frozen shared cutoffs 55 and 70;
- training and held-out split rules;
- false-reassurance and robust-identification criteria;
- five-point / 0.05 improvement rule for family-specific bands;
- minimum completed stratum size and Monte Carlo precision rule;
- non-significant-result policy;
- allowed exclusions and failure reporting;
- primary and sensitivity analyses.

`README.md` must document exact smoke, pilot, full, resume, analysis, and artifact
generation commands.

**Step 5: Ignore only large generated artifacts**

Add:

```gitignore
/manuscript/calibration/artifacts/checkpoints/
/manuscript/calibration/artifacts/raw/
/manuscript/calibration/artifacts/pilot/
```

Do not ignore `config/`, `published/`, manifests, summary CSV files, figures, or
reports.

**Step 6: Run tests and commit**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
git add .gitignore manuscript/calibration
git commit -m "docs: freeze calibration analysis contract"
```

Expected: scenario-schema test passes.

### Task 2: Implement the common replicate and audit schemas

**Files:**
- Create: `manuscript/calibration/R/schema.R`
- Create: `manuscript/calibration/tests/testthat/test-result-schema.R`

**Step 1: Write failing schema tests**

Require every completed replicate to contain:

```r
calibration_replicate_columns <- c(
  "scenario_id", "replicate_id", "analysis_family", "endpoint",
  "design_layer", "truth_class", "target_conclusion",
  "screening_conclusion", "selected", "analysis_conclusion",
  "original_p", "effective_p", "jackknife_stability",
  "fragility_component", "fragility_k", "fragility_pct",
  "bootstrap_reproducibility", "overall_score", "assigned_label",
  "n", "replicate_seed", "bootstrap_seed", "runtime_seconds",
  "status", "failure_stage", "failure_class", "failure_message"
)
```

Test `validate_replicates()` for missing columns, duplicate
`scenario_id`/`replicate_id` pairs, invalid scores, invalid statuses, and
non-finite successful results.

**Step 2: Verify RED**

Run the standalone calibration tests. Expected: missing schema helpers.

**Step 3: Implement constructors and validators**

Implement:

```r
new_calibration_replicate <- function(...) { ... }
validate_calibration_scenarios <- function(x) { ... }
validate_calibration_replicates <- function(x) { ... }
new_calibration_failure <- function(scenario, replicate_id, stage, condition) { ... }
```

Use explicit errors; never repair or silently coerce invalid artifacts.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
git add manuscript/calibration/R/schema.R manuscript/calibration/tests/testthat/test-result-schema.R
git commit -m "feat: define calibration artifact schemas"
```

### Task 3: Add deterministic seeds, atomic checkpoints, and resume behavior

**Files:**
- Create: `manuscript/calibration/R/seeds.R`
- Create: `manuscript/calibration/R/checkpoints.R`
- Create: `manuscript/calibration/tests/testthat/test-reproducibility.R`
- Create: `manuscript/calibration/tests/testthat/test-checkpoints.R`

**Step 1: Write failing deterministic-seed tests**

Test that `scenario_seed(scenario_id, master_seed)`,
`replicate_seed(scenario_seed, replicate_id)`, and
`bootstrap_seed(replicate_seed)` return stable integers; different IDs must not
collide in the test fixture. Test that mapping the same replicate IDs in forward
and reverse order produces identical generated data hashes.

**Step 2: Write failing checkpoint tests**

Test that:

- writes use a temporary file followed by atomic rename;
- a complete checkpoint is skipped on resume;
- an invalid or truncated checkpoint fails validation and is not treated as
  complete;
- manifest hashes prevent resuming with changed scenario definitions.

**Step 3: Implement deterministic seeds without global-order dependence**

Use a stable string-to-integer hash implemented in base R and set the RNG kind
explicitly:

```r
RNGkind("L'Ecuyer-CMRG")
```

Every selected dataset and bootstrap call receives its own recorded seed.
Worker count and execution order must not affect results.

**Step 4: Implement checkpoint functions**

Implement:

```r
checkpoint_path <- function(root, scenario_id, stratum) { ... }
write_checkpoint <- function(x, path, manifest_hash) { ... }
read_checkpoint <- function(path, manifest_hash) { ... }
checkpoint_complete <- function(path, manifest_hash, target_n) { ... }
```

**Step 5: Run tests and commit**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
git add manuscript/calibration/R manuscript/calibration/tests/testthat
git commit -m "feat: make calibration runs deterministic and resumable"
```

## Phase 2: Implement and Validate Analysis Adapters

### Task 4: Add two-sample, rank, and proportion adapters

**Files:**
- Create: `manuscript/calibration/R/adapters_two_sample.R`
- Create: `manuscript/calibration/tests/testthat/test-adapters-two-sample.R`
- Modify: `manuscript/calibration/config/scenarios.R`

**Step 1: Write parity tests first**

For Welch, paired t, Wilcoxon, paired Wilcoxon, Brunner-Munzel, Fisher,
chi-square, and `prop.test`, generate fixed datasets and assert:

```r
screen <- adapter$primary_decision(data, scenario)
full <- adapter$run_robustness(data, scenario, n_boot = 5L)
testthat::expect_equal(screen$p_value, full$original_p, tolerance = 1e-10)
testthat::expect_identical(screen$conclusion, full$original_significant)
```

**Step 2: Verify RED**

Run `test-adapters-two-sample.R`; expect missing adapter failures.

**Step 3: Implement generators and adapters**

Generators must cover normal, heavy-tailed, heteroscedastic, paired,
directionally contaminated, balanced/imbalanced binary, and sparse binary
data. Screening uses the same base test and correction settings as the public
function. Full analysis calls `stabilitest::robustness_analysis()`.

**Step 4: Expand core, stress, and held-out scenarios**

Add at least three sample sizes and null/moderate/large truth classes. Reserve
parameter combinations—not merely different seeds—for held-out validation.

**Step 5: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-adapters-two-sample.R")'
git add manuscript/calibration
git commit -m "feat: add two-sample calibration adapters"
```

### Task 5: Add LM/ANCOVA and GLM adapters

**Files:**
- Create: `manuscript/calibration/R/adapters_models.R`
- Create: `manuscript/calibration/tests/testthat/test-adapters-models.R`
- Modify: `manuscript/calibration/config/scenarios.R`

**Step 1: Write LM/ANCOVA parity tests**

Cover single coefficients, multi-df factors, prognostic covariates, omitted
analysis rows, imbalance, and heteroscedasticity. Compare the screening
coefficient or `drop1(..., test = "F")` result with `robustness_lm()`.

**Step 2: Write binomial and Poisson parity tests**

Cover single and multi-df terms, binomial logit, Poisson log, exposure offsets,
and observation weights. Screening must use the same coefficient or
`drop1(..., test = "Chisq")` decision as `robustness_glm()`.

**Step 3: Verify RED**

Run the model adapter test; expect missing adapter failures.

**Step 4: Implement model generators and adapters**

Return both generated data and truth metadata. Preserve original row IDs so
missing rows and `obs_weights` remain aligned. Record separation,
non-convergence, aliased terms, and degenerate outcomes as explicit screening
or full-analysis failures.

**Step 5: Expand scenarios**

Include the approved covariate-strength, imbalance, missingness,
heteroscedasticity, prevalence, OR, rate, IRR, exposure, overdispersion, and
multi-df factors across core/stress/validation layers.

**Step 6: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-adapters-models.R")'
git add manuscript/calibration
git commit -m "feat: add regression calibration adapters"
```

### Task 6: Add Cox adapters

**Files:**
- Create: `manuscript/calibration/R/adapters_survival.R`
- Create: `manuscript/calibration/tests/testthat/test-adapters-survival.R`
- Modify: `manuscript/calibration/config/scenarios.R`

**Step 1: Write parity tests**

For single and multi-df terms, compare the screening Wald/LRT result with
`robustness_surv()`. Include exponential and Weibull baselines and fixed
censoring seeds.

**Step 2: Verify RED**

Expected: survival adapter missing.

**Step 3: Implement survival generation**

Generate event times from specified hazards, censoring times calibrated to the
target censoring fraction, and optional time-varying effects for PH stress
scenarios. Store realized event fraction and PH-stress metadata.

**Step 4: Implement screening and full adapters**

Use `survival::coxph()` with the same term-resolution semantics as the public
API. Record no-event, all-censored, non-convergent, and vanished-factor cases.

**Step 5: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-adapters-survival.R")'
git add manuscript/calibration
git commit -m "feat: add Cox calibration adapters"
```

### Task 7: Add equivalence and non-inferiority adapters

**Files:**
- Create: `manuscript/calibration/R/adapters_tost.R`
- Create: `manuscript/calibration/tests/testthat/test-adapters-tost.R`
- Modify: `manuscript/calibration/config/scenarios.R`

**Step 1: Write parity tests**

Cover equivalence and NI for mean, risk difference, and odds ratio; paired and
unpaired mean designs; both `higher_is_better` directions; symmetric and
asymmetric equivalence bounds. Require exact parity for `p_eff` and conclusion.

**Step 2: Verify RED**

Expected: TOST adapter missing.

**Step 3: Implement truth classification and generators**

Truth must be defined relative to the configured margin:

```r
classify_tost_truth <- function(effect, bounds, type, higher_is_better) {
  # returns false_conclusion, boundary_adjacent, or clear_conclusion
}
```

Do not infer truth from the realized sample estimate.

**Step 4: Implement screening and full adapters**

Screen with endpoint-specific formulas identical to the public implementation;
run full analyses through `robustness_tost()`.

**Step 5: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-adapters-tost.R")'
git add manuscript/calibration
git commit -m "feat: add TOST and NI calibration adapters"
```

## Phase 3: Build the Publication-Grade Runner

### Task 8: Implement deterministic two-stage screening and stratum selection

**Files:**
- Create: `manuscript/calibration/R/screening.R`
- Create: `manuscript/calibration/tests/testthat/test-screening.R`

**Step 1: Write failing selection tests**

Test that screening:

- records every generated dataset in the denominator;
- selects the requested quota per truth-by-conclusion stratum;
- produces identical selected replicate IDs for one versus multiple workers;
- stops with a precise status when a maximum screening budget cannot fill a
  stratum;
- never substitutes a failed screening fit for a non-conclusion.

**Step 2: Verify RED**

Run `test-screening.R`; expect missing functions.

**Step 3: Implement screening**

Implement:

```r
screen_scenario <- function(scenario, adapter, target_by_stratum,
                            max_draws, workers = 1L, checkpoint_root) { ... }
select_stratified_replicates <- function(screened, targets) { ... }
```

Selection must use a deterministic priority generated from the replicate seed,
not completion order.

**Step 4: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-screening.R")'
git add manuscript/calibration
git commit -m "feat: add stratified calibration screening"
```

### Task 9: Implement the full robustness executor and failure audit

**Files:**
- Create: `manuscript/calibration/R/executor.R`
- Create: `manuscript/calibration/tests/testthat/test-executor.R`

**Step 1: Write failing executor tests**

Use tiny fake adapters to test success, full-fit error, subset failure,
non-finite metrics, timeout/status propagation, checkpoint resume, and serial
versus parallel identity.

**Step 2: Verify RED**

Expected: executor missing.

**Step 3: Implement one-replicate execution**

Implement `run_selected_replicate()` with `tryCatch()`, elapsed-time capture,
schema construction, and explicit failure stages. Extract shared metrics by
their aligned aliases and preserve engine-specific diagnostics separately.

**Step 4: Implement scenario execution**

Implement `run_full_scenario()` using serial `lapply()` and base
`parallel` workers. Do not nest bootstrap parallelism. Checkpoint after a
bounded batch and validate before marking a scenario complete.

**Step 5: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-executor.R")'
git add manuscript/calibration
git commit -m "feat: execute calibration analyses with audit logging"
```

### Task 10: Add the CLI, pilot mode, smoke mode, and manifests

**Files:**
- Create: `manuscript/calibration/run_calibration.R`
- Create: `manuscript/calibration/R/cli.R`
- Create: `manuscript/calibration/R/manifest.R`
- Create: `manuscript/calibration/tests/testthat/test-cli.R`

**Step 1: Write CLI parsing tests**

Cover:

```text
--mode smoke|pilot|full
--phase screen|analyse|all
--engine all|two_sample|proportion|lm|binomial|poisson|cox|tost
--scenario <id>
--workers <positive integer>
--resume
--master-seed <integer>
--output <path>
```

Reject unknown flags, missing values, invalid workers, and incompatible modes.

**Step 2: Verify RED**

Expected: CLI helpers missing.

**Step 3: Implement CLI and manifests**

Every run writes the scenario-manifest hash, seed ledger, Git commit, dirty
status, command, package versions, R session, start/end time, worker count,
and output hashes. Full mode refuses a dirty checkout unless an explicit
`--allow-dirty` flag is present and recorded.

**Step 4: Add smoke and pilot behavior**

- Smoke: one scenario per engine, two selected replicates per stratum,
  `n_boot = 5`.
- Pilot: all core scenario shapes, 10–25 completed replicates per stratum,
  `n_boot = 50`, with runtime/failure projections.
- Full: frozen quotas and `n_boot = 1000`.

**Step 5: Run the smoke workflow**

```bash
Rscript manuscript/calibration/run_calibration.R \
  --mode smoke --phase all --engine all --workers 1 \
  --output /tmp/stabilitest-calibration-smoke
```

Expected: all adapters complete, artifacts validate, and the command exits 0.

**Step 6: Commit**

```bash
git add manuscript/calibration
git commit -m "feat: add calibration runner and manifests"
```

## Phase 4: Fit and Validate Calibration Policies

### Task 11: Implement uncertainty and operating-characteristic summaries

**Files:**
- Create: `manuscript/calibration/R/uncertainty.R`
- Create: `manuscript/calibration/R/summarise.R`
- Create: `manuscript/calibration/tests/testthat/test-summaries.R`

**Step 1: Write tests using hand-calculated fixtures**

Test Wilson one-sided bounds, scenario-cluster resampling, false reassurance,
robust identification, balanced ordinal accuracy, median ordering, completion
rates, and failure rates.

**Step 2: Verify RED**

Expected: summary functions missing.

**Step 3: Implement summary functions**

Core functions:

```r
wilson_interval <- function(x, n, conf_level = 0.95) { ... }
score_operating_characteristics <- function(replicates, cutoffs) { ... }
check_median_ordering <- function(replicates) { ... }
cluster_bootstrap_metrics <- function(replicates, statistic, B, seed) { ... }
monte_carlo_target_met <- function(summary, sap) { ... }
```

Cluster by scenario, not individual replicate, for pooled uncertainty.

**Step 4: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-summaries.R")'
git add manuscript/calibration
git commit -m "feat: summarise calibration operating characteristics"
```

### Task 12: Implement threshold fitting and one-time held-out validation

**Files:**
- Create: `manuscript/calibration/R/thresholds.R`
- Create: `manuscript/calibration/analyse_calibration.R`
- Create: `manuscript/calibration/tests/fixtures/training-replicates.rds`
- Create: `manuscript/calibration/tests/fixtures/validation-replicates.rds`
- Create: `manuscript/calibration/tests/testthat/test-thresholds.R`

**Step 1: Write frozen-fixture tests**

Test that:

- the shared 55/70 bands are always evaluated first;
- validation data cannot be passed to threshold fitting;
- cutoffs are ordered and bounded in [0, 100];
- family-specific bands require both at least 0.05 held-out improvement and a
  five-point material difference;
- a failed family becomes `uncalibrated`, not silently shared;
- repeated analysis produces byte-identical registry candidates.

**Step 2: Verify RED**

Expected: threshold helpers missing.

**Step 3: Implement constrained ordinal fitting**

Search ordered integer cutoff pairs on training data, maximize balanced ordinal
accuracy subject to the SAP false-reassurance and robust-identification
constraints, and resolve ties deterministically toward the shared cutoffs.

**Step 4: Implement the locked validation flow**

`analyse_calibration.R` must:

1. validate training and held-out manifests;
2. evaluate shared bands on training;
3. fit candidate family exceptions only where needed;
4. freeze and hash the candidate registry;
5. evaluate that registry once on held-out data;
6. produce `validated`, `family_specific`, or `uncalibrated` outcomes without
   refitting on held-out results.

**Step 5: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-thresholds.R")'
git add manuscript/calibration
git commit -m "feat: fit and validate calibration policies"
```

### Task 13: Implement the non-significant-result analysis

**Files:**
- Create: `manuscript/calibration/R/non_significant.R`
- Create: `manuscript/calibration/tests/testthat/test-non-significant.R`
- Modify: `manuscript/calibration/analyse_calibration.R`

**Step 1: Write policy tests**

Test that current Robust/Moderate/Fragile labels are never automatically
approved for non-significant results. Require separate discrimination,
ordering, and cross-family consistency outputs. When criteria fail, registry
status must be `bands_not_applicable` with no cutoffs.

**Step 2: Verify RED**

Expected: non-significant policy missing.

**Step 3: Implement exploratory summaries**

Compare true-null non-rejections with false negatives using score/component
distributions, AUC, ordering, sample-size sensitivity, and stress scenarios.
Do not introduce new categorical names in this task.

**Step 4: Verify and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-non-significant.R")'
git add manuscript/calibration
git commit -m "feat: evaluate non-significant score applicability"
```

## Phase 5: Run, Freeze, and Publish the Calibration

### Task 14: Run and review the pilot before freezing the full manifest

**Files:**
- Create: `manuscript/calibration/published/pilot-runtime-summary.csv`
- Create: `manuscript/calibration/published/pilot-failure-summary.csv`
- Modify: `manuscript/calibration/config/scenarios.R`
- Modify: `manuscript/calibration/CALIBRATION_SAP.md`

**Step 1: Run the pilot**

```bash
Rscript manuscript/calibration/run_calibration.R \
  --mode pilot --phase all --engine all --workers 1 \
  --output manuscript/calibration/artifacts/pilot
```

**Step 2: Review feasibility without examining threshold outcomes**

Use runtime, completion, and failure summaries only. Do not tune scenario truth
classes or cutoffs from pilot score distributions.

**Step 3: Freeze quotas and runtime plan**

Set per-stratum targets, maximum screening draws, worker plan, and acceptable
failure limits in the SAP. Hash the finalized scenario manifest.

**Step 4: Run all calibration infrastructure tests**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
```

**Step 5: Commit the frozen full-run contract**

```bash
git add manuscript/calibration
git commit -m "docs: freeze publication calibration manifest"
```

### Task 15: Execute the publication-grade training and validation runs

**Files:**
- Create: `manuscript/calibration/published/training-manifest.json` or `.dput`
- Create: `manuscript/calibration/published/validation-manifest.json` or `.dput`
- Create: `manuscript/calibration/published/output-hashes.txt`

**Step 1: Run training scenarios**

```bash
Rscript manuscript/calibration/run_calibration.R \
  --mode full --phase all --engine all --workers <N> --resume \
  --output manuscript/calibration/artifacts/raw/training
```

Expected: all core training strata meet their frozen completion/precision
targets or are explicitly marked unsupported.

**Step 2: Fit and freeze the candidate registry**

```bash
Rscript manuscript/calibration/analyse_calibration.R \
  --training manuscript/calibration/artifacts/raw/training \
  --freeze-candidate manuscript/calibration/artifacts/raw/candidate-registry.rds
```

**Step 3: Run held-out scenarios**

```bash
Rscript manuscript/calibration/run_calibration.R \
  --mode full --phase all --engine all --workers <N> --resume \
  --validation-only \
  --output manuscript/calibration/artifacts/raw/validation
```

**Step 4: Evaluate once without refitting**

```bash
Rscript manuscript/calibration/analyse_calibration.R \
  --candidate manuscript/calibration/artifacts/raw/candidate-registry.rds \
  --validation manuscript/calibration/artifacts/raw/validation \
  --publish manuscript/calibration/published
```

**Step 5: Independently review the frozen analysis**

Request statistical and code review. Verify scenario balance, manifest hashes,
failure exclusions, uncertainty calculations, and the no-refit guarantee before
any package behavior changes.

**Step 6: Commit only compact publication artifacts**

```bash
git add manuscript/calibration/published manuscript/calibration/CALIBRATION_SAP.md
git commit -m "data: publish analysis-specific calibration results"
```

Do not commit raw checkpoints or selected datasets.

## Phase 6: Integrate Validated Calibration into the Package

### Task 16: Add the machine-readable registry and applicability API

**Files:**
- Create: `inst/extdata/calibration-registry.csv`
- Create: `R/calibration_registry.R`
- Create: `tests/testthat/test-calibration-registry.R`
- Modify: `R/robustness_shared.R`

**Step 1: Write failing registry tests**

Test exact registry columns, unique family/conclusion keys, ordered cutoffs,
manifest hashes, calibration versions, and supported statuses:

```r
c("validated_shared", "validated_family_specific",
  "uncalibrated", "bands_not_applicable")
```

Test that `calibration_for_result()` returns the correct entry for every public
result family and never returns categorical cutoffs for non-significant results
unless explicitly validated by the published registry.

**Step 2: Verify RED**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry", stop_on_failure = TRUE)'
```

**Step 3: Implement registry loading and selection**

Implement internal helpers:

```r
load_calibration_registry <- function() { ... }
calibration_key <- function(result_style, analysis_type, endpoint,
                            conclusion_type, significant) { ... }
calibration_for_result <- function(...) { ... }
interpret_score <- function(score, calibration) { ... }
```

Return `NA_character_` for categorical interpretation when status is
`uncalibrated` or `bands_not_applicable`.

**Step 4: Verify and commit**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry", stop_on_failure = TRUE)'
git add inst/extdata/calibration-registry.csv R tests/testthat/test-calibration-registry.R
git commit -m "feat: add validated calibration registry"
```

### Task 17: Attach calibration metadata to every result class

**Files:**
- Modify: `R/robustness_analysis.R`
- Modify: `R/robustness_models.R`
- Modify: `R/robustness_tost.R`
- Modify: `R/robustness_shared.R`
- Modify: `tests/testthat/test-robustness_analysis.R`
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `tests/testthat/test-robustness_tost.R`

**Step 1: Write failing cross-class tests**

Require every result object to contain:

```r
calibration <- list(
  version = <character>,
  family = <character>,
  status = <character>,
  applicable = <logical>,
  cutoff_fragile = <numeric or NA>,
  cutoff_robust = <numeric or NA>,
  manifest_hash = <character>
)
```

Test validated shared, family-specific, uncalibrated, and non-significant paths.
Test print methods do not emit a Robust/Moderate/Fragile label when
`applicable = FALSE`.

**Step 2: Verify RED**

Run all three focused test files; expect missing calibration metadata.

**Step 3: Implement shared attachment**

Add one helper that receives the completed result, resolves its registry key,
sets the interpretation label, and appends calibration metadata. Call it from
all public analysis paths after common aliases are aligned.

**Step 4: Update print and narrative output**

For unsupported results, print the numeric score followed by
`categorical bands not applicable` or `analysis family not yet calibrated`.
Do not silently fall back to Welch thresholds.

**Step 5: Verify and commit**

```bash
Rscript -e 'devtools::test(filter = "robustness_analysis|edge-cases|robustness_tost", stop_on_failure = TRUE)'
git add R tests/testthat
git commit -m "feat: apply analysis-specific calibration policy"
```

## Phase 7: Split and Update User Documentation

### Task 18: Create the five-vignette structure

**Files:**
- Modify: `vignettes/pain-case-study.Rmd`
- Create: `vignettes/two-sample-comparisons.Rmd`
- Create: `vignettes/model-based-analyses.Rmd`
- Create: `vignettes/equivalence-noninferiority.Rmd`
- Create: `vignettes/calibration-interpretation.Rmd`

**Step 1: Add vignette build tests/checks**

Create a lightweight script or test that verifies unique vignette titles,
expected cross-links, no duplicated canonical function sections, and presence
of the calibration-status table.

**Step 2: Keep the pain vignette focused**

Retain the pain walkthrough and core component explanation. Replace the broad
API survey with a short navigation section linking to the specialized
vignettes.

**Step 3: Write the analysis-family vignettes**

- Two-sample: Welch/paired t, Wilcoxon, Brunner-Munzel, Fisher/chi-square/prop.
- Models: LM/ANCOVA, binomial/Poisson GLM, Cox, term selection, weights,
  missing rows, convergence, censoring.
- TOST/NI: endpoint types, margins, direction, effective p-values, sparse-data
  caveats.

Use one canonical explanation per function and modest evaluated examples.

**Step 4: Write the calibration vignette**

Read compact published artifacts and the installed registry. Explain the score,
validation design, shared/family-specific policy, uncertainty, supported
conditions, non-significant behavior, and reporting language. Do not recompute
publication simulations during vignette build.

**Step 5: Build all vignettes**

```bash
Rscript -e 'devtools::build_vignettes()'
```

Expected: five HTML vignettes and extracted R files build successfully.

**Step 6: Commit**

```bash
git add vignettes
git commit -m "docs: split statistical analysis vignettes"
```

### Task 19: Update package and manuscript documentation

**Files:**
- Modify: `README.md`
- Modify: `NEWS.md`
- Modify: `R/robustness_analysis.R`
- Modify: `R/robustness_models.R`
- Modify: `R/robustness_tost.R`
- Modify: `manuscript/robustness_analysis_manuscript.md`
- Modify: `manuscript/simulation_study.R`
- Modify: `manuscript/build_pdf.sh`
- Regenerate: `man/*.Rd`
- Regenerate: `doc/*`
- Regenerate: `manuscript/robustness_analysis_manuscript.pdf`

**Step 1: Update source documentation**

Replace claims that the Welch bands apply generally with registry-based wording.
Report the frozen analysis-specific results and uncertainty without overstating
unsupported families. Explain non-significant behavior explicitly.

**Step 2: Preserve the original Welch simulation as historical/reference work**

Make `simulation_study.R` point to the modular calibration workflow for current
results while retaining a documented command for reproducing the original
12-scenario study.

**Step 3: Regenerate help and artifacts**

```bash
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'devtools::build_vignettes()'
bash manuscript/build_pdf.sh
```

**Step 4: Search for stale claims**

```bash
rg -n "not separately calibrated|assumed transferable|bands do not apply|chance.*52|large.*75" \
  README.md NEWS.md R man vignettes manuscript doc
```

Expected: every remaining statement matches the published registry and clearly
states its scope.

**Step 5: Commit**

```bash
git add README.md NEWS.md R man vignettes manuscript
git commit -m "docs: publish analysis-specific calibration guidance"
```

## Phase 8: Final Verification and Release Readiness

### Task 20: Run complete verification and independent review

**Files:**
- Verify all changed files

**Step 1: Run calibration tests and smoke workflow**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
Rscript manuscript/calibration/run_calibration.R \
  --mode smoke --phase all --engine all --workers 2 \
  --output /tmp/stabilitest-calibration-final-smoke
```

Expected: all tests and adapters pass; parallel smoke results match serial
fixtures.

**Step 2: Run package verification**

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
Rscript manuscript/test_simulation_entrypoint.R
R CMD build .
R CMD check --as-cran stabilitest_*.tar.gz
```

Expected: no failures, errors, or unexpected warnings/notes.

**Step 3: Verify publication artifacts**

Recompute hashes, validate manifests and registry provenance, confirm held-out
data were evaluated only once, and compare every manuscript/vignette table with
the compact published CSV source.

**Step 4: Request code and statistical review**

Use `@superpowers:requesting-code-review`. Require reviewers to inspect adapter
parity, seed independence, failure accounting, train/validation separation,
threshold decision rules, registry integration, and documentation claims.

**Step 5: Run final diff checks**

```bash
git diff --check
git status --short
```

Confirm that raw calibration data, checkpoints, and unrelated user-owned files
are not staged.

**Step 6: Complete the branch**

Use `@superpowers:verification-before-completion`, then
`@superpowers:finishing-a-development-branch` to choose local merge, PR, branch
retention, or confirmed discard.
