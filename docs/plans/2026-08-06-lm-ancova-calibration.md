# LM/ANCOVA Categorical-Band Calibration Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build, execute, and publish an independently validated categorical-band calibration for eligible significant canonical 1-df ANCOVA treatment effects, while leaving all other linear-model results numeric-only.

**Architecture:** Add an isolated `lm_ancova` calibration study beneath the existing calibration harness, reusing shared schemas, execution, checkpoint, seed, and uncertainty helpers. Add a fail-closed runtime analysis profile to `robustness_lm()`, but do not activate labels until the full training -> frozen candidate -> fresh held-out gate passes and its artifacts are reviewed. Freeze a separate row-level synthetic pain-trial dataset before production calibration and use it only in a manuscript section after the calibration results and in a matching package vignette.

**Tech Stack:** R >= 4.2, base `stats::lm`, noncentral t calculations, `testthat`, `tibble`, `dplyr`, existing `stabilitest` calibration helpers, `devtools`, `roxygen2`, and `rcmdcheck`.

---

## Execution rules

- Work in a dedicated `codex/lm-ancova-calibration` worktree.
- Use `@superpowers:test-driven-development` for every code task.
- Use `@superpowers:systematic-debugging` for any unexpected failure.
- Use `@superpowers:verification-before-completion` before each gate is called complete.
- Do not inspect held-out scores before the candidate artifact and hash are frozen.
- Do not update the active package registry unless the held-out gate passes.
- Do not reuse the historical Task 15 validation results.
- Freeze the illustrative dataset's generator parameters and seed before
  production score inspection; never regenerate or select it by p-value,
  component metric, score, or categorical band.
- Keep the illustrative case-study ID and seed out of every calibration
  scenario, screening run, training artifact, candidate fit, and held-out run.
- Keep large raw/checkpoint outputs ignored; commit only code, SAPs, compact
  summaries, manifests, registries, and hash ledgers.

### Task 1: Add the canonical LM analysis profile

**Files:**
- Modify: `R/robustness_models.R:84-140`
- Modify: `R/robustness_models.R:426-475`
- Test: `tests/testthat/test-robustness_analysis.R`

**Step 1: Write failing profile-shape tests**

Add tests that call a new internal `lm_calibration_profile()` directly and via
`robustness_lm()`:

```r
test_that("canonical ANCOVA produces an eligible structural profile", {
  set.seed(101)
  dat <- data.frame(
    outcome = rnorm(80),
    treatment = factor(rep(c("A", "B"), each = 40)),
    baseline = rnorm(80)
  )
  fit <- lm(outcome ~ treatment + baseline, dat)
  term <- resolve_model_term(fit, "treatmentB")

  profile <- lm_calibration_profile(
    fit, term, original_n = nrow(dat), alpha = 0.05,
    n_boot = 1000L,
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    max_removal_pct = 0.30
  )

  expect_true(profile$canonical_ancova)
  expect_identical(profile$term_type, "single")
  expect_identical(profile$term_df, 1L)
  expect_identical(profile$treatment_levels, 2L)
  expect_identical(profile$baseline_count, 1L)
  expect_false(profile$omitted_rows)
})
```

Add separate expectations showing `canonical_ancova = FALSE` for a continuous
target coefficient, a three-level treatment, an interaction, an extra
covariate, a transformation, and an omitted row.

**Step 2: Run the focused test and verify failure**

Run:

```bash
Rscript -e 'devtools::test(filter = "robustness_analysis")'
```

Expected: FAIL because `lm_calibration_profile()` and
`result$analysis_profile` do not exist.

**Step 3: Implement coefficient ownership and profile extraction**

Add helpers that use the fitted terms and model-matrix `assign` attribute,
rather than parsing coefficient names:

```r
coefficient_term_label <- function(fit, coefficient) {
  mm <- stats::model.matrix(fit)
  index <- match(coefficient, colnames(mm))
  if (is.na(index)) return(NA_character_)
  assignment <- attr(mm, "assign")[[index]]
  if (is.na(assignment) || assignment == 0L) return(NA_character_)
  attr(stats::terms(fit), "term.labels")[[assignment]]
}

lm_calibration_profile <- function(fit, term_spec, original_n, alpha, n_boot,
                                   weights, max_removal_pct) {
  frame <- stats::model.frame(fit)
  response <- stats::model.response(frame)
  labels <- attr(stats::terms(fit), "term.labels")
  target_label <- coefficient_term_label(fit, term_spec$coef_name)
  direct_labels <- labels[grepl("^[.A-Za-z][.A-Za-z0-9_]*$", labels)]
  baseline_labels <- setdiff(direct_labels, target_label)
  target <- if (length(target_label) == 1L && target_label %in% names(frame)) {
    frame[[target_label]]
  } else NULL
  baseline <- if (length(baseline_labels) == 1L &&
                  baseline_labels %in% names(frame)) frame[[baseline_labels]] else NULL
  omitted <- !is.null(fit$na.action) || stats::nobs(fit) != original_n
  canonical <- identical(term_spec$type, "single") &&
    identical(as.integer(term_spec$ndf), 1L) && is.numeric(response) &&
    is.factor(target) && nlevels(target) == 2L &&
    length(labels) == 2L && length(direct_labels) == 2L &&
    is.numeric(baseline) && !omitted

  list(
    version = "lm-profile-1", canonical_ancova = canonical,
    term_type = term_spec$type, term_df = as.integer(term_spec$ndf),
    target_term = target_label,
    treatment_levels = if (is.factor(target)) nlevels(target) else NA_integer_,
    baseline_count = as.integer(length(baseline_labels)),
    response_numeric = is.numeric(response), baseline_numeric = is.numeric(baseline),
    additive_direct_terms = length(labels) == 2L && length(direct_labels) == 2L,
    omitted_rows = omitted, n = as.integer(stats::nobs(fit)),
    alpha = alpha, n_boot = as.integer(n_boot), weights = weights,
    max_removal_pct = max_removal_pct
  )
}
```

Attach the profile and explicit `n_boot` to the result before calibration is
resolved.

**Step 4: Run the focused test and verify success**

Run the command from Step 2.

Expected: PASS, including existing coefficient and joint-term tests.

**Step 5: Commit**

```bash
git add R/robustness_models.R tests/testthat/test-robustness_analysis.R
git commit -m "feat: record canonical ancova analysis profiles"
```

### Task 2: Make `lm_ancova` calibration fail closed on the profile

**Files:**
- Modify: `R/calibration_registry.R:348-492`
- Modify: `R/robustness_models.R:468-473`
- Test: `tests/testthat/test-calibration-registry.R`
- Test: `tests/testthat/test-robustness_analysis.R`

**Step 1: Write failing resolver tests with a custom validated row**

Construct a custom registry by changing only the `lm_ancova` row to
`validated_method_specific` with fixture cutoffs 50/65. Assert that a complete
canonical profile is applicable and that each of these is inapplicable:

- missing profile;
- joint term;
- noncanonical formula;
- `n < 40` or `n > 240`;
- nondefault alpha, `n_boot`, weights, or removal budget.

Use a helper fixture:

```r
canonical_profile_fixture <- function(...) {
  utils::modifyList(list(
    version = "lm-profile-1", canonical_ancova = TRUE,
    term_type = "single", term_df = 1L, treatment_levels = 2L,
    baseline_count = 1L, response_numeric = TRUE, baseline_numeric = TRUE,
    additive_direct_terms = TRUE, omitted_rows = FALSE, n = 80L,
    alpha = 0.05, n_boot = 1000L,
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    max_removal_pct = 0.30
  ), list(...))
}
```

**Step 2: Run the focused test and verify failure**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry|robustness_analysis")'
```

Expected: FAIL because the resolver ignores `analysis_profile`.

**Step 3: Implement unit-specific applicability**

Extend `resolve_result_calibration()` and `attach_result_calibration()` with an
optional `analysis_profile = NULL`. Add:

```r
.is_supported_lm_ancova_profile <- function(profile) {
  is.list(profile) && identical(profile$version, "lm-profile-1") &&
    isTRUE(profile$canonical_ancova) &&
    identical(profile$term_type, "single") && identical(profile$term_df, 1L) &&
    identical(profile$treatment_levels, 2L) &&
    identical(profile$baseline_count, 1L) &&
    isTRUE(profile$response_numeric) && isTRUE(profile$baseline_numeric) &&
    isTRUE(profile$additive_direct_terms) && !isTRUE(profile$omitted_rows) &&
    profile$n >= 40L && profile$n <= 240L &&
    isTRUE(all.equal(profile$alpha, 0.05)) &&
    identical(profile$n_boot, 1000L) &&
    .is_default_calibration_design(profile$weights, profile$max_removal_pct)
}
```

After exact registry lookup and before returning a validated `lm_ancova` row,
return an inapplicable result when this predicate is false. Pass
`out$analysis_profile` from `robustness_lm()`.

Do not change Welch resolution behavior. Keep the active `lm_ancova` registry
row uncalibrated during Gate A.

**Step 4: Run focused tests**

Run the command from Step 2.

Expected: PASS; Welch tests remain unchanged and all noncanonical LM fixtures
return `NA` labels.

**Step 5: Commit**

```bash
git add R/calibration_registry.R R/robustness_models.R \
  tests/testthat/test-calibration-registry.R \
  tests/testthat/test-robustness_analysis.R
git commit -m "feat: gate ancova bands on canonical profiles"
```

### Task 3: Scaffold the isolated ANCOVA study and scenario contract

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/README.md`
- Create: `manuscript/calibration/studies/lm_ancova/R/load_study.R`
- Create: `manuscript/calibration/studies/lm_ancova/config/scenarios.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-scenarios.R`
- Modify: `.gitignore`

**Step 1: Write failing scenario-contract tests**

Test that `lm_ancova_scenarios()` returns only `calibration_unit =
"lm_ancova"`, contains unique IDs and seeds, and exactly crosses:

```r
training <- expand.grid(
  n = c(40L, 80L, 160L), baseline_r2 = c(0.10, 0.40, 0.70),
  truth_class = c("null", "borderline", "clear")
)
validation <- expand.grid(
  n = c(60L, 120L, 240L), baseline_r2 = c(0.25, 0.55),
  truth_class = c("null", "borderline", "clear")
)
```

Assert that stress rows are marked `design_layer = "stress"` and never
`validation`, and that all rows pass `validate_calibration_scenarios()`.

**Step 2: Run the study test and verify failure**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
```

Expected: FAIL because the study loader and scenarios do not exist.

**Step 3: Implement the study loader and scenario builder**

`load_lm_ancova_study()` should:

1. call the existing `load_calibration(project_root, envir)`;
2. source study `R/*.R` except itself;
3. source the study scenario file last so `lm_ancova_scenarios()` is
   authoritative without changing `calibration_scenarios()` globally.

Build core/validation rows with the existing schema and nested parameters:

```r
parameters = list(
  generator = list(
    n = n, baseline_r2 = baseline_r2, target_power = target_power,
    allocation = 0.5, residual_sd = 1, effect_direction = 1
  ),
  analysis = list(
    formula = "outcome ~ treatment + baseline",
    term = "treatmentB", alpha = 0.05
  ),
  screening = list(conclusions = "significant", target_n = 100L)
)
```

Use `target_power = 0`, `0.60`, and `0.90` for null, borderline, and clear.
Add named stress rows for allocation, heteroscedasticity, heavy tails,
missingness, nonlinearity, and interaction.

Ignore only large study output directories:

```gitignore
/manuscript/calibration/studies/lm_ancova/artifacts/checkpoints/
/manuscript/calibration/studies/lm_ancova/artifacts/raw/
/manuscript/calibration/studies/lm_ancova/artifacts/pilot/
```

**Step 4: Run the study tests**

Run the command from Step 2.

Expected: PASS with 27 training core rows, 18 held-out rows, and the frozen
stress-row count.

**Step 5: Commit**

```bash
git add .gitignore manuscript/calibration/studies/lm_ancova
git commit -m "feat: define isolated ancova calibration study"
```

### Task 4: Implement and verify power-targeted generation

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/R/power.R`
- Create: `manuscript/calibration/studies/lm_ancova/R/generator.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-power.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-generator.R`

**Step 1: Write failing analytic power tests**

Test monotonicity, exact null effect, and recovered target power:

```r
testthat::test_that("ANCOVA effect solver reaches frozen power targets", {
  for (n in c(40L, 80L, 160L, 240L)) {
    for (target in c(0.60, 0.90)) {
      beta <- solve_ancova_effect(n, target, alpha = 0.05, residual_sd = 1)
      testthat::expect_equal(
        ancova_nominal_power(beta, n, alpha = 0.05, residual_sd = 1),
        target, tolerance = 1e-8
      )
    }
  }
})
```

Test that generated treatment allocation is exactly balanced when `n` is even,
baseline `R^2` is encoded through `gamma = sqrt(R2 / (1 - R2))`, and identical
seeds reproduce identical data.

**Step 2: Run tests and verify failure**

Run the study test command from Task 3.

Expected: FAIL because power and generator functions are missing.

**Step 3: Implement the noncentral-t solver**

```r
ancova_nominal_power <- function(beta, n, alpha = 0.05, residual_sd = 1,
                                 allocation = 0.5) {
  n1 <- round(n * allocation)
  n0 <- n - n1
  df <- n - 3L
  se <- residual_sd * sqrt(1 / n1 + 1 / n0)
  ncp <- beta / se
  critical <- stats::qt(1 - alpha / 2, df)
  stats::pt(-critical, df, ncp = ncp) +
    1 - stats::pt(critical, df, ncp = ncp)
}

solve_ancova_effect <- function(n, target_power, alpha = 0.05,
                                residual_sd = 1, allocation = 0.5) {
  if (identical(target_power, 0) || target_power == alpha) return(0)
  stats::uniroot(
    function(beta) ancova_nominal_power(beta, n, alpha, residual_sd,
                                         allocation) - target_power,
    interval = c(0, 10 * residual_sd)
  )$root
}
```

Implement `generate_lm_ancova()` with exact randomized allocation, shuffled
treatment labels, standard-normal baseline, the solved effect, and stress
switches. Return the same `data`, `truth`, `status`, and row-ID structure used
by existing model generators.

**Step 4: Add an independent Monte Carlo power verifier**

Implement `verify_ancova_power(scenario, draws, seed)` using only `lm()` and the
coefficient p-value. Unit tests may use 2,000 draws and tolerance 0.04; the
production power gate will use a larger frozen count and tolerance 0.02.

**Step 5: Run study tests**

Expected: PASS and no robustness score is computed by power verification.

**Step 6: Commit**

```bash
git add manuscript/calibration/studies/lm_ancova/R \
  manuscript/calibration/studies/lm_ancova/tests/testthat
git commit -m "feat: generate power-targeted ancova scenarios"
```

### Task 5: Add screening and robustness adapter parity

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/R/adapter.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-adapter.R`
- Modify: `manuscript/calibration/run_calibration.R:131-154`
- Modify: `manuscript/calibration/tests/testthat/test-cli.R`

**Step 1: Write failing parity tests**

For canonical generated data, compare:

```r
screen <- ancova_primary_decision(generated$data, scenario)
full <- robustness_lm(
  outcome ~ treatment + baseline, generated$data,
  term = "treatmentB", n_boot = 1L, seed = 44
)
expect_equal(screen$p_value, full$original_p, tolerance = 1e-12)
expect_equal(screen$estimate, full$original_estimate, tolerance = 1e-12)
expect_identical(screen$conclusion, full$original_significant)
```

Also test row IDs, missing-row failure metadata, and that the adapter returns
the executor's expected `generate`, `primary_decision`, and `run_robustness`
functions.

**Step 2: Write a failing optional-quota test for the shared runner**

Create a scenario with:

```r
parameters[[1]]$screening <- list(conclusions = "significant", target_n = 100L)
```

Assert `.calibration_target_by_stratum()` returns only
`null::significant = 100L`, while a historical scenario without this block
retains its existing two-conclusion behavior.

**Step 3: Run focused tests and verify failure**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-cli.R")'
Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
```

Expected: FAIL on the missing adapter and optional quota behavior.

**Step 4: Implement the adapter and backward-compatible quota override**

In `.calibration_target_by_stratum()`, read the optional screening block first:

```r
screening <- scenario$parameters[[1L]]$screening
if (is.list(screening) && length(screening$conclusions)) {
  target <- as.integer(screening$target_n)
  keys <- paste(scenario$truth_class[[1L]], screening$conclusions, sep = "::")
  return(stats::setNames(rep(target, length(keys)), keys))
}
```

Fall through to the unchanged historical behavior otherwise. Implement
`lm_ancova_adapter()` using `generate_lm_ancova()`, `ancova_primary_decision()`,
and `robustness_lm()`.

**Step 5: Run both focused suites**

Expected: PASS with no existing runner-test changes other than the new case.

**Step 6: Commit**

```bash
git add manuscript/calibration/run_calibration.R \
  manuscript/calibration/tests/testthat/test-cli.R \
  manuscript/calibration/studies/lm_ancova
git commit -m "feat: add ancova calibration adapter"
```

### Task 6: Add the isolated runner, manifests, and resume safety

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/run_calibration.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-runner.R`
- Modify: `manuscript/calibration/R/executor.R:270-350`
- Modify: `manuscript/calibration/tests/testthat/test-executor.R`

**Step 1: Write failing isolated-runner tests**

Source the study runner in a new environment and assert:

- smoke selection contains only `lm_ancova` scenarios;
- training excludes validation and validation-only selects only held-out rows;
- adapter dispatch returns `lm_ancova_adapter()`;
- the manifest hashes only the study scenario table;
- the command recorded in the manifest names the study runner.

**Step 2: Write a failing checkpoint mutation test**

Run a one-replicate fixture, mutate one scenario parameter, and assert resume
fails with `checkpoint manifest hash mismatch` rather than silently recomputing
or reusing rows.

**Step 3: Run tests and verify failure**

Run the study suite and the shared executor test.

Expected: FAIL because no study entry point exists and executor resume currently
swallows invalid checkpoints.

**Step 4: Implement the study wrapper**

The wrapper should load shared calibration code into a private environment,
source study helpers, source the shared runner, override only scenario and
adapter lookup in that private environment, and call `run_calibration()` with
an explicit project root and command. Do not change the historical scenario
function.

**Step 5: Make resume mismatch fatal**

In `run_full_scenario()`, distinguish a missing checkpoint from a present but
invalid checkpoint. If the file exists and `read_checkpoint()` rejects its
manifest hash or schema, propagate a clear error. Continue to reuse a valid,
complete checkpoint.

**Step 6: Run focused tests**

Expected: PASS, including all existing checkpoint and executor cases.

**Step 7: Commit**

```bash
git add manuscript/calibration/R/executor.R \
  manuscript/calibration/tests/testthat/test-executor.R \
  manuscript/calibration/studies/lm_ancova
git commit -m "feat: run isolated ancova calibration"
```

### Task 7: Implement deterministic ANCOVA cutoff fitting

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/R/thresholds.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-thresholds.R`

**Step 1: Create synthetic score fixtures**

Build one fixture where 50/70 is uniquely feasible and another where no pair
can satisfy false reassurance and robust identification simultaneously. Include
`scenario_id`, `truth_class`, `analysis_conclusion = "significant"`, and
`overall_score`.

**Step 2: Write failing metric and gate tests**

Test:

- false reassurance is `P(score > L | null, significant)`;
- robust identification is `P(score > U | clear, significant)`;
- class accuracy uses Fragile/Moderate/Robust for null/borderline/clear;
- balanced accuracy is the mean of the three class accuracies;
- each hard point and Wilson-bound constraint rejects independently;
- median ordering rejects a reversal;
- the infeasible fixture returns `status = "uncalibrated"` and
  `reason = "no_feasible_thresholds"`.

**Step 3: Run study tests and verify failure**

Expected: FAIL because ANCOVA threshold functions are missing.

**Step 4: Implement metrics and ordered integer search**

Implement:

```r
ancova_cutoff_metrics <- function(data, cutoffs) { ... }
ancova_training_feasible <- function(metrics) { ... }
fit_lm_ancova_cutoffs <- function(training) { ... }
```

Search `L = 0:99` and `U = (L + 1):100`. Filter on the frozen criteria, then
order feasible rows by:

```r
order(
  -balanced_accuracy,
  -minimum_class_accuracy,
  -constraint_safety_margin,
  lower_cutoff,
  upper_cutoff
)
```

Return the complete grid diagnostics with the selected row so review can
reproduce the decision. Calculate Welch 55/70 metrics in a separate comparator
field; never use them as fallback cutoffs.

**Step 5: Run study tests**

Expected: PASS with the exact known pair and deterministic repeated results.

**Step 6: Commit**

```bash
git add manuscript/calibration/studies/lm_ancova/R/thresholds.R \
  manuscript/calibration/studies/lm_ancova/tests/testthat/test-thresholds.R
git commit -m "feat: fit ancova-specific score bands"
```

### Task 8: Freeze candidates and evaluate clustered held-out uncertainty

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/analyse_calibration.R`
- Create: `manuscript/calibration/studies/lm_ancova/R/validation.R`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-validation.R`

**Step 1: Write failing cluster-bootstrap tests**

Use a small fixed fixture with three scenarios and a fixed bootstrap seed.
Assert identical bounds across repeated calls and verify that resampling units
are whole scenario IDs rather than rows.

**Step 2: Write failing freeze/no-refit tests**

Test that:

- `freeze_lm_ancova_candidate()` hashes the candidate plus scenario manifest;
- any candidate mutation changes the hash;
- `validate_lm_ancova_candidate()` accepts only a frozen object and matching
  training/validation manifest hashes;
- validation has no call path to `fit_lm_ancova_cutoffs()`;
- no training candidate leaves held-out unopened;
- held-out failure returns `uncalibrated` without a replacement pair.

**Step 3: Run study tests and verify failure**

Expected: FAIL because validation and freeze functions are missing.

**Step 4: Implement deterministic cluster bounds**

Resample unique `scenario_id` values with replacement using a fixed seed,
include every row belonging to each sampled scenario, recompute the statistic,
and take one-sided empirical 5th/95th percentiles. Return both Wilson and
cluster bounds and use the more conservative bound for acceptance.

**Step 5: Implement held-out gates**

Require:

- false-reassurance point <= 0.05 and conservative upper <= 0.10;
- robust-identification point >= 0.70 and conservative lower >= 0.60;
- balanced accuracy >= 0.70 and cluster lower >= 0.65;
- every class accuracy >= 0.60;
- ordered medians within matched held-out blocks;
- each required scenario quota >= 100.

Set `validation_refit = FALSE` in every result and manifest-facing summary.

**Step 6: Run study tests**

Expected: PASS, including deliberate mutation and no-candidate cases.

**Step 7: Commit**

```bash
git add manuscript/calibration/studies/lm_ancova/analyse_calibration.R \
  manuscript/calibration/studies/lm_ancova/R/validation.R \
  manuscript/calibration/studies/lm_ancova/tests/testthat/test-validation.R
git commit -m "feat: validate frozen ancova bands"
```

### Task 9: Publish complete failure accounting and immutable artifacts

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/tools/assemble_replicates.R`
- Create: `manuscript/calibration/studies/lm_ancova/tools/freeze_and_publish.R`
- Create: `manuscript/calibration/studies/lm_ancova/published/README.md`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-publication.R`

**Step 1: Write failing assembly-accounting tests**

Create fixture rows with completed, failed, and excluded statuses. Assert the
publication manifest reports all attempts and that per-scenario failure rates
are calculated before completed-only analysis tables are formed.

Test hard failure for:

- required quota shortfall;
- scenario failure rate > 5%;
- missing scenario checkpoint;
- nonzero assembly child-process status;
- stale artifact already present at the publication destination;
- missing required hash target.

**Step 2: Run study tests and verify failure**

Expected: FAIL because study publication tools are missing.

**Step 3: Implement assembly and publication**

Write separate outputs for:

- completed training/validation replicates;
- full audit rows;
- scenario occupancy summary;
- failure/exclusion summary;
- power-verification summary;
- candidate diagnostics;
- validation diagnostics;
- method-specific registry CSV/RDS;
- training and validation manifests;
- output hash ledger.

Publish atomically through a temporary directory. Refuse overwrite unless an
explicit development-only test flag is supplied; production publication should
require an empty destination and a clean checkout.

**Step 4: Run study tests**

Expected: PASS and every compact publication file appears in the hash ledger.

**Step 5: Commit**

```bash
git add manuscript/calibration/studies/lm_ancova/tools \
  manuscript/calibration/studies/lm_ancova/published \
  manuscript/calibration/studies/lm_ancova/tests/testthat/test-publication.R
git commit -m "feat: publish auditable ancova calibration"
```

### Task 10: Add a reduced end-to-end fixture

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/tests/fixtures/training-replicates.rds`
- Create: `manuscript/calibration/studies/lm_ancova/tests/fixtures/validation-replicates.rds`
- Create: `manuscript/calibration/studies/lm_ancova/tests/testthat/test-end-to-end.R`

**Step 1: Write a failing end-to-end test**

The test must run:

```text
load study -> validate scenarios -> fit training -> freeze candidate ->
validate held-out without refit -> publish to tempdir -> verify hashes
```

Assert exact fixture candidate cutoffs and hashes, `validation_refit = FALSE`,
and correct registry status.

**Step 2: Generate deterministic reduced fixtures**

Use a committed fixture-generation function with fixed seeds. Keep fixture
scores small and intentionally constructed; do not substitute them for the
production simulation. Save RDS with serialization version 2.

**Step 3: Run the test and verify failure before installing fixtures**

Expected: FAIL on missing fixture files.

**Step 4: Install fixtures and rerun**

Run the complete study test directory twice.

Expected: PASS both times with identical hashes.

**Step 5: Commit**

```bash
git add -f manuscript/calibration/studies/lm_ancova/tests/fixtures \
  manuscript/calibration/studies/lm_ancova/tests/testthat/test-end-to-end.R
git commit -m "test: lock ancova calibration workflow"
```

### Task 11: Freeze the illustrative synthetic ANCOVA pain trial

**Files:**
- Create: `data-raw/pain_ancova_trial.R`
- Create: `data/pain_ancova_trial.rda`
- Create: `R/data_pain_ancova.R`
- Create: `tests/testthat/test-data-pain-ancova.R`
- Create: `manuscript/calibration/studies/lm_ancova/manuscript.md`
- Create: `vignettes/ancova-case-study.Rmd`

**Step 1: Write failing dataset-contract tests**

Require the packaged object to be a complete 80-row data frame with exactly:

```r
c("subject_id", "arm", "baseline_pain", "week12_pain", "change")
```

Test unique subject IDs, 40 participants per arm, factor levels
`c("Placebo", "Active")`, numeric pain fields, `change == week12_pain -
baseline_pain` to floating-point tolerance, no missing values, and plausible
0--100 baseline/outcome values.

Source `data-raw/pain_ancova_trial.R` into a clean environment and require exact
identity between the regenerated and packaged objects. Assert its seed is not
present in `lm_ancova_scenarios()` or either calibration seed ledger.

**Step 2: Run focused tests and verify failure**

```bash
Rscript -e 'devtools::test(filter = "data-pain-ancova")'
```

Expected: FAIL because `pain_ancova_trial` and its frozen generator do not
exist.

**Step 3: Implement and freeze the generator before inspecting its analysis**

Use one committed generator with an exact balanced randomized assignment and a
single unconditional draw. Freeze constants in the script:

```r
PAIN_ANCOVA_CASE_SEED <- 20260806L
PAIN_ANCOVA_CASE_N <- 80L

generate_pain_ancova_trial <- function(seed = PAIN_ANCOVA_CASE_SEED) {
  set.seed(seed)
  arm <- sample(rep(c("Placebo", "Active"), each = PAIN_ANCOVA_CASE_N / 2L))
  baseline <- round(
    pmin(90, pmax(35, stats::rnorm(PAIN_ANCOVA_CASE_N, 65, 12))), 1
  )
  week12 <- round(
    20 + 0.60 * baseline - 7.5 * (arm == "Active") +
      stats::rnorm(PAIN_ANCOVA_CASE_N, 0, 10),
    1
  )
  data.frame(
    subject_id = sprintf("PAIN-A%03d", seq_len(PAIN_ANCOVA_CASE_N)),
    arm = factor(arm, levels = c("Placebo", "Active")),
    baseline_pain = baseline,
    week12_pain = week12,
    change = round(week12 - baseline, 1),
    stringsAsFactors = FALSE
  )
}
```

If the plausibility test shows values outside 0--100, revise the prespecified
DGP and seed before saving or analyzing the object. Once the contract passes,
save the first generated dataset with serialization version 2. Do not inspect
its coefficient p-value, robustness score, or band before committing this
freeze.

**Step 4: Document the package dataset**

In `R/data_pain_ancova.R`, state that the data are fixed, synthetic,
prospectively frozen, and excluded from calibration fitting and validation.
Document all five fields and reference the data-raw generator.

**Step 5: Commit the frozen data before any primary or robustness analysis**

```bash
git add data-raw/pain_ancova_trial.R data/pain_ancova_trial.rda \
  R/data_pain_ancova.R tests/testthat/test-data-pain-ancova.R
git commit -m "data: freeze synthetic ancova pain trial"
```

Record this commit, the generator seed, and the dataset MD5. From this point,
changing the DGP, seed, or rows requires a new explicitly versioned case-study
dataset; it may never be changed to improve the observed analysis.

**Step 6: Create manuscript and vignette structure without outcome claims**

The study manuscript must order its major sections as:

```text
Methods
Calibration results
Illustrative synthetic case study
Discussion
```

The case-study section and `vignettes/ancova-case-study.Rmd` must contain the
same primary call:

```r
case_result <- robustness_lm(
  week12_pain ~ arm + baseline_pain,
  pain_ancova_trial,
  term = "armActive",
  alpha = 0.05,
  n_boot = 1000,
  max_removal_pct = 0.30,
  weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
  seed = 1408
)
```

At Gate A, describe the dataset and intended outputs but make no claim about
the final score or band. State that the vignette will honestly render either a
validated label or an uncalibrated numeric-only result.

**Step 7: Run dataset tests and render the vignette**

```bash
Rscript -e 'devtools::test(filter = "data-pain-ancova")'
Rscript -e 'rmarkdown::render("vignettes/ancova-case-study.Rmd", output_dir = tempdir(), quiet = TRUE)'
```

Expected: PASS; the vignette renders with the active Gate A policy and does not
claim a calibrated ANCOVA band.

**Step 8: Commit the case-study manuscript and vignette structure**

```bash
git add manuscript/calibration/studies/lm_ancova/manuscript.md \
  vignettes/ancova-case-study.Rmd
git commit -m "docs: add synthetic ancova pain case study"
```

Record both commits and the dataset MD5 in the ANCOVA SAP before Task 14 begins.

### Task 12: Freeze the ANCOVA SAP and Gate A documentation

**Files:**
- Create: `manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md`
- Modify: `manuscript/calibration/studies/lm_ancova/README.md`
- Modify: `manuscript/calibration/README.md`
- Modify: `README.md`
- Modify: `NEWS.md`
- Modify: `tools/check-calibration-documentation.R`
- Test: `tests/testthat/test-calibration-documentation.R`

**Step 1: Write failing documentation-audit assertions**

Require documentation to state:

- canonical significant 1-df treatment coefficient only;
- 60%/90% power-defined truth strata;
- multi-df labels remain suppressed;
- score weights remain frozen;
- 55/70 is a Welch comparator, not ANCOVA fallback;
- active `lm_ancova` remains uncalibrated until Gate B;
- `pain_ancova_trial` is a prospectively frozen synthetic illustration that
  never enters training or held-out evidence;
- the manuscript case study follows, rather than precedes, calibration results.

**Step 2: Run the audit and verify failure**

```bash
Rscript tools/check-calibration-documentation.R
```

Expected: FAIL on missing ANCOVA policy text.

**Step 3: Write the frozen SAP and commands**

Document exact scenario grids, power validation draw count/tolerance, master
seeds, scenario quotas, `n_boot = 1000`, full screening budget, failure limits,
cutoff search, tie-breaks, held-out bounds, cluster bootstrap seed/draw count,
publication hash targets, the frozen case-study commit/seed/dataset MD5, and an
assertion that the case-study ID and seed are absent from calibration ledgers.
Include canonical smoke, pilot, training, freeze, held-out, and publication
commands.

**Step 4: Update user-facing Gate A wording**

Describe the new study as planned/implemented infrastructure only. Do not claim
validated ANCOVA bands and do not change the active registry row.

**Step 5: Run documentation and test suites**

```bash
Rscript tools/check-calibration-documentation.R
Rscript -e 'devtools::test()'
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary")'
Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
Rscript -e 'rmarkdown::render("vignettes/ancova-case-study.Rmd", output_dir = tempdir(), quiet = TRUE)'
```

Expected: all PASS.

**Step 6: Commit**

```bash
git add README.md NEWS.md manuscript/calibration/README.md \
  manuscript/calibration/studies/lm_ancova tools/check-calibration-documentation.R \
  tests/testthat/test-calibration-documentation.R
git commit -m "docs: freeze ancova calibration protocol"
```

### Task 13: Verify Gate A before spending production compute

**Files:**
- No code changes expected.

**Step 1: Run formatting and source-tree checks**

```bash
git diff --check
Rscript tools/check-calibration-documentation.R
```

Expected: PASS with no whitespace errors.

**Step 2: Run all package and calibration tests**

```bash
Rscript -e 'devtools::test()'
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary")'
Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
Rscript -e 'rmarkdown::render("vignettes/ancova-case-study.Rmd", output_dir = tempdir(), quiet = TRUE)'
```

Expected: all PASS.

**Step 3: Regenerate package documentation and confirm no unexpected drift**

```bash
Rscript -e 'roxygen2::roxygenise()'
git status --short
```

Expected: only intentional documentation changes. Commit any generated `man/`
or `NAMESPACE` changes with the owning source change; otherwise restore nothing
and investigate drift.

**Step 4: Run package check**

```bash
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
```

Expected: 0 errors, 0 warnings, and only the accepted `New submission` NOTE.

**Step 5: Record the Gate A review commit**

If verification generated no changes, record the verified commit hash in the
pilot manifest rather than creating an empty commit.

### Task 14: Run pilot, power gate, and production training

**Files:**
- Create after successful runs: compact files under
  `manuscript/calibration/studies/lm_ancova/artifacts/summaries/`
- Modify: `manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md`

**Step 1: Run smoke and pilot without score inspection**

```bash
Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode smoke --phase all --engine lm --workers 1 \
  --output /tmp/stabilitest-lm-ancova-smoke

Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode pilot --phase all --engine lm --workers 1 \
  --output manuscript/calibration/studies/lm_ancova/artifacts/pilot
```

Expected: complete wiring, runtime, occupancy, and failure summaries. Do not
open pilot score distributions.

**Step 2: Run the frozen primary-test-only power verification**

Use the draw count and tolerance frozen in the SAP. Expected: every canonical
borderline/clear scenario is within 0.02 of 0.60/0.90; null type-I error is
within its frozen tolerance of 0.05. If not, revise the design under a new
manifest version before running robustness training.

**Step 3: Freeze the execution manifest hash in the SAP**

Record the exact scenario hash, code commit, power artifact hash, runtime
projection, quotas, and worker plan. Commit:

```bash
git add manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md \
  manuscript/calibration/studies/lm_ancova/artifacts/summaries
git commit -m "docs: freeze ancova production execution"
```

**Step 4: Run production training only**

```bash
Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode full --phase all --engine lm --workers <N> --resume \
  --output manuscript/calibration/studies/lm_ancova/artifacts/raw/training
```

Expected: all core eligible quotas complete, <=5% failures per required
scenario, and no validation scenario/seed accessed.

**Step 5: Fit and freeze the candidate**

Run the study assembly/analysis command against training only. If no feasible
candidate exists:

- publish the explicit training-stage `uncalibrated/no_feasible_thresholds`
  artifact;
- do not run validation;
- skip to Task 16's uncalibrated path.

If a candidate exists, commit its compact diagnostics, manifest, and hash
before continuing.

### Task 15: Open held-out once and publish the decision

**Files:**
- Create: compact artifacts under
  `manuscript/calibration/studies/lm_ancova/published/`
- Modify: `manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md`

**Step 1: Verify the frozen candidate and clean checkout**

Confirm the candidate hash, scenario manifest hash, training commit, and empty
validation output directory. Abort on any mismatch.

**Step 2: Run held-out validation once**

```bash
Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode full --phase all --engine lm --workers <N> --resume \
  --validation-only \
  --output manuscript/calibration/studies/lm_ancova/artifacts/raw/validation
```

Expected: validation rows only, complete quotas, acceptable failure rate, and
no fitting call.

**Step 3: Freeze and publish**

```bash
Rscript manuscript/calibration/studies/lm_ancova/tools/freeze_and_publish.R
```

Expected: `validation_refit = FALSE`, matching manifests, full failure
accounting, and a complete hash ledger.

**Step 4: Independently review the statistical and code artifacts**

Review scenario balance, power verification, candidate selection, conservative
bounds, per-scenario diagnostics, failures, exclusions, hashes, and runtime
applicability. Do not update package runtime behavior until both reviews agree
with the published status.

**Step 5: Commit immutable compact publication artifacts**

```bash
git add manuscript/calibration/studies/lm_ancova/published \
  manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md
git commit -m "data: publish ancova calibration decision"
```

### Task 16: Integrate the reviewed outcome into the active package policy

**Files:**
- Modify: `inst/extdata/calibration-registry.csv`
- Modify: `R/calibration_registry.R:144-198`
- Modify: `R/robustness_models.R:358-407`
- Modify: `tests/testthat/test-calibration-registry.R`
- Modify: `tests/testthat/test-robustness_analysis.R`
- Modify: `README.md`
- Modify: `NEWS.md`
- Modify: `man/robustness_lm.Rd` through roxygen generation
- Modify: `man/pain_ancova_trial.Rd` through roxygen generation
- Modify: `manuscript/calibration/README.md`
- Modify: `manuscript/calibration/studies/lm_ancova/manuscript.md`
- Modify: `vignettes/ancova-case-study.Rmd`
- Modify: `tools/check-calibration-documentation.R`

**Step 1: Choose the path strictly from the reviewed artifact**

- If status is `validated_method_specific`, continue with the frozen cutoffs,
  version, source hash, and supported conditions.
- If status is `uncalibrated`, keep NA cutoffs and label suppression; update
  only provenance and documentation of the negative result.

Never substitute Welch 55/70 values.

**Step 2: Write the failing active-registry test**

For a validated result, assert an eligible canonical significant fixture gets
the exact frozen label at both boundaries, while every rejected profile remains
`NA`. For an uncalibrated result, assert all LM profiles remain `NA` and the new
version/source is recorded.

**Step 3: Run focused tests and verify failure**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry|robustness_analysis")'
```

Expected: FAIL until the active contract and CSV match the reviewed artifact.

**Step 4: Update the active contract and registry**

Copy only reviewed values. For a validated row, use:

```text
status = validated_method_specific
cutoff_fragile = <frozen L>
cutoff_robust = <frozen U>
version = <frozen calibration version>
source = <immutable artifact/commit reference>
supported_conditions = <approved canonical profile text>
```

Keep all other calibration units unchanged.

**Step 5: Populate the post-results case study without selecting the data**

Run the exact frozen analysis once and populate the manuscript section after
the calibration results with the adjusted treatment estimate and p-value,
profile eligibility, component metrics, composite score, label status, and
deletion/influence figures. Update the vignette to explain the same outcome.

If calibration validated but the frozen case result is non-significant, report
that no band is applicable. If calibration remained uncalibrated, report the
numeric score/components and the suppressed label. Do not regenerate, replace,
or edit the dataset in either case.

**Step 6: Update documentation and regenerate roxygen output**

```bash
Rscript -e 'roxygen2::roxygenise()'
```

Ensure examples do not imply multi-df or arbitrary-LM coverage.

**Step 7: Run final verification**

```bash
git diff --check
Rscript tools/check-calibration-documentation.R
Rscript -e 'devtools::test()'
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary")'
Rscript -e 'testthat::test_dir("manuscript/calibration/studies/lm_ancova/tests/testthat", reporter = "summary")'
Rscript -e 'rmarkdown::render("vignettes/ancova-case-study.Rmd", output_dir = tempdir(), quiet = TRUE)'
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
```

Expected: all tests pass; package check has 0 errors, 0 warnings, and only the
accepted `New submission` NOTE.

**Step 8: Commit the reviewed integration**

```bash
git add inst/extdata/calibration-registry.csv R/calibration_registry.R \
  R/robustness_models.R tests/testthat README.md NEWS.md man \
  manuscript/calibration/README.md \
  manuscript/calibration/studies/lm_ancova/manuscript.md \
  vignettes/ancova-case-study.Rmd tools/check-calibration-documentation.R
git commit -m "feat: integrate ancova calibration decision"
```

### Task 17: Final review and branch handoff

**Files:**
- No code changes expected.

**Step 1: Review the complete diff against the design**

Confirm that the implementation still satisfies
`docs/plans/2026-08-06-lm-ancova-calibration-design.md`, especially the 1-df
scope, no-refit rule, fail-closed behavior, post-results manuscript ordering,
and complete separation of `pain_ancova_trial` from calibration evidence.

**Step 2: Confirm repository state and commits**

```bash
git status --short
git log --oneline --decorate -15
```

Expected: clean worktree and reviewable, task-scoped commits.

**Step 3: Use `@superpowers:requesting-code-review`**

Request review of both runtime safety and calibration methodology. Address
findings with `@superpowers:receiving-code-review` and rerun the affected
verification commands.

**Step 4: Use `@superpowers:finishing-a-development-branch`**

Present merge/PR/cleanup choices only after every required check passes and the
published status is accurately reflected in runtime behavior.
