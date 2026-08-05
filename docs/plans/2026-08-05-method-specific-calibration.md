# Method-Specific Calibration and Interpretation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove `two_sample` as an active calibration identity, add exact method-level registry lookup, preserve narrowly applicable Welch bands, and suppress categorical labels for every uncalibrated result while retaining numeric scores and components.

**Architecture:** Keep the exported analysis functions unchanged, but attach calibration metadata only after each wrapper has resolved its actual method, endpoint, and observed conclusion. Add a package registry keyed by calibration unit rather than execution engine; migrate the simulation pipeline to separate routing (`analysis_engine`) from calibration identity (`calibration_family`, `calibration_unit`). Preserve Task 15 artifacts as an inactive historical freeze and prepare future runs to calibrate one method at a time.

**Tech Stack:** R 4.2+, base R, tibble/dplyr, testthat, devtools, roxygen2, rcmdcheck, the existing calibration simulation pipeline.

---

## Execution Baseline

The integrated calibration implementation is on `codex/calibration-task1` at
`5d83d03`; it is not on `main`. The existing worktree for that branch contains
an uncommitted independent review, so do not modify or clean it.

Before Task 1, use `@superpowers:using-git-worktrees` to create a fresh
worktree and branch from `codex/calibration-task1`:

```bash
git worktree add .worktrees/method-specific-calibration \
  -b codex/method-specific-calibration codex/calibration-task1
cd .worktrees/method-specific-calibration
git cherry-pick 0d867cc
```

After this plan is committed, also cherry-pick its commit or merge
`codex/method-specific-calibration-design` before implementation. Verify the
baseline before changing code:

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
git status --short
```

Expected: both suites pass and the implementation worktree is clean.

### Task 1: Add the method-level registry contract

**Files:**
- Create: `inst/extdata/calibration-registry.csv`
- Create: `R/calibration_registry.R`
- Create: `tests/testthat/test-calibration-registry.R`

**Step 1: Write failing registry and mapping tests**

Create `tests/testthat/test-calibration-registry.R` with exact mapping coverage:

```r
test_that("every public two-vector test maps to one calibration unit", {
  expected <- c(
    "t.test" = "welch_unpaired",
    "paired.t.test" = "paired_t",
    "wilcoxon" = "wilcoxon_rank_sum",
    "wilcoxon.paired" = "wilcoxon_signed_rank",
    "brunner_munzel" = "brunner_munzel",
    "fisher" = "fisher_exact",
    "chisq" = "chi_square_2x2",
    "prop" = "two_sample_prop"
  )
  expect_identical(
    unname(vapply(names(expected), calibration_unit_for_test, character(1))),
    unname(expected)
  )
  expect_false("two_sample" %in% unname(expected))
})

test_that("model and TOST wrappers map independently of execution engine", {
  expect_identical(calibration_unit_for_model("lm"), "lm_ancova")
  expect_identical(calibration_unit_for_model("glm", family = "binomial"),
                   "glm_binomial")
  expect_identical(calibration_unit_for_model("glm", family = "poisson"),
                   "glm_poisson")
  expect_identical(calibration_unit_for_model("cox"), "cox_ph")
  expect_identical(calibration_unit_for_tost("mean"), "tost_mean")
  expect_identical(calibration_unit_for_tost("prop"), "tost_risk_difference")
  expect_identical(calibration_unit_for_tost("or"), "tost_odds_ratio")
})

test_that("the installed registry contains no generic two_sample key", {
  registry <- load_calibration_registry()
  expect_false(any(registry$calibration_unit == "two_sample"))
  expect_true(all(c(
    "family", "calibration_unit", "endpoint", "conclusion_type",
    "status", "cutoff_fragile", "cutoff_robust", "version", "source",
    "supported_conditions"
  ) %in% names(registry)))
})
```

Add validation tests for duplicate compound keys, uncalibrated rows with
non-missing cutoffs, validated rows without provenance, unknown status values,
and missing required columns.

**Step 2: Run the tests and verify RED**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry", stop_on_failure = TRUE)'
```

Expected: failure because the mapping and registry functions do not exist.

**Step 3: Add the registry CSV**

Create one row for each calibration unit in the approved design. The active
Welch row is:

```csv
family,calibration_unit,endpoint,conclusion_type,status,cutoff_fragile,cutoff_robust,version,source,supported_conditions
continuous_parametric,welch_unpaired,mean_difference,significant,validated_method_specific,55,70,welch-2026-1,manuscript/simulation_results.csv,"independent groups; Welch test; significant superiority conclusion; default score definition and weights; original documented simulation scope"
```

All other rows use `status = uncalibrated`, empty cutoffs, version
`taxonomy-2026-1`, and an explanatory source/condition string. Add separate
TOST rows for equivalence and non-inferiority conclusion types.

**Step 4: Implement registry loading, validation, and exact mappings**

In `R/calibration_registry.R`, implement:

```r
.calibration_statuses <- c(
  "validated_method_specific", "uncalibrated", "bands_not_applicable"
)

calibration_unit_for_test <- function(test_type) {
  units <- c(
    "t.test" = "welch_unpaired",
    "paired.t.test" = "paired_t",
    "wilcoxon" = "wilcoxon_rank_sum",
    "wilcoxon.paired" = "wilcoxon_signed_rank",
    "brunner_munzel" = "brunner_munzel",
    "fisher" = "fisher_exact",
    "chisq" = "chi_square_2x2",
    "prop" = "two_sample_prop"
  )
  if (length(test_type) != 1L || is.na(test_type) || !test_type %in% names(units)) {
    stop("unknown calibration test type", call. = FALSE)
  }
  unname(units[[test_type]])
}

calibration_unit_for_model <- function(engine, family = NULL) {
  if (identical(engine, "lm")) return("lm_ancova")
  if (identical(engine, "cox")) return("cox_ph")
  if (identical(engine, "glm") && identical(family, "binomial")) return("glm_binomial")
  if (identical(engine, "glm") && identical(family, "poisson")) return("glm_poisson")
  stop("unknown model calibration unit", call. = FALSE)
}

calibration_unit_for_tost <- function(endpoint) {
  units <- c(mean = "tost_mean", prop = "tost_risk_difference",
             or = "tost_odds_ratio")
  if (length(endpoint) != 1L || is.na(endpoint) || !endpoint %in% names(units)) {
    stop("unknown TOST calibration endpoint", call. = FALSE)
  }
  unname(units[[endpoint]])
}
```

Implement `validate_calibration_registry()` using the compound key
`calibration_unit + endpoint + conclusion_type`. Require missing cutoffs for
uncalibrated/inapplicable rows and finite ordered cutoffs for validated rows.
Implement `load_calibration_registry()` with an optional `path` argument for
tests and `system.file("extdata", "calibration-registry.csv", package =
"stabilitest")` for installed use.

**Step 5: Run tests and verify GREEN**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry", stop_on_failure = TRUE)'
```

Expected: mapping and registry-contract tests pass.

**Step 6: Commit**

```bash
git add inst/extdata/calibration-registry.csv R/calibration_registry.R \
  tests/testthat/test-calibration-registry.R
git commit -m "feat: add method-specific calibration registry"
```

### Task 2: Resolve observed conclusions and Welch applicability

**Files:**
- Modify: `R/calibration_registry.R`
- Modify: `tests/testthat/test-calibration-registry.R`

**Step 1: Write failing applicability tests**

Test the exact policies without running expensive robustness analyses:

```r
test_that("only supported significant Welch results receive cutoffs", {
  supported <- resolve_result_calibration(
    calibration_unit = "welch_unpaired",
    endpoint = "mean_difference",
    conclusion_type = "significant",
    weights = c(jackknife = .4, fragility = .4, bootstrap = .2),
    max_removal_pct = .30
  )
  expect_true(supported$applicable)
  expect_identical(supported$status, "validated_method_specific")
  expect_equal(c(supported$cutoff_fragile, supported$cutoff_robust), c(55, 70))

  nonsig <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "non_significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_false(nonsig$applicable)
  expect_identical(nonsig$status, "bands_not_applicable")

  custom_weights <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "significant",
    c(jackknife = .5, fragility = .3, bootstrap = .2), .30
  )
  expect_false(custom_weights$applicable)
  expect_identical(custom_weights$status, "uncalibrated")
})

test_that("uncalibrated methods fail closed", {
  x <- resolve_result_calibration(
    "paired_t", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_false(x$applicable)
  expect_true(all(is.na(c(x$cutoff_fragile, x$cutoff_robust))))
})
```

Also test conclusion helpers for significant/non-significant model results and
equivalent/not-equivalent/non-inferior/not-non-inferior TOST results.

**Step 2: Run tests and verify RED**

Run the focused test command from Task 1. Expected: missing conclusion and
resolution helpers.

**Step 3: Implement fail-closed resolution**

Add:

```r
score_label_from_calibration <- function(score, calibration) {
  if (!isTRUE(calibration$applicable)) return(NA_character_)
  if (score > calibration$cutoff_robust) return("Robust")
  if (score > calibration$cutoff_fragile) return("Moderately Robust")
  "Fragile"
}

.uncalibrated_result <- function(unit, endpoint, conclusion, status, reason) {
  list(
    version = "taxonomy-2026-1", family = NA_character_,
    calibration_unit = unit, endpoint = endpoint,
    conclusion_type = conclusion, status = status, applicable = FALSE,
    cutoff_fragile = NA_real_, cutoff_robust = NA_real_,
    source = NA_character_, supported_conditions = reason
  )
}
```

`resolve_result_calibration()` performs an exact registry lookup. Before lookup,
non-significant superiority and unsuccessful TOST conclusions return
`bands_not_applicable`. A validated Welch row becomes applicable only with the
default weights and `max_removal_pct = 0.30`; other observable configurations
return `uncalibrated`. The original distributional scope remains explicit
metadata because it cannot be proven automatically from the result object.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry", stop_on_failure = TRUE)'
git add R/calibration_registry.R tests/testthat/test-calibration-registry.R
git commit -m "feat: resolve calibration applicability safely"
```

### Task 3: Attach calibration metadata to two-vector results

**Files:**
- Modify: `R/robustness_analysis.R:475-580`
- Modify: `R/robustness_shared.R:64-72,212-238`
- Modify: `tests/testthat/test-robustness_analysis.R`
- Modify: `tests/testthat/test-edge-cases.R`

**Step 1: Write failing result-object tests**

Add small deterministic tests using `n_boot = 10`:

```r
test_that("significant default Welch has narrow calibrated metadata", {
  set.seed(41)
  result <- robustness_analysis(rep(3, 12) + rnorm(12, 0, .1),
                                rep(0, 12) + rnorm(12, 0, .1),
                                test_type = "t.test", n_boot = 10, seed = 41)
  expect_identical(result$calibration$calibration_unit, "welch_unpaired")
  expect_true(result$calibration$applicable)
  expect_match(result$robustness_interpretation,
               "Robust|Moderately Robust|Fragile")
})

test_that("other two-vector methods preserve scores but suppress labels", {
  methods <- c("paired.t.test", "wilcoxon", "wilcoxon.paired",
               "brunner_munzel", "fisher", "chisq", "prop")
  # Use method-appropriate fixed fixtures already present in this test file.
  results <- lapply(methods, run_small_method_fixture)
  expect_true(all(vapply(results, function(x) is.numeric(
    x$robustness_metrics$overall_robustness), logical(1))))
  expect_true(all(vapply(results, function(x) is.na(
    x$robustness_interpretation), logical(1))))
  expect_false(any(vapply(results, function(x)
    x$calibration$calibration_unit == "two_sample", logical(1))))
})
```

Add negative Welch cases for non-significance, custom weights, and a custom
removal budget.

**Step 2: Run focused tests and verify RED**

```bash
Rscript -e 'devtools::test(filter = "robustness_analysis|edge-cases", stop_on_failure = TRUE)'
```

Expected: results have no calibration metadata and still assign shared labels.

**Step 3: Attach metadata after method resolution**

Remove direct use of `robustness_band_label()` from
`robustness_analysis()`. After `sample_info` is complete:

```r
unit <- calibration_unit_for_test(test_type)
conclusion <- if (original_significant) "significant" else "non_significant"
calibration <- resolve_result_calibration(
  unit, effect_type, conclusion, weights, max_removal_pct
)
robustness_interpretation <- score_label_from_calibration(
  robustness_score$overall_robustness, calibration
)
```

Add `calibration = calibration` to the result. Keep
`align_robustness_result_aliases()` responsible only for alias consistency, not
registry lookup.

Retire `robustness_band_label()` as a shared unconditional helper. If legacy
tests need a low-level score classifier, replace it with the calibration-aware
helper and never call it without metadata.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'devtools::test(filter = "robustness_analysis|edge-cases", stop_on_failure = TRUE)'
git add R/robustness_analysis.R R/robustness_shared.R \
  tests/testthat/test-robustness_analysis.R tests/testthat/test-edge-cases.R
git commit -m "feat: attach method calibration to two-vector results"
```

### Task 4: Attach uncalibrated metadata to model and TOST results

**Files:**
- Modify: `R/robustness_models.R:322-353,416-459,520-560,680-731`
- Modify: `R/robustness_tost.R:717-745`
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `tests/testthat/test-robustness_tost.R`

**Step 1: Write failing cross-engine tests**

For small existing LM, binomial GLM, Poisson GLM, Cox, and TOST fixtures,
require:

```r
expect_identical(lm_result$calibration$calibration_unit, "lm_ancova")
expect_identical(binomial_result$calibration$calibration_unit, "glm_binomial")
expect_identical(poisson_result$calibration$calibration_unit, "glm_poisson")
expect_identical(cox_result$calibration$calibration_unit, "cox_ph")
expect_identical(tost_result$calibration$calibration_unit, "tost_mean")

for (result in list(lm_result, binomial_result, poisson_result,
                    cox_result, tost_result)) {
  expect_identical(result$calibration$status, "uncalibrated")
  expect_false(result$calibration$applicable)
  expect_true(is.na(result$interpretation_label))
  expect_true(is.finite(result$metrics$overall_robustness))
}
```

Test successful and unsuccessful TOST conclusions; unsuccessful conclusions
must use `bands_not_applicable`.

**Step 2: Run and verify RED**

```bash
Rscript -e 'devtools::test(filter = "edge-cases|robustness_tost", stop_on_failure = TRUE)'
```

Expected: current shared engine assigns categorical labels.

**Step 3: Move interpretation out of the shared model engine**

In `robustness_engine()`, construct metrics but set neither a score-derived
label nor calibration metadata. Add a shared attachment helper:

```r
attach_result_calibration <- function(out, unit, endpoint, conclusion) {
  calibration <- resolve_result_calibration(
    unit, endpoint, conclusion, out$weights, out$max_removal_pct
  )
  out$calibration <- calibration
  out$interpretation_label <- score_label_from_calibration(
    out$metrics$overall_robustness, calibration
  )
  align_robustness_result_aliases(out, style = "model")
}
```

Call it only after each wrapper has set identifying metadata:

- `robustness_lm()`: `lm_ancova`, `coefficient`, significant/non-significant;
- `robustness_glm()`: `glm_binomial` or `glm_poisson`;
- `robustness_surv()`: `cox_ph`, `hazard_ratio`;
- `robustness_tost()`: endpoint-specific unit and
  equivalent/not-equivalent/noninferior/not-noninferior conclusion.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'devtools::test(filter = "edge-cases|robustness_tost", stop_on_failure = TRUE)'
git add R/robustness_models.R R/robustness_tost.R R/calibration_registry.R \
  tests/testthat/test-edge-cases.R tests/testthat/test-robustness_tost.R
git commit -m "feat: suppress uncalibrated model and TOST labels"
```

### Task 5: Make print and narrative output calibration-aware

**Files:**
- Modify: `R/robustness_analysis.R:587-657,668-730`
- Modify: `R/robustness_models.R:744-800`
- Modify: `R/robustness_tost.R:752-800`
- Modify: `tests/testthat/test-robustness_analysis.R`
- Modify: `tests/testthat/test-edge-cases.R`
- Modify: `tests/testthat/test-robustness_tost.R`

**Step 1: Write failing output tests**

Capture output for calibrated, uncalibrated, and non-applicable results:

```r
text <- capture.output(print(uncalibrated_result))
expect_match(text, "OVERALL ROBUSTNESS: [0-9.]+/100")
expect_match(text, "categorical bands not calibrated for this method")
expect_false(any(grepl("\\((Robust|Moderately Robust|Fragile)\\)", text)))

welch_text <- capture.output(print(calibrated_welch_result))
expect_match(welch_text, "Welch calibration")
expect_match(welch_text, "Robust|Moderately Robust|Fragile")
```

For `interpret = TRUE`, assert that uncalibrated narratives include the numeric
score and components but do not generate recommendations by comparing the
score with 55/70.

**Step 2: Run and verify RED**

Run the three focused test files. Expected: format strings print `NA` labels or
old unconditional recommendations.

**Step 3: Implement shared display helpers**

Add:

```r
format_score_interpretation <- function(score, calibration, label) {
  if (isTRUE(calibration$applicable)) {
    sprintf("%.1f/100 (%s; %s)", score, label, calibration$version)
  } else {
    sprintf("%.1f/100 (categorical bands not calibrated for this method)", score)
  }
}
```

Use it in all print methods. In `generate_interpretation()`, choose the
score-band recommendation only for applicable calibration. For uncalibrated
results, use a neutral recommendation that directs users to the component
metrics without calling the result robust, moderate, or fragile.

Legacy objects without `$calibration` print `legacy result: calibration status
unknown` and do not receive a reconstructed Welch label.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'devtools::test(filter = "robustness_analysis|edge-cases|robustness_tost", stop_on_failure = TRUE)'
git add R tests/testthat
git commit -m "feat: make robustness output calibration-aware"
```

### Task 6: Separate simulation routing from calibration identity

**Files:**
- Modify: `manuscript/calibration/config/scenarios.R`
- Modify: `manuscript/calibration/R/schema.R`
- Modify: `manuscript/calibration/R/cli.R`
- Modify: `manuscript/calibration/run_calibration.R`
- Modify: `manuscript/calibration/R/executor.R`
- Modify: `manuscript/calibration/tests/testthat/test-scenario-schema.R`
- Modify: `manuscript/calibration/tests/testthat/test-result-schema.R`
- Modify: `manuscript/calibration/tests/testthat/test-cli.R`
- Modify: `manuscript/calibration/tests/testthat/test-executor.R`

**Step 1: Write failing schema tests**

Require scenario rows to contain:

```r
c("analysis_engine", "calibration_family", "calibration_unit")
```

Assert:

```r
expect_false("two_sample" %in% scenarios$calibration_unit)
expect_false("two_sample" %in% scenarios$calibration_family)
expect_true("two_sample" %in% scenarios$analysis_engine)
expect_identical(
  unique(scenarios$calibration_unit[scenarios$parameters |> purrr::map_chr(
    ~ .x$analysis$test_type %||% "") == "t.test"]),
  "welch_unpaired"
)
```

Replicate-schema tests require the three fields and verify that execution
routing can still use `analysis_engine = "two_sample"` without treating it as
a calibration key.

**Step 2: Run and verify RED**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-scenario-schema.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-result-schema.R")'
```

Expected: new columns are absent.

**Step 3: Migrate the scenario and replicate schemas**

Rename routing use of `analysis_family` to `analysis_engine`. Add method-level
calibration metadata to every scenario. Map the existing scenarios as follows:

```text
t.test             -> continuous_parametric / welch_unpaired
paired.t.test      -> continuous_parametric / paired_t
wilcoxon           -> rank_nonparametric / wilcoxon_rank_sum
wilcoxon.paired    -> rank_nonparametric / wilcoxon_signed_rank
brunner_munzel     -> rank_nonparametric / brunner_munzel
fisher             -> binary_proportion / fisher_exact
chisq              -> binary_proportion / chi_square_2x2
prop               -> binary_proportion / two_sample_prop
lm                 -> linear_model / lm_ancova
binomial GLM       -> generalized_linear_model / glm_binomial
Poisson GLM        -> generalized_linear_model / glm_poisson
Cox                -> survival / cox_ph
TOST endpoint      -> equivalence_noninferiority / endpoint-specific TOST key
```

Update CLI `--engine` filtering and adapter dispatch to use
`analysis_engine`. Do not rename the CLI option in this release.

**Step 4: Verify schema, CLI, and executor tests**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-scenario-schema.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-result-schema.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-cli.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-executor.R")'
```

Expected: all pass and smoke routing still reaches every adapter.

**Step 5: Commit**

```bash
git add manuscript/calibration/config manuscript/calibration/R \
  manuscript/calibration/run_calibration.R manuscript/calibration/tests/testthat
git commit -m "refactor: separate calibration units from execution engines"
```

### Task 7: Fit future thresholds by calibration unit and applicable conclusion

**Files:**
- Modify: `manuscript/calibration/R/thresholds.R:117-330`
- Modify: `manuscript/calibration/R/summarise.R:33-135`
- Modify: `manuscript/calibration/tests/testthat/test-thresholds.R`
- Modify: `manuscript/calibration/tests/testthat/test-summaries.R`

**Step 1: Write failing estimand tests**

Create a fixture where stable non-significant null rows have high scores but
null false-positive rows have low scores. Verify only the latter enter false
reassurance:

```r
fixture <- data.frame(
  calibration_unit = "lm_ancova",
  design_layer = "core",
  truth_class = c(rep("null", 200), rep("clear", 100)),
  screening_conclusion = c(rep("non_significant", 100),
                           rep("significant", 100), rep("significant", 100)),
  target_conclusion = c(rep("non_significant", 200), rep("significant", 100)),
  overall_score = c(rep(95, 100), rep(40, 100), rep(85, 100)),
  status = "completed"
)
metrics <- threshold_metrics_for_applicable(fixture, c(55, 70))
expect_equal(metrics$false_reassurance, 0)
expect_equal(metrics$robust_identification, 1)
expect_equal(metrics$false_reassurance_n, 100)
```

Add an equivalent TOST fixture and assert that not-equivalent rows are excluded
from equivalence-band fitting. Assert candidate fitting groups by
`calibration_unit`, never `analysis_engine` or broad family.

**Step 2: Run and verify RED**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-thresholds.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-summaries.R")'
```

Expected: current metrics count all null/clear rows and group by
`analysis_family`.

**Step 3: Implement the corrected estimand**

Add one shared predicate:

```r
band_applicable_conclusion <- function(data) {
  conclusion <- as.character(data$screening_conclusion)
  conventional <- conclusion == "significant"
  tost_success <- conclusion %in% c("equivalent", "noninferior")
  conventional | tost_success
}
```

For a superiority calibration unit:

- false-reassurance universe: `truth_class == "null"` and observed conclusion
  is significant;
- robust-identification universe: non-null clear truth and observed conclusion
  is the correct significant direction;
- ordinal accuracy and median ordering: only applicable conclusions.

For TOST, use successful equivalence or non-inferiority conclusions and the
corresponding truth definition. Return denominators explicitly. Group fitting,
validation, registry freezing, and summaries by `calibration_unit`.

This is a correction to the pre-specified estimand, not permission to validate
against the already inspected Task 15 held-out data. Mark any reanalysis of
those rows as exploratory.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-thresholds.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-summaries.R")'
git add manuscript/calibration/R/thresholds.R \
  manuscript/calibration/R/summarise.R \
  manuscript/calibration/tests/testthat/test-thresholds.R \
  manuscript/calibration/tests/testthat/test-summaries.R
git commit -m "fix: calibrate only applicable method conclusions"
```

### Task 8: Preserve Task 15 as inactive historical evidence

**Files:**
- Create: `manuscript/calibration/published/README.md`
- Modify: `manuscript/calibration/README.md`
- Modify: `manuscript/calibration/tests/testthat/test-publication-artifacts.R`

**Step 1: Write failing archival-policy tests**

Require:

```r
archive_note <- readLines(file.path("..", "..", "published", "README.md"))
expect_true(any(grepl("historical", archive_note, ignore.case = TRUE)))
expect_true(any(grepl("not active", archive_note, ignore.case = TRUE)))
expect_true(file.exists(file.path("..", "..", "published",
                                  "calibration-registry.csv")))

active <- load_calibration_registry()
historical <- read.csv(file.path("..", "..", "published",
                                 "calibration-registry.csv"))
expect_false(any(active$calibration_unit == "two_sample"))
expect_true(any(historical$analysis_family == "two_sample"))
```

The last assertion deliberately preserves the old evidence while proving it is
not the active package registry.

**Step 2: Run and verify RED**

Expected: the publication directory has no explicit archival boundary.

**Step 3: Document the boundary**

State that the broad-family registry, manifests, summaries, and hashes are the
immutable Task 15 experiment. Identify `inst/extdata/calibration-registry.csv`
as the only active runtime registry. State that inspected Task 15 validation
rows cannot be reused as fresh confirmation.

Do not rename, delete, or regenerate historical artifacts in this task.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-publication-artifacts.R")'
git add manuscript/calibration/README.md \
  manuscript/calibration/published/README.md \
  manuscript/calibration/tests/testthat/test-publication-artifacts.R
git commit -m "docs: archive broad-family calibration evidence"
```

### Task 9: Repair publication provenance for future method runs

**Files:**
- Modify: `manuscript/calibration/tools/assemble_replicates.R`
- Modify: `manuscript/calibration/tools/freeze_and_publish.R`
- Create: `manuscript/calibration/tests/testthat/test-publication-tools.R`
- Modify: `manuscript/calibration/tests/testthat/test-publication-artifacts.R`

**Step 1: Refactor source-safe helpers and write failing tests**

Expose helper functions without executing the CLI when sourced. Test that:

- completed rows go to the fitting table;
- all failed/excluded rows go to an audit table;
- attempted = completed + failed + excluded;
- scenarios missing checkpoints are derived as unsupported;
- an assembly subprocess failure stops publication;
- production hashes include the registry RDS and exclude pilot CSVs.

Use temporary checkpoint fixtures; do not touch production raw artifacts.

**Step 2: Run and verify RED**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-publication-tools.R")'
```

Expected: current assembly drops failures and the freezer ignores `system2()`
status.

**Step 3: Preserve audit rows and enforce command status**

Change assembly to write:

```text
training-replicates.rds
validation-replicates.rds
training-audit.rds
validation-audit.rds
```

The fitting RDS files contain completed rows only. Audit files contain every
attempted row and scenario/checkpoint status. Build manifest counts and
unsupported reasons from the audit objects and frozen scenario registry.

Capture and check assembly status:

```r
status <- system2("Rscript", args, stdout = "", stderr = "")
if (!identical(status, 0L)) {
  stop("calibration replicate assembly failed", call. = FALSE)
}
```

Hash production manifests, registry CSV, registry RDS, non-significant registry,
and compact failure summary. Do not include pilot summaries in the production
ledger.

**Step 4: Verify GREEN and commit**

```bash
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-publication-tools.R")'
Rscript -e 'testthat::test_file("manuscript/calibration/tests/testthat/test-publication-artifacts.R")'
git add manuscript/calibration/tools manuscript/calibration/tests/testthat
git commit -m "fix: preserve calibration publication provenance"
```

### Task 10: Update package and manuscript documentation

**Files:**
- Modify: `README.md`
- Modify: `NEWS.md`
- Modify: `R/robustness_analysis.R`
- Modify: `R/robustness_models.R`
- Modify: `R/robustness_tost.R`
- Modify: `vignettes/pain-case-study.Rmd`
- Modify: `manuscript/robustness_analysis_manuscript.md`
- Modify: `manuscript/CALIBRATION_SAP.md` if present, otherwise `manuscript/calibration/CALIBRATION_SAP.md`
- Regenerate: `man/*.Rd`

**Step 1: Add a stale-claim check**

Create or extend a documentation test/script that fails on claims that 55/70
apply to all engines, model families, TOST, non-significant conclusions, or the
generic `two_sample` calibration family.

**Step 2: Run and verify RED**

```bash
rg -n "shared by two-sample|assumed transferable|calibrated bands|Robust.*55|two_sample" \
  README.md NEWS.md R man vignettes manuscript
```

Expected: stale broad-calibration claims and historical references requiring
classification are found.

**Step 3: Update source documentation**

Document:

- numeric score and components remain available everywhere;
- categorical bands are suppressed when uncalibrated;
- the only active validated row is narrow `welch_unpaired`;
- the public dispatcher still supports all existing tests;
- Task 15 broad-family results are historical and inactive;
- `lm_ancova` is the next independent calibration target.

Historical text may retain `two_sample` when explicitly labeled as Task 15
history. Active API/calibration descriptions may not use it as a registry key.

**Step 4: Regenerate help and verify**

```bash
Rscript -e 'roxygen2::roxygenise()'
Rscript -e 'devtools::build_vignettes()'
Rscript -e 'devtools::test(filter = "documentation|calibration-registry", stop_on_failure = TRUE)'
git diff --check
```

Expected: documentation builds, focused tests pass, and no unexplained broad
calibration claims remain.

**Step 5: Commit**

```bash
git add README.md NEWS.md R man vignettes manuscript
git commit -m "docs: publish method-specific calibration policy"
```

### Task 11: Run complete verification and review

**Files:**
- Verify all changed files

**Step 1: Run focused package tests**

```bash
Rscript -e 'devtools::test(filter = "calibration-registry|robustness_analysis|edge-cases|robustness_tost", stop_on_failure = TRUE)'
```

Expected: all focused tests pass with no warnings or skips.

**Step 2: Run the calibration infrastructure suite**

```bash
Rscript -e 'testthat::test_dir("manuscript/calibration/tests/testthat", reporter = "summary", stop_on_failure = TRUE)'
```

Expected: all calibration tests pass. Do not run a publication-scale
simulation.

**Step 3: Run a smoke workflow**

```bash
Rscript manuscript/calibration/run_calibration.R \
  --mode smoke --phase all --engine all --workers 2 \
  --output /tmp/stabilitest-method-calibration-smoke
```

Expected: all execution engines complete and every replicate records an exact
calibration unit.

**Step 4: Run full package verification**

```bash
Rscript -e 'devtools::test(stop_on_failure = TRUE)'
Rscript manuscript/test_simulation_entrypoint.R
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), error_on = "warning")'
```

Expected: tests pass; R CMD check has no error or warning and only the accepted
new-submission note.

**Step 5: Request independent review**

Use `@superpowers:requesting-code-review`. Require review of:

- exact method mapping and absence of active `two_sample` calibration;
- fail-closed registry behavior;
- narrow Welch applicability;
- label suppression without loss of scores/components;
- corrected applicable-conclusion estimand;
- routing versus calibration identity;
- Task 15 archival boundary;
- failure accounting and artifact hashes.

**Step 6: Final verification and branch completion**

Use `@superpowers:verification-before-completion`, then:

```bash
git diff --check
git status --short
git log --oneline codex/calibration-task1..HEAD
```

Expected: clean worktree and focused commits. Use
`@superpowers:finishing-a-development-branch` to choose PR, merge, retention,
or cleanup.

## Follow-Up: First Single-Family Calibration

Do not include this in the taxonomy-release implementation. After that release
is integrated, create a new design and plan specifically for `lm_ancova`.
Freeze new training and held-out scenarios, use only applicable significant
conclusions, and do not reuse the inspected Task 15 validation block as final
confirmation.
