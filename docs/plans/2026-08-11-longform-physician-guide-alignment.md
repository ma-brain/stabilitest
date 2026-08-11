# Long-form Manuscript and Physician Guide Alignment Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Keep the long-form manuscript and physician's guide active while
removing unsupported Welch 55/70 verdicts and aligning both documents with the
prospective `welch-2026-2` fail-closed result.

**Architecture:** Treat the active calibration registry and committed Welch
verdict artifact as the scientific source of truth. Add file-specific policy
checks, revise the Markdown and DOCX sources in place, regenerate the long-form
PDF, and render both deliverables for page-by-page QA. Preserve all useful
numeric and component-level interpretation; suppress only unearned Welch
categorical labels.

**Tech Stack:** R >= 4.2, base-R ZIP/XML reading, `testthat`, Markdown, Pandoc,
Typst, Python 3 with `python-docx`, LibreOffice-based DOCX rendering, Poppler,
Git.

---

## Governing decisions

- Read the approved design first:
  `docs/plans/2026-08-11-longform-physician-guide-alignment-design.md`.
- Keep both `manuscript/robustness_analysis_manuscript.md` and
  `manuscript/stabilitest-physicians-guide.docx` as active documents.
- Historical Welch simulations may be described only as exploratory or
  descriptive. They do not license 55/70 labels.
- The prospective Welch result is `no_feasible_thresholds`, version
  `welch-2026-2`, frozen candidate hash
  `9c45481b952cab7cb9b9086e37924a39d83fe0484628745dbe2e79eb33e8797d`.
- Because training found no feasible candidate, held-out validation was not
  opened. Do not imply that held-out confirmation failed.
- A Welch score of 72.5 remains a valid numeric summary but is not a validated
  `Robust` verdict.
- Fisher's exact test remains the only active categorical calibration, within
  its documented profile and jackknife-light weights.
- Do not change runtime code, registry entries, calibration artifacts, or the R
  Journal submission package.
- Do not submit or publish anything externally.

## Task 1: Record the isolated baseline and render the physician guide

**Files:**

- Read: `AGENTS.md`
- Read: `inst/extdata/calibration-registry.csv`
- Read: `manuscript/calibration/studies/welch_unpaired/artifacts/published/verdict.json`
- Read: `manuscript/robustness_analysis_manuscript.md`
- Read: `manuscript/stabilitest-physicians-guide.docx`
- Read: `manuscript/build_pdf.sh`

**Step 1: Confirm the authoritative Welch facts**

Run:

```sh
Rscript -e 'x <- read.csv("inst/extdata/calibration-registry.csv", check.names = FALSE); print(x[x$method == "welch_unpaired", ])'
sed -n '1,220p' manuscript/calibration/studies/welch_unpaired/artifacts/published/verdict.json
```

Expected: the registry reports `uncalibrated`, `welch-2026-2`, empty cutoffs,
and `no_feasible_thresholds`; the verdict reports the same outcome, the frozen
hash, and that held-out data were not opened.

**Step 2: Confirm the clean package baseline**

Run:

```sh
git status --short
Rscript tools/check-calibration-documentation.R
Rscript -e 'testthat::test_local(reporter = "summary")'
```

Expected: empty Git status, the documentation audit passes, and all tests pass.

**Step 3: Render the unmodified physician guide for comparison**

Run:

```sh
rm -rf /tmp/stabilitest-physician-guide-before
env TMPDIR=/private/tmp /Users/Marius/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 \
  /Users/Marius/.codex/plugins/cache/openai-primary-runtime/documents/26.805.11740/skills/documents/render_docx.py \
  manuscript/stabilitest-physicians-guide.docx \
  --output_dir /tmp/stabilitest-physician-guide-before --emit_pdf
pdfinfo /tmp/stabilitest-physician-guide-before/stabilitest-physicians-guide.pdf
```

Expected: rendering succeeds and produces page PNGs plus a PDF. Inspect every
baseline page so later layout changes can be distinguished from pre-existing
layout.

**Step 4: Record the current stale claims without changing files**

Run:

```sh
rg -n -i 'Welch|55|70|72\.5|Robust|held-out|uncalibrated' \
  manuscript/robustness_analysis_manuscript.md
```

Extract the DOCX paragraphs with `python-docx` and confirm the stale active
claims in Section 4 and the worked example. Do not commit generated baseline
renders.

## Task 2: Add file-specific fail-closed documentation checks

**Files:**

- Modify: `tools/check-calibration-documentation.R`

**Step 1: Add a DOCX text reader and document-scoped assertions**

After the existing aggregate Welch assertions, add helpers equivalent to:

```r
read_docx_text <- function(path) {
  con <- unz(path, "word/document.xml", open = "r")
  on.exit(close(con), add = TRUE)
  xml <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"),
               collapse = "")
  xml <- gsub("</w:p>", "\n", xml, fixed = TRUE)
  xml <- gsub("<[^>]+>", "", xml, perl = TRUE)
  xml <- gsub("&amp;", "&", xml, fixed = TRUE)
  xml <- gsub("&lt;", "<", xml, fixed = TRUE)
  xml <- gsub("&gt;", ">", xml, fixed = TRUE)
  xml
}

assert_in_document <- function(document_text, pattern, description,
                               ignore.case = TRUE) {
  if (!grepl(pattern, document_text, ignore.case = ignore.case, perl = TRUE)) {
    violations <<- c(violations, description)
  }
}
```

Read the long-form Markdown separately and read the physician DOCX through
`read_docx_text()`. For each document independently require:

- `welch-2026-2`;
- `no_feasible_thresholds`;
- wording that the held-out data or validation were not opened;
- wording that Welch categorical labels or verdicts are suppressed;
- wording that numeric scores and component metrics remain available.

For each document independently reject:

```r
"72\\.?5.{0,40}(—|-|is|classified).{0,20}Robust"
"Welch.{0,120}(>\\s*70|above 70).{0,40}Robust"
"Welch.{0,120}55.{0,30}70.{0,60}(validated|active|reference range)"
```

Historical 55/70 discussion remains permitted when it is explicitly described
as historical, exploratory, retired, or non-licensing.

**Step 2: Run the audit and verify the new checks fail**

Run:

```sh
Rscript tools/check-calibration-documentation.R
```

Expected: FAIL with document-specific messages for both the long-form
manuscript and physician guide. The failure proves the old aggregate audit gap
is now covered.

Do not commit the red state.

## Task 3: Correct the long-form manuscript while preserving its scope

**Files:**

- Modify: `manuscript/robustness_analysis_manuscript.md`
- Reference: `manuscript/rjournal/stabilitest.Rmd`
- Reference: `manuscript/calibration/studies/welch_unpaired/artifacts/published/verdict.json`
- Reference: `inst/extdata/calibration-registry.csv`

**Step 1: Revise the abstract and framing**

Update the abstract's Methods, Results, and Conclusions so they state:

- the historical simulation is descriptive evidence only;
- prospective Welch training found `no_feasible_thresholds` under the frozen
  gates;
- the candidate hash was frozen before any validation opening;
- held-out validation was not opened;
- Welch is numeric-only and Fisher is the only active categorical mapping.

Remove language saying the framework validated 55/70 for Welch or that Welch
has an applicable active three-band configuration.

**Step 2: Replace the calibration-policy section**

Rewrite Section 2.3 to distinguish:

1. **Welch historical comparator:** the 55/70 bands are retired exploratory
   anchors and never emitted by the current runtime.
2. **Welch prospective study:** version `welch-2026-2`, frozen candidate hash,
   `no_feasible_thresholds`, held-out not opened, numeric-only output.
3. **Fisher active calibration:** Fragile iff score <= 58 and Not fragile iff
   score > 58, only under the documented profile and weights `(0, 0.5, 0.5)`.
4. **Other methods:** numeric outputs with categorical labels suppressed.

**Step 3: Recast the historical simulation**

Keep the study design, table, and empirical findings, but call its score
separation exploratory. Explicitly state that the overlap among
chance-significant and true-effect scores was too large to satisfy the later
prospective false-reassurance and identification gates. Remove “the calibrated
bands follow directly” and any diagnostic claims that 1-2 or 10-14 removals
prove null or genuine-effect status.

**Step 4: Correct the chronic-pain case study**

Retain score 72.5, jackknife results, removal set, bootstrap same-decision rate,
and clinical follow-up. Replace the verdict with wording equivalent to:

> Overall numeric score: 72.5/100; no categorical Welch verdict is emitted.

Explain that the components support data review and sensitivity reporting but
do not validate a `Robust` diagnosis. Rewrite the suggested CSR text so it
reports numeric components and explicitly says Welch categorical labels were
suppressed under `welch-2026-2`.

**Step 5: Align downstream recommendations**

Revise the Discussion, Recommendations, Regulatory Alignment, Limitations,
Future Directions, Conclusions, reproducibility note, and Appendix B SAP
template. Remove all instructions to pre-specify or report Welch 55/70 bands.
Retain Fisher's licensed two-band use and descriptive reporting for all other
methods.

**Step 6: Scan for residual active claims**

Run:

```sh
rg -n -i 'Welch|55/70|72\.5|Robust|Moderately Robust|Fragile|held-out|no_feasible_thresholds' \
  manuscript/robustness_analysis_manuscript.md
```

Expected: every remaining 55/70 reference is explicitly historical,
exploratory, retired, or non-licensing; 72.5 is numeric-only; the prospective
verdict and unopened held-out status are present.

**Step 7: Run the documentation audit**

Run:

```sh
Rscript tools/check-calibration-documentation.R
```

Expected: it still fails only on physician-guide checks. No long-form-specific
violation remains.

Do not commit until Task 4 restores the complete audit to green.

## Task 4: Correct and render the physician's guide

**Files:**

- Create: `tools/update-physicians-guide.py`
- Modify: `manuscript/stabilitest-physicians-guide.docx`

**Step 1: Write a deterministic DOCX updater**

Use `python-docx` to load the existing guide and make exact-match,
paragraph-level replacements. The script must stop if an expected old
paragraph or table cell is absent and must preserve paragraph styles, list
styles, tables, headers, footers, section geometry, and images.

Replace these substantive claims:

- Section 2.2: describe the 1-2 versus 10-14 removal pattern as historical
  simulation evidence, not validation or a diagnostic signature.
- Section 4: replace the active Welch 55/70 reference ranges with the
  prospective `welch-2026-2` fail-closed result, including the frozen hash,
  `no_feasible_thresholds`, and unopened held-out data; retain Fisher as the
  only active categorical mapping.
- Section 5: report 72.5 as a numeric score without a Robust label; retain the
  55 leave-one-out results, six-patient removal set, 92% bootstrap result, and
  chart-review recommendations.
- Section 6: rename the heading to cover both Welch and adjusted-analysis
  negative results; use the first two paragraphs for the Welch training
  failure and the next two for the existing ANCOVA negative-result summary.
- Section 7: make the less-than-5% rule a review prompt rather than a validated
  cutoff, and require readers to verify that any label has a current
  method-specific calibration.
- Glossary: remove “one or two = the signature of a chance finding”; update
  Calibration and Fail-closed definitions; keep the Fisher entry narrowly
  scoped.

The guide must contain these exact audit-friendly statements:

> The prospective Welch study, version welch-2026-2, ended at training with
> no_feasible_thresholds.

> Because no candidate met the frozen gates, the held-out validation data were
> not opened.

> Welch categorical labels are suppressed; numeric scores and component
> metrics remain available.

**Step 2: Run the updater**

Run:

```sh
/Users/Marius/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 \
  tools/update-physicians-guide.py \
  manuscript/stabilitest-physicians-guide.docx
```

Expected: the script reports every replaced paragraph/table cell and saves the
DOCX once. A second run must stop cleanly as “already updated” or make no byte
changes.

**Step 3: Extract and review the resulting text**

Use `python-docx` to print every non-empty paragraph and table row. Confirm all
required scientific statements and all retained case-study numbers. Search the
extracted text for stale active Welch 55/70 or `72.5 - Robust` language.

**Step 4: Run the documentation audit to green**

Run:

```sh
Rscript tools/check-calibration-documentation.R
```

Expected: PASS, including independent checks for the long form and guide.

**Step 5: Render and compare the guide**

Run:

```sh
rm -rf /tmp/stabilitest-physician-guide-after
env TMPDIR=/private/tmp /Users/Marius/.cache/codex-runtimes/codex-primary-runtime/dependencies/python/bin/python3 \
  /Users/Marius/.codex/plugins/cache/openai-primary-runtime/documents/26.805.11740/skills/documents/render_docx.py \
  manuscript/stabilitest-physicians-guide.docx \
  --output_dir /tmp/stabilitest-physician-guide-after --emit_pdf
pdfinfo /tmp/stabilitest-physician-guide-after/stabilitest-physicians-guide.pdf
```

Inspect every output page. Compare before/after page counts and images. Accept
pagination changes caused by corrected text, but reject clipping, overflow,
orphaned headings, broken bullets, damaged tables, or unexpected style changes.
Iterate the DOCX updater and render until clean.

## Task 5: Regenerate the long-form PDF and verify both deliverables

**Files:**

- Modify: `manuscript/robustness_analysis_manuscript.pdf`
- Verify: `manuscript/robustness_analysis_manuscript.md`
- Verify: `manuscript/stabilitest-physicians-guide.docx`
- Verify: `tools/check-calibration-documentation.R`
- Verify: `tools/update-physicians-guide.py`

**Step 1: Build the long-form PDF**

Run:

```sh
manuscript/build_pdf.sh
pdfinfo manuscript/robustness_analysis_manuscript.pdf
rm -rf /tmp/stabilitest-longform-pages
mkdir -p /tmp/stabilitest-longform-pages
pdftoppm -png -r 120 manuscript/robustness_analysis_manuscript.pdf \
  /tmp/stabilitest-longform-pages/page
```

Expected: Pandoc/Typst succeeds and all pages render to PNG. Inspect every page
for clipped tables, overfull text, bad line breaks, malformed math/symbols,
blank pages, and heading widows. Revise and rebuild until clean.

**Step 2: Verify source/output consistency**

Run `pdftotext` on the long-form PDF and the rendered guide PDF. Confirm the
required Welch statements are present and the stale active claims are absent
from final rendered outputs, not only their sources.

**Step 3: Run targeted and full verification**

Run:

```sh
Rscript tools/check-calibration-documentation.R
Rscript -e 'testthat::test_local(reporter = "summary")'
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
git diff --check
git status --short --untracked-files=all
```

Expected:

- documentation audit passes;
- all `testthat` tests pass;
- `R CMD check` has no errors or warnings and only the accepted new-submission
  NOTE, if present;
- no whitespace errors;
- only the planned files are changed.

**Step 4: Commit the synchronized update**

Because `manuscript/` is ignored, force-add only the four intentional
manuscript artifacts and add the tracked tools normally:

```sh
git add tools/check-calibration-documentation.R tools/update-physicians-guide.py
git add -f manuscript/robustness_analysis_manuscript.md \
  manuscript/robustness_analysis_manuscript.pdf \
  manuscript/stabilitest-physicians-guide.docx
git diff --cached --check
git commit -m "docs: align long-form guides with Welch recalibration"
```

**Step 5: Verify the committed tree**

Run:

```sh
git status --short
git show --stat --oneline HEAD
Rscript tools/check-calibration-documentation.R
```

Expected: clean status, exactly the planned files in the commit, and a passing
documentation audit.

## Task 6: Complete the branch

Use `superpowers:verification-before-completion` before making any completion
claim. Then use `superpowers:requesting-code-review` and
`superpowers:finishing-a-development-branch` as required by the execution
workflow. Present the verified integration options to the user; do not merge,
push, publish, or delete the worktree without explicit direction.
