# Long-form Manuscript and Physician Guide Alignment Design

**Date:** 2026-08-11

**Status:** Approved

## Purpose

Keep the long-form robustness manuscript and the physician's guide as active
companion documents while bringing both into agreement with the completed
prospective Welch recalibration. The documents must no longer present the
historical 55/70 thresholds as validated, active reference bands.

The technical manuscript remains the detailed methodological companion to the
R Journal article. The physician's guide remains a plain-language explanation
for clinicians. Neither document is replaced or marked obsolete.

## Governing scientific result

The authoritative sources are the published Welch verdict artifact and the
active calibration registry row for `welch_unpaired`:

- study version `welch-2026-2`;
- outcome `no_feasible_thresholds`;
- frozen candidate hash
  `9c45481b952cab7cb9b9086e37924a39d83fe0484628745dbe2e79eb33e8797d`;
- held-out validation was not opened because training produced no feasible
  categorical candidate;
- Welch numeric scores and component metrics remain available;
- Welch categorical labels are suppressed.

The historical simulation remains useful descriptive evidence about observed
score and removal-index distributions. It does not license 55/70 labels or a
claim that an individual result is Robust, Moderately Robust, or Fragile.
Fisher's exact test remains the only active categorical calibration, within its
documented eligibility profile and jackknife-light weighting.

## Long-form manuscript revision

Update `manuscript/robustness_analysis_manuscript.md` throughout rather than
adding an isolated erratum. Preserve the manuscript's long-form structure,
technical detail, simulation tables, worked example, references, and
appendices.

The revision will:

- rewrite the abstract and conclusions so Welch is numeric-only;
- replace the active Welch 55/70 policy with the prospective fail-closed
  outcome and explain why historical exploratory separation was insufficient;
- retain historical simulation results as descriptive evidence, clearly
  separated from confirmatory calibration;
- report the study version, frozen candidate hash, training outcome, and the
  fact that held-out validation remained unopened;
- change the chronic-pain example from `72.5/100 — Robust` to a numeric score
  with component-level interpretation and clinical follow-up;
- remove suggested reporting text, recommendations, limitations, and SAP
  language that authorize Welch categorical labels;
- retain Fisher's validated two-band mapping with its exact profile and
  weights;
- update package/version companion wording only where required for internal
  consistency.

Regenerate `manuscript/robustness_analysis_manuscript.pdf` with the repository's
Pandoc/Typst build script. Inspect every rendered page for overflow, clipped
tables, broken headings, and malformed symbols.

## Physician's guide revision

Edit `manuscript/stabilitest-physicians-guide.docx` in place while preserving
its visual identity, section structure, tables, and clinician-facing tone.
Use minimal paragraph-level replacements rather than reconstructing the file.

The revision will:

- explain that Welch recalibration stopped at training because no thresholds
  met the frozen safety and usefulness gates;
- state that the held-out data were therefore not opened;
- explain that a Welch score such as 72.5 is not a validated diagnosis or
  verdict;
- retain the clinical value of the leave-one-out, removal-fragility, bootstrap,
  and named-patient outputs;
- revise the chronic-pain example to recommend component-level review without
  calling it Robust;
- preserve the independently validated Fisher mapping and clearly limit it to
  its calibrated profile and weights;
- update the checklist and glossary so clinicians verify method-specific
  calibration and recognize numeric-only output as deliberate fail-closed
  behavior.

Render the edited DOCX to PDF and page images using the document workflow.
Inspect every page and compare it with the baseline to confirm that only
intended text and necessary pagination changes occurred.

## Documentation guardrails

Strengthen `tools/check-calibration-documentation.R` with file-specific checks.
The existing aggregate corpus audit can pass when one document contains the
correct Welch policy even if another still contains an active 55/70 claim.

The revised audit will independently require the long-form manuscript to name
the Welch fail-closed outcome, label suppression, and unopened held-out data.
It will also inspect text extracted from the physician's guide, or a committed
plain-text policy companion derived from it if direct DOCX inspection would
make the audit environment-dependent. The guardrail will reject active Welch
55/70 verdict language and a `72.5 — Robust` interpretation in either active
document while allowing clearly marked historical discussion.

Tests are written before document edits so the current stale documents produce
the expected failure. After the document changes, run the targeted audit, the
full `testthat` suite, `R CMD check --as-cran`, both document builds, and visual
QA.

## Reader-facing documentation addendum

Following implementation review, extend the alignment beyond the two companion
documents to the package's current reader-facing documentation:

- `README.md`;
- `NEWS.md`;
- package vignettes;
- public roxygen help and its generated `man/` pages;
- `manuscript/methodological_review.md`, which must be clearly identified as a
  historical review and corrected where it describes the current policy.

Remove internal workflow vocabulary such as “Gate A,” “Gate B,” “Task 15,”
“Track A,” and “Track E” from explanatory prose. Replace it with the scientific
event it represented: protocol freeze, training selection, held-out
confirmation, historical simulation, jackknife-light ANCOVA attempt, or
violation-detection study. Raw hashes and machine reason codes should appear in
reader-facing prose only when necessary for reproducibility; the README and
vignettes should link to the detailed artifacts instead.

Replace the opaque statement that labels are `NA` for uncalibrated methods with
plain language: for analysis types without validated cutoffs, stabilitest still
reports the numeric score and stress-test details but leaves the categorical
label blank. Explain once that this deliberate blank output is the package's
fail-closed behavior.

Exact gate, track, task, hash, and reason identifiers remain in calibration
protocols, published audit artifacts, implementation comments where needed,
and planning documents. Those materials serve reproducibility rather than
general user education.

## Scope and release

This change edits documentation and documentation tests only. It does not
change calibration artifacts, runtime registry behavior, package APIs, numeric
algorithms, or the R Journal submission package. It does not submit or publish
anything externally.
