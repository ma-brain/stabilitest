# Gap analysis: `robustness_analysis_manuscript.md` (v2.0, package v0.5.1) vs R Journal requirements

**Date:** 2026-08-06
**Target venue:** The R Journal (https://journal.r-project.org/)
**Verdict in one line:** the statistical content is publishable, but the manuscript is
currently written for a clinical-biostatistics journal; an R Journal submission
needs a package-centric rewrite in rjtools format, a CRAN release, live computed
outputs, a related-packages section, and figures.

---

## A. Hard requirements not currently met

| # | Requirement | Current state | Gap |
| --- | --- | --- | --- |
| A1 | Package on CRAN (or Bioconductor) | `cran-comments.md` shows a *pending new submission*; README installs via `devtools::install_local()` | Must be accepted on CRAN before (or alongside) submission; README needs `install.packages("stabilitest")` |
| A2 | rjtools R Markdown format, HTML + PDF | Pandoc markdown → typst PDF via `build_pdf.sh` | Full re-typesetting with `rjtools::create_article()`; `.bib` citations; `_Rpackages.txt` dependency list |
| A3 | All outputs computed from code, not typed in | Section 3 tables are static; the text admits results came from "an algorithmically identical implementation" with a different RNG | Reviewers will reject this phrasing. Regenerate Task 15-style results *with the released package*, commit the artifact (`.rds`), and have the Rmd read and tabulate it |
| A4 | Fully reproducible in < 10 minutes | 12 scenarios × 500 replications with B = 200 bootstrap + greedy search will far exceed 10 min; simulation script needs dev-only `pkgload`/`tidyverse` | Ship precomputed simulation artifacts + a clearly documented regeneration script; keep live chunks to the case study and small demos; drop `pkgload`/`tidyverse` from the reproduction path |
| A5 | ≤ 20 pages; abstract ≤ 250 words, plain text | ~15–20 pages equivalent already, before adding figures and package content; abstract is a structured clinical abstract (Background/Objective/Methods/Results/Conclusions) | Rewrite abstract as a single plain paragraph; cut the historical material (see C3) to make room |
| A6 | Title suitable for the venue | Clinical-journal title ("…of Statistical Tests in Clinical Trials") | Retitle package-first, e.g. "stabilitest: Robustness and Fragility Analysis of Statistical Test Conclusions in R" |

## B. Content the R Journal expects that is absent

| # | Expected element | Gap |
| --- | --- | --- |
| B1 | **Related R packages section** | No CRAN package comparison anywhere. Reviewers will expect explicit positioning against (verify current CRAN status of each): `fragility` (fragility index for binary-outcome trials), `fragilityindex`, `sensemakr` (regression sensitivity), `konfound` (robustness of inferences), `EValue`, `boot`/`bootstrap`, `influence.ME`, `car` influence diagnostics, and Broderick et al.'s `zaminfluence` (GitHub-only — worth citing as non-CRAN). One table: package / question answered / endpoint types / how stabilitest differs |
| B2 | **Figures** | The manuscript has zero figures. The package has `plot()` methods — show them: (i) greedy p-value trajectory with the flip point, (ii) jackknife influence display, (iii) bootstrap p distribution, (iv) a simulation summary figure (score distributions null vs d = 0.8). R Journal articles are read as HTML; figures are effectively mandatory |
| B3 | **Package design/API section** | R Journal wants the software described as software: dispatcher design (`robustness_analysis()` + `robustness_lm/glm/surv/tost`), return-object classes, print/plot methods, the calibration-registry mechanism (`inst/extdata/calibration-registry.csv`, fail-closed label resolution), argument conventions (weights, `n_boot`, `max_removal_pct`, seeds). Currently compressed into two paragraphs (§2.4) |
| B4 | **Worked examples across engines** | Only the Welch case study is worked. Add short live examples for ANCOVA (`pain_ancova_trial` — currently only in the worktree; ship it in the release), GLM or Cox, and TOST — each 5–10 lines of code with real output |
| B5 | **Performance/scalability** | One sentence (§2.4). Add a small timing table (n vs engine vs seconds) generated in the Rmd |
| B6 | **Testing/QA statement** | The package's strongest credibility asset — the testthat suite, the calibration harness, frozen SAPs, hash ledgers — is invisible in the manuscript. One paragraph turns engineering into reviewer trust |

## C. Internal consistency and staleness issues

| # | Issue | Detail |
| --- | --- | --- |
| C1 | **Stale claim: "the next independent calibration target is `lm_ancova`"** (§1.4, §5.5, README) | The v1/v2 studies (worktree `codex/lm-ancova-calibration`) already ran and BOTH failed closed (`no_feasible_thresholds`): v1 at Gate A, and v2 after its sealed pilot false-GO'd exactly as the reanalysis predicted (predicted best RI at FR-safe cutoff 0.542, empirical 0.554, gate 0.70). A v3 re-aim is drafted (`docs/plans/2026-08-06-lm-ancova-v3-design.md`). The manuscript must either (a) stay silent and remove the forward-looking claim, or (b) — far stronger — report the negative result and its mechanism (score is p-monotone; gates require AUC ≈ 0.94 vs ≈ 0.89 delivered), now including the prediction-then-confirmation of the pilot-gate defect. Two rigorously documented fail-closed calibrations plus a confirmed prediction are a *differentiator*, not a weakness |
| C2 | **Seed/config incoherence** | Case study uses `n_boot = 2000, seed = 14` while the package default is `seed = 123`; the text carries apologetic RNG caveats ("±~1 point across RNG streams"). For a reproducibility-focused venue, recompute everything with package defaults in the Rmd so caveats disappear |
| C3 | **Historical layering (Task 15 "inactive" tables)** | Two of six sections (§3, §4 headers say "historical/inactive") — confusing for readers who lack the project history. Restructure: present the *current* framework and calibration policy as primary; move the Welch calibration evidence into a single "Calibration" section that presents it as the active evidence base for the one labeled unit; push v1-vs-v2 methodology archaeology (§1.3, review references) into a short design-history paragraph or drop it |
| C4 | **Version-2 changelog framing** (§1.3) | "What changed in version 2" is meaningful inside the project, not to an R Journal reader meeting the package fresh. Fold the four fixed weaknesses into the Methods rationale (each is a good *motivation* for the current design) |
| C5 | **Repository/availability metadata** | Reference list cites GitHub `ma-brain/stabilitest`; DESCRIPTION/README should carry matching `URL`/`BugReports`; the pkgdown site (if `docs/` is one) should be linked. CRAN policy checks will also look at these |

## D. Methodological exposures a referee is likely to raise (pre-empt them)

1. **The composite score is largely a monotone transform of the p-value.** The
   internal reanalysis (worktree, `tools/reanalysis/`) measured Spearman ≈ 0.97–0.99
   between components and −log₁₀(p) in the canonical ANCOVA design, and the
   manuscript already concedes this for the bootstrap component. State it openly as
   a scope condition: *in clean parametric settings the score re-expresses the
   p-value; its added value is (i) the identification of the specific carrying
   observations, (ii) the worst-case deletion bound, and (iii) behavior under
   assumption violations* — and cite the fail-closed calibrations as evidence the
   authors do not over-claim.
2. **Arbitrary composite weights.** Already handled honestly (§2.2, §5.4(6)); keep
   the "heuristic communication device" framing front and center.
3. **Greedy bound vs exact minimal subset.** Cited (§5.4(2)); consider adding the
   integer-programming reference as future work — reviewers from the Broderick
   lineage will look for it.
4. **Fragility-index critiques.** Potter (2020) is cited; make sure the response
   (trajectory reporting, upper-bound framing) appears where the index is defined,
   not only in Limitations.
5. **Label suppression policy.** This is the paper's most distinctive
   methodological stance (fail-closed calibration registry). Elevate it from policy
   description (§1.4) to a named contribution with the v1 ANCOVA negative result as
   its demonstration.

## E. Recommended submission plan (ordered)

1. Land the CRAN release (0.5.x) — blocker for everything else.
2. Decide the ANCOVA narrative (report the fail-closed result; recommended) and
   merge the relevant worktree artifacts so the paper cites released material.
3. Restructure per C3/C4 into: Introduction → Related packages (B1) → Methods →
   Package design & API (B3) → Calibration policy & evidence (incl. negative
   result) → Examples across engines (B4, with figures B2) → Performance (B5) →
   Discussion/limitations.
4. Port to rjtools; make every number chunk-computed (A3), with precomputed
   heavy artifacts + regeneration scripts (A4).
5. Rewrite abstract/title (A5/A6); add `_Rpackages.txt`, cover letter.
6. Fresh `rcmdcheck` + full reproduction run of the Rmd from a clean clone,
   timed, before submission.
