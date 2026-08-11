# R Journal final check report

Date: 2026-08-11

Source revision: `12079a2` plus this audit report

Result: **PASS WITH EXPLICITLY AUTHORIZED CRAN REGISTRY EXCEPTION**

No external submission was performed. The human author remains responsible for
the R Journal upload.

## CRAN version gate

The package, article, production registry, and evidence manifests all identify
`stabilitest` version 0.6.0. The canonical URL
`https://CRAN.R-project.org/package=stabilitest` returned HTTP 303 and redirected
to `https://cran.r-project.org/web/packages/stabilitest/index.html`, which
returned HTTP 404 on 2026-08-11. Consequently, live CRAN parity could not be
confirmed, and `rjtools::check_packages_available()` reported only
`stabilitest` as unavailable.

Execution initially stopped at this gate as the plan required. The user then
explicitly authorized completion under the earlier planning assumption that
version 0.6.0 is the publication target. This report preserves the live 404 as
an unresolved registry exception; it does not represent the CRAN check as a
pass. The canonical page must be checked again before human upload.

## Package verification

| Check | Result | Wall time |
| --- | --- | ---: |
| `Rscript -e 'devtools::test()'` | 1,018 pass; 0 fail, warn, or skip | 57.3 s |
| `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))` | 0 errors, 0 warnings, 1 NOTE | 88.6 s |

The accepted NOTE was `New submission` with the package maintainer identity.

## Article verification

- `check-article-claims.R`: passed for all three reader-facing files.
- `rjtools::check_title()`: passed.
- `rjtools::check_folder_structure()`: no problematic file; it emitted the
  standard reminder to remove `.log`, `.aux`, and `.out` files.
- `rjtools::check_bib_doi()`: all references have a DOI or URL.
- `rjtools::check_pkg_label()`: package markup passed.
- `rjtools::check_packages_available()`: reported only `stabilitest` as absent
  from CRAN, matching the authorized live-registry exception above.

`rjtools::initial_check_article()` did not complete. With rjtools 1.0.18.1 it
passed naming, directory-name, motivation-letter, title-case, section-case, and
plain-abstract checks, then crashed in `check_spelling()`:

```text
Error in (abstract_loc + 1):(bib_loc - 1) : argument of length 0
Calls: initial_check_article -> check_spelling
```

This crash is recorded as a checker defect, not as a manuscript pass. Every
individual checker required by the execution plan was run, with the results
above.

## Mechanical constraints

| Constraint | Evidence | Result |
| --- | --- | --- |
| PDF length | 10 A4 pages; limit 20 | Pass |
| Abstract length | 152 words; limit 250 | Pass |
| PDF render time | 6.80 s from extracted archive | Pass |
| HTML render time | 6.23 s from extracted archive | Pass |
| Article time budget | each format under 10 minutes | Pass |
| Archive size | 716,682 bytes; limit 10 MB | Pass |
| Forbidden files | no `.log`, `.aux`, `.out`, smoke, checkpoint, cache, or temporary paths in ZIP | Pass |
| Citations/references | no undefined citation or reference in final wrapper log | Pass |
| Overfull boxes | none in final wrapper log | Pass |
| Figure accessibility | 2 of 2 images have explicit non-empty alt text | Pass |
| Table accessibility | 3 of 3 tables have explicit non-empty captions | Pass |
| Version parity inside submission | DESCRIPTION, article, artifacts, and registries all 0.6.0 | Pass |
| Live CRAN version parity | canonical package page returned 404 | Authorized exception |
| Welch verdict parity | article, active registry, and published verdict all uncalibrated | Pass |
| Held-out validation | `held_out_opened = FALSE`; no validation result exists or was opened | Pass |

The published Welch verdict is `uncalibrated` with reason
`no_feasible_thresholds`. The active registry row is `welch-2026-2`, both
cutoffs are `NA`, numeric diagnostics remain available, and categorical Welch
labels remain suppressed.

## Generated-output provenance

The archive was extracted into a second temporary directory and rebuilt using
the installed stabilitest 0.6.0 package.

- Extracted R Markdown SHA-256 matched the committed source exactly:
  `e073fc4a21c7d89584b96a0a0e499c25340473a9d20373eb0b6c159f5b14ee23`.
- Regenerated TeX matched the committed TeX byte for byte:
  `77ddf9c2de4fd6345ea29fd7d6b5499fd06baa9cc36ed09767aebcdb03f76389`.
- Regenerated `RJwrapper.tex` matched the archived wrapper byte for byte:
  `027df16d8f20454502dea157d1529918f8c37e150156d2a443b51c025f2d28ef`.
- The freshly rendered archived final PDF had SHA-256
  `ebd47f160c149fb9e0973a17b24920db1280e7c8fe65601b23db9d188688323f`.
- Regenerated PDF container metadata differed, but its layout-preserving
  extracted text matched the archived PDF exactly:
  `baddc256f847cd9ba73c6bdffb026415d5ea2318d3388b1d7511f15e67d5a62f`.
- Regenerated HTML differed only in the embedded Radix resource-manifest list,
  because the allowlisted archive contains a different support-file set than
  the worktree. After removing that non-visible manifest node, the committed
  and regenerated DOM were identical, with SHA-256
  `e7b824074d25629324cb6f68a50157b4b6f196a78b6d2a7a89338da20f37dcba`.

The generated wrapper also compiled directly from the archived TeX,
bibliography, style, and LaTeX figure files. Four LaTeX passes produced a clean
10-page PDF with no unresolved references or overfull boxes. Both extracted
artifact-contract and article-claim audits passed.

## Archive

Path:
`/Users/Marius/Github/stabilitest-rjournal-submission.zip`

Size: 716,682 bytes (101 ZIP entries)

SHA-256:
`9122fc36d3b29a1dd1028d77db5ccb49630dc151ed161c1e1e759a8b70874f6e`

The bundle was assembled from an explicit allowlist. It contains the final PDF,
R Markdown, generated TeX and wrapper, bibliography and journal style, required
HTML and LaTeX figures, reproduction instructions and package list, motivation
letter, evidence artifacts and generation/audit scripts, production registry
snapshot and package metadata, compact Welch published evidence and study
instructions, and the optional related-package and calibration-methods notes.

The Welch study's validation implementation source is included for method
transparency, but no held-out validation output or validation result is present.
The ZIP is outside the repository and does not contain itself.

## Visual review

All 10 regenerated article pages and the one-page motivation letter were
rendered to PNG and inspected. Headers, footers, page numbering, title wrapping,
tables, formulae, figures, captions, code/output adjacency, references, and
author block were legible with no clipping, overlap, missing glyph, or orphaned
content.

The regenerated HTML passed structural and content inspection. Its canonical
title matched, both figures had non-empty alt text, all three tables had
captions, and all 109 anchor elements had non-empty targets. After removing the
non-visible Radix resource-manifest node, its DOM matched the committed HTML
byte for byte. The only reader-visible change from the previously inspected
render was the synchronized test count from 1,011 to 1,018.

Poppler emitted font-cache write warnings in the sandbox, but all requested PNG
pages were created and visually verified. The R Journal PDF render also emitted
a non-fatal microtype patch warning; the final authoritative wrapper log was
clean after the fourth LaTeX pass.

No external R Journal or CRAN submission was attempted.
