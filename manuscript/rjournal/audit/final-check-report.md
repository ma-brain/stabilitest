# R Journal final check report

Date: 2026-08-10

Source revision: `feec693` plus this audit report

Result: **PASS WITH EXPLICITLY AUTHORIZED CRAN REGISTRY EXCEPTION**

No external submission was performed. The human author remains responsible for
the R Journal upload.

## CRAN version gate

The package, article, production registry, and evidence manifests all identify
`stabilitest` version 0.6.0. The canonical URL
`https://CRAN.R-project.org/package=stabilitest` returned HTTP 303 and redirected
to `https://cran.r-project.org/web/packages/stabilitest/index.html`, which
returned HTTP 404 on 2026-08-10. Consequently, live CRAN parity could not be
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
| `Rscript -e 'devtools::test()'` | 1,011 pass; 0 fail, warn, or skip | 56.89 s |
| `rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))` | 0 errors, 0 warnings, 1 NOTE | 101.82 s |

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
| PDF render time | 6.00 s from extracted archive | Pass |
| HTML render time | 6.43 s from extracted archive | Pass |
| Article time budget | each format under 10 minutes | Pass |
| Archive size | 716,672 bytes; limit 10 MB | Pass |
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
  `e6bc31e579cbc8c3cf6dade5142303cb18098a4d72cf9f06ca9a063e9eb64ff4`.
- Regenerated `RJwrapper.tex` matched the archived wrapper byte for byte:
  `027df16d8f20454502dea157d1529918f8c37e150156d2a443b51c025f2d28ef`.
- The archived final PDF matched the committed PDF byte for byte:
  `e101c19964f5c2f8edefc464e936c440478d60b3662215dc7954f485b164c66c`.
- Regenerated PDF container metadata differed, but its layout-preserving
  extracted text matched the archived PDF exactly:
  `372a85bf1f798fd8f3e98abf242afe081602aa0f4151adfaf306e457dd0f8e2f`.
- Regenerated HTML differed only in the embedded Radix resource-manifest list,
  because the allowlisted archive contains a different support-file set than
  the worktree. After removing that non-visible manifest node, the committed
  and regenerated DOM were identical, with SHA-256
  `2d8e7bf9703515672dad69d17813ed7f60bb52b96b25b96e583067add1cb880e`.

The generated wrapper also compiled directly from the archived TeX,
bibliography, style, and LaTeX figure files. Four LaTeX passes produced a clean
10-page PDF with no unresolved references or overfull boxes. Both extracted
artifact-contract and article-claim audits passed.

## Archive

Path:
`/Users/Marius/.codex/worktrees/ee43/stabilitest-rjournal-submission.zip`

Size: 716,672 bytes (101 ZIP entries)

SHA-256:
`4186abd615af0a6d72be0845c4a364c142f60c88cb48b563051d519b0c0fccc8`

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

The regenerated HTML was served locally and inspected in the app browser. The
canonical title and heading matched, both figures loaded with non-empty alt
text, all three tables had captions, all 128 links had non-empty targets, and
the related-package table and both figures rendered cleanly. All requested
support assets returned HTTP 200; the browser's unrelated favicon request
returned 404.

Poppler emitted font-cache write warnings in the sandbox, but all requested PNG
pages were created and visually verified. The R Journal PDF render also emitted
a non-fatal microtype patch warning; the final authoritative wrapper log was
clean after the fourth LaTeX pass.

No external R Journal or CRAN submission was attempted.
