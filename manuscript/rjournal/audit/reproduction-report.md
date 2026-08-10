# R Journal reproduction rehearsal report

Date: 2026-08-10

Base revision: `924d1a9decf76253006f58274d181f660777dd95`

Rehearsal checkout: a fresh local clone with the intended Task 13 documentation
changes applied and an isolated user library

Result: **PASS**

## Scope and environment

The commands in `manuscript/rjournal/REPRODUCE.md` were followed in order from
the root of a clean temporary checkout. The full calibration was not rerun; its
documented smoke mode was run instead. No held-out Welch validation data were
opened.

- R 4.6.1 (2026-06-24), `aarch64-apple-darwin23`
- macOS Tahoe 26.6.1
- locale `C.UTF-8`; time zone `Europe/Bucharest`
- pandoc 3.10.1
- pdfTeX 3.141592653-2.6-1.40.29 (TeX Live 2026)
- BLAS: Apple R framework reference BLAS
- LAPACK 3.12.1

Dependency versions:

| Package | Version | Package | Version |
| --- | --- | --- | --- |
| stabilitest | 0.6.0 | devtools | 2.5.2 |
| digest | 0.6.39 | dplyr | 1.2.1 |
| ggplot2 | 4.0.3 | jsonlite | 2.0.0 |
| knitr | 1.51 | patchwork | 1.3.2 |
| purrr | 1.2.2 | rcmdcheck | 1.4.0 |
| rjtools | 1.0.18.1 | rmarkdown | 2.31 |
| survival | 3.8.9 | testthat | 3.3.2 |
| tibble | 3.3.1 | | |

## Command record

Wall times are `/usr/bin/time -p` real times. Exit codes are from the commands,
not inferred from their printed summaries.

| Step | Command | Exit | Wall time | Result |
| --- | --- | ---: | ---: | --- |
| Dependencies | R expression from `REPRODUCE.md` reading `_Rpackages.txt` | 0 | 0.97 s | All 14 non-local dependencies available |
| Package install | `R CMD INSTALL --no-multiarch --with-keep.source .` | 0 | 2.02 s | Installed stabilitest 0.6.0 into isolated library |
| PDF render | `rmarkdown::render(..., "rjtools::rjournal_pdf_article")` | 0 | 7.04 s | 10-page A4 PDF |
| HTML render | `rmarkdown::render(..., "rjtools::rjournal_web_article")` | 0 | 6.16 s | HTML and support files produced |
| Artifact contracts | `Rscript manuscript/rjournal/tools/check-artifact-contracts.R` | 0 | 0.13 s | Five evidence directories passed |
| Article claims | `Rscript manuscript/rjournal/tools/check-article-claims.R` | 0 | 0.10 s | Three reader-facing files passed |
| Optional smoke | `Rscript manuscript/rjournal/tools/regenerate-simulation.R --smoke` | 0 | 7.95 s | 12 scenarios; separate `simulation-smoke/` output |
| Package tests | `Rscript -e 'devtools::test()'` | 0 | 57.58 s | 1,011 pass; 0 fail/warn/skip |
| Package check | `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'` | 0 | 100.93 s | 0 errors, 0 warnings, 1 NOTE |

Dependency verification through both article renders took 16.19 seconds. The
article reproduction is therefore well below the ten-minute journal limit.
Including both audits, the optional smoke test, all tests, and `R CMD check`,
the successful rehearsal commands took about 183 seconds. Full calibration is
explicitly optional and is not included in either total.

The sole `R CMD check` NOTE was:

```text
Maintainer: ‘Marius Ardelean <marally@gmail.com>’
New submission
```

## Output hashes and equality assessment

Clean-checkout render hashes:

```text
ba62b4ea0472fde4742d872f483baec243ced345c94205d829c12e50acbc33d5  manuscript/rjournal/stabilitest.pdf
7e75fcc476548a412ceddcdb60e13325b54d9e4f37692b8d3fe091981aa57483  manuscript/rjournal/stabilitest.html
```

The clean HTML was byte-identical to the committed HTML. The clean PDF and the
committed PDF had different container hashes (`ba62...` versus `e101...`), as
expected for regenerated PDF metadata, but both had 10 A4 pages and their
layout-preserving extracted text was byte-identical:

```text
372a85bf1f798fd8f3e98abf242afe081602aa0f4151adfaf306e457dd0f8e2f  extracted PDF text (both files)
```

The regenerated one-page motivation letter was also visually inspected after
rendering. Its SHA-256 hash is:

```text
07d956284018cd281afc2f390a2bfe8153371127504658f1a0b0d6aecccf4190  manuscript/rjournal/motivation-letter.pdf
```

These results support exact equality for the HTML and substantive equality for
the PDF in the recorded environment. Other supported platforms may change PDF
metadata, font metrics, timing, or graphics encodings; the artifact contracts,
tables, figures, and statistical conclusions are the cross-platform criteria.

## Rehearsal findings and resolutions

1. The restricted execution sandbox initially prevented the HTML renderer and
   CRAN incoming-feasibility check from resolving CRAN metadata. The identical
   commands succeeded with network access. This is an execution-environment
   restriction, not an article or package dependency failure.
2. The first network-enabled source-package check exposed a Welch documentation
   test that assumed the source-only `manuscript/` tree was present, although
   `.Rbuildignore` intentionally excludes that tree. Comparable calibration
   tests already skip in a built tarball. The Welch test received the same
   source-tree guard; its 135 assertions still run and pass in a checkout, and
   the subsequent full source-package check completed with only the expected
   NOTE.
3. The PDF build printed a non-fatal `microtype` patch warning. The authoritative
   article log contained no overfull boxes, undefined references, fatal errors,
   or LaTeX warnings, and all ten pages had already passed visual inspection.

No validation artifact was accessed or generated during this rehearsal, and no
external submission was made.
