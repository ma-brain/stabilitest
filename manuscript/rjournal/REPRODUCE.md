# Reproducing the article

Everything below assumes a clean clone of the repository and a working R
installation. Run all commands from the repository root. The recorded rehearsal
used R 4.6.1 on aarch64 macOS 26.6.1, pandoc 3.10.1, and TeX Live 2026. The
committed `\pandocbounded` fallback also rendered successfully in this setup;
no untested minimum TeX Live release is claimed.

## 0. Prerequisites

Install R >= 4.2, pandoc (bundled with RStudio or installed separately), and a
LaTeX distribution for PDF output. Then install every package needed to build
the package, render the article, regenerate evidence, and run the optional
checks. `_Rpackages.txt` is the authoritative list; `stabilitest` itself is
installed from this checkout in the next step.

```r
packages <- setdiff(
  trimws(readLines("manuscript/rjournal/_Rpackages.txt")),
  c("", "stabilitest")
)
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
```

## 1. Install the package

```sh
R CMD INSTALL --no-multiarch --with-keep.source .
```

The article loads `stabilitest` with `library()`; it does not use
`pkgload::load_all()`. Consequently, the render exercises the installed package
in the same way as a reader's session.

## 2. Render the article

```sh
Rscript -e 'rmarkdown::render("manuscript/rjournal/stabilitest.Rmd", output_format = "rjtools::rjournal_pdf_article")'
Rscript -e 'rmarkdown::render("manuscript/rjournal/stabilitest.Rmd", output_format = "rjtools::rjournal_web_article")'
```

Outputs are `manuscript/rjournal/stabilitest.pdf` and
`manuscript/rjournal/stabilitest.html`. Heavy calibration results are read from
the committed frozen artifacts; the live chunks are short worked examples.
The recorded clean-checkout reproduction of both formats completed well within
the R Journal's ten-minute article budget.

## 3. Audit the artifact contracts

```sh
Rscript manuscript/rjournal/tools/check-artifact-contracts.R
Rscript manuscript/rjournal/tools/check-article-claims.R
```

These checks verify artifact schemas, hashes, provenance links, and the
reader-facing consistency rules used by the manuscript.

## 4. Optional calibration smoke test

The article reads the committed artifacts under `manuscript/rjournal/artifacts/`.
Regenerating the full evidence is not required to reproduce the article. To
exercise the simulation pipeline end to end without modifying the published
artifacts, run:

```sh
Rscript manuscript/rjournal/tools/regenerate-simulation.R --smoke
```

The smoke run uses 5 replications and 20 bootstrap iterations and writes to
`manuscript/rjournal/artifacts/simulation-smoke/`, not to the frozen evidence
directory.

For a deliberate full regeneration, run the following scripts. Their manifests
record package and R versions, seeds, runtime, script hashes, and source-study
provenance.

| Script | Output | Typical recorded runtime |
| --- | --- | ---: |
| `regenerate-simulation.R` | `artifacts/simulation/` | about 17 minutes |
| `regenerate-case-study.R` | `artifacts/case-study/` | about 10 seconds |
| `regenerate-timing.R` | `artifacts/timing/` | about 25 seconds |
| `regenerate-testcount.R` | `artifacts/testing/` | about 50 seconds |
| `regenerate-welch-calibration-summary.R` | `artifacts/welch-calibration/` | under 1 second |

```sh
Rscript manuscript/rjournal/tools/regenerate-simulation.R
Rscript manuscript/rjournal/tools/regenerate-case-study.R
Rscript manuscript/rjournal/tools/regenerate-timing.R
Rscript manuscript/rjournal/tools/regenerate-testcount.R
Rscript manuscript/rjournal/tools/regenerate-welch-calibration-summary.R
```

The full calibration is intentionally outside the journal's article-render
budget. It is optional because the committed generation scripts and frozen
artifacts are the reviewable evidence interface.

## 5. Verify the package

```sh
Rscript -e 'devtools::test()'
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
```

Expected: all tests pass; `R CMD check` reports 0 errors and 0 warnings. On the
recorded platform it reports only the benign `New submission` NOTE.

The source-tree audit reads UTF-8 documentation containing mathematical
symbols. Use a UTF-8 locale if the platform default is not UTF-8, for example:

```sh
LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test()'
```

## 6. Record the environment and output hashes

```sh
Rscript -e 'sessionInfo()'
Rscript -e 'p <- scan("manuscript/rjournal/_Rpackages.txt", what = "", quiet = TRUE); for (x in p) cat(x, as.character(packageVersion(x)), "\n")'
shasum -a 256 manuscript/rjournal/stabilitest.pdf manuscript/rjournal/stabilitest.html
```

Exact artifact equality is expected in the recorded environment. On other
supported platforms, the substantive tables, figures, statistical conclusions,
and audit contracts should reproduce, while byte-level PDF/HTML hashes, timing,
and graphics metadata may differ. Document such platform differences rather
than treating them as statistical discrepancies. No lockfile or container is
included because exact cross-platform byte equality is not a stated submission
requirement.

The complete 2026-08-10 clean-checkout rehearsal, including commands, versions,
exit codes, wall times, hashes, and observed platform differences, is recorded
in `manuscript/rjournal/audit/reproduction-report.md`.
