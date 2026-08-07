# Reproducing the article

Everything below assumes a clean clone of the repository and a working R
installation. All commands are run **from the repository root** unless stated
otherwise. Timings are wall-clock on an Apple aarch64 laptop (R 4.6.1) and are
what the rehearsal described at the bottom of this file actually measured.

## 0. Prerequisites

R >= 4.2, pandoc (bundled with RStudio, or installed separately), and a LaTeX
installation for the PDF output. If you use TinyTeX, note that the article
requires **TeX Live 2026 or newer**: the LaTeX writer in current pandoc emits
`\pandocbounded`, which needs an `l3backend` newer than the TeX Live 2025 tree
ships. Upgrade with:

```r
tinytex::reinstall_tinytex(repository = "https://mirror.ctan.org/systems/texlive/tlnet")
```

The article's YAML also defines a `\pandocbounded` fallback, so an older tree
may still work; the upgrade is the reliable path.

## 1. Install the package (~30 seconds)

```sh
R CMD INSTALL --no-multiarch --with-keep.source .
```

The article loads `stabilitest` with `library()` only. It never uses
`pkgload::load_all()`, so what you knit is what the released package does.

## 2. Install article dependencies (~1 minute)

The packages the article itself needs are listed in `_Rpackages.txt`:

```r
install.packages(c("ggplot2", "knitr", "rmarkdown", "rjtools",
                   "survival", "patchwork"))
```

## 3. Knit the article (~10 seconds each)

```sh
cd manuscript/rjournal
Rscript -e 'rmarkdown::render("stabilitest.Rmd", output_format = "rjtools::rjournal_pdf_article")'
Rscript -e 'rmarkdown::render("stabilitest.Rmd", output_format = "rjtools::rjournal_web_article")'
```

This is the full reproduction of the article as published. Heavy evidence
loads from the committed artifacts under `artifacts/`; the only code that runs
live is the short worked examples in the Examples section. Both formats
complete in well under the journal's 10-minute budget.

Outputs: `stabilitest.pdf` (10 pages) and `stabilitest.html`.

## 4. Optional: regenerate the evidence artifacts from scratch

The committed artifacts under `artifacts/` are what the article reads. They
were produced by the scripts in `tools/`, each of which uses only the
installed package and a fixed master seed, and each of which writes a manifest
recording package version, R version, seeds, and runtime alongside its output.

Regenerating them is **not** required to reproduce the article. Run these only
if you want to verify the evidence itself.

| Script | Produces | Runtime |
| --- | --- | --- |
| `tools/regenerate-simulation.R` | `artifacts/simulation/` — 12 scenarios x 500 replications, full replicate-level results + summary | **~17 minutes** |
| `tools/regenerate-case-study.R` | `artifacts/case-study/` — Welch case study at package defaults, `n_boot = 2000` | ~10 seconds |
| `tools/regenerate-timing.R` | `artifacts/timing/` — performance table | ~25 seconds |
| `tools/regenerate-testcount.R` | `artifacts/testing/` — test-suite size for the QA section | ~50 seconds |

```sh
# from the repository root
Rscript manuscript/rjournal/tools/regenerate-simulation.R
Rscript manuscript/rjournal/tools/regenerate-case-study.R
Rscript manuscript/rjournal/tools/regenerate-timing.R
Rscript manuscript/rjournal/tools/regenerate-testcount.R
```

The simulation script accepts `--smoke` (5 replications, `n_boot = 20`, ~7
seconds) to verify the pipeline end to end without the full run, and
`--nrep`/`--n-boot`/`--output-dir` to vary the design. A smoke run writes to a
separate directory and will not overwrite the published artifacts.

Because every scenario seed is derived deterministically from the master seed
recorded in `artifacts/simulation/manifest.json`, a full rerun on the same
package version reproduces the committed numbers exactly.

## 5. Optional: verify the package itself

```sh
Rscript -e 'devtools::test()'
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'
```

Expected: all tests pass; `R CMD check` reports 0 errors, 0 warnings, and a
single "New submission" NOTE.

**Locale note.** The test suite includes a source-tree audit that reads
UTF-8 documentation containing mathematical symbols. Run it under a UTF-8
locale; under `LC_ALL=C` the audit fails spuriously on encoding, not on
content:

```sh
LC_ALL=en_US.UTF-8 Rscript -e 'devtools::test()'
```

## Rehearsal record

The steps above were rehearsed on 2026-08-07 against the branch as committed.
Package install, dependency install, and both knits completed as described.
The full simulation regeneration was run once end to end (17.3 minutes,
recorded in `artifacts/simulation/manifest.json`) and its output is what the
article reads.
