# AGENTS.md

## Cursor Cloud specific instructions

`stabilitest` is a single **R package** (R `>= 4.2`), not a web/service app. There
are **no runtime services** (no server, database, ports, or daemons). "Running
the application" means loading the library into an R session and calling its
functions; "end-to-end" verification is the `testthat` suite and `R CMD check`.

### Environment (already provisioned by the startup update script)
- R runtime + all package dependencies are installed as **binary `.deb`s** via
  the [r2u](https://eddelbuettel.github.io/r2u/) apt repo (packages named
  `r-cran-*`) plus the CRAN apt repo for `r-base-core`. These live system-wide in
  `/usr/lib/R/site-library`.
- Do **not** use `pak`/`remotes`/`install.packages()` to (re)install the declared
  dependencies: on this box they re-resolve against CRAN/PPM and **compile from
  source** (slow and brittle, e.g. `Matrix`/`survival` take minutes). Prefer the
  `r-cran-*` apt binaries. If you must install an R package interactively, note
  that `/usr/local/lib/R/site-library` is only writable via `sudo`
  (e.g. `sudo Rscript -e '...'`).

### Common commands (run from repo root `/workspace`)
- Load for development: `Rscript -e 'devtools::load_all(".")'`
- Run tests: `Rscript -e 'devtools::test()'` (takes ~40s)
- Lint/build/check (mirrors the `R-CMD-check` GitHub Action; there is no separate
  linter): `Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))'`.
  A clean run reports only the benign `New submission` NOTE.
- Vignette rebuild works because `pandoc` is installed and on `PATH`.

### Quick smoke test of core functionality
```r
Rscript -e 'devtools::load_all("."); print(robustness_analysis(pain_treatment, pain_placebo, test_type="t.test", n_boot=2000, interpret=TRUE))'
```
Bundled datasets `pain_treatment` / `pain_placebo` are available after
`load_all()`. See `README.md` for the full API (e.g. `robustness_analysis`,
`robustness_lm`, `robustness_glm`, `robustness_surv`, `robustness_tost`).

### Regenerating docs
`NAMESPACE` and `man/` are generated; regenerate with
`Rscript -e 'roxygen2::roxygenise()'` after changing roxygen comments.
