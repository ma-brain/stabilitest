## Test environments

* local macOS (aarch64), R 4.6.1
* win-builder (release and devel): human action pending before CRAN upload

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New submission

This release advances the local 0.6.0 line to 0.6.1. The package
has not yet been published on CRAN, so `R CMD check --as-cran` still reports
the incoming-feasibility "New submission" note. No other NOTES remain after
excluding development-only paths (`.worktrees`, `.claude`, `graphify-out`,
`manuscript/`, `docs/`, `tools/`) from the build.

## Downstream dependencies

There are currently no reverse dependencies.
