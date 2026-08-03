# Simulation Entry Point and Weight Validation Design

## Context

`manuscript/simulation_study.R` currently calls
`source("robustness_analysis.R")`. That path is resolved against the caller's
working directory, so the script fails both from the repository root and from
the `manuscript/` directory. Even if the path were changed to
`R/robustness_analysis.R`, sourcing that file alone would omit shared helpers
and would not reproduce normal package namespace loading.

The composite-score validator currently checks only length, non-negativity,
and sum. The score calculation indexes the vector by the names `jackknife`,
`fragility`, and `bootstrap`, so unnamed or incorrectly named vectors pass the
validator and fail later with `subscript out of bounds`. Non-finite values can
also escape the validator and produce internal conditional errors.

## Goals

- Run the simulation entry point against the exact package checkout containing
  the manuscript script, regardless of the caller's working directory.
- Fail early and clearly when the checkout cannot be identified or loaded.
- Treat composite weights as a named API object whose labels determine each
  component's meaning.
- Accept the three required weight names in any order.
- Reject malformed weights before any robustness computation begins.
- Cover the repaired behavior with focused regression checks while preserving
  all existing results and defaults.

## Non-goals

- Changing the simulation scenarios, random seeds, replication counts, or
  reported summary statistics.
- Turning the manuscript simulation into an installed command-line interface.
- Normalizing weights automatically or accepting aliases for component names.
- Adding new scoring components or changing the default 0.4/0.4/0.2 weights.

## Simulation Loading Design

The script will determine its own absolute path in two supported launch modes:

1. Direct execution with `Rscript manuscript/simulation_study.R`, using the
   `--file=` command argument.
2. Execution through `source()`, using the active source frame's `ofile` value.

The repository root is the parent of the script's `manuscript/` directory. The
script will verify that the candidate contains both `DESCRIPTION` and
`R/robustness_analysis.R`. If path discovery or verification fails, it will
stop with an actionable message rather than depend on the current directory.

After discovery, the script will require `pkgload` and call
`pkgload::load_all()` on the repository root with helpers disabled. This loads
the current checkout using package namespace semantics, including all shared
helpers and imports, instead of manually maintaining a fragile source order.
The existing simulation functions and interactive execution guard remain
unchanged.

The script remains excluded from the built package. `pkgload` is therefore a
development/manuscript runtime prerequisite rather than a runtime dependency
of the installed package. Documentation will state the launch command and this
prerequisite.

## Weight Validation Contract

`validate_alpha_weights()` will validate weights in this order:

1. The object is numeric, has length three, has non-empty unique names, and its
   names are exactly `jackknife`, `fragility`, and `bootstrap`.
2. Every value is finite and non-negative.
3. Values sum to one within the existing `1e-8` tolerance.

Each category receives a stable, specific error message. Name order is not
significant because score calculation already uses named indexing. Validation
will not silently reorder or normalize the user-supplied vector, so result
objects continue to preserve the supplied representation.

Because two-sample analyses call this validator directly and model/TOST
analyses share `robustness_engine()`, one implementation enforces the same
contract across all public analysis families.

## Testing

- Add table-driven edge-case tests for unnamed, missing-name, duplicate-name,
  unknown-name, `NA`, `NaN`, infinite, negative, and non-unit-sum vectors.
- Verify a correctly named vector in non-canonical order is accepted and maps
  each component to the intended coefficient.
- Exercise both direct two-sample validation and the shared model/TOST engine
  so the public interfaces cannot drift.
- Run the simulation script as a subprocess from the repository root and from
  a different working directory. Both smoke runs should load the checkout,
  define the simulation functions, and exit without starting the interactive
  500-replication grid.
- Run focused tests, the full test suite, `git diff --check`, and a clean source
  archive `R CMD check` before completion.

## Documentation

Update the manuscript reproduction instructions and NEWS to explain that the
simulation script loads the current checkout through `pkgload`, can be launched
from any working directory, and now reports precise composite-weight contract
violations.
