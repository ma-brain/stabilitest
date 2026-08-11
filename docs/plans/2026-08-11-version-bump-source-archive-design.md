# Version-bump source archive design

Date: 2026-08-11

## Goal

Build the R source package archive and PDF reference manual automatically
whenever the package version in `DESCRIPTION` changes on `main`, and never
build them for an unrelated push.
The workflow must not create a Git tag, GitHub Release, CRAN submission, or R
Journal submission.

## Trigger and gate

GitHub Actions watches pushes to `main` whose changed paths include
`DESCRIPTION`. The job reads the current `Version:` field and the same field
from the pre-push revision identified by `github.event.before`.

The archive steps run only when both fields are valid, non-empty versions and
the values differ. A `DESCRIPTION` metadata edit that leaves `Version:`
unchanged may start a lightweight workflow run, but it must not install build
dependencies, build an archive, or upload an artifact.

## Build and output

For a genuine version bump, the workflow:

1. checks out the full history needed for the comparison;
2. installs release R, TinyTeX, and the package build dependencies;
3. runs `R CMD build .`;
4. requires the resulting filename to be
   `stabilitest_<DESCRIPTION-Version>.tar.gz`;
5. runs `R CMD Rd2pdf .` and requires the non-empty output
   `stabilitest_<DESCRIPTION-Version>-manual.pdf`; and
6. uploads both files as a GitHub Actions artifact named for the version.

The artifact is a downloadable workflow artifact. The workflow has read-only
repository permissions and does not create releases or tags.

## Repository guidance

`AGENTS.md` will document:

- the version-bump-only source archive and PDF-manual contract;
- that CRAN publication is not a prerequisite for building the archive;
- the canonical R Journal evidence, script, and generated-figure locations;
- fail-closed calibration and held-out-data safeguards;
- required documentation, test, and package-check commands.

## Verification

Verification covers YAML parsing, the configured push/path trigger, the
old-versus-new version gate, conditional build/upload steps, exact source
archive and manual filenames, a non-empty manual PDF, read-only permissions,
and absence of tag, release, CRAN, or external-submission actions. The
repository documentation audit and package tests must continue to pass.
