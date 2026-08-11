#!/usr/bin/env Rscript

workflow_path <- file.path(".github", "workflows", "build-archive.yaml")

if (!file.exists(workflow_path)) {
  stop("Missing workflow: ", workflow_path, call. = FALSE)
}

workflow <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")
violations <- character()

require_pattern <- function(pattern, message, fixed = FALSE) {
  if (!grepl(pattern, workflow, perl = !fixed, fixed = fixed)) {
    violations <<- c(violations, message)
  }
}

forbid_pattern <- function(pattern, message) {
  if (grepl(pattern, workflow, perl = TRUE, ignore.case = TRUE)) {
    violations <<- c(violations, message)
  }
}

require_pattern(
  "(?ms)^on:\\s*\\n\\s+push:\\s*\\n\\s+branches:\\s*\\[main\\]\\s*\\n\\s+paths:\\s*\\n\\s+-\\s+DESCRIPTION\\s*$",
  "trigger must be a push to main limited to DESCRIPTION changes"
)
forbid_pattern("(?m)^\\s*(tags|workflow_dispatch|pull_request|schedule):",
               "tag, manual, pull-request, and schedule triggers are forbidden")
require_pattern("(?m)^permissions:\\s+read-all\\s*$",
                "workflow permissions must remain read-all")
require_pattern("uses:\\s*actions/checkout@v5", "checkout@v5 is required")
require_pattern("fetch-depth:\\s*0", "checkout must fetch history for comparison")
require_pattern("id:\\s*version", "version detection step must have id: version")
require_pattern("github\\.event\\.before", "pre-push revision must come from github.event.before")
require_pattern("git show", "previous DESCRIPTION must be read with git show")
require_pattern("\\^Version:", "Version fields must be extracted from DESCRIPTION")
require_pattern("current_version", "current DESCRIPTION version must be named explicitly")
require_pattern("previous_version", "previous DESCRIPTION version must be named explicitly")
require_pattern("current_version.*previous_version|previous_version.*current_version",
                "current and previous versions must be compared")
require_pattern("changed=false", "unchanged versions must emit changed=false")
require_pattern("changed=true", "version bumps must emit changed=true")
require_pattern("current=.*GITHUB_OUTPUT", "new version must be exposed as current output")
require_pattern("previous=.*GITHUB_OUTPUT", "old version must be exposed as previous output")

conditional <- "if: steps.version.outputs.changed == 'true'"
conditional_count <- lengths(regmatches(workflow, gregexpr(conditional, workflow, fixed = TRUE)))
if (conditional_count < 5L) {
  violations <- c(
    violations,
    paste(
      "R setup, TinyTeX setup, dependency setup, build, and upload must all",
      "be version-change conditional"
    )
  )
}

require_pattern("R CMD build \\.", "workflow must run R CMD build .")
require_pattern(
  "stabilitest_\\$\\{\\{ steps\\.version\\.outputs\\.current \\}\\}\\.tar\\.gz",
  "archive filename must use the current DESCRIPTION version"
)
require_pattern("test -f", "workflow must require the exact archive file to exist")
require_pattern("uses:\\s*r-lib/actions/setup-tinytex@v2",
                "TinyTeX setup is required for the PDF manual")
require_pattern("R CMD Rd2pdf[^\\n]*[[:space:]]\\.",
                "workflow must run R CMD Rd2pdf for the package root")
require_pattern(
  "stabilitest_\\$\\{\\{ steps\\.version\\.outputs\\.current \\}\\}-manual\\.pdf",
  "manual filename must use the current DESCRIPTION version"
)
require_pattern("test -s", "workflow must require a non-empty manual PDF")
require_pattern("id:\\s*package_files",
                "build step must expose the validated package file paths")
require_pattern("uses:\\s*actions/upload-artifact@v4",
                "workflow must upload with actions/upload-artifact@v4")
require_pattern("steps\\.package_files\\.outputs\\.archive",
                "upload must use the validated archive path output")
require_pattern("steps\\.package_files\\.outputs\\.manual",
                "upload must use the validated manual path output")

forbid_pattern("softprops/action-gh-release", "GitHub Release creation is forbidden")
forbid_pattern("(?m)^[^#\\n]*\\bgh\\s+release\\b", "gh release commands are forbidden")
forbid_pattern("(?m)^[^#\\n]*\\bgit\\s+tag\\b", "git tag commands are forbidden")
forbid_pattern("contents:\\s*write", "write permission to repository contents is forbidden")
forbid_pattern("(?m)^[^#\\n]*(R CMD check.*--as-cran|devtools::release|submit_cran)",
               "CRAN submission or release commands are forbidden")

if (length(violations)) {
  cat("Version-bump archive workflow audit failed:\n")
  cat(paste0("- ", violations, collapse = "\n"), "\n")
  quit(status = 1L)
}

cat("Version-bump archive workflow audit passed.\n")
