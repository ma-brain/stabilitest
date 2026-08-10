#!/usr/bin/env Rscript

# Reject internal audit terminology and unsupported scientific claims from the
# reader-facing R Journal article and submission materials.

.script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) != 1L) stop("cannot locate article claim audit", call. = FALSE)
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

script_path <- .script_path()
rjournal_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
files <- file.path(rjournal_root, c(
  "stabilitest.Rmd",
  "motivation-letter/motivation-letter.md",
  "related-packages.md"
))
missing <- files[!file.exists(files)]
if (length(missing)) {
  stop("article claim audit inputs are missing: ",
       paste(basename(missing), collapse = ", "), call. = FALSE)
}

banned <- c(
  "Task 15",
  "Task 4",
  "Gate B",
  "v3 Track E",
  "genuine effect rather than chance",
  "information-theoretic",
  "near-optimal for monotone",
  "factor of two to five",
  "computed with package defaults",
  "about 3,400 tests at n = 55"
)

violations <- character()
for (path in files) {
  lines <- readLines(path, warn = FALSE)
  for (phrase in banned) {
    hits <- grep(phrase, lines, fixed = TRUE)
    if (!length(hits)) next
    relative <- substring(path, nchar(rjournal_root) + 2L)
    violations <- c(
      violations,
      sprintf("%s:%d: banned phrase '%s'", relative, hits, phrase)
    )
  }
}

if (length(violations)) {
  cat("R Journal article claim audit failed:\n")
  cat(paste0("- ", violations, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}

cat("R Journal article claim audit passed for ", length(files),
    " reader-facing files.\n", sep = "")
