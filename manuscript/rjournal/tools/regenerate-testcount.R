# ==============================================================================
# R Journal evidence artifact: test-suite size for the Testing and QA section.
#
# The article may not transcribe a test count by hand, so it is recorded here
# from an actual devtools::test() run and committed as an artifact.
# ==============================================================================

OUTPUT_DIR <- file.path("manuscript", "rjournal", "artifacts", "testing")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

res <- as.data.frame(devtools::test())

counts <- list(
  pass = sum(res$passed),
  fail = sum(res$failed),
  warn = sum(res$warning),
  skip = sum(res$skipped),
  files = length(unique(res$file))
)

saveRDS(counts, file.path(OUTPUT_DIR, "test-counts.rds"))
writeLines(c(
  sprintf("generated_at: %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  sprintf("package_version: %s", as.character(utils::packageVersion("stabilitest"))),
  sprintf("r_version: %s", R.version.string),
  sprintf("pass: %d", counts$pass),
  sprintf("fail: %d", counts$fail),
  sprintf("warn: %d", counts$warn),
  sprintf("skip: %d", counts$skip),
  sprintf("files: %d", counts$files)
), file.path(OUTPUT_DIR, "manifest.txt"))

str(counts)
message("Test-count artifact written to ", OUTPUT_DIR)
