# ==============================================================================
# R Journal evidence artifact: test-suite size for the Testing and QA section.
#
# The article may not transcribe a test count by hand, so it is recorded here
# from an actual devtools::test() run and committed as an artifact.
# ==============================================================================

OUTPUT_DIR <- file.path("manuscript", "rjournal", "artifacts", "testing")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

t_start <- Sys.time()
res <- as.data.frame(devtools::test())
t_end <- Sys.time()

counts <- list(
  pass = sum(res$passed),
  fail = sum(res$failed),
  warn = sum(res$warning),
  skip = sum(res$skipped),
  files = length(unique(res$file))
)

saveRDS(counts, file.path(OUTPUT_DIR, "test-counts.rds"))

.script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE),
                             value = TRUE)[[1L]]),
  mustWork = TRUE
)
.project_root <- normalizePath(file.path(dirname(.script_path), "..", "..", ".."),
                               mustWork = TRUE)
.verdict_path <- file.path(
  .project_root, "manuscript", "calibration", "studies", "welch_unpaired",
  "published", "VERDICT.json"
)
.registry <- stabilitest:::load_calibration_registry()
.welch <- .registry[.registry$calibration_unit == "welch_unpaired", , drop = FALSE]
manifest <- list(
  artifact_role = "package_test_summary",
  generated_at = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
  runtime_seconds = as.numeric(difftime(t_end, t_start, units = "secs")),
  package_version = as.character(utils::packageVersion("stabilitest")),
  git_commit = system2("git", c("-C", .project_root, "rev-parse", "HEAD"),
                       stdout = TRUE),
  r_version = R.version.string,
  script_path = file.path("manuscript", "rjournal", "tools",
                          basename(.script_path)),
  script_sha256 = digest::digest(file = .script_path, algo = "sha256"),
  seed = "test-suite-managed",
  source_study_verdict_hash = digest::digest(file = .verdict_path,
                                             algo = "sha256"),
  welch_calibration_status = .welch$status,
  welch_label_emitted = identical(.welch$status, "validated_method_specific"),
  pass = counts$pass,
  fail = counts$fail,
  warn = counts$warn,
  skip = counts$skip,
  files = counts$files
)
jsonlite::write_json(
  manifest, file.path(OUTPUT_DIR, "manifest.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15
)

str(counts)
message("Test-count artifact written to ", OUTPUT_DIR)
