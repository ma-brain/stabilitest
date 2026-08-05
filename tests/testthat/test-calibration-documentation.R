test_that("active documentation states the method-specific calibration policy", {
  root <- normalizePath(testthat::test_path("..", ".."))
  policy_files <- file.path(root, c(
    "README.md", "NEWS.md", "R/robustness_analysis.R",
    "R/robustness_models.R", "R/robustness_tost.R",
    "vignettes/pain-case-study.Rmd",
    "manuscript/robustness_analysis_manuscript.md",
    "manuscript/methodological_review.md",
    "manuscript/calibration/CALIBRATION_SAP.md"))
  existing <- policy_files[file.exists(policy_files)]
  text <- paste(unlist(lapply(existing, readLines, warn = FALSE)), collapse = "\n")

  expect_match(text, "numeric scores and (all )?component metrics",
               ignore.case = TRUE)
  expect_match(text, "labels are suppressed|categorical labels are suppressed",
               ignore.case = TRUE)
  expect_match(text, "welch_unpaired", ignore.case = FALSE)
  expect_match(text, "public `?robustness_analysis[(][)]`? dispatcher",
               ignore.case = TRUE)
  expect_match(text, "lm_ancova", ignore.case = FALSE)
})

test_that("stale broad calibration claims are rejected unless historical", {
  root <- normalizePath(testthat::test_path("..", ".."))
  scan_files <- file.path(root, c(
    "README.md", "NEWS.md", "R", "man", "vignettes",
    "manuscript/robustness_analysis_manuscript.md",
    "manuscript/methodological_review.md",
    "manuscript/calibration/CALIBRATION_SAP.md"))
  files <- unique(unlist(lapply(scan_files, function(path) {
    if (dir.exists(path)) {
      return(list.files(path, pattern = "\\.(R|Rd|md|Rmd)$",
                        recursive = TRUE, full.names = TRUE))
    }
    if (file.exists(path)) path else character()
  })))

  # These phrases previously asserted that the 55/70 mapping transferred to
  # every engine or that the generic two_sample family was an active key.
  stale_patterns <- c(
    "assumed transferable",
    "score bands are shared",
    "bands were calibrated by simulation",
    "calibrated interpretation bands.*(all|every|shared)",
    "calibration (family|unit).{0,40}two_sample",
    "two_sample.{0,40}calibration (family|unit)"
  )

  historical_context <- function(lines, index, path) {
    if (grepl("/published/", path, fixed = TRUE)) return(TRUE)
    lo <- max(1L, index - 40L)
    hi <- min(length(lines), index + 3L)
    grepl("Task 15|historical|inactive|archive|archived",
          paste(lines[lo:hi], collapse = " "), ignore.case = TRUE)
  }

  violations <- character()
  for (path in files) {
    lines <- readLines(path, warn = FALSE)
    for (pattern in stale_patterns) {
      hits <- grep(pattern, lines, ignore.case = TRUE)
      if (length(hits) == 0L) next
      active_hits <- hits[!vapply(hits, historical_context, logical(1),
                                  lines = lines, path = path)]
      if (length(active_hits)) {
        violations <- c(violations,
                        sprintf("%s:%s: %s", path, active_hits,
                                trimws(lines[active_hits])))
      }
    }
  }
  expect_true(length(violations) == 0L,
              info = paste(violations, collapse = "\n"))
})

test_that("the active registry has no generic two_sample calibration key", {
  root <- normalizePath(testthat::test_path("..", ".."))
  registry <- utils::read.csv(file.path(root, "inst", "extdata",
                                        "calibration-registry.csv"),
                              stringsAsFactors = FALSE)
  expect_false(any(registry$calibration_unit == "two_sample"))
  expect_true(any(registry$calibration_unit == "welch_unpaired"))
})
