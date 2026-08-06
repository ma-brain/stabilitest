test_that("the active registry has no generic two_sample calibration key", {
  root <- normalizePath(testthat::test_path("..", ".."))
  source_registry <- file.path(root, "inst", "extdata",
                               "calibration-registry.csv")
  registry_path <- if (file.exists(source_registry)) {
    source_registry
  } else {
    system.file("extdata", "calibration-registry.csv", package = "stabilitest")
  }
  expect_true(nzchar(registry_path) && file.exists(registry_path),
              info = "installed calibration registry is missing")
  registry <- utils::read.csv(registry_path,
                              stringsAsFactors = FALSE)
  expect_false(any(registry$calibration_unit == "two_sample"))
  expect_true(any(registry$calibration_unit == "welch_unpaired"))
})

test_that("Gate A ANCOVA documentation audit passes in the source tree", {
  root <- normalizePath(testthat::test_path("..", ".."))
  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  skip_if_not(file.exists(audit), "documentation audit tool is not shipped")
  sap <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova", "CALIBRATION_SAP.md"
  )
  skip_if_not(file.exists(sap), "ANCOVA SAP is not shipped in the package tarball")
  status <- system2("Rscript", audit, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(status, "status"), NULL)
  expect_true(any(grepl("audit passed", status, ignore.case = TRUE)))
})

test_that("Gate B uncalibrated ANCOVA decision is published and documented", {
  root <- normalizePath(testthat::test_path("..", ".."))
  published <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova", "published"
  )
  skip_if_not(dir.exists(published), "ANCOVA published/ directory missing")

  required <- c(
    "candidate.rds",
    "candidate-diagnostics.json",
    "training-occupancy.csv",
    "training-failures.csv",
    "power-verification.csv",
    "training-manifest.rds",
    "registry.csv",
    "registry.rds",
    "hash_ledger.rds"
  )
  for (name in required) {
    expect_true(file.exists(file.path(published, name)), info = name)
  }

  candidate <- readRDS(file.path(published, "candidate.rds"))
  expect_identical(candidate$status, "uncalibrated")
  expect_identical(candidate$reason, "no_feasible_thresholds")
  expect_true(all(is.na(candidate$cutoffs)))
  expect_false(isTRUE(candidate$held_out_opened))

  registry <- utils::read.csv(file.path(published, "registry.csv"),
                              stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_true(any(registry$calibration_unit == "lm_ancova"))
  ancova <- registry[registry$calibration_unit == "lm_ancova", , drop = FALSE]
  expect_identical(ancova$status, "uncalibrated")
  expect_true(all(is.na(ancova$cutoff_fragile)))
  expect_true(all(is.na(ancova$cutoff_robust)))
  expect_match(paste(ancova$supported_conditions, collapse = " "),
               "no_feasible_thresholds", fixed = TRUE)

  policy_files <- file.path(root, c(
    "README.md", "NEWS.md",
    "manuscript/calibration/README.md",
    "manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md"
  ))
  text <- paste(unlist(lapply(policy_files, readLines, warn = FALSE)),
                collapse = "\n")
  expect_match(text, "no_feasible_thresholds", fixed = TRUE)
  expect_match(text, "held-out not opened|held.out not opened|validation.*not opened",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "Gate B.{0,80}(fail-closed|uncalibrated)|fail-closed.{0,80}Gate B",
               perl = TRUE, ignore.case = TRUE)
})
