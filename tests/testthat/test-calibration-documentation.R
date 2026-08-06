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

test_that("the calibration documentation audit passes", {
  root <- normalizePath(testthat::test_path("..", ".."))
  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  expect_true(file.exists(audit), info = "audit script is missing")
  status <- system2(
    file.path(R.home("bin"), "Rscript"), audit,
    stdout = TRUE, stderr = TRUE
  )
  attr(status, "status") <- attr(status, "status")
  if (is.null(attr(status, "status"))) attr(status, "status") <- 0L
  expect_equal(attr(status, "status"), 0L,
               info = paste(status, collapse = "\n"))
})

test_that("the proportions Phase 1 SAP freezes the key constants", {
  root <- normalizePath(testthat::test_path("..", ".."))
  sap <- file.path(root, "manuscript", "calibration", "studies",
                   "binary_proportion", "CALIBRATION_SAP.md")
  expect_true(file.exists(sap), info = "proportions SAP is missing")
  text <- paste(readLines(sap, warn = FALSE), collapse = "\n")
  expect_true(grepl("fragility = 0.5.*bootstrap = 0.5.*jackknife = 0", text))
  expect_true(grepl("clear exact power 0.95", text))
  expect_true(grepl("projected RI >= 0.72", text))
  expect_true(grepl("20260809L", text))
  expect_true(grepl("appear in no[[:space:]]+calibration ledger", text))
})
