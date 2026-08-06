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
