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
