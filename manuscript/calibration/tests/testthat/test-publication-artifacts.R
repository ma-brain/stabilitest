test_project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
pkgload::load_all(test_project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

testthat::test_that("published training and validation manifests are compact and paired", {
  published <- file.path("..", "..", "published")
  training_path <- file.path(published, "training-manifest.dput")
  validation_path <- file.path(published, "validation-manifest.dput")
  hashes_path <- file.path(published, "output-hashes.txt")
  testthat::expect_true(file.exists(training_path))
  testthat::expect_true(file.exists(validation_path))
  testthat::expect_true(file.exists(hashes_path))

  training <- dget(training_path)
  validation <- dget(validation_path)
  required <- c(
    "artifact_kind", "status", "mode", "split", "scenario_manifest_hash",
    "scenario_count", "n_boot", "reduced_fixture", "target_replicates",
    "completed_replicates", "failed_replicates", "unsupported"
  )
  testthat::expect_true(all(required %in% names(training)))
  testthat::expect_true(all(required %in% names(validation)))
  testthat::expect_identical(training$artifact_kind, "calibration-publication-manifest")
  testthat::expect_identical(validation$artifact_kind, "calibration-publication-manifest")
  testthat::expect_identical(training$split, "training")
  testthat::expect_identical(validation$split, "validation")
  testthat::expect_identical(training$scenario_manifest_hash, validation$scenario_manifest_hash)
  testthat::expect_identical(training$status, "publication_run")
  testthat::expect_identical(validation$status, "publication_run")
  testthat::expect_false(isTRUE(training$reduced_fixture))
  testthat::expect_false(isTRUE(validation$reduced_fixture))
  testthat::expect_identical(training$n_boot, 1000L)
  testthat::expect_identical(validation$n_boot, 1000L)
  testthat::expect_identical(training$scenario_count, 46L)
  testthat::expect_gt(training$completed_replicates, 0L)
  testthat::expect_gt(validation$completed_replicates, 0L)
  testthat::expect_true(training$failed_replicates >= 0L)
  testthat::expect_true(validation$failed_replicates >= 0L)
  testthat::expect_true(is.character(training$unsupported))
  testthat::expect_true(is.character(validation$unsupported))
  testthat::expect_true(any(grepl("binomial_stress_separation", training$unsupported)))
  testthat::expect_false(isTRUE(training$validation_refit))
  testthat::expect_false(isTRUE(validation$validation_refit))

  hashes <- readLines(hashes_path, warn = FALSE)
  testthat::expect_true(length(hashes) >= 3L)
  testthat::expect_true(all(grepl("^[[:xdigit:]]{32}  manuscript/calibration/published/[^ ]+$", hashes)))
  testthat::expect_false(any(grepl("raw|checkpoint|selected", hashes, ignore.case = TRUE)))
  testthat::expect_false(any(grepl("output-hashes[.]txt", hashes, fixed = TRUE)))
  entries <- strsplit(hashes, "  ", fixed = TRUE)
  for (entry in entries) {
    path <- entry[[2L]]
    local <- file.path("..", "..", "published", basename(path))
    testthat::expect_true(file.exists(local))
    testthat::expect_identical(unname(as.character(tools::md5sum(local))), entry[[1L]])
  }
})

testthat::test_that("Task 15 publication is explicitly historical and inactive", {
  published <- file.path("..", "..", "published")
  archive_note <- readLines(file.path(published, "README.md"), warn = FALSE)
  testthat::expect_true(any(grepl("historical", archive_note, ignore.case = TRUE)))
  testthat::expect_true(any(grepl("not active", archive_note, ignore.case = TRUE)))
  testthat::expect_true(file.exists(file.path(published, "calibration-registry.csv")))

  active <- stabilitest:::load_calibration_registry()
  historical <- utils::read.csv(
    file.path(published, "calibration-registry.csv"),
    stringsAsFactors = FALSE
  )
  testthat::expect_false(any(active$calibration_unit == "two_sample"))
  testthat::expect_true(any(historical$analysis_family == "two_sample"))
})

testthat::test_that("published registry records production freeze without refit", {
  published <- file.path("..", "..", "published")
  training <- dget(file.path(published, "training-manifest.dput"))
  validation <- dget(file.path(published, "validation-manifest.dput"))
  registry <- utils::read.csv(file.path(published, "calibration-registry.csv"),
                              stringsAsFactors = FALSE)
  for (manifest in list(training, validation)) {
    testthat::expect_identical(manifest$status, "publication_run")
    testthat::expect_true(length(manifest$unsupported) >= 1L)
    testthat::expect_true(any(grepl("binomial_stress_separation|unsupported",
                                    manifest$unsupported, ignore.case = TRUE)))
    testthat::expect_true(all(!grepl("/artifacts/raw|/checkpoints", unlist(manifest),
                                     ignore.case = TRUE)))
  }
  testthat::expect_true(nrow(registry) >= 7L)
  testthat::expect_true(all(c("analysis_family", "status", "status_public", "reason") %in%
                              names(registry)))
  testthat::expect_true(all(registry$status_public %in%
                              c("validated_shared", "validated_family_specific",
                                "uncalibrated", "bands_not_applicable")))
  # Production freeze found no feasible FR/RI cutoffs under the locked SAP.
  testthat::expect_true(all(registry$status == "uncalibrated"))
  testthat::expect_true(all(registry$reason == "no_feasible_thresholds"))
})

testthat::test_that("production hash policy excludes pilot summaries", {
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "manuscript", "calibration", "tools", "freeze_and_publish.R"), env)
  published <- file.path(root, "manuscript", "calibration", "published")
  targets <- env$production_hash_targets(published)
  testthat::expect_true("calibration-registry.rds" %in% targets)
  testthat::expect_false(any(grepl("pilot", targets, ignore.case = TRUE)))
})

testthat::test_that("reduced publication fixture preserves the locked no-refit flow", {
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "manuscript", "calibration", "R", "load_calibration.R"), env)
  env$load_calibration(project_root = root, envir = env)
  sys.source(file.path(root, "manuscript", "calibration", "analyse_calibration.R"), env)
  output <- tempfile("calibration-publication-")
  on.exit(unlink(output, recursive = TRUE, force = TRUE), add = TRUE)
  fixture_pub <- file.path(root, "manuscript", "calibration", "tests", "fixtures", "published")
  result <- env$calibration_analysis_from_files(
    file.path(root, "manuscript", "calibration", "tests", "fixtures", "training-replicates.rds"),
    file.path(root, "manuscript", "calibration", "tests", "fixtures", "validation-replicates.rds"),
    dget(file.path(fixture_pub, "training-manifest.dput")),
    dget(file.path(fixture_pub, "validation-manifest.dput")),
    output = output,
    minimum_stratum_n = 100L
  )
  testthat::expect_identical(result$validation$refit, FALSE)
  # These are deterministic hashes of the reduced fixture after the
  # calibration-unit and applicable-conclusion schema changes.  The published
  # Task 15 hashes above remain immutable historical artifacts.
  testthat::expect_identical(result$candidate_hash,
                             "a989adce37ff2ae727ba010bb724625c")
  testthat::expect_identical(result$registry_hash,
                             "ca9c9f1cd0ad951774df68ff09940886")
  testthat::expect_true(file.exists(file.path(output, "calibration-registry.csv")))
  testthat::expect_true(file.exists(file.path(output, "non-significant-registry.csv")))
})
