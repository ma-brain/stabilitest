testthat::test_that("publication tools are source-safe and preserve audit rows", {
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  assemble_file <- file.path(root, "manuscript", "calibration", "tools", "assemble_replicates.R")
  freeze_file <- file.path(root, "manuscript", "calibration", "tools", "freeze_and_publish.R")
  assembly_env <- new.env(parent = globalenv())
  freeze_env <- new.env(parent = globalenv())
  sys.source(assemble_file, envir = assembly_env)
  sys.source(freeze_file, envir = freeze_env)
  testthat::expect_true(is.function(assembly_env$collect_checkpoint_split))
  testthat::expect_true(is.function(assembly_env$assemble_replicates))
  testthat::expect_true(is.function(freeze_env$run_assembly_subprocess))
  testthat::expect_true(is.function(freeze_env$build_publication_manifest))

  raw <- tempfile("publication-checkpoints-")
  dir.create(raw, recursive = TRUE)
  scenarios <- data.frame(
    scenario_id = c("complete", "missing"), design_layer = c("core", "core"),
    stringsAsFactors = FALSE
  )
  rows <- data.frame(
    scenario_id = c("complete", "complete", "complete"), replicate_id = 1:3,
    status = c("completed", "failed", "excluded"),
    original_p = c(.01, NA, NA), effective_p = c(.02, NA, NA),
    stringsAsFactors = FALSE
  )
  checkpoint <- file.path(raw, "training", "complete", "full.rds")
  dir.create(dirname(checkpoint), recursive = TRUE)
  saveRDS(list(payload = list(replicates = rows)), checkpoint)
  # Validation uses its own sparse fixture; the missing scenario is derived as
  # unsupported from the frozen scenario registry.
  validation_checkpoint <- file.path(raw, "validation", "complete", "full.rds")
  dir.create(dirname(validation_checkpoint), recursive = TRUE)
  saveRDS(list(payload = list(replicates = rows[1, , drop = FALSE])), validation_checkpoint)

  result <- assembly_env$collect_checkpoint_split(
    "training", checkpoint_root = raw, scenarios = scenarios, project_root = root
  )
  testthat::expect_equal(nrow(result$fitting), 1L)
  testthat::expect_equal(nrow(result$audit), 3L)
  testthat::expect_true(all(result$audit$status %in% c("failed", "excluded", "unsupported")))
  testthat::expect_true(any(result$audit$status == "unsupported" & result$audit$scenario_id == "missing"))
  counts <- table(c(result$fitting$status, result$audit$status))
  testthat::expect_identical(unname(if ("completed" %in% names(counts)) counts[["completed"]] else 0L), 1L)
  testthat::expect_identical(unname(if ("failed" %in% names(counts)) counts[["failed"]] else 0L), 1L)
  testthat::expect_identical(unname(if ("excluded" %in% names(counts)) counts[["excluded"]] else 0L), 1L)

  manifest <- freeze_env$build_publication_manifest(
    "training", FALSE, result$fitting, result$audit, scenarios
  )
  testthat::expect_identical(manifest$attempted_replicates, 3L)
  testthat::expect_identical(manifest$completed_replicates, 1L)
  testthat::expect_identical(manifest$failed_replicates, 1L)
  testthat::expect_identical(manifest$excluded_replicates, 1L)
  testthat::expect_true(any(grepl("missing", manifest$unsupported)))

  complete_manifest <- freeze_env$build_publication_manifest(
    "training", FALSE, result$fitting, data.frame(), scenarios[1, , drop = FALSE]
  )
  testthat::expect_length(complete_manifest$unsupported, 0L)

  summary_path <- tempfile(fileext = ".csv")
  freeze_env$write_failure_summary(result$audit, result$audit, summary_path)
  summary <- utils::read.csv(summary_path, stringsAsFactors = FALSE)
  testthat::expect_true(all(summary$split %in% c("training", "validation")))
})

testthat::test_that("assembly subprocess failures stop publication", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "tools", "freeze_and_publish.R"), envir = env)
  testthat::expect_error(env$publication_assembly_status(1L), "assembly failed")
  script <- tempfile(fileext = ".R")
  writeLines("quit(status = 17, save = 'no')", script)
  testthat::expect_error(
    env$run_assembly_subprocess(script, tempfile(fileext = ".rds"), tempfile(fileext = ".rds")),
    "assembly failed"
  )
})

testthat::test_that("production ledger hashes registry RDS and excludes pilot CSVs", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("..", "..", "tools", "freeze_and_publish.R"), envir = env)
  published <- tempfile("published-")
  dir.create(published)
  files <- c("training-manifest.dput", "validation-manifest.dput",
             "calibration-registry.csv", "calibration-registry.rds",
             "non-significant-registry.csv", "failure-summary.csv",
             "pilot-runtime-summary.csv", "pilot-failure-summary.csv")
  for (file in files) writeLines(file, file.path(published, file))
  env$write_output_hashes(published)
  hashes <- readLines(file.path(published, "output-hashes.txt"), warn = FALSE)
  testthat::expect_true(any(grepl("calibration-registry[.]rds", hashes)))
  testthat::expect_true(any(grepl("failure-summary[.]csv", hashes)))
  testthat::expect_false(any(grepl("pilot", hashes, ignore.case = TRUE)))
})
