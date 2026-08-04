checkpoint_path_file <- file.path("..", "..", "R", "checkpoints.R")
testthat::expect_true(file.exists(checkpoint_path_file))

checkpoint_env <- new.env(parent = globalenv())
sys.source(checkpoint_path_file, envir = checkpoint_env)

testthat::test_that("checkpoint paths are deterministic and scoped by scenario and stratum", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")

  testthat::expect_identical(
    path,
    file.path(root, "checkpoints", "two_sample_smoke", "selected.rds")
  )
  testthat::expect_false(file.exists(path))
})

testthat::test_that("checkpoint paths reject traversal and empty components", {
  root <- tempfile("calibration-artifacts-")
  invalid_components <- c("", ".", "..", "nested/scenario", "nested\\scenario")
  for (invalid in invalid_components) {
    testthat::expect_error(
      checkpoint_env$checkpoint_path(root, invalid, "selected"),
      "scenario_id|path"
    )
    testthat::expect_error(
      checkpoint_env$checkpoint_path(root, "two_sample_smoke", invalid),
      "stratum|path"
    )
  }
})

testthat::test_that("checkpoints round-trip with manifest validation", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  value <- data.frame(replicate_id = 1:3, score = c(70, 80, 90))

  checkpoint_env$write_checkpoint(value, path, manifest_hash = "manifest-v1")
  testthat::expect_true(file.exists(path))
  testthat::expect_identical(checkpoint_env$read_checkpoint(path, "manifest-v1"), value)
  testthat::expect_true(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 3L))
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 4L))
  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v2"),
    "manifest"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v2", target_n = 1L))
})

testthat::test_that("compressed RDS envelopes are rejected with a clear format error", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  value <- data.frame(replicate_id = 1:3, score = c(70, 80, 90))
  envelope <- list(
    version = 1L,
    manifest_hash = "manifest-v1",
    complete = TRUE,
    target_n = 3L,
    payload = value
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(envelope, path, version = 3, compress = TRUE)

  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v1"),
    "unsupported compressed checkpoint"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 3L))

  connection <- file(path, open = "ab")
  writeBin(charToRaw("trailing-garbage"), connection)
  close(connection)
  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v1"),
    "unsupported compressed checkpoint"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 3L))
})

testthat::test_that("truncated checkpoints are rejected and are not resumable", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  checkpoint_env$write_checkpoint(data.frame(replicate_id = 1:3), path, "manifest-v1")

  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  testthat::expect_gt(length(bytes), 10L)
  writeBin(bytes[seq_len(max(1L, length(bytes) %/% 2L))], path)

  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v1"),
    "invalid|truncated|checkpoint"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 1L))
})

testthat::test_that("checkpoints with trailing bytes are rejected and are not resumable", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  checkpoint_env$write_checkpoint(data.frame(replicate_id = 1:3), path, "manifest-v1")

  connection <- file(path, open = "ab")
  writeBin(charToRaw("trailing-garbage"), connection)
  close(connection)

  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v1"),
    "invalid|trailing|checkpoint"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 1L))
})

testthat::test_that("even zero trailing bytes invalidate a checkpoint", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  checkpoint_env$write_checkpoint(data.frame(replicate_id = 1:3), path, "manifest-v1")

  connection <- file(path, open = "ab")
  writeBin(as.raw(c(0L, 0L)), connection)
  close(connection)

  testthat::expect_error(
    checkpoint_env$read_checkpoint(path, "manifest-v1"),
    "trailing|checkpoint"
  )
  testthat::expect_false(checkpoint_env$checkpoint_complete(path, "manifest-v1", target_n = 1L))
})

testthat::test_that("malformed target_n metadata is rejected", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  malformed <- list(target_n = "three", replicates = data.frame(replicate_id = 1:3))

  testthat::expect_error(
    checkpoint_env$write_checkpoint(malformed, path, "manifest-v1"),
    "target_n"
  )

  inconsistent <- list(target_n = 5L, replicates = data.frame(replicate_id = 1:3))
  testthat::expect_error(
    checkpoint_env$write_checkpoint(inconsistent, path, "manifest-v1"),
    "target_n|replicate"
  )
})

testthat::test_that("failed writes clean temporary files", {
  root <- tempfile("calibration-artifacts-")
  path <- checkpoint_env$checkpoint_path(root, "two_sample_smoke", "selected")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  testthat::expect_error(
    checkpoint_env$write_checkpoint(data.frame(replicate_id = 1:3), path, "manifest-v1"),
    "rename|checkpoint|directory"
  )
  testthat::expect_length(
    list.files(dirname(path), pattern = "[.]tmp", all.files = TRUE),
    0L
  )
})
