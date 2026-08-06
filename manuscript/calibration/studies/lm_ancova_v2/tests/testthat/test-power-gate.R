.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

test_that("v2 power gate script selects core+validation at frozen clear power", {
  study <- .study_root()
  stamp <- file.path(study, "artifacts", "summaries", "SCORE_PILOT_GATE.json")
  testthat::skip_if_not(file.exists(stamp), "SCORE_PILOT_GATE.json missing")

  out <- tempfile(fileext = ".csv")
  log <- tempfile(fileext = ".log")
  # Low-draw smoke: exercise wiring only. Production gate uses 10000 draws.
  status <- system2(
    "Rscript",
    c(
      file.path(study, "tools", "verify_power_gate.R"),
      "--draws", "200",
      "--seed", "20260806",
      "--output", out
    ),
    stdout = log,
    stderr = log
  )
  # Exit may be 1 under low draws; CSV + meta must still be written.
  testthat::expect_true(status %in% c(0L, 1L))
  testthat::expect_true(file.exists(out))
  table <- utils::read.csv(out, stringsAsFactors = FALSE)
  testthat::expect_identical(nrow(table), 45L)
  testthat::expect_true(all(table$design_layer %in% c("core", "validation")))
  testthat::expect_false(any(grepl("stress", table$scenario_id)))
  testthat::expect_true(all(table$truth_class %in% c("null", "borderline", "clear")))
  clear <- table[table$truth_class == "clear", , drop = FALSE]
  testthat::expect_true(all(abs(clear$target - 0.90) < 1e-12))
  testthat::expect_true(all(!table$used_robustness_score))
  testthat::expect_true(all(is.finite(table$achieved)))
  testthat::expect_true(file.exists(sub("\\.csv$", ".rds", out)))
})
