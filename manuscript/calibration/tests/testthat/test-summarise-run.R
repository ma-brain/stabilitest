testthat::test_that("run summaries preserve method-specific calibration identity", {
  root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
  script <- file.path(root, "manuscript", "calibration", "tools", "summarise_run.R")
  training <- tempfile(fileext = ".rds")
  validation <- tempfile(fileext = ".rds")
  output <- tempfile(fileext = ".csv")

  rows <- data.frame(
    scenario_id = rep("lm_fixture", 2L),
    analysis_engine = rep("lm", 2L),
    calibration_family = rep("linear_model", 2L),
    calibration_unit = rep("lm_ancova", 2L),
    endpoint = rep("coefficient", 2L),
    design_layer = rep("core", 2L),
    truth_class = rep("clear", 2L),
    screening_conclusion = rep("significant", 2L),
    status = rep("completed", 2L),
    overall_score = c(42, 82),
    stringsAsFactors = FALSE
  )
  selected <- structure(seq_len(nrow(rows)), status = "completed")
  screened <- rows[1L, , drop = FALSE]
  saveRDS(list(
    analyse = list(rows),
    screen = list(list(selected = selected, screened = screened, denominator = 2L))
  ), training)

  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(script, "lm", training, validation, output),
    stdout = FALSE, stderr = FALSE
  )
  testthat::expect_identical(status, 0L)
  summary <- utils::read.csv(output, stringsAsFactors = FALSE)
  testthat::expect_true(all(c("analysis_engine", "calibration_family",
                              "calibration_unit", "endpoint") %in% names(summary)))
  testthat::expect_false("analysis_family" %in% names(summary))
  testthat::expect_identical(summary$calibration_unit, "lm_ancova")
})
