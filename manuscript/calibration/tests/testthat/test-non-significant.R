# Exploratory non-significant-result policy tests.

test_project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
pkgload::load_all(test_project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

non_sig_env <- new.env(parent = globalenv())
sys.source(file.path("..", "..", "R", "non_significant.R"), envir = non_sig_env)
sys.source(file.path("..", "..", "analyse_calibration.R"), envir = non_sig_env)

non_sig_fixture <- function() {
  data.frame(
    analysis_engine = rep(c("lm", "cox"), each = 8L),
    calibration_family = rep(c("linear_model", "survival"), each = 8L),
    calibration_unit = rep(c("lm_ancova", "cox_ph"), each = 8L),
    endpoint = rep(c("coefficient", "hazard_ratio"), each = 8L),
    truth_class = rep(c("null", "borderline", "clear", "null"), each = 4L),
    target_conclusion = rep("non_significant", 16L),
    analysis_conclusion = rep("non_significant", 16L),
    overall_score = c(82, 85, 80, 84, 35, 42, 38, 45,
                      20, 28, 25, 30, 75, 78, 80, 76),
    jackknife_stability = c(80, 82, 79, 81, 32, 40, 36, 43,
                            18, 25, 22, 28, 70, 75, 78, 73),
    fragility_component = c(84, 87, 81, 85, 38, 44, 40, 48,
                            22, 30, 26, 31, 79, 81, 83, 78),
    n = rep(c(40L, 80L, 160L, 320L), 4L),
    design_layer = rep(c("core", "core", "validation", "stress"), 4L),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("non-significant policy exposes separate evidence components", {
  result <- non_sig_env$analyse_non_significant(non_sig_fixture())
  testthat::expect_true(all(c("status", "cutoffs", "discrimination",
                              "ordering", "cross_family_consistency",
                              "summaries") %in% names(result)))
  testthat::expect_identical(result$status, "bands_not_applicable")
  testthat::expect_true(all(is.na(result$cutoffs)))
  testthat::expect_true(is.list(result$discrimination))
  testthat::expect_true(is.list(result$ordering))
  testthat::expect_true(is.list(result$cross_family_consistency))
  testthat::expect_true(is.data.frame(result$summaries$distributions))
  testthat::expect_true(is.data.frame(result$summaries$sample_size))
  testthat::expect_true(is.data.frame(result$summaries$stress))
  testthat::expect_true(is.data.frame(result$summaries$sample_size_auc))
  testthat::expect_true(is.data.frame(result$summaries$stress_auc))
})

testthat::test_that("existing score labels cannot approve non-significant bands", {
  data <- non_sig_fixture()
  data$assigned_label <- "robust"
  result <- non_sig_env$analyse_non_significant(data)
  testthat::expect_identical(result$status, "bands_not_applicable")
  testthat::expect_true(all(is.na(result$cutoffs)))
  testthat::expect_false(isTRUE(result$approved))
})

testthat::test_that("missing or weak discrimination keeps bands inapplicable", {
  data <- non_sig_fixture()
  data$overall_score <- 50
  result <- non_sig_env$analyse_non_significant(data)
  testthat::expect_false(isTRUE(result$discrimination$pass))
  testthat::expect_false(isTRUE(result$ordering$pass))
  testthat::expect_identical(result$status, "bands_not_applicable")
  testthat::expect_true(all(is.na(result$cutoffs)))
})

testthat::test_that("non-significant rows are identified from observed conclusions", {
  data <- non_sig_fixture()
  data$analysis_conclusion[1L] <- "significant"
  result <- non_sig_env$analyse_non_significant(data)
  testthat::expect_true(result$diagnostics$n_non_significant < nrow(data))
  testthat::expect_true(result$diagnostics$n_false_negatives >= 1L)
})

testthat::test_that("compact p-value artifacts receive an explicit conclusion", {
  data <- non_sig_fixture()
  data$analysis_conclusion <- NULL
  data$target_conclusion <- NULL
  data$original_p <- rep(c(0.4, 0.3, 0.01, 0.2), 4L)
  result <- non_sig_env$analyse_non_significant(data)
  testthat::expect_identical(result$diagnostics$n_non_significant, 12L)
})

testthat::test_that("invalid input fails explicitly", {
  testthat::expect_error(non_sig_env$analyse_non_significant(data.frame()),
                         "required|calibration_unit")
  bad <- non_sig_fixture()
  bad$overall_score[1L] <- Inf
  testthat::expect_error(non_sig_env$analyse_non_significant(bad), "finite|overall_score")
})

testthat::test_that("failed rows with missing metrics are ignored safely", {
  data <- non_sig_fixture()
  data$status <- "completed"
  failed <- data[1L, , drop = FALSE]
  failed$status <- "failed"
  failed$overall_score <- NA_real_
  failed$analysis_conclusion <- NA_character_
  failed$target_conclusion <- NA_character_
  combined <- rbind(data, failed)
  result <- non_sig_env$analyse_non_significant(combined)
  testthat::expect_identical(result$diagnostics$n_rows, nrow(data))
  testthat::expect_identical(result$diagnostics$n_non_significant, nrow(data))
})

testthat::test_that("analysis entrypoint attaches held-out exploratory policy", {
  base_result <- list(registry = data.frame(calibration_unit = "lm_ancova"))
  attached <- non_sig_env$attach_non_significant_analysis(base_result, non_sig_fixture())
  testthat::expect_identical(attached$non_significant$split, "validation")
  testthat::expect_identical(attached$non_significant$status, "bands_not_applicable")
  testthat::expect_true(all(is.na(attached$non_significant$cutoffs)))
  testthat::expect_identical(attached$non_significant_registry$status[[1L]],
                             "bands_not_applicable")
  testthat::expect_true(all(is.na(attached$non_significant_registry$lower_cutoff)))
  testthat::expect_true(all(c("analysis_engine", "calibration_family",
                              "calibration_unit", "endpoint") %in%
                            names(attached$non_significant_registry)))
})
