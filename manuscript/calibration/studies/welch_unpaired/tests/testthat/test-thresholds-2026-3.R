.welch3_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch3_code <- function(envir = parent.frame()) {
  root <- .welch3_study_root()
  sys.source(file.path(root, "R", "thresholds.R"), envir = envir)
  sys.source(file.path(root, "R", "thresholds_2026_3.R"), envir = envir)
  sys.source(file.path(root, "R", "validation.R"), envir = envir)
}

# Synthetic significant rows: three archetypes, three clusters per truth class.
# `p_clear_range` sets the log10 range of clear-row p-values.
.welch3_rows <- function(seed = 1, per_cell = 150,
                         null_mean = 40, clear_mean = 90,
                         p_clear_range = c(2.5, 7)) {
  set.seed(seed)
  cells <- expand.grid(
    archetype = c("W01", "W02", "W03"),
    truth = c("null", "borderline", "clear"),
    stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(cells)), function(i) {
    truth <- cells$truth[[i]]
    mean_score <- c(null = null_mean, borderline = 60, clear = clear_mean)[[truth]]
    p <- switch(
      truth,
      null = stats::runif(per_cell, 0, 0.05),
      borderline = 10^(-stats::runif(per_cell, 1.5, 3)),
      clear = 10^(-stats::runif(per_cell, p_clear_range[[1L]], p_clear_range[[2L]]))
    )
    data.frame(
      scenario_id = paste0("T3-", cells$archetype[[i]], "-", truth),
      archetype_id = cells$archetype[[i]],
      design_layer = "training",
      truth = truth,
      original_p = p,
      original_significant = TRUE,
      status = "completed",
      overall_robustness = pmin(100, pmax(0, round(
        stats::rnorm(per_cell, mean_score, 8)
      ))),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

testthat::test_that("2026-3 scenarios derive from 2026-2 with fixed IDs and seeds", {
  root <- .welch3_study_root()
  source(file.path(root, "config", "scenarios.R"), local = TRUE)
  source(file.path(root, "config", "scenarios_2026_3.R"), local = TRUE)
  old <- welch_scenarios()
  new <- welch_scenarios_2026_3()

  testthat::expect_equal(nrow(new), nrow(old))
  testthat::expect_equal(new$seed, old$seed + 100000L)
  testthat::expect_equal(min(new$seed[new$design_layer == "training"]), 202710001L)
  testthat::expect_equal(min(new$seed[new$design_layer == "validation"]), 202720001L)
  testthat::expect_equal(min(new$seed[new$design_layer == "stress"]), 202730001L)
  testthat::expect_true(all(grepl("^(WTR3|WVA3|WST3)-", new$scenario_id)))
  testthat::expect_length(intersect(new$scenario_id, old$scenario_id), 0L)
  testthat::expect_length(intersect(new$seed, old$seed), 0L)
  testthat::expect_false(anyDuplicated(new$scenario_id) > 0L)
  testthat::expect_false(anyDuplicated(new$seed) > 0L)
  keep <- setdiff(names(old), c("scenario_id", "seed"))
  testthat::expect_equal(new[keep], old[keep])
})

testthat::test_that("2026-3 gate constants match the SAP", {
  .load_welch3_code()
  g <- WELCH3_GATE_CONSTANTS
  testthat::expect_equal(g$false_reassurance_point_max, 0.10)
  testthat::expect_equal(g$false_reassurance_upper_max, 0.15)
  testthat::expect_equal(g$clear_identification_point_min, 0.60)
  testthat::expect_equal(g$clear_identification_lower_min, 0.50)
  testthat::expect_equal(g$archetype_false_reassurance_max, 0.25)
  testthat::expect_equal(g$neighbourhood, 2L)
  testthat::expect_equal(g$cutoff_range, 50L:90L)
  testthat::expect_equal(g$cluster_bootstrap_seed, 202740001L)
  testthat::expect_equal(g$baseline_bootstrap_seed, 202760001L)
  testthat::expect_equal(g$baseline_min_gain, 0.02)
})

testthat::test_that("Stable label includes the cutoff (score >= U)", {
  .load_welch3_code()
  data <- .welch3_rows()
  m <- welch3_candidate_metrics(data, 65L, cluster_B = 0L)
  null <- data$truth == "null"
  testthat::expect_equal(m$false_reassurance,
                         mean(data$overall_robustness[null] >= 65))
  at_cutoff <- data
  at_cutoff$overall_robustness <- 65
  testthat::expect_equal(
    welch3_candidate_metrics(at_cutoff, 65L, cluster_B = 0L)$band_occupancy, 1
  )
  testthat::expect_equal(
    welch3_candidate_metrics(at_cutoff, 66L, cluster_B = 0L)$band_occupancy, 0
  )
})

testthat::test_that("archetype guard fails closed and flags a bad archetype", {
  .load_welch3_code()
  data <- .welch3_rows()
  good <- welch3_candidate_metrics(data, 70L, cluster_B = 0L)
  testthat::expect_true(welch3_candidate_gate(good)$feasible)

  no_archetype <- good
  no_archetype["archetype_false_reassurance"] <- list(NULL)
  gate <- welch3_candidate_gate(no_archetype)
  testthat::expect_false(gate$feasible)
  testthat::expect_true("archetype_false_reassurance" %in% gate$reasons)

  skewed <- good
  skewed$archetype_false_reassurance[["W01"]] <- 0.30
  testthat::expect_true(
    "archetype_false_reassurance" %in% welch3_candidate_gate(skewed)$reasons
  )
})

testthat::test_that("neighbourhood rule needs k feasible neighbours each side", {
  .load_welch3_code()
  ok <- .welch3_neighbourhood_ok(c(F, T, T, T, T, T, F, T, T, T), 2L)
  testthat::expect_equal(ok, c(F, F, F, T, F, F, F, F, F, F))
  testthat::expect_false(any(.welch3_neighbourhood_ok(rep(TRUE, 4L), 2L)))
})

testthat::test_that("candidate fit selects a cutoff and a comparator", {
  .load_welch3_code()
  fit <- fit_welch3_candidate(.welch3_rows(), cluster_B = 50L)
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$mapping_type, "single_band")
  testthat::expect_true(fit$cutoffs[[1L]] %in% 52:88)
  testthat::expect_true(fit$grid$eligible[fit$grid$label == fit$cutoffs[[1L]]])
  testthat::expect_identical(fit$p_comparator$status, "comparator")
  testthat::expect_true(fit$p_comparator$q %in% WELCH3_GATE_CONSTANTS$p_grid)
})

testthat::test_that("overlapping classes give no_feasible_thresholds", {
  .load_welch3_code()
  data <- .welch3_rows(null_mean = 80, clear_mean = 82)
  fit <- fit_welch3_candidate(data, cluster_B = 50L)
  testthat::expect_identical(fit$status, "no_feasible_thresholds")
  testthat::expect_identical(fit$mapping_type, "none")
  testthat::expect_true(all(is.na(fit$cutoffs)))
  testthat::expect_null(fit$p_comparator)
})

testthat::test_that("validation passes when gates hold and the score beats p", {
  .load_welch3_code()
  training <- .welch3_rows(seed = 1)
  frozen <- freeze_welch_candidate(
    fit_welch3_candidate(training, cluster_B = 50L), context = list(test = TRUE)
  )
  testthat::expect_identical(frozen$status, "candidate")
  out <- validate_frozen_welch3_candidate(
    frozen, .welch3_rows(seed = 2), cluster_B = 50L
  )
  testthat::expect_true(out$held_out_opened)
  testthat::expect_false(out$validation_refit)
  testthat::expect_identical(out$status, "validated_method_specific")
  testthat::expect_true(out$baseline$beats_baseline)
})

testthat::test_that("validation fails closed without a usable p comparator", {
  .load_welch3_code()
  # Clear p-values too weak for any comparator to hit RI >= 0.60.
  training <- .welch3_rows(seed = 1, p_clear_range = c(1.5, 2.5))
  fit <- fit_welch3_candidate(training, cluster_B = 50L)
  testthat::expect_identical(fit$status, "candidate")
  testthat::expect_identical(fit$p_comparator$status, "no_eligible_comparator")
  frozen <- freeze_welch_candidate(fit, context = list(test = TRUE))
  out <- validate_frozen_welch3_candidate(
    frozen, .welch3_rows(seed = 2, p_clear_range = c(1.5, 2.5)), cluster_B = 50L
  )
  testthat::expect_identical(out$status, "uncalibrated")
  testthat::expect_match(out$reason, "p_comparator_unavailable")
  testthat::expect_false(out$held_out_opened)
})

testthat::test_that("validation does not open held-out data for a failed freeze", {
  .load_welch3_code()
  fit <- fit_welch3_candidate(
    .welch3_rows(null_mean = 80, clear_mean = 82), cluster_B = 50L
  )
  frozen <- freeze_welch_candidate(fit, context = list(test = TRUE))
  out <- validate_frozen_welch3_candidate(frozen, .welch3_rows(seed = 2))
  testthat::expect_identical(out$status, "uncalibrated")
  testthat::expect_false(out$held_out_opened)
})

testthat::test_that("tampered frozen candidate is rejected", {
  .load_welch3_code()
  frozen <- freeze_welch_candidate(
    fit_welch3_candidate(.welch3_rows(), cluster_B = 50L),
    context = list(test = TRUE)
  )
  frozen$hash_payload$candidate$cutoffs[[1L]] <-
    frozen$hash_payload$candidate$cutoffs[[1L]] + 1L
  testthat::expect_error(
    validate_frozen_welch3_candidate(frozen, .welch3_rows(seed = 2)),
    "hash mismatch"
  )
})
