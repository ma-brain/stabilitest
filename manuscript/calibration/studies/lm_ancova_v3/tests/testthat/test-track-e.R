.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_lm_ancova_v3_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_lm_ancova_v3_study(project_root = .project_root(), envir = env)
  env
}

.fixture_rows <- function(n_clean = 40L, n_viol = 40L, score_shift = 20,
                          p_shift = 0, violation_type = "heteroscedastic",
                          n_cell = 80L) {
  clean_id <- sprintf("lm_ancova_v3_clean_n%d_r2_40_clear", n_cell)
  viol_id <- sprintf(
    "lm_ancova_v3_viol_n%d_r2_40_clear_%s", n_cell, violation_type
  )
  # Higher score / smaller p among clean when score_shift > 0 and p_shift == 0.
  clean_score <- stats::runif(n_clean, 60, 90)
  viol_score <- clean_score[seq_len(n_viol)] - score_shift
  clean_p <- 10^(-(stats::runif(n_clean, 2, 5)))
  viol_p <- pmin(1, clean_p[seq_len(n_viol)] * 10^(p_shift))

  rbind(
    data.frame(
      scenario_id = clean_id,
      replicate_id = seq_len(n_clean),
      truth_class = "clear",
      analysis_conclusion = "significant",
      status = "completed",
      design_layer = "core",
      overall_score = clean_score,
      original_p = clean_p,
      violation_type = NA_character_,
      matched_clean_id = NA_character_,
      sample_size = n_cell,
      stringsAsFactors = FALSE
    ),
    data.frame(
      scenario_id = viol_id,
      replicate_id = seq_len(n_viol),
      truth_class = "clear",
      analysis_conclusion = "significant",
      status = "completed",
      design_layer = "core",
      overall_score = viol_score,
      original_p = viol_p,
      violation_type = violation_type,
      matched_clean_id = clean_id,
      sample_size = n_cell,
      stringsAsFactors = FALSE
    )
  )
}

test_that("Track E AUC uses pre-specified score orientation clean > violated", {
  env <- .load_lm_ancova_v3_study_env()
  set.seed(1L)
  # Perfect separation: all clean scores > all violated scores.
  data <- data.frame(
    overall_score = c(80, 70, 20, 10),
    is_clean = c(TRUE, TRUE, FALSE, FALSE)
  )
  auc <- env$lm_ancova_v3_mann_whitney_auc(
    data$overall_score, data$is_clean
  )
  testthat::expect_equal(auc, 1)
})

test_that("Track E chooses p orientation that favors p", {
  env <- .load_lm_ancova_v3_study_env()
  # Clean has LARGER p (worse -log10 p), so as-is -log10p favors violated.
  # Empirically favoring p should invert.
  marker <- -log10(c(0.04, 0.03, 0.001, 0.0005))
  is_clean <- c(TRUE, TRUE, FALSE, FALSE)
  oriented <- env$lm_ancova_v3_orient_p_marker(marker, is_clean)
  testthat::expect_identical(oriented$direction, "inverted")
  testthat::expect_gt(oriented$auc, 0.5)
})

test_that("identical score and p rankings yield DeltaAUC = 0 and not confirmed", {
  env <- .load_lm_ancova_v3_study_env()
  set.seed(20260807L)
  # Same numeric values for score and -log10(p) ranking: make original_p
  # a monotone transform of overall_score so orientations align.
  n <- 30L
  score <- c(stats::runif(n, 50, 90), stats::runif(n, 10, 49))
  # Map score to p so -log10(p) is exactly score/20 (same ranking).
  p <- 10^(-(score / 20))
  data <- data.frame(
    scenario_id = rep(
      c("lm_ancova_v3_clean_n80_r2_40_clear",
        "lm_ancova_v3_viol_n80_r2_40_clear_heteroscedastic"),
      each = n
    ),
    replicate_id = rep(seq_len(n), 2L),
    truth_class = "clear",
    analysis_conclusion = "significant",
    status = "completed",
    design_layer = "core",
    overall_score = score,
    original_p = p,
    violation_type = rep(c(NA_character_, "heteroscedastic"), each = n),
    matched_clean_id = rep(
      c(NA_character_, "lm_ancova_v3_clean_n80_r2_40_clear"), each = n
    ),
    sample_size = 80L,
    stringsAsFactors = FALSE
  )

  result <- env$analyse_lm_ancova_v3_track_e(
    data,
    cluster_B = 1000L,
    cluster_seed = 20260807L
  )
  testthat::expect_equal(result$pooled$delta_auc, 0, tolerance = 1e-10)
  testthat::expect_identical(result$verdict, "not confirmed")
  testthat::expect_false(isTRUE(result$confirmed))
})

test_that("Track E reports pooled and per-violation DeltaAUC with frozen gate", {
  env <- .load_lm_ancova_v3_study_env()
  set.seed(11L)
  rows <- rbind(
    .fixture_rows(score_shift = 25, p_shift = 0, violation_type = "heteroscedastic",
                  n_cell = 40L),
    .fixture_rows(score_shift = 25, p_shift = 0, violation_type = "heavy_tails",
                  n_cell = 40L),
    .fixture_rows(score_shift = 25, p_shift = 0, violation_type = "heteroscedastic",
                  n_cell = 80L),
    .fixture_rows(score_shift = 25, p_shift = 0, violation_type = "heavy_tails",
                  n_cell = 80L)
  )
  # Duplicate clean rows appear once per fixture call; keep unique clean
  # replicates per n by rebuilding a cleaner fixture once.
  set.seed(11L)
  mk <- function(n_cell, vtype) {
    clean_id <- sprintf("lm_ancova_v3_clean_n%d_r2_40_clear", n_cell)
    viol_id <- sprintf("lm_ancova_v3_viol_n%d_r2_40_clear_%s", n_cell, vtype)
    n <- 40L
    clean_score <- stats::runif(n, 70, 95)
    viol_score <- clean_score - 30
    clean_p <- 10^(-stats::runif(n, 3, 6))
    # p barely separates so score wins large DeltaAUC
    viol_p <- pmin(1, clean_p * 1.05)
    rbind(
      data.frame(
        scenario_id = clean_id, replicate_id = seq_len(n),
        truth_class = "clear", analysis_conclusion = "significant",
        status = "completed", design_layer = "core",
        overall_score = clean_score, original_p = clean_p,
        violation_type = NA_character_, matched_clean_id = NA_character_,
        sample_size = n_cell, stringsAsFactors = FALSE
      ),
      data.frame(
        scenario_id = viol_id, replicate_id = seq_len(n),
        truth_class = "clear", analysis_conclusion = "significant",
        status = "completed", design_layer = "core",
        overall_score = viol_score, original_p = viol_p,
        violation_type = vtype, matched_clean_id = clean_id,
        sample_size = n_cell, stringsAsFactors = FALSE
      )
    )
  }
  # One clean block per n, shared across violations.
  set.seed(11L)
  data <- NULL
  for (n_cell in c(40L, 80L)) {
    clean_id <- sprintf("lm_ancova_v3_clean_n%d_r2_40_clear", n_cell)
    n <- 40L
    clean_score <- stats::runif(n, 70, 95)
    clean_p <- 10^(-stats::runif(n, 3, 6))
    clean_block <- data.frame(
      scenario_id = clean_id, replicate_id = seq_len(n),
      truth_class = "clear", analysis_conclusion = "significant",
      status = "completed", design_layer = "core",
      overall_score = clean_score, original_p = clean_p,
      violation_type = NA_character_, matched_clean_id = NA_character_,
      sample_size = n_cell, stringsAsFactors = FALSE
    )
    data <- rbind(data, clean_block)
    for (vtype in c("heteroscedastic", "heavy_tails")) {
      viol_id <- sprintf("lm_ancova_v3_viol_n%d_r2_40_clear_%s", n_cell, vtype)
      viol_block <- data.frame(
        scenario_id = viol_id, replicate_id = seq_len(n),
        truth_class = "clear", analysis_conclusion = "significant",
        status = "completed", design_layer = "core",
        overall_score = clean_score - 30, original_p = pmin(1, clean_p * 1.05),
        violation_type = vtype, matched_clean_id = clean_id,
        sample_size = n_cell, stringsAsFactors = FALSE
      )
      data <- rbind(data, viol_block)
    }
  }

  result <- env$analyse_lm_ancova_v3_track_e(
    data,
    cluster_B = 1000L,
    cluster_seed = 20260807L
  )

  testthat::expect_true(is.data.frame(result$per_violation))
  testthat::expect_true(all(c("violation_type", "delta_auc") %in% names(result$per_violation)))
  testthat::expect_gte(nrow(result$per_violation), 2L)
  testthat::expect_true(is.finite(result$pooled$delta_auc))
  testthat::expect_true(is.finite(result$pooled$ci_lower))
  testthat::expect_true(is.finite(result$pooled$ci_upper))
  testthat::expect_true(result$pooled$delta_auc >= 0.10)
  testthat::expect_true(result$pooled$ci_lower > 0)
  testthat::expect_identical(result$verdict, "confirmed")
  testthat::expect_true(isTRUE(result$confirmed))

  # Deterministic cluster bootstrap under frozen seed.
  again <- env$analyse_lm_ancova_v3_track_e(
    data,
    cluster_B = 1000L,
    cluster_seed = 20260807L
  )
  testthat::expect_equal(again$pooled$ci_lower, result$pooled$ci_lower)
  testthat::expect_equal(again$pooled$ci_upper, result$pooled$ci_upper)
  testthat::expect_equal(again$pooled$delta_auc, result$pooled$delta_auc)
})
