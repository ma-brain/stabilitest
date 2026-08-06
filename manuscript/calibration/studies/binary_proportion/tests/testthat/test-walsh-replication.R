.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_bp_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)
  env$load_binary_proportion_study(project_root = .project_root(), envir = env)
  env
}

.bp_scenario <- function(seed = 61001L) {
  list(
    scenario_id = "test_scn", analysis_engine = "proportion",
    calibration_family = "binary_proportion", calibration_unit = "fisher_exact",
    endpoint = "risk_difference", design_layer = "core", truth_class = "clear",
    target_conclusion = "significant", sample_size = 200L,
    scenario_seed = as.integer(seed),
    parameters = list(list(
      generator = list(n_per_arm = 100, p0 = 0.25, target_power = 0.95,
                       solve_exact_power = TRUE, diagnostic_only = FALSE,
                       allocation = 0.5, effect_direction = 1),
      analysis = list(test_type = "fisher", alpha = 0.05,
                      weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)),
      screening = list(conclusions = "significant", target_n = 100L)
    ))
  )
}

# --- Walsh event-flip fragility index ----------------------------------------

test_that("walsh_event_flip_fi counts events to flip until significance changes", {
  env <- .load_bp_env()
  # A clearly significant table: 20/50 vs 5/50.  Fisher is significant.
  g1 <- c(rep(1L, 20), rep(0L, 30))
  g2 <- c(rep(1L, 5), rep(0L, 45))
  fi <- env$walsh_event_flip_fi(g1, g2, alpha = 0.05,
                                 direction = "overturn")
  testthat::expect_true(is.integer(fi))
  testthat::expect_gte(as.integer(fi), 1L)
  # Convention: flip events in the smaller-event arm (control, g2) toward
  # non-significance.  Confirm the documented flip direction is recorded.
  arm <- attr(fi, "flip_arm")
  testthat::expect_true(arm %in% c("group1", "group2"))
  testthat::expect_identical(arm, "group2")
})

test_that("walsh_event_flip_fi is 0 when already non-significant", {
  env <- .load_bp_env()
  # Null table: 6/50 vs 5/50 (p ~ 1, not significant).
  g1 <- c(rep(1L, 6), rep(0L, 44))
  g2 <- c(rep(1L, 5), rep(0L, 45))
  fi <- env$walsh_event_flip_fi(g1, g2, alpha = 0.05,
                                 direction = "overturn")
  testthat::expect_equal(as.integer(fi), 0L)
})

test_that("walsh_event_flip_fi is deterministic for identical inputs", {
  env <- .load_bp_env()
  g1 <- c(rep(1L, 18), rep(0L, 32))
  g2 <- c(rep(1L, 6), rep(0L, 44))
  a <- env$walsh_event_flip_fi(g1, g2, alpha = 0.05, direction = "overturn")
  b <- env$walsh_event_flip_fi(g1, g2, alpha = 0.05, direction = "overturn")
  testthat::expect_identical(a, b)
})

test_that("walsh FI matches a brute-force event-flip on a significant fixture", {
  # Classic Walsh FI on a significant table: minimum number of 0->1 flips in the
  # smaller-event (control) arm (adding events to shrink the disparity) to make
  # Fisher non-significant.  Verified by brute force.
  env <- .load_bp_env()
  e1 <- 20L; n1 <- 50L; e2 <- 5L; n2 <- 50L
  g1 <- c(rep(1L, e1), rep(0L, n1 - e1))
  g2 <- c(rep(1L, e2), rep(0L, n2 - e2))
  fi <- env$walsh_event_flip_fi(g1, g2, alpha = 0.05, direction = "overturn")
  brute <- NA_integer_
  for (k in seq_len(n2 - e2)) {
    tab <- matrix(c(e1, n1 - e1, e2 + k, n2 - e2 - k), 2)
    if (stats::fisher.test(tab)$p.value >= 0.05) {
      brute <- k
      break
    }
  }
  testthat::expect_false(is.na(brute))
  testthat::expect_equal(as.integer(fi), brute)
  testthat::expect_identical(attr(fi, "flip_arm"), "group2")
})

# --- Replication draws -------------------------------------------------------

test_that("replication draw uses a dedicated seed stream from master 20260808", {
  env <- .load_bp_env()
  scen <- .bp_scenario(seed = 61001L)
  # The replication seed is derived from the master (20260808) + scenario +
  # replicate id, distinct from the screening replicate seed.
  rseed <- env$prop_replication_seed(scenario_seed = 61001L, replicate_id = 1L)
  testthat::expect_true(is.integer(rseed) && length(rseed) == 1L &&
                         !is.na(rseed) && rseed > 0L)
  # Distinct from the screening replicate seed at the same coordinates.
  screen <- env$replicate_seed(scenario_seed = 61001L, replicate_id = 1L)
  testthat::expect_false(identical(rseed, screen))
})

test_that("replication draw is one primary-test-only replicate, deterministic", {
  env <- .load_bp_env()
  scen <- .bp_scenario(seed = 61001L)
  draw1 <- env$prop_replication_draw(scen, replicate_id = 1L)
  draw2 <- env$prop_replication_draw(scen, replicate_id = 1L)
  testthat::expect_equal(draw1$p, draw2$p)
  # Primary-test-only: it returns a p-value and conclusion, no robustness score.
  testthat::expect_true(is.numeric(draw1$p) && length(draw1$p) == 1L &&
                         is.finite(draw1$p))
  testthat::expect_true(is.logical(draw1$significant) && length(draw1$significant) == 1L)
  testthat::expect_null(draw1$overall_score)
  # The recorded seed is the replication seed.
  testthat::expect_equal(draw1$replication_seed,
                         env$prop_replication_seed(61001L, 1L))
})

test_that("replication seed stream never collides with other seed columns", {
  env <- .load_bp_env()
  scenarios <- env$binary_proportion_scenarios()
  # Replication seeds across the study must be distinct from scenario seeds,
  # replicate seeds, and bootstrap seeds derived from the same coordinates.
  repl <- env$prop_replication_seed(scenario_seed = 61001L, replicate_id = 1L)
  testthat::expect_false(repl %in% scenarios$scenario_seed)
  testthat::expect_false(identical(repl, env$replicate_seed(61001L, 1L)))
  testthat::expect_false(identical(repl, env$bootstrap_seed(
    env$replicate_seed(61001L, 1L))))
})
