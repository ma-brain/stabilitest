.study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.project_root <- function() {
  normalizePath(file.path(.study_root(), "..", "..", "..", ".."), mustWork = TRUE)
}

.load_binary_proportion_study_env <- function() {
  env <- new.env(parent = globalenv())
  loader <- file.path(.study_root(), "R", "load_study.R")
  sys.source(loader, envir = env)
  env$load_binary_proportion_study(project_root = .project_root(), envir = env)
  env
}

# Seed ranges already in use by earlier studies.  The proportions ledger must
# be disjoint from every one of them (plan: training 61001+, validation
# 62001+, stress 63001+).
.prior_study_seed_ranges <- function() {
  env <- new.env(parent = globalenv())
  shared <- file.path(.project_root(), "manuscript", "calibration", "R",
                      "load_calibration.R")
  sys.source(shared, envir = env)
  env$load_calibration(project_root = .project_root(), envir = env)
  # Welch + the historical mixed-family scenario table.
  welch <- env$calibration_scenarios()
  seeds <- welch$scenario_seed
  # lm_ancova study (sibling branch) uses 31001+/32001+/33001+.  These are not
  # present on main, but we assert the proportions seeds never collide with the
  # documented lm_ancova blocks either, so the disjointness survives a merge.
  seeds <- c(seeds, 31001L:33300L)
  seeds
}

test_that("binary_proportion scenarios form the frozen Phase 1 contract", {
  env <- .load_binary_proportion_study_env()
  scenarios <- env$binary_proportion_scenarios()

  testthat::expect_true(env$validate_calibration_scenarios(scenarios))
  testthat::expect_true(all(scenarios$calibration_unit == "fisher_exact"))
  testthat::expect_true(all(scenarios$analysis_engine == "proportion"))
  testthat::expect_true(all(scenarios$calibration_family == "binary_proportion"))
  testthat::expect_true(all(scenarios$endpoint == "risk_difference"))
  testthat::expect_false(anyDuplicated(scenarios$scenario_id) > 0L)
  testthat::expect_false(anyDuplicated(scenarios$scenario_seed) > 0L)

  # Only fisher_exact in Phase 1: chi_square_2x2 / two_sample_prop never appear.
  testthat::expect_false(
    any(scenarios$calibration_unit %in% c("chi_square_2x2", "two_sample_prop"))
  )

  training <- expand.grid(
    n = c(25L, 50L, 100L, 200L),
    p0 = c(0.10, 0.25, 0.50),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  validation <- expand.grid(
    n = c(35L, 75L, 150L),
    p0 = c(0.15, 0.40),
    truth_class = c("null", "borderline", "clear"),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  core <- scenarios[scenarios$design_layer == "core", , drop = FALSE]
  held_out <- scenarios[scenarios$design_layer == "validation", , drop = FALSE]
  stress <- scenarios[scenarios$design_layer == "stress", , drop = FALSE]

  testthat::expect_identical(nrow(core), 36L)
  testthat::expect_identical(nrow(held_out), 18L)

  # Frozen screening / compute constants on every required scenario.
  testthat::expect_true(all(core$n_boot == 1000L))
  testthat::expect_true(all(held_out$n_boot == 1000L))
  testthat::expect_true(all(core$max_removal_pct == 0.30))
  testthat::expect_true(all(core$target_conclusion == "significant"))

  core_keys <- data.frame(
    n = vapply(core$parameters, function(p) as.integer(p$generator$n_per_arm),
               integer(1)),
    p0 = vapply(core$parameters, function(p) as.numeric(p$generator$p0),
                numeric(1)),
    truth_class = core$truth_class,
    stringsAsFactors = FALSE
  )
  held_keys <- data.frame(
    n = vapply(held_out$parameters, function(p) as.integer(p$generator$n_per_arm),
               integer(1)),
    p0 = vapply(held_out$parameters, function(p) as.numeric(p$generator$p0),
                numeric(1)),
    truth_class = held_out$truth_class,
    stringsAsFactors = FALSE
  )

  order_keys <- function(x) {
    x[order(x$n, x$p0, x$truth_class), , drop = FALSE]
  }
  testthat::expect_equal(order_keys(core_keys), order_keys(training),
                         ignore_attr = TRUE)
  testthat::expect_equal(order_keys(held_keys), order_keys(validation),
                         ignore_attr = TRUE)
})

test_that("truth targets are exact-power-defined and borderline is diagnostic only", {
  env <- .load_binary_proportion_study_env()
  scenarios <- env$binary_proportion_scenarios()
  params <- scenarios$parameters

  target_for <- function(truth_class) {
    switch(truth_class, null = 0, borderline = 0.60, clear = 0.95, NA_real_)
  }

  for (i in seq_len(nrow(scenarios))) {
    truth <- as.character(scenarios$truth_class[[i]])
    gen <- params[[i]]$generator
    testthat::expect_equal(gen$target_power, target_for(truth),
                           info = scenarios$scenario_id[[i]])
    testthat::expect_true(gen$solve_exact_power,
                          info = scenarios$scenario_id[[i]])
  }

  borderline <- scenarios[scenarios$truth_class == "borderline", , drop = FALSE]
  testthat::expect_true(all(vapply(
    borderline$parameters,
    function(p) isTRUE(p$generator$diagnostic_only),
    logical(1)
  )))
})

test_that("stress rows are diagnostic and never validated or accepted", {
  env <- .load_binary_proportion_study_env()
  scenarios <- env$binary_proportion_scenarios()
  stress <- scenarios[scenarios$design_layer == "stress", , drop = FALSE]

  testthat::expect_true(nrow(stress) >= 1L)
  testthat::expect_true(all(stress$design_layer == "stress"))
  testthat::expect_false(any(stress$design_layer == "validation"))
  # Every stress row declares a stress generator switch.
  stress_kinds <- vapply(stress$parameters, function(p) {
    p$generator$stress %||% NA_character_
  }, character(1))
  testthat::expect_false(any(is.na(stress_kinds)))
})

test_that("proportion seed ranges are disjoint from every earlier study", {
  env <- .load_binary_proportion_study_env()
  scenarios <- env$binary_proportion_scenarios()
  prop_seeds <- scenarios$scenario_seed

  prior <- .prior_study_seed_ranges()
  testthat::expect_false(any(prop_seeds %in% prior))

  # Frozen ledger blocks: training 61001+, validation 62001+, stress 63001+.
  core <- scenarios[scenarios$design_layer == "core", , drop = FALSE]
  held <- scenarios[scenarios$design_layer == "validation", , drop = FALSE]
  stress <- scenarios[scenarios$design_layer == "stress", , drop = FALSE]
  testthat::expect_true(all(core$scenario_seed >= 61001L))
  testthat::expect_true(all(held$scenario_seed >= 62001L))
  testthat::expect_true(all(stress$scenario_seed >= 63001L))

  # Replication / cluster-bootstrap masters (20260808) are not reused as
  # scenario seeds either.
  testthat::expect_false(20260808L %in% prop_seeds)
})
