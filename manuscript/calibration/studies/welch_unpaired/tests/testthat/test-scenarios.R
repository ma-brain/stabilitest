study_dir <- normalizePath(
  file.path(testthat::test_path("..", "..")),
  mustWork = TRUE
)

testthat::test_that("Welch scenarios satisfy the frozen study contract", {
  source(file.path(study_dir, "config", "scenarios.R"), local = TRUE)
  s <- welch_scenarios()

  required <- c(
    "scenario_id", "design_layer", "truth", "distribution",
    "n_group1", "n_group2", "sd_group1", "sd_group2", "delta",
    "alpha", "n_boot", "max_removal_pct", "replicate_quota", "seed",
    "study_role"
  )
  testthat::expect_true(all(required %in% names(s)))
  testthat::expect_setequal(
    unique(s$design_layer),
    c("training", "validation", "stress")
  )
  testthat::expect_true(all(c("null", "borderline", "clear") %in% s$truth))
  testthat::expect_true(all(s$alpha == 0.05))
  testthat::expect_true(all(s$n_boot == 1000L))
  testthat::expect_true(all(s$max_removal_pct == 0.30))
  testthat::expect_true(all(s$replicate_quota >= 250L))
  testthat::expect_identical(anyDuplicated(s$scenario_id), 0L)
  testthat::expect_identical(anyDuplicated(s$seed), 0L)
  testthat::expect_length(
    intersect(
      s$seed[s$design_layer == "training"],
      s$seed[s$design_layer == "validation"]
    ),
    0L
  )
})

testthat::test_that("core archetypes and truth targets are explicit", {
  source(file.path(study_dir, "config", "scenarios.R"), local = TRUE)
  s <- welch_scenarios()
  core <- s[s$design_layer %in% c("training", "validation"), , drop = FALSE]

  testthat::expect_identical(nrow(core), 36L)
  testthat::expect_true(all(core$distribution == "normal"))
  testthat::expect_identical(
    as.integer(table(core$design_layer)),
    c(18L, 18L)
  )
  testthat::expect_true(all(table(core$design_layer, core$truth) == 6L))

  target <- setNames(core$target_power, core$truth)
  testthat::expect_equal(unname(target["null"]), 0)
  testthat::expect_equal(unname(target["borderline"]), 0.60)
  testthat::expect_equal(unname(target["clear"]), 0.95)
  testthat::expect_true(all(core$study_role == ifelse(
    core$design_layer == "training", "candidate_fitting", "confirmation"
  )))
})

testthat::test_that("stress rows are diagnostic and excluded from gates", {
  source(file.path(study_dir, "config", "scenarios.R"), local = TRUE)
  s <- welch_scenarios()
  stress <- s[s$design_layer == "stress", , drop = FALSE]

  testthat::expect_identical(nrow(stress), 6L)
  testthat::expect_setequal(
    unique(stress$distribution),
    c("student_t5", "centered_lognormal", "gross_error_treatment")
  )
  testthat::expect_setequal(unique(stress$truth), c("null", "clear"))
  testthat::expect_true(all(stress$study_role == "stress_reporting"))
  testthat::expect_false(any(stress$required_for_fit))
  testthat::expect_false(any(stress$required_for_acceptance))
})
