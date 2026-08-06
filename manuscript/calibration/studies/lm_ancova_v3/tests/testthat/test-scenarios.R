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

.param_flag <- function(parameters, name) {
  vapply(parameters, function(p) isTRUE(p[[name]]), logical(1))
}

.VIOLATIONS <- c(
  "allocation_2to1",
  "heteroscedastic",
  "heavy_tails",
  "missing_baseline",
  "interaction"
)

test_that("lm_ancova_v3 scenarios form the frozen Track E contract", {
  env <- .load_lm_ancova_v3_study_env()
  scenarios <- env$lm_ancova_v3_scenarios()

  testthat::expect_true(env$validate_calibration_scenarios(scenarios))
  testthat::expect_true(all(scenarios$calibration_unit == "lm_ancova_v3"))
  testthat::expect_false(anyDuplicated(scenarios$scenario_id) > 0L)
  testthat::expect_false(anyDuplicated(scenarios$scenario_seed) > 0L)
  testthat::expect_true(all(grepl("^lm_ancova_v3_", scenarios$scenario_id)))
  testthat::expect_identical(nrow(scenarios), 24L)

  primary <- scenarios[!.param_flag(scenarios$parameters, "diagnostic_only"), , drop = FALSE]
  diagnostic <- scenarios[.param_flag(scenarios$parameters, "diagnostic_only"), , drop = FALSE]
  testthat::expect_identical(nrow(primary), 18L)
  testthat::expect_identical(nrow(diagnostic), 6L)

  clean_clear <- primary[
    vapply(primary$parameters, function(p) {
      vt <- p$violation_type
      is.null(vt) || !nzchar(as.character(vt))
    }, logical(1)),
    ,
    drop = FALSE
  ]
  violated_clear <- primary[
    vapply(primary$parameters, function(p) {
      vt <- p$violation_type
      !is.null(vt) && nzchar(as.character(vt))
    }, logical(1)),
    ,
    drop = FALSE
  ]
  testthat::expect_identical(nrow(clean_clear), 3L)
  testthat::expect_identical(nrow(violated_clear), 15L)
  testthat::expect_true(all(clean_clear$truth_class == "clear"))
  testthat::expect_true(all(violated_clear$truth_class == "clear"))
  testthat::expect_true(all(diagnostic$truth_class == "null"))

  clean_ns <- sort(vapply(
    clean_clear$parameters, function(p) as.integer(p$generator$n), integer(1)
  ))
  testthat::expect_identical(clean_ns, c(40L, 80L, 160L))
  testthat::expect_true(all(
    vapply(clean_clear$parameters, function(p) {
      identical(as.numeric(p$generator$baseline_r2), 0.40) &&
        identical(as.numeric(p$generator$target_power), 0.90)
    }, logical(1))
  ))

  testthat::expect_true(all(
    vapply(violated_clear$parameters, function(p) {
      as.character(p$violation_type) %in% .VIOLATIONS &&
        !is.null(p$matched_clean_id) &&
        nzchar(as.character(p$matched_clean_id))
    }, logical(1))
  ))
  testthat::expect_true(all(
    as.character(vapply(
      violated_clear$parameters, function(p) as.character(p$matched_clean_id), character(1)
    )) %in% clean_clear$scenario_id
  ))

  # Each clean clear cell has exactly five matched violations.
  for (cid in clean_clear$scenario_id) {
    matched <- violated_clear[
      vapply(violated_clear$parameters, function(p) {
        identical(as.character(p$matched_clean_id), cid)
      }, logical(1)),
      ,
      drop = FALSE
    ]
    testthat::expect_identical(nrow(matched), 5L)
    vtypes <- sort(vapply(
      matched$parameters, function(p) as.character(p$violation_type), character(1)
    ))
    testthat::expect_identical(vtypes, sort(.VIOLATIONS))

    clean_row <- clean_clear[clean_clear$scenario_id == cid, , drop = FALSE]
    cg <- clean_row$parameters[[1L]]$generator
    for (i in seq_len(nrow(matched))) {
      vg <- matched$parameters[[i]]$generator
      testthat::expect_identical(as.integer(vg$n), as.integer(cg$n))
      testthat::expect_equal(as.numeric(vg$baseline_r2), as.numeric(cg$baseline_r2))
      testthat::expect_equal(as.numeric(vg$target_power), as.numeric(cg$target_power))
    }
  }

  # Diagnostic null pairs at n = 80 only (clean + 5 violated); quota 50.
  diag_ns <- unique(vapply(
    diagnostic$parameters, function(p) as.integer(p$generator$n), integer(1)
  ))
  testthat::expect_identical(diag_ns, 80L)
  diag_clean <- diagnostic[
    vapply(diagnostic$parameters, function(p) {
      vt <- p$violation_type
      is.null(vt) || !nzchar(as.character(vt))
    }, logical(1)),
    ,
    drop = FALSE
  ]
  diag_viol <- diagnostic[
    vapply(diagnostic$parameters, function(p) {
      vt <- p$violation_type
      !is.null(vt) && nzchar(as.character(vt))
    }, logical(1)),
    ,
    drop = FALSE
  ]
  testthat::expect_identical(nrow(diag_clean), 1L)
  testthat::expect_identical(nrow(diag_viol), 5L)
  testthat::expect_true(all(
    vapply(diagnostic$parameters, function(p) {
      identical(as.integer(p$screening$target_n), 50L)
    }, logical(1))
  ))
  testthat::expect_true(all(
    vapply(primary$parameters, function(p) {
      identical(as.integer(p$screening$target_n), 100L)
    }, logical(1))
  ))

  # Seeds: Track E uses 54001+ only; parked Track D and v1/v2 ranges absent.
  testthat::expect_true(min(scenarios$scenario_seed) >= 54001L)
  forbidden <- c(
    31001L:33006L,
    41001L:41027L,
    42001L:42018L,
    43001L:43006L,
    51001L:53999L
  )
  testthat::expect_length(intersect(scenarios$scenario_seed, forbidden), 0L)
  testthat::expect_false(any(scenarios$scenario_seed %in% 41001L:53999L))

  # Loader reuses v1 analytic helpers without mutating v1 / v2.
  testthat::expect_true(is.function(env$solve_ancova_effect))
  testthat::expect_true(is.function(env$generate_lm_ancova))
  testthat::expect_true(is.function(env$ancova_nominal_power))
})

test_that("study root cwd fallback never resolves to v1 or v2", {
  env <- new.env(parent = globalenv())
  sys.source(file.path(.study_root(), "R", "load_study.R"), envir = env)

  v1_root <- normalizePath(
    file.path(.project_root(), "manuscript", "calibration", "studies", "lm_ancova"),
    mustWork = TRUE
  )
  v2_root <- normalizePath(
    file.path(.project_root(), "manuscript", "calibration", "studies", "lm_ancova_v2"),
    mustWork = TRUE
  )
  v3_root <- .study_root()

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(v1_root)
  resolved_from_v1 <- env$.lm_ancova_v3_study_root(script_path = NULL)
  testthat::expect_identical(resolved_from_v1, v3_root)
  testthat::expect_false(identical(resolved_from_v1, v1_root))
  testthat::expect_false(identical(resolved_from_v1, v2_root))

  setwd(v2_root)
  resolved_from_v2 <- env$.lm_ancova_v3_study_root(script_path = NULL)
  testthat::expect_identical(resolved_from_v2, v3_root)

  setwd(dirname(v1_root))
  resolved_from_studies <- env$.lm_ancova_v3_study_root(script_path = NULL)
  testthat::expect_identical(resolved_from_studies, v3_root)
})
