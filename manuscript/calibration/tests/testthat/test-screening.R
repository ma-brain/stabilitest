test_project_root <- normalizePath(file.path("..", "..", "..", ".."), mustWork = TRUE)
pkgload::load_all(
  test_project_root, export_all = FALSE, helpers = FALSE, quiet = TRUE
)

screening_path <- file.path("..", "..", "R", "screening.R")
testthat::expect_true(file.exists(screening_path))

screening_env <- new.env(parent = globalenv())
sys.source(screening_path, envir = screening_env)

fake_adapter <- function() {
  list(
    generate = function(scenario, seed) {
      # The seed is the only source of randomness.  This deliberately emits a
      # failed screening fit for a deterministic subset of generated draws.
      list(value = as.integer(seed))
    },
    primary_decision = function(data, scenario) {
      if (data$value %% 7L == 0L) {
        stop("synthetic screening failure", call. = FALSE)
      }
      list(
        p = if (data$value %% 2L == 0L) 0.01 else 0.60,
        conclusion = if (data$value %% 2L == 0L) "significant" else "non_significant",
        status = "completed"
      )
    }
  )
}

fake_scenario <- list(
  scenario_id = "screening_fixture",
  analysis_family = "two_sample",
  endpoint = "mean_difference",
  design_layer = "core",
  truth_class = "null",
  target_conclusion = "non_significant",
  scenario_seed = 12345L,
  sample_size = 20L
)

testthat::test_that("screening records every draw and selects deterministic quotas", {
  targets <- c("null::significant" = 2L, "null::non_significant" = 2L)
  first <- screening_env$screen_scenario(
    fake_scenario, fake_adapter(), targets, max_draws = 20L, workers = 1L
  )
  second <- screening_env$screen_scenario(
    fake_scenario, fake_adapter(), targets, max_draws = 20L, workers = 2L
  )

  testthat::expect_equal(nrow(first$screened), 20L)
  testthat::expect_equal(first$denominator, 20L)
  testthat::expect_true(all(c("replicate_id", "replicate_seed", "status",
                              "screening_conclusion", "selected", "priority") %in%
                            names(first$screened)))
  testthat::expect_equal(
    first$screened$replicate_id[first$screened$selected],
    second$screened$replicate_id[second$screened$selected]
  )
  testthat::expect_equal(sum(first$screened$selected), 4L)
  testthat::expect_identical(first$status, "complete")
  testthat::expect_identical(second$status, "complete")
})

testthat::test_that("screening reports an incomplete quota without fabricating conclusions", {
  targets <- c("null::significant" = 100L)
  result <- screening_env$screen_scenario(
    fake_scenario, fake_adapter(), targets, max_draws = 8L, workers = 1L
  )

  testthat::expect_identical(result$status, "incomplete")
  testthat::expect_true(length(result$missing) == 1L)
  testthat::expect_gte(result$missing[[1L]]$needed, 96L)
  failed <- result$screened$status == "failed"
  testthat::expect_true(any(failed))
  testthat::expect_true(all(is.na(result$screened$screening_conclusion[failed])))
  testthat::expect_false(any(result$screened$selected[failed]))
})

testthat::test_that("selection helper is deterministic and excludes failed rows", {
  screened <- tibble::tibble(
    truth_class = rep("clear", 4L),
    screening_conclusion = c("significant", "significant", NA_character_, "significant"),
    status = c("completed", "completed", "failed", "completed"),
    replicate_id = c(1L, 2L, 3L, 4L),
    replicate_seed = c(11L, 12L, 13L, 14L),
    priority = c(9L, 2L, 1L, 4L),
    selected = FALSE
  )
  selected <- screening_env$select_stratified_replicates(
    screened, c("clear::significant" = 2L)
  )
  testthat::expect_equal(selected$replicate_id, c(2L, 4L))
  testthat::expect_false(any(selected$status == "failed"))
  testthat::expect_identical(attr(selected, "status"), "complete")
})

testthat::test_that("screening rejects integers outside the base-R seed range", {
  bad_scenario <- fake_scenario
  bad_scenario$scenario_seed <- -.Machine$integer.max - 1
  testthat::expect_error(
    screening_env$screen_scenario(
      bad_scenario, fake_adapter(), c("null::significant" = 1L), max_draws = 1L
    ),
    "scenario_seed must be one finite integer"
  )
  testthat::expect_error(
    screening_env$screen_scenario(
      fake_scenario, fake_adapter(), c("null::significant" = 1L),
      max_draws = -.Machine$integer.max - 1
    ),
    "max_draws must be one positive integer"
  )
})
