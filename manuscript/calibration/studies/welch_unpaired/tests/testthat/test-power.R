.welch_test_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

testthat::test_that("Welch power solver reaches the frozen targets", {
  source(file.path(.welch_test_study_root(), "R", "power.R"), local = TRUE)

  testthat::expect_equal(
    welch_power(delta = 0, n1 = 50, n2 = 50, sd1 = 1, sd2 = 1),
    0.05,
    tolerance = 0.01
  )

  d60 <- solve_welch_delta(
    target_power = 0.60, n1 = 50, n2 = 50, sd1 = 1, sd2 = 1
  )
  d95 <- solve_welch_delta(
    target_power = 0.95, n1 = 50, n2 = 50, sd1 = 1, sd2 = 1
  )
  testthat::expect_lt(d60, d95)
  testthat::expect_equal(
    welch_power(d60, 50, 50, 1, 1),
    0.60,
    tolerance = 0.005
  )
  testthat::expect_equal(
    welch_power(d95, 50, 50, 1, 1),
    0.95,
    tolerance = 0.005
  )
})

testthat::test_that("Welch power validates targets and design inputs", {
  source(file.path(.welch_test_study_root(), "R", "power.R"), local = TRUE)

  testthat::expect_error(
    solve_welch_delta(0.05, 50, 50, 1, 1),
    "strictly between alpha and 1"
  )
  testthat::expect_error(
    solve_welch_delta(1, 50, 50, 1, 1),
    "strictly between alpha and 1"
  )
  testthat::expect_error(
    welch_power(1, 1, 50, 1, 1),
    "at least 2"
  )
  testthat::expect_error(
    welch_power(1, 50, 50, 0, 1),
    "positive"
  )
})
