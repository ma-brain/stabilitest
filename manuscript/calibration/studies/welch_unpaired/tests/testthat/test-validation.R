.welch_validation_study_root <- function() {
  normalizePath(file.path(testthat::test_path("..", "..")), mustWork = TRUE)
}

.load_welch_validation_code <- function(envir = parent.frame()) {
  root <- .welch_validation_study_root()
  sys.source(file.path(root, "R", "thresholds.R"), envir = envir)
  sys.source(file.path(root, "R", "validation.R"), envir = envir)
}

testthat::test_that("conservative bounds use the adverse Wilson/cluster value", {
  .load_welch_validation_code()
  testthat::expect_equal(
    welch_conservative_bound(0.08, 0.11, side = "upper"),
    0.11
  )
  testthat::expect_equal(
    welch_conservative_bound(0.64, 0.59, side = "lower"),
    0.59
  )
})

testthat::test_that("scenario-cluster bounds are deterministic and preserve RNG", {
  .load_welch_validation_code()
  data <- data.frame(
    scenario_id = rep(c("a", "b", "c"), each = 4L),
    value = c(rep(0, 4), rep(0.5, 4), rep(1, 4)),
    stringsAsFactors = FALSE
  )
  set.seed(90210)
  state <- .Random.seed
  a <- welch_cluster_bound(
    data, function(rows) mean(rows$value), side = "upper",
    B = 100L, seed = 202640001L
  )
  b <- welch_cluster_bound(
    data, function(rows) mean(rows$value), side = "upper",
    B = 100L, seed = 202640001L
  )
  testthat::expect_identical(a, b)
  testthat::expect_identical(.Random.seed, state)
  testthat::expect_identical(a$n_clusters, 3L)
})
