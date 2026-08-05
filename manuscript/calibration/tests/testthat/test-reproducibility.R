seed_path <- file.path("..", "..", "R", "seeds.R")
testthat::expect_true(file.exists(seed_path))

seed_env <- new.env(parent = globalenv())
sys.source(seed_path, envir = seed_env)

testthat::test_that("seed derivation is stable and collision-resistant", {
  ids <- sprintf("scenario-%03d", 1:64)
  seeds_a <- vapply(ids, seed_env$scenario_seed, integer(1), master_seed = 20260804L)
  seeds_b <- vapply(ids, seed_env$scenario_seed, integer(1), master_seed = 20260804L)

  testthat::expect_type(seeds_a, "integer")
  testthat::expect_identical(
    RNGkind(),
    c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  )
  testthat::expect_identical(seeds_a, seeds_b)
  testthat::expect_true(all(seeds_a > 0L))
  testthat::expect_length(unique(seeds_a), length(ids))

  replicate_seeds <- vapply(seq_len(64L), function(id) {
    seed_env$replicate_seed(seeds_a[[1L]], id)
  }, integer(1))
  testthat::expect_length(unique(replicate_seeds), length(replicate_seeds))
  testthat::expect_true(all(replicate_seeds > 0L))

  bootstrap_seeds <- vapply(replicate_seeds, seed_env$bootstrap_seed, integer(1))
  testthat::expect_length(unique(bootstrap_seeds), length(bootstrap_seeds))
  testthat::expect_true(all(bootstrap_seeds > 0L))
})

testthat::test_that("seed derivation pins RNG subkinds for reproducible data", {
  seed_env$scenario_seed("subkind-invariant", 20260804L)
  seed_a <- seed_env$replicate_seed(12345L, 7L)
  set.seed(seed_a)
  generated_a <- c(rnorm(16L), rbinom(16L, size = 1L, prob = 0.4))

  suppressWarnings(RNGkind("Mersenne-Twister", "Box-Muller", "Rounding"))
  seed_env$scenario_seed("subkind-invariant", 20260804L)
  seed_b <- seed_env$replicate_seed(12345L, 7L)
  set.seed(seed_b)
  generated_b <- c(rnorm(16L), rbinom(16L, size = 1L, prob = 0.4))

  testthat::expect_identical(seed_a, seed_b)
  testthat::expect_identical(generated_a, generated_b)
  testthat::expect_identical(
    RNGkind(),
    c("L'Ecuyer-CMRG", "Inversion", "Rejection")
  )
})

testthat::test_that("replicate generation is independent of execution order", {
  scenario <- seed_env$scenario_seed("order-invariant", 20260804L)
  replicate_ids <- c(1L, 2L, 7L, 11L, 23L)

  data_hash <- function(replicate_id) {
    seed <- seed_env$replicate_seed(scenario, replicate_id)
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(seed)
    values <- rnorm(32L)
    raw <- serialize(values, NULL, version = 2L)
    hash <- 104729
    for (byte in as.integer(raw)) {
      hash <- (hash * 131 + byte) %% 2147483647
    }
    as.integer(hash + 1)
  }

  forward <- vapply(replicate_ids, data_hash, integer(1))
  reverse <- vapply(rev(replicate_ids), data_hash, integer(1))
  testthat::expect_identical(forward, rev(reverse))
})
