test_that("GLM dot formulas use only user predictors, including with weights", {
  set.seed(42)
  dat <- data.frame(x = rnorm(30),
                    y = rpois(30, exp(seq(-1, 1, length.out = 30))))
  for (w in list(NULL, rep(c(1, 2, 3), 10))) {
    fit <- glm(y ~ ., dat, family = poisson(), weights = w)
    result <- robustness_glm(y ~ ., dat, term = "x", family = poisson(),
                              obs_weights = w, n_boot = 10,
                              max_removal_pct = .1)
    explicit <- robustness_glm(y ~ x, dat, term = "x", family = poisson(),
                                obs_weights = w, n_boot = 10,
                                max_removal_pct = .1)
    expect_lt(abs(result$original_p - coef(summary(fit))["x", "Pr(>|z|)"]), 1e-10)
    expect_lt(abs(result$original_estimate - coef(fit)[["x"]]), 1e-10)
    expect_lt(max(abs(unlist(result$metrics) - unlist(explicit$metrics)),
                  na.rm = TRUE), 1e-10)
  }
})

test_that("model resampling never substitutes a new factor reference level", {
  set.seed(13)
  dat <- data.frame(arm = factor(c("A", rep("B", 6), rep("C", 6))))
  dat$y <- c(0, rep(10, 6), rep(11, 6)) + rnorm(13)
  dat$count <- c(1, 3, 5, 7, 2, 4, 6, 7, 4, 3, 6, 5, 2)
  results <- list(
    robustness_lm(y ~ arm, dat, "armC", n_boot = 20,
                  max_removal_pct = .08),
    robustness_glm(count ~ arm, dat, "armC", family = poisson(),
                   n_boot = 20, max_removal_pct = .08)
  )
  for (result in results) {
    # Once the sole A observation is removed, C versus A is not estimable.
    expect_length(which(result$jackknife$row == 1 &
                          is.finite(result$jackknife$p_value)), 0L)
    set.seed(123)
    no_reference <- vapply(seq_len(20), function(i) {
      !1L %in% sample(13, replace = TRUE)
    }, logical(1))
    expect_length(which(result$bootstrap$iteration %in% which(no_reference) &
                          is.finite(result$bootstrap$p_value)), 0L)
  }
})

test_that("joint term tests reject a reduced factor hypothesis", {
  set.seed(21)
  dat <- data.frame(arm = factor(rep(c("A", "B", "C"), each = 8)),
                    y = rnorm(24), count = rpois(24, 4),
                    time = rexp(24), event = rep(1, 24))
  fits <- list(lm(y ~ arm, dat), glm(count ~ arm, dat, family = poisson()))
  reduced <- list(lm(y ~ arm, dat[dat$arm != "A", ]),
                  glm(count ~ arm, dat[dat$arm != "A", ], family = poisson()))
  testers <- list(stabilitest:::lm_term_test, stabilitest:::glm_term_test)
  if (requireNamespace("survival", quietly = TRUE)) {
    fits[[3]] <- survival::coxph(survival::Surv(time, event) ~ arm, dat)
    reduced[[3]] <- survival::coxph(survival::Surv(time, event) ~ arm,
                                    dat[dat$arm != "A", ])
    testers[[3]] <- stabilitest:::surv_term_test
  }
  for (i in seq_along(fits)) {
    spec <- stabilitest:::resolve_model_term(fits[[i]], "arm")
    expect_length(testers[[i]](reduced[[i]], spec), 0L)
  }
})

test_that("refits reject loss of an adjustment coefficient", {
  set.seed(27)
  dat <- data.frame(arm = factor(rep(c("A", "B", "C"), each = 8)),
                    y = rnorm(24), count = rpois(24, 4),
                    time = rexp(24), event = rep(1, 24))
  dat$z <- as.numeric(dat$arm == "B")
  dat$z[1] <- 2  # Deleting this row aliases z with armB.
  reduced_data <- dat[-1, ]
  fits <- list(lm(y ~ arm + z, dat),
               glm(count ~ arm + z, dat, family = poisson()))
  reduced <- list(lm(y ~ arm + z, reduced_data),
                  glm(count ~ arm + z, reduced_data, family = poisson()))
  testers <- list(stabilitest:::lm_term_test, stabilitest:::glm_term_test)
  if (requireNamespace("survival", quietly = TRUE)) {
    fits[[3]] <- survival::coxph(survival::Surv(time, event) ~ arm + z, dat)
    reduced[[3]] <- survival::coxph(survival::Surv(time, event) ~ arm + z,
                                    reduced_data)
    testers[[3]] <- stabilitest:::surv_term_test
  }
  for (i in seq_along(fits)) {
    expect_true(is.finite(coef(fits[[i]])[["z"]]))
    expect_true(is.na(coef(reduced[[i]])[["z"]]))
    for (term in c("armB", "arm")) {
      spec <- stabilitest:::resolve_model_term(fits[[i]], term)
      expect_length(testers[[i]](reduced[[i]], spec), 0L)
    }
  }
  result <- robustness_lm(y ~ arm + z, dat, "armB", n_boot = 10,
                           max_removal_pct = .05)
  expect_length(which(result$jackknife$row == 1 &
                        is.finite(result$jackknife$p_value)), 0L)
})

test_that("GLM bookkeeping does not overwrite user predictors", {
  set.seed(43)
  dat <- data.frame(y = rpois(24, 3), .__row_id__ = rnorm(24),
                    .__obs_w__ = rnorm(24))
  w <- rep(c(1, 2), 12)
  fit <- glm(y ~ ., dat, family = poisson(), weights = w)
  result <- robustness_glm(y ~ ., dat, term = ".__row_id__",
                            family = poisson(), obs_weights = w,
                            n_boot = 10, max_removal_pct = .1)
  expect_lt(abs(result$original_p -
                  coef(summary(fit))[".__row_id__", "Pr(>|z|)"]), 1e-10)
  expect_lt(abs(result$original_estimate - coef(fit)[[".__row_id__"]]), 1e-10)
})

test_that("model resampling retains failures and reports valid denominators", {
  set.seed(1)
  dat <- data.frame(y = rnorm(12), arm = factor(c("B", rep("A", 11))))
  result <- robustness_lm(y ~ arm, dat, "armB", n_boot = 100,
                           max_removal_pct = .1)
  expect_length(result$jackknife$p_value, 12L)
  expect_length(result$bootstrap$p_value, 100L)
  expect_length(which(is.na(result$jackknife$p_value)), 1L)
  expect_length(which(is.na(result$bootstrap$p_value)), 38L)
  expect_length(result$resampling$jackknife$n_failed, 1L)
  expect_length(result$resampling$bootstrap$n_failed, 1L)
  expect_equal(result$resampling$jackknife$n_valid, 11L)
  expect_equal(result$resampling$bootstrap$n_valid, 62L)
  expect_equal(result$resampling$bootstrap$n_failed, 38L)
  valid <- is.finite(result$bootstrap$p_value)
  expect_equal(result$metrics$bootstrap_reproducibility,
               100 * mean(result$bootstrap$conclusion_match[valid]))
  expect_true(all(is.finite(unlist(result$metrics[c(
    "overall_robustness", "jackknife_n_influential",
    "jackknife_pct_influential"
  )]))))
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "11/12 valid", fixed = TRUE)
  expect_match(printed, "62/100 valid", fixed = TRUE)
})

test_that("an entirely failed model bootstrap cannot return a NaN score", {
  set.seed(1)
  dat <- data.frame(y = rnorm(12), arm = factor(c("B", rep("A", 11))))
  expect_error(robustness_lm(y ~ arm, dat, "armB", n_boot = 1,
                             seed = 3, max_removal_pct = .1),
               "No valid bootstrap replicates")
})

test_that("TOST exposes failed resamples and rejects an empty bootstrap", {
  x <- c(2, rep(1, 4))
  y <- rep(0, 5)
  result <- robustness_tost(x, y, paired = TRUE, margin = 3,
                             n_boot = 50, max_removal_pct = .2)
  expect_equal(result$resampling$jackknife$n_failed, 1L)
  expect_equal(result$resampling$bootstrap$n_failed, 15L)
  expect_length(result$jackknife$p_value, 5L)
  expect_length(result$bootstrap$p_value, 50L)
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "35/50 valid", fixed = TRUE)
  expect_error(robustness_tost(x, y, paired = TRUE, margin = 3,
                               n_boot = 1, seed = 3, max_removal_pct = .2),
               "No valid bootstrap replicates")
})

test_that("Cox refits retain local formula functions for single and joint terms", {
  skip_if_not_installed("survival")
  make_case <- function() {
    scale_by <- 10
    d <- 2  # Must not be shadowed by an internal data binding.
    local_transform <- function(x) x / scale_by
    set.seed(4)
    dat <- data.frame(time = rexp(30), event = rep(1, 30), x = rnorm(30),
                      arm = factor(rep(c("A", "B", "C"), 10)))
    list(data = dat,
         formula = survival::Surv(time, event) ~ arm + local_transform(x/d))
  }
  case <- make_case()
  fit <- survival::coxph(case$formula, case$data, model = TRUE)
  for (term in c("local_transform(x/d)", "arm")) {
    result <- robustness_surv(case$formula, case$data, term, n_boot = 10,
                               max_removal_pct = .05)
    expected <- if (term == "arm") {
      drop1(fit, scope = ~ arm, test = "Chisq")["arm", "Pr(>Chi)"]
    } else {
      coef(summary(fit))[term, "Pr(>|z|)"]
    }
    expect_equal(result$original_p, unname(expected))
    expect_length(result$bootstrap$p_value, 10L)
    expect_true(all(is.finite(result$bootstrap$p_value)))
  }
})
