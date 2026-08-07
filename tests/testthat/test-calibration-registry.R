test_that("every public two-vector test maps to one calibration unit", {
  expected <- c(
    "t.test" = "welch_unpaired",
    "paired.t.test" = "paired_t",
    "wilcoxon" = "wilcoxon_rank_sum",
    "wilcoxon.paired" = "wilcoxon_signed_rank",
    "brunner_munzel" = "brunner_munzel",
    "fisher" = "fisher_exact",
    "chisq" = "chi_square_2x2",
    "prop" = "two_sample_prop"
  )

  expect_identical(
    unname(vapply(names(expected), calibration_unit_for_test, character(1))),
    unname(expected)
  )
  expect_false("two_sample" %in% unname(expected))
  expect_error(calibration_unit_for_test("anova"),
               "unknown calibration test type")
})

test_that("model and TOST wrappers map independently of execution engine", {
  expect_identical(calibration_unit_for_model("lm"), "lm_ancova")
  expect_identical(calibration_unit_for_model("glm", family = "binomial"),
                   "glm_binomial")
  expect_identical(calibration_unit_for_model("glm", family = "poisson"),
                   "glm_poisson")
  expect_identical(calibration_unit_for_model("cox"), "cox_ph")
  expect_identical(calibration_unit_for_tost("mean"), "tost_mean")
  expect_identical(calibration_unit_for_tost("prop"), "tost_risk_difference")
  expect_identical(calibration_unit_for_tost("or"), "tost_odds_ratio")

  expect_error(calibration_unit_for_model("glm", family = "gaussian"),
               "unknown model calibration unit")
  expect_error(calibration_unit_for_tost("median"),
               "unknown TOST calibration endpoint")
})

test_that("mapping helpers reject non-character and non-scalar inputs", {
  invalid_test_types <- list(
    factor("fisher"), 1, list("fisher"), NA_character_, character(),
    c("t.test", "fisher")
  )
  for (value in invalid_test_types) {
    expect_error(calibration_unit_for_test(value),
                 "unknown calibration test type")
  }

  invalid_engines <- list(
    factor("lm"), 1, list("lm"), NA_character_, character(), c("lm", "cox")
  )
  for (value in invalid_engines) {
    expect_error(calibration_unit_for_model(value),
                 "unknown model calibration unit")
  }
  for (value in list(factor("binomial"), 1, list("binomial"))) {
    expect_error(calibration_unit_for_model("glm", family = value),
                 "unknown model calibration unit")
  }

  invalid_endpoints <- list(
    factor("or"), 1, list("or"), NA_character_, character(), c("mean", "or")
  )
  for (value in invalid_endpoints) {
    expect_error(calibration_unit_for_tost(value),
                 "unknown TOST calibration endpoint")
  }
})

test_that("the installed registry contains no generic two_sample key", {
  registry <- load_calibration_registry()

  expect_false(any(registry$calibration_unit == "two_sample"))
  expect_true(all(c(
    "family", "calibration_unit", "endpoint", "conclusion_type",
    "status", "cutoff_fragile", "cutoff_robust", "version", "source",
    "supported_conditions"
  ) %in% names(registry)))
})

test_that("the installed registry exactly matches the active taxonomy", {
  registry <- load_calibration_registry()
  actual <- registry[c(
    "family", "calibration_unit", "endpoint", "conclusion_type", "status",
    "cutoff_fragile", "cutoff_robust", "version"
  )]
  expected <- data.frame(
    family = c(
      rep("continuous_parametric", 2), rep("rank_nonparametric", 3),
      rep("binary_proportion", 3), rep("linear_model", 2),
      rep("generalized_linear_model", 2), "survival",
      rep("equivalence_noninferiority", 6)
    ),
    calibration_unit = c(
      "welch_unpaired", "paired_t", "wilcoxon_rank_sum",
      "wilcoxon_signed_rank", "brunner_munzel", "fisher_exact",
      "chi_square_2x2", "two_sample_prop", "lm_ancova", "lm_ancova_v2",
      "glm_binomial", "glm_poisson", "cox_ph", "tost_mean", "tost_mean",
      "tost_risk_difference", "tost_risk_difference", "tost_odds_ratio",
      "tost_odds_ratio"
    ),
    endpoint = c(
      "mean_difference", "mean_difference", "location_shift",
      "location_shift", "location_shift", "risk_difference",
      "risk_difference", "risk_difference", "coefficient", "coefficient",
      "coefficient", "coefficient", "hazard_ratio", "mean_difference",
      "mean_difference", "risk_difference", "risk_difference", "odds_ratio",
      "odds_ratio"
    ),
    conclusion_type = c(
      rep("significant", 13), "equivalence", "noninferiority",
      "equivalence", "noninferiority", "equivalence", "noninferiority"
    ),
    status = c(
      "validated_method_specific", rep("uncalibrated", 4),
      "validated_method_specific", rep("uncalibrated", 13)
    ),
    cutoff_fragile = c(55, rep(NA_real_, 4), 58, rep(NA_real_, 13)),
    cutoff_robust = c(70, rep(NA_real_, 18)),
    version = c(
      "welch-2026-1", rep("taxonomy-2026-1", 4), "fisher-2026-1",
      rep("taxonomy-2026-1", 2), "lm-ancova-2026-1", "lm-ancova-v2-2026-1",
      rep("taxonomy-2026-1", 9)
    ),
    stringsAsFactors = FALSE
  )
  row.names(actual) <- NULL

  expect_identical(actual, expected)
})

test_that("active lm_ancova Gate B remains uncalibrated with auditable reason", {
  registry <- load_calibration_registry()
  ancova <- registry[registry$calibration_unit == "lm_ancova", , drop = FALSE]

  expect_identical(nrow(ancova), 1L)
  expect_identical(ancova$status, "uncalibrated")
  expect_identical(ancova$version, "lm-ancova-2026-1")
  expect_true(is.na(ancova$cutoff_fragile))
  expect_true(is.na(ancova$cutoff_robust))
  expect_match(ancova$source, "manuscript/calibration/studies/lm_ancova/published",
               fixed = TRUE)
  expect_match(ancova$supported_conditions, "no_feasible_thresholds", fixed = TRUE)
  expect_match(ancova$supported_conditions, "held-out not opened|held.out not opened",
               perl = TRUE)
  expect_match(ancova$supported_conditions, "labels? suppressed|categorical bands suppressed",
               perl = TRUE, ignore.case = TRUE)
  expect_match(ancova$supported_conditions,
               "Welch 55/70 is not an ANCOVA fallback|not an ANCOVA fallback",
               perl = TRUE)
  expect_match(ancova$supported_conditions,
               "9ccfc2fca7c0a07c19a3a18838e9a3f2",
               fixed = TRUE)

  resolved <- resolve_result_calibration(
    "lm_ancova", "coefficient", "significant",
    c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2), 0.30,
    analysis_profile = .canonical_profile_fixture()
  )
  expect_false(resolved$applicable)
  expect_identical(resolved$status, "uncalibrated")
  expect_true(all(is.na(c(resolved$cutoff_fragile, resolved$cutoff_robust))))
  expect_match(resolved$supported_conditions, "no_feasible_thresholds", fixed = TRUE)
  expect_true(is.na(score_label_from_calibration(80, resolved)))
  expect_true(is.na(score_label_from_calibration(60, resolved)))
  expect_true(is.na(score_label_from_calibration(40, resolved)))
})

test_that("active lm_ancova_v2 Gate B remains uncalibrated with auditable reason", {
  registry <- load_calibration_registry()
  ancova_v2 <- registry[registry$calibration_unit == "lm_ancova_v2", , drop = FALSE]

  expect_identical(nrow(ancova_v2), 1L)
  expect_identical(ancova_v2$status, "uncalibrated")
  expect_identical(ancova_v2$version, "lm-ancova-v2-2026-1")
  expect_true(is.na(ancova_v2$cutoff_fragile))
  expect_true(is.na(ancova_v2$cutoff_robust))
  expect_match(
    ancova_v2$source,
    "manuscript/calibration/studies/lm_ancova_v2/published",
    fixed = TRUE
  )
  expect_match(ancova_v2$supported_conditions, "no_feasible_thresholds",
               fixed = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "jackknife-light|fragility=0\\.5|jackknife = 0",
               perl = TRUE, ignore.case = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "two-band|Fragile/Not fragile|Fragile / Not fragile",
               perl = TRUE, ignore.case = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "held-out not opened|held.out not opened",
               perl = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "labels? suppressed|categorical bands suppressed",
               perl = TRUE, ignore.case = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "Welch 55/70 is not an ANCOVA fallback|not an ANCOVA fallback",
               perl = TRUE)
  expect_match(ancova_v2$supported_conditions,
               "3dc2a1f840b3eb725bea629dc130f070",
               fixed = TRUE)

  # v1 historical row must remain intact beside v2.
  ancova_v1 <- registry[registry$calibration_unit == "lm_ancova", , drop = FALSE]
  expect_identical(nrow(ancova_v1), 1L)
  expect_identical(ancova_v1$version, "lm-ancova-2026-1")

  resolved <- resolve_result_calibration(
    "lm_ancova_v2", "coefficient", "significant",
    c(jackknife = 0, fragility = 0.5, bootstrap = 0.5), 0.30,
    analysis_profile = .canonical_profile_fixture()
  )
  expect_false(resolved$applicable)
  expect_identical(resolved$status, "uncalibrated")
  expect_true(all(is.na(c(resolved$cutoff_fragile, resolved$cutoff_robust))))
  expect_match(resolved$supported_conditions, "no_feasible_thresholds",
               fixed = TRUE)
  expect_true(is.na(score_label_from_calibration(80, resolved)))
  expect_true(is.na(score_label_from_calibration(50, resolved)))
  expect_true(is.na(score_label_from_calibration(20, resolved)))

  # Default interactive lm resolution stays on historical v1 unit.
  expect_identical(calibration_unit_for_model("lm"), "lm_ancova")
})

test_that("the Welch row cites durable tracked provenance", {
  registry <- load_calibration_registry()
  welch <- registry[registry$calibration_unit == "welch_unpaired", ]

  expect_identical(
    welch$source,
    paste0(
      "git:f26e559f098efa9ba0fe6b143f419d076ffb50fc:",
      "manuscript/robustness_analysis_manuscript.md"
    )
  )
  expect_match(welch$supported_conditions, "Section 3", fixed = TRUE)
})

.valid_calibration_registry <- function() {
  data.frame(
    family = "continuous_parametric",
    calibration_unit = "welch_unpaired",
    endpoint = "mean_difference",
    conclusion_type = "significant",
    status = "validated_method_specific",
    cutoff_fragile = 55,
    cutoff_robust = 70,
    version = "welch-2026-1",
    source = paste0(
      "git:f26e559f098efa9ba0fe6b143f419d076ffb50fc:",
      "manuscript/robustness_analysis_manuscript.md"
    ),
    supported_conditions = "independent Welch comparison",
    stringsAsFactors = FALSE
  )
}

test_that("registry validation rejects duplicate compound keys", {
  registry <- .valid_calibration_registry()
  registry <- rbind(registry, registry)

  expect_error(validate_calibration_registry(registry),
               "duplicate calibration registry key")
})

test_that("registry validation rejects cutoffs on uncalibrated rows", {
  registry <- .valid_calibration_registry()
  registry$status <- "uncalibrated"

  expect_error(validate_calibration_registry(registry),
               "uncalibrated.*missing cutoffs")
})

test_that("registry validation requires validated provenance", {
  for (field in c("version", "source", "supported_conditions")) {
    registry <- .valid_calibration_registry()
    registry[[field]] <- ""

    expect_error(validate_calibration_registry(registry),
                 "validated rows require provenance")
  }
})

test_that("registry validation rejects unknown statuses", {
  registry <- .valid_calibration_registry()
  registry$status <- "experimental"

  expect_error(validate_calibration_registry(registry),
               "unknown calibration status")
})

test_that("registry validation rejects missing required columns", {
  registry <- .valid_calibration_registry()
  registry$source <- NULL

  expect_error(validate_calibration_registry(registry),
               "required columns")
})

test_that("registry validation rejects duplicate column names", {
  registry <- .valid_calibration_registry()
  registry$duplicate_family <- registry$family
  names(registry)[ncol(registry)] <- "family"

  expect_error(validate_calibration_registry(registry),
               "exactly the required columns")
})

test_that("registry validation rejects an empty registry", {
  registry <- .valid_calibration_registry()[0, ]

  expect_error(validate_calibration_registry(registry),
               "must contain at least one row")
})

test_that("active registry validation rejects incomplete taxonomies", {
  registry <- load_calibration_registry()

  expect_error(
    validate_active_calibration_registry(registry[-1, ]),
    "active calibration registry must exactly match"
  )
})

test_that("active registry validation locks family and Welch calibration", {
  registry <- load_calibration_registry()
  welch <- registry$calibration_unit == "welch_unpaired"
  mutations <- list(
    family = "two_sample",
    cutoff_fragile = 54,
    cutoff_robust = 71,
    version = "welch-untracked"
  )

  for (field in names(mutations)) {
    changed <- registry
    changed[welch, field] <- mutations[[field]]
    expect_error(
      validate_active_calibration_registry(changed),
      "active calibration registry must exactly match"
    )
  }
})

test_that("active registry validation locks fisher_exact Gate B calibration", {
  registry <- load_calibration_registry()
  fisher <- registry$calibration_unit == "fisher_exact"
  mutations <- list(
    status = "uncalibrated",
    cutoff_fragile = 57,
    cutoff_robust = 70,
    version = "fisher-untracked"
  )

  for (field in names(mutations)) {
    changed <- registry
    changed[fisher, field] <- mutations[[field]]
    if (identical(field, "status")) {
      changed$cutoff_fragile[fisher] <- NA_real_
      changed$cutoff_robust[fisher] <- NA_real_
    }
    expect_error(
      validate_active_calibration_registry(changed),
      "active calibration registry must exactly match"
    )
  }
})

test_that("registry validation requires finite ordered validated cutoffs", {
  registry <- .valid_calibration_registry()
  registry$cutoff_robust <- 55
  expect_error(validate_calibration_registry(registry),
               "finite ordered cutoffs")

  registry <- .valid_calibration_registry()
  registry$cutoff_fragile <- Inf
  expect_error(validate_calibration_registry(registry),
               "finite ordered cutoffs")
})

test_that("registry validation accepts two-band NA robust cutoffs", {
  registry <- .valid_calibration_registry()
  registry$cutoff_robust <- NA_real_
  expect_silent(validate_calibration_registry(registry))

  registry <- .valid_calibration_registry()
  registry$cutoff_fragile <- NA_real_
  registry$cutoff_robust <- NA_real_
  expect_error(validate_calibration_registry(registry),
               "finite ordered cutoffs")

  registry <- .valid_calibration_registry()
  registry$cutoff_fragile <- Inf
  registry$cutoff_robust <- NA_real_
  expect_error(validate_calibration_registry(registry),
               "finite ordered cutoffs")
})

test_that("load_calibration_registry validates an explicit path", {
  registry <- .valid_calibration_registry()
  path <- tempfile(fileext = ".csv")
  utils::write.csv(registry, path, row.names = FALSE, na = "")

  expect_identical(load_calibration_registry(path), registry)
})

test_that("registry loading rejects malformed cutoff text before coercion", {
  for (status in c("uncalibrated", "bands_not_applicable")) {
    registry <- .valid_calibration_registry()
    registry$status <- status
    registry$cutoff_fragile <- "not-a-cutoff"
    registry$cutoff_robust <- NA_character_
    path <- tempfile(fileext = ".csv")
    utils::write.csv(registry, path, row.names = FALSE, na = "")

    expect_error(
      load_calibration_registry(path),
      "calibration cutoffs must be numeric or missing"
    )
  }
})

test_that("only supported significant Welch results receive cutoffs", {
  supported <- resolve_result_calibration(
    calibration_unit = "welch_unpaired",
    endpoint = "mean_difference",
    conclusion_type = "significant",
    weights = c(jackknife = .4, fragility = .4, bootstrap = .2),
    max_removal_pct = .30
  )
  expect_true(supported$applicable)
  expect_identical(supported$status, "validated_method_specific")
  expect_equal(c(supported$cutoff_fragile, supported$cutoff_robust), c(55, 70))

  nonsig <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "non_significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_false(nonsig$applicable)
  expect_identical(nonsig$status, "bands_not_applicable")

  custom_weights <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "significant",
    c(jackknife = .5, fragility = .3, bootstrap = .2), .30
  )
  expect_false(custom_weights$applicable)
  expect_identical(custom_weights$status, "uncalibrated")
  expect_true(all(is.na(c(custom_weights$cutoff_fragile,
                          custom_weights$cutoff_robust))))
})

test_that("uncalibrated methods fail closed", {
  x <- resolve_result_calibration(
    "paired_t", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_false(x$applicable)
  expect_identical(x$status, "uncalibrated")
  expect_true(all(is.na(c(x$cutoff_fragile, x$cutoff_robust))))

  expect_identical(superiority_conclusion_type(TRUE), "significant")
  expect_identical(superiority_conclusion_type(FALSE), "non_significant")
  expect_identical(tost_conclusion_type("equivalence", TRUE), "equivalence")
  expect_identical(tost_conclusion_type("equivalence", FALSE),
                   "not_equivalent")
  expect_identical(tost_conclusion_type("noninferiority", TRUE),
                   "noninferiority")
  expect_identical(tost_conclusion_type("noninferiority", FALSE),
                   "not_non_inferior")
})

test_that("non-significant and unsuccessful conclusions are band-inapplicable", {
  cases <- list(
    c("welch_unpaired", "mean_difference", "non_significant"),
    c("tost_mean", "mean_difference", "not_equivalent"),
    c("tost_risk_difference", "risk_difference", "not_non_inferior"),
    c("tost_odds_ratio", "odds_ratio", "not_non_inferior")
  )
  for (case in cases) {
    result <- resolve_result_calibration(
      case[[1]], case[[2]], case[[3]],
      c(jackknife = .4, fragility = .4, bootstrap = .2), .30
    )
    expect_false(result$applicable)
    expect_identical(result$status, "bands_not_applicable")
    expect_true(all(is.na(c(result$cutoff_fragile, result$cutoff_robust))))
  }
})

test_that("score labels require applicable calibration", {
  calibrated <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_identical(score_label_from_calibration(80, calibrated), "Robust")
  expect_identical(score_label_from_calibration(60, calibrated),
                   "Moderately Robust")
  expect_identical(score_label_from_calibration(55, calibrated), "Fragile")

  uncalibrated <- resolve_result_calibration(
    "paired_t", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30
  )
  expect_true(is.na(score_label_from_calibration(80, uncalibrated)))
})

test_that("two-band calibration labels Fragile / Not fragile", {
  two_band <- list(
    applicable = TRUE,
    status = "validated_method_specific",
    cutoff_fragile = 58,
    cutoff_robust = NA_real_
  )
  expect_identical(score_label_from_calibration(58, two_band), "Fragile")
  expect_identical(score_label_from_calibration(58.5, two_band), "Not fragile")
  expect_identical(score_label_from_calibration(100, two_band), "Not fragile")
  expect_false(identical(
    score_label_from_calibration(100, two_band), "Robust"
  ))

  # Inf robust is not two-band; labeler keys on is.na, not !is.finite.
  inf_robust <- list(
    applicable = TRUE,
    status = "validated_method_specific",
    cutoff_fragile = 58,
    cutoff_robust = Inf
  )
  expect_true(is.na(score_label_from_calibration(80, inf_robust)))
})

test_that("caller-supplied malformed registries fail closed", {
  malformed <- .valid_calibration_registry()
  malformed$cutoff_fragile <- NA_real_

  result <- resolve_result_calibration(
    "welch_unpaired", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30,
    registry = malformed
  )
  expect_false(result$applicable)
  expect_identical(result$status, "uncalibrated")
  expect_true(all(is.na(c(result$cutoff_fragile, result$cutoff_robust))))

  expect_false(resolve_result_calibration(
    "welch_unpaired", "mean_difference", "significant",
    c(jackknife = .4, fragility = .4, bootstrap = .2), .30,
    registry = data.frame(not_a_registry = 1)
  )$applicable)
})

# --- proportion profile plumbing + Gate B activation -------------------------
# Phase 1 fisher_exact is calibrated under jackknife-light weights
# (fragility=0.5, bootstrap=0.5, jackknife=0). Default 0.4/0.4/0.2 scores remain
# numeric-only: labels require the explicit calibrated weight design.

.fisher_exact_weights <- c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
.default_score_weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

.canonical_prop_profile <- function(...) {
  utils::modifyList(list(
    version = "prop-profile-1", calibration_unit = "fisher_exact",
    canonical_fisher = TRUE, two_arm_individual_level = TRUE,
    complete_cases = TRUE, n1 = 100L, n2 = 100L, allocation_ratio = 1,
    events1 = 45L, non_events1 = 55L, rate1 = 0.45,
    events2 = 20L, non_events2 = 80L, rate2 = 0.20,
    alpha = 0.05, n_boot = 1000L, correct = TRUE,
    weights = .fisher_exact_weights,
    max_removal_pct = 0.30
  ), list(...))
}

.validated_fisher_exact_registry <- function() {
  registry <- load_calibration_registry()
  idx <- registry$calibration_unit == "fisher_exact"
  registry$status[idx] <- "validated_method_specific"
  registry$cutoff_fragile[idx] <- 58
  registry$cutoff_robust[idx] <- NA_real_
  registry$version[idx] <- "fisher-2026-1"
  registry$source[idx] <- "study:binary_proportion@cc3344931614"
  registry$supported_conditions[idx] <-
    "canonical significant two-arm fisher_exact (binary_proportion Phase 1)"
  registry
}

test_that("validated fisher_exact applies only to a complete canonical profile", {
  registry <- .validated_fisher_exact_registry()
  weights <- .fisher_exact_weights

  eligible <- resolve_result_calibration(
    "fisher_exact", "risk_difference", "significant",
    weights, 0.30,
    registry = registry,
    analysis_profile = .canonical_prop_profile()
  )
  expect_true(eligible$applicable)
  expect_identical(eligible$status, "validated_method_specific")
  expect_equal(eligible$cutoff_fragile, 58)
  expect_true(is.na(eligible$cutoff_robust))
  expect_identical(eligible$version, "fisher-2026-1")

  inapplicable <- list(
    missing_profile = NULL,
    wrong_version = .canonical_prop_profile(version = "other"),
    wrong_unit = .canonical_prop_profile(calibration_unit = "chi_square_2x2"),
    noncanonical = .canonical_prop_profile(canonical_fisher = FALSE),
    not_two_arm = .canonical_prop_profile(two_arm_individual_level = FALSE),
    incomplete = .canonical_prop_profile(complete_cases = FALSE),
    n1_small = .canonical_prop_profile(n1 = 24L, events1 = 5L,
                                        non_events1 = 19L, rate1 = 5 / 24),
    n1_large = .canonical_prop_profile(n1 = 201L, events1 = 90L,
                                        non_events1 = 111L, rate1 = 90 / 201),
    n2_small = .canonical_prop_profile(n2 = 24L, events2 = 5L,
                                        non_events2 = 19L, rate2 = 5 / 24),
    unbalanced = .canonical_prop_profile(n1 = 120L, n2 = 60L,
                                          allocation_ratio = 2),
    control_rate_low = .canonical_prop_profile(
      events2 = 2L, non_events2 = 98L, rate2 = 0.02),
    control_rate_high = .canonical_prop_profile(
      events2 = 102L, non_events2 = 18L, rate2 = 0.85),
    control_events_lt3 = .canonical_prop_profile(
      events2 = 2L, non_events2 = 98L, rate2 = 0.02),
    alpha = .canonical_prop_profile(alpha = 0.01),
    n_boot = .canonical_prop_profile(n_boot = 500L),
    weights = .canonical_prop_profile(weights = .default_score_weights),
    removal = .canonical_prop_profile(max_removal_pct = 0.20)
  )

  for (name in names(inapplicable)) {
    result <- resolve_result_calibration(
      "fisher_exact", "risk_difference", "significant",
      weights, 0.30,
      registry = registry,
      analysis_profile = inapplicable[[name]]
    )
    expect_false(result$applicable, info = name)
    expect_true(is.na(result$cutoff_fragile), info = name)
    expect_true(is.na(result$cutoff_robust), info = name)
  }
})

test_that("fisher_exact profile bounds use the frozen edges inclusively", {
  registry <- .validated_fisher_exact_registry()
  weights <- .fisher_exact_weights
  probe <- function(profile) {
    resolve_result_calibration(
      "fisher_exact", "risk_difference", "significant",
      weights, 0.30, registry = registry, analysis_profile = profile
    )$applicable
  }

  # Frozen bounds: per-arm n in [25, 200]; allocation ratio in [0.8, 1.25];
  # observed control-arm rate in [0.08, 0.55] with >= 3 events and >= 3
  # non-events in the control arm.  All edges below are the boundary values
  # themselves and must remain eligible.
  expect_true(probe(.canonical_prop_profile(n1 = 25L, events1 = 8L,
                                             non_events1 = 17L, rate1 = 8 / 25)))
  expect_true(probe(.canonical_prop_profile(n1 = 200L)))
  expect_true(probe(.canonical_prop_profile(
    n2 = 100L, events2 = 8L, non_events2 = 92L, rate2 = 0.08)))
  expect_true(probe(.canonical_prop_profile(
    n2 = 100L, events2 = 55L, non_events2 = 45L, rate2 = 0.55)))
  expect_true(probe(.canonical_prop_profile(
    n1 = 125L, n2 = 100L, allocation_ratio = 1.25)))
  expect_true(probe(.canonical_prop_profile(
    n1 = 80L, n2 = 100L, allocation_ratio = 0.8)))
})

test_that("active fisher_exact Gate B applies only under jackknife-light weights", {
  # Canonical profile + explicit calibrated weights → applicable two-band.
  activated <- resolve_result_calibration(
    "fisher_exact", "risk_difference", "significant",
    .fisher_exact_weights, 0.30,
    analysis_profile = .canonical_prop_profile()
  )
  expect_true(activated$applicable)
  expect_identical(activated$status, "validated_method_specific")
  expect_equal(activated$cutoff_fragile, 58)
  expect_true(is.na(activated$cutoff_robust))
  expect_identical(activated$version, "fisher-2026-1")
  expect_identical(activated$source, "study:binary_proportion@cc3344931614")

  # Default interactive weights remain suppressed even with a canonical profile.
  default_weights <- resolve_result_calibration(
    "fisher_exact", "risk_difference", "significant",
    .default_score_weights, 0.30,
    analysis_profile = .canonical_prop_profile(weights = .default_score_weights)
  )
  expect_false(default_weights$applicable)
  expect_identical(default_weights$status, "uncalibrated")
  expect_true(all(is.na(c(
    default_weights$cutoff_fragile, default_weights$cutoff_robust
  ))))

  # Non-canonical profile suppresses even under calibrated weights.
  noncanonical <- resolve_result_calibration(
    "fisher_exact", "risk_difference", "significant",
    .fisher_exact_weights, 0.30,
    analysis_profile = .canonical_prop_profile(canonical_fisher = FALSE)
  )
  expect_false(noncanonical$applicable)
  expect_true(all(is.na(c(
    noncanonical$cutoff_fragile, noncanonical$cutoff_robust
  ))))
})

test_that("Welch resolution ignores analysis_profile", {
  supported <- resolve_result_calibration(
    calibration_unit = "welch_unpaired",
    endpoint = "mean_difference",
    conclusion_type = "significant",
    weights = c(jackknife = .4, fragility = .4, bootstrap = .2),
    max_removal_pct = .30,
    analysis_profile = .canonical_prop_profile()
  )
  expect_true(supported$applicable)
  expect_identical(supported$status, "validated_method_specific")
})

test_that("malformed calibration metadata suppresses score labels", {
  malformed <- list(
    list(applicable = TRUE, status = NULL,
         cutoff_fragile = 55, cutoff_robust = 70),
    list(applicable = TRUE, status = "uncalibrated",
         cutoff_fragile = 55, cutoff_robust = 70),
    list(applicable = TRUE, status = "bands_not_applicable",
         cutoff_fragile = 55, cutoff_robust = 70),
    list(applicable = TRUE, status = "not_a_status",
         cutoff_fragile = 55, cutoff_robust = 70),
    list(applicable = TRUE, status = "validated_method_specific",
         cutoff_fragile = NA_real_, cutoff_robust = 70),
    list(applicable = TRUE, status = "validated_method_specific",
         cutoff_fragile = 55, cutoff_robust = Inf),
    list(applicable = TRUE, status = "validated_method_specific",
         cutoff_fragile = c(55, 56), cutoff_robust = 70),
    list(applicable = TRUE, status = "validated_method_specific",
         cutoff_fragile = 70, cutoff_robust = 55),
    NULL
  )
  for (calibration in malformed) {
    expect_true(is.na(score_label_from_calibration(80, calibration)))
  }
})

.canonical_profile_fixture <- function(...) {
  utils::modifyList(list(
    version = "lm-profile-1", canonical_ancova = TRUE,
    term_type = "single", term_df = 1L, treatment_levels = 2L,
    baseline_count = 1L, response_numeric = TRUE, baseline_numeric = TRUE,
    additive_direct_terms = TRUE, omitted_rows = FALSE, n = 80L,
    alpha = 0.05, n_boot = 1000L,
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    max_removal_pct = 0.30
  ), list(...))
}

.validated_lm_ancova_registry <- function() {
  registry <- load_calibration_registry()
  idx <- registry$calibration_unit == "lm_ancova"
  registry$status[idx] <- "validated_method_specific"
  registry$cutoff_fragile[idx] <- 50
  registry$cutoff_robust[idx] <- 65
  registry$version[idx] <- "lm-ancova-fixture-1"
  registry$source[idx] <- "fixture:validated-lm-ancova"
  registry$supported_conditions[idx] <- "canonical significant 1-df ANCOVA fixture"
  registry
}

test_that("validated lm_ancova applies only to a complete canonical profile", {
  registry <- .validated_lm_ancova_registry()
  weights <- c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2)

  eligible <- resolve_result_calibration(
    "lm_ancova", "coefficient", "significant",
    weights, 0.30,
    registry = registry,
    analysis_profile = .canonical_profile_fixture()
  )
  expect_true(eligible$applicable)
  expect_identical(eligible$status, "validated_method_specific")
  expect_equal(c(eligible$cutoff_fragile, eligible$cutoff_robust), c(50, 65))

  inapplicable <- list(
    missing_profile = NULL,
    joint_term = .canonical_profile_fixture(term_type = "joint", term_df = 2L,
                                            canonical_ancova = FALSE),
    noncanonical = .canonical_profile_fixture(canonical_ancova = FALSE),
    small_n = .canonical_profile_fixture(n = 39L),
    large_n = .canonical_profile_fixture(n = 241L),
    alpha = .canonical_profile_fixture(alpha = 0.01),
    n_boot = .canonical_profile_fixture(n_boot = 500L),
    weights = .canonical_profile_fixture(
      weights = c(jackknife = 0.5, fragility = 0.3, bootstrap = 0.2)
    ),
    removal = .canonical_profile_fixture(max_removal_pct = 0.20)
  )

  for (name in names(inapplicable)) {
    result <- resolve_result_calibration(
      "lm_ancova", "coefficient", "significant",
      weights, 0.30,
      registry = registry,
      analysis_profile = inapplicable[[name]]
    )
    expect_false(result$applicable, info = name)
    expect_true(is.na(result$cutoff_fragile), info = name)
    expect_true(is.na(result$cutoff_robust), info = name)
  }
})

test_that("Welch resolution ignores analysis_profile", {
  supported <- resolve_result_calibration(
    calibration_unit = "welch_unpaired",
    endpoint = "mean_difference",
    conclusion_type = "significant",
    weights = c(jackknife = .4, fragility = .4, bootstrap = .2),
    max_removal_pct = .30,
    analysis_profile = .canonical_profile_fixture()
  )
  expect_true(supported$applicable)
  expect_identical(supported$status, "validated_method_specific")
})
