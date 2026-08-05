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
      rep("binary_proportion", 3), "linear_model",
      rep("generalized_linear_model", 2), "survival",
      rep("equivalence_noninferiority", 6)
    ),
    calibration_unit = c(
      "welch_unpaired", "paired_t", "wilcoxon_rank_sum",
      "wilcoxon_signed_rank", "brunner_munzel", "fisher_exact",
      "chi_square_2x2", "two_sample_prop", "lm_ancova", "glm_binomial",
      "glm_poisson", "cox_ph", "tost_mean", "tost_mean",
      "tost_risk_difference", "tost_risk_difference", "tost_odds_ratio",
      "tost_odds_ratio"
    ),
    endpoint = c(
      "mean_difference", "mean_difference", "location_shift",
      "location_shift", "location_shift", "risk_difference",
      "risk_difference", "risk_difference", "coefficient", "coefficient",
      "coefficient", "hazard_ratio", "mean_difference", "mean_difference",
      "risk_difference", "risk_difference", "odds_ratio", "odds_ratio"
    ),
    conclusion_type = c(
      rep("significant", 12), "equivalence", "noninferiority",
      "equivalence", "noninferiority", "equivalence", "noninferiority"
    ),
    status = c("validated_method_specific", rep("uncalibrated", 17)),
    cutoff_fragile = c(55, rep(NA_real_, 17)),
    cutoff_robust = c(70, rep(NA_real_, 17)),
    version = c("welch-2026-1", rep("taxonomy-2026-1", 17)),
    stringsAsFactors = FALSE
  )
  row.names(actual) <- NULL

  expect_identical(actual, expected)
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
