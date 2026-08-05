# Method-specific calibration registry helpers.
#
# These functions are intentionally internal. Public analysis functions use
# them to identify the exact calibration unit without exposing registry
# implementation details as part of the package API.

.calibration_statuses <- c(
  "validated_method_specific", "uncalibrated", "bands_not_applicable"
)

.calibration_registry_columns <- c(
  "family", "calibration_unit", "endpoint", "conclusion_type", "status",
  "cutoff_fragile", "cutoff_robust", "version", "source",
  "supported_conditions"
)

.is_scalar_character <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value)
}

calibration_unit_for_test <- function(test_type) {
  units <- c(
    "t.test" = "welch_unpaired",
    "paired.t.test" = "paired_t",
    "wilcoxon" = "wilcoxon_rank_sum",
    "wilcoxon.paired" = "wilcoxon_signed_rank",
    "brunner_munzel" = "brunner_munzel",
    "fisher" = "fisher_exact",
    "chisq" = "chi_square_2x2",
    "prop" = "two_sample_prop"
  )
  if (!.is_scalar_character(test_type) ||
      !test_type %in% names(units)) {
    stop("unknown calibration test type", call. = FALSE)
  }
  unname(units[[test_type]])
}

calibration_unit_for_model <- function(engine, family = NULL) {
  if (!.is_scalar_character(engine) ||
      (!is.null(family) && !.is_scalar_character(family))) {
    stop("unknown model calibration unit", call. = FALSE)
  }
  if (identical(engine, "lm")) return("lm_ancova")
  if (identical(engine, "cox")) return("cox_ph")
  if (identical(engine, "glm") && identical(family, "binomial")) {
    return("glm_binomial")
  }
  if (identical(engine, "glm") && identical(family, "poisson")) {
    return("glm_poisson")
  }
  stop("unknown model calibration unit", call. = FALSE)
}

calibration_unit_for_tost <- function(endpoint) {
  units <- c(
    mean = "tost_mean",
    prop = "tost_risk_difference",
    or = "tost_odds_ratio"
  )
  if (!.is_scalar_character(endpoint) ||
      !endpoint %in% names(units)) {
    stop("unknown TOST calibration endpoint", call. = FALSE)
  }
  unname(units[[endpoint]])
}

validate_calibration_registry <- function(registry) {
  if (!is.data.frame(registry) ||
      length(names(registry)) != length(.calibration_registry_columns) ||
      anyDuplicated(names(registry)) ||
      !setequal(names(registry), .calibration_registry_columns)) {
    stop(
      paste(
        "calibration registry must contain exactly the required columns:",
        paste(.calibration_registry_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (nrow(registry) == 0L) {
    stop("calibration registry must contain at least one row", call. = FALSE)
  }

  key_columns <- c("calibration_unit", "endpoint", "conclusion_type")
  missing_key <- vapply(
    registry[key_columns],
    function(x) any(is.na(x) | !nzchar(trimws(as.character(x)))),
    logical(1)
  )
  if (any(missing_key)) {
    stop("calibration registry keys must be non-missing", call. = FALSE)
  }

  compound_key <- do.call(paste, c(registry[key_columns], sep = "\r"))
  if (anyDuplicated(compound_key)) {
    stop("duplicate calibration registry key", call. = FALSE)
  }

  if (anyNA(registry$status) ||
      any(!registry$status %in% .calibration_statuses)) {
    stop("unknown calibration status", call. = FALSE)
  }

  cutoff_columns <- c("cutoff_fragile", "cutoff_robust")
  if (!all(vapply(registry[cutoff_columns], is.numeric, logical(1)))) {
    stop("calibration cutoffs must be numeric", call. = FALSE)
  }

  no_bands <- registry$status %in% c("uncalibrated", "bands_not_applicable")
  has_cutoff <- !is.na(registry$cutoff_fragile) |
    !is.na(registry$cutoff_robust)
  if (any(no_bands & has_cutoff)) {
    stop(
      "uncalibrated and inapplicable rows must have missing cutoffs",
      call. = FALSE
    )
  }

  validated <- registry$status == "validated_method_specific"
  ordered_cutoffs <- is.finite(registry$cutoff_fragile) &
    is.finite(registry$cutoff_robust) &
    registry$cutoff_fragile < registry$cutoff_robust
  if (any(validated & !ordered_cutoffs)) {
    stop("validated rows require finite ordered cutoffs", call. = FALSE)
  }

  provenance_columns <- c("version", "source", "supported_conditions")
  complete_provenance <- vapply(
    seq_len(nrow(registry)),
    function(i) {
      values <- unlist(registry[i, provenance_columns], use.names = FALSE)
      all(!is.na(values) & nzchar(trimws(as.character(values))))
    },
    logical(1)
  )
  if (any(validated & !complete_provenance)) {
    stop("validated rows require provenance, version, and source", call. = FALSE)
  }

  invisible(registry)
}

.active_calibration_registry_contract <- data.frame(
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

validate_active_calibration_registry <- function(registry) {
  validate_calibration_registry(registry)
  contract_columns <- names(.active_calibration_registry_contract)
  actual <- registry[contract_columns]
  expected <- .active_calibration_registry_contract
  sort_rows <- function(x) {
    keys <- x[c("calibration_unit", "endpoint", "conclusion_type")]
    x[do.call(order, keys), , drop = FALSE]
  }
  actual <- sort_rows(actual)
  expected <- sort_rows(expected)
  row.names(actual) <- NULL
  row.names(expected) <- NULL

  if (!identical(actual, expected)) {
    stop(
      "active calibration registry must exactly match the approved taxonomy",
      call. = FALSE
    )
  }
  invisible(registry)
}

.parse_calibration_cutoff <- function(values) {
  missing <- is.na(values) | !nzchar(trimws(as.character(values)))
  parsed <- suppressWarnings(as.numeric(as.character(values)))
  if (any(!missing & is.na(parsed))) {
    stop("calibration cutoffs must be numeric or missing", call. = FALSE)
  }
  parsed
}

load_calibration_registry <- function(path = NULL) {
  active_registry <- is.null(path)
  if (active_registry) {
    path <- system.file(
      "extdata", "calibration-registry.csv", package = "stabilitest"
    )
  }
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path)) {
    stop("calibration registry file not found", call. = FALSE)
  }

  registry <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  registry$cutoff_fragile <- .parse_calibration_cutoff(
    registry$cutoff_fragile
  )
  registry$cutoff_robust <- .parse_calibration_cutoff(
    registry$cutoff_robust
  )
  validate_calibration_registry(registry)
  if (active_registry) validate_active_calibration_registry(registry)
  registry
}

# Return the observed superiority conclusion in the vocabulary used by the
# registry.  Keeping this conversion in one place prevents a logical result
# from being mistaken for a calibration key downstream.
superiority_conclusion_type <- function(significant) {
  if (!is.logical(significant) || length(significant) != 1L ||
      is.na(significant)) {
    stop("significant must be a single non-missing logical", call. = FALSE)
  }
  if (isTRUE(significant)) "significant" else "non_significant"
}

# Return the observed TOST/NI conclusion in the vocabulary used by the
# registry.  Failed superiority/equivalence decisions are intentionally kept
# distinct so they can be marked band-inapplicable rather than treated as
# an uncalibrated success.
tost_conclusion_type <- function(type, successful) {
  if (!.is_scalar_character(type) ||
      !type %in% c("equivalence", "noninferiority", "non_inferiority",
                   "non-inferiority")) {
    stop("unknown TOST conclusion type", call. = FALSE)
  }
  if (!is.logical(successful) || length(successful) != 1L ||
      is.na(successful)) {
    stop("successful must be a single non-missing logical", call. = FALSE)
  }
  canonical <- if (identical(type, "equivalence")) {
    "equivalence"
  } else {
    "noninferiority"
  }
  if (isTRUE(successful)) canonical else if (canonical == "equivalence") {
    "not_equivalent"
  } else {
    "not_non_inferior"
  }
}

# Descriptive aliases used by callers that prefer the longer mapping-helper
# names.  They remain internal implementation details (the public dispatcher
# is unchanged).
conclusion_type_for_superiority <- superiority_conclusion_type
conclusion_type_for_tost <- tost_conclusion_type

.uncalibrated_result <- function(unit, endpoint, conclusion, status,
                                 reason) {
  if (!.is_scalar_character(unit)) unit <- NA_character_
  if (!.is_scalar_character(endpoint)) endpoint <- NA_character_
  if (!.is_scalar_character(conclusion)) conclusion <- NA_character_
  if (!.is_scalar_character(status) || !status %in% .calibration_statuses) {
    status <- "uncalibrated"
  }
  if (!.is_scalar_character(reason)) reason <- "No applicable calibration"
  list(
    version = "taxonomy-2026-1",
    family = NA_character_,
    calibration_unit = unit,
    endpoint = endpoint,
    conclusion_type = conclusion,
    status = status,
    applicable = FALSE,
    cutoff_fragile = NA_real_,
    cutoff_robust = NA_real_,
    source = NA_character_,
    supported_conditions = reason
  )
}

.registry_result <- function(row, status = row$status, applicable = FALSE,
                             reason = NULL) {
  reason <- if (is.null(reason)) row$supported_conditions else reason
  list(
    version = as.character(row$version),
    family = as.character(row$family),
    calibration_unit = as.character(row$calibration_unit),
    endpoint = as.character(row$endpoint),
    conclusion_type = as.character(row$conclusion_type),
    status = as.character(status),
    applicable = isTRUE(applicable),
    cutoff_fragile = if (isTRUE(applicable)) as.numeric(row$cutoff_fragile) else NA_real_,
    cutoff_robust = if (isTRUE(applicable)) as.numeric(row$cutoff_robust) else NA_real_,
    source = as.character(row$source),
    supported_conditions = as.character(reason)
  )
}

.canonical_conclusion_type <- function(conclusion) {
  if (!.is_scalar_character(conclusion)) return(NA_character_)
  switch(
    conclusion,
    significant = "significant",
    non_significant = "non_significant",
    `non-significant` = "non_significant",
    equivalence = "equivalence",
    equivalent = "equivalence",
    not_equivalence = "not_equivalent",
    not_equivalent = "not_equivalent",
    `not-equivalent` = "not_equivalent",
    noninferiority = "noninferiority",
    non_inferiority = "noninferiority",
    `non-inferiority` = "noninferiority",
    non_inferior = "noninferiority",
    `non-inferior` = "noninferiority",
    not_noninferiority = "not_non_inferior",
    `not-noninferiority` = "not_non_inferior",
    not_non_inferior = "not_non_inferior",
    `not-non-inferior` = "not_non_inferior",
    NA_character_
  )
}

.default_calibration_weights <- c(
  jackknife = 0.4, fragility = 0.4, bootstrap = 0.2
)

.is_default_calibration_design <- function(weights, max_removal_pct) {
  weights_ok <- is.numeric(weights) && length(weights) == 3L &&
    !is.null(names(weights)) && !anyNA(names(weights)) &&
    !anyDuplicated(names(weights)) &&
    setequal(names(weights), names(.default_calibration_weights)) &&
    all(is.finite(weights)) &&
    isTRUE(all.equal(
      unname(weights[names(.default_calibration_weights)]),
      unname(.default_calibration_weights), tolerance = 1e-8
    ))
  removal_ok <- is.numeric(max_removal_pct) &&
    length(max_removal_pct) == 1L && is.finite(max_removal_pct) &&
    isTRUE(all.equal(as.numeric(max_removal_pct), 0.30, tolerance = 1e-8))
  weights_ok && removal_ok
}

# Resolve a result to exactly one registry row.  Resolution is deliberately
# fail-closed: only the narrow validated Welch configuration receives cutoffs;
# every other method/configuration retains scores but has no categorical bands.
resolve_result_calibration <- function(calibration_unit, endpoint,
                                       conclusion_type, weights,
                                       max_removal_pct,
                                       registry = NULL) {
  unit <- if (.is_scalar_character(calibration_unit)) {
    calibration_unit
  } else {
    NA_character_
  }
  endpoint_value <- if (.is_scalar_character(endpoint)) endpoint else NA_character_
  conclusion <- .canonical_conclusion_type(conclusion_type)

  if (is.null(registry)) {
    registry <- tryCatch(load_calibration_registry(), error = function(e) NULL)
  }
  if (is.null(registry)) {
    return(.uncalibrated_result(
      unit, endpoint_value, conclusion,
      "uncalibrated", "Calibration registry could not be loaded"
    ))
  }
  registry_error <- tryCatch({
    validate_calibration_registry(registry)
    NULL
  }, error = function(e) e)
  if (inherits(registry_error, "error")) {
    return(.uncalibrated_result(
      unit, endpoint_value, conclusion,
      "uncalibrated", "Calibration registry failed validation"
    ))
  }

  # A non-significant superiority result, or an unsuccessful equivalence/NI
  # result, has no robustness band by definition.  It is not a missing row.
  if (identical(conclusion, "non_significant") ||
      identical(conclusion, "not_equivalent") ||
      identical(conclusion, "not_non_inferior")) {
    return(.uncalibrated_result(
      unit, endpoint_value, conclusion,
      "bands_not_applicable", "Observed conclusion is not eligible for bands"
    ))
  }

  if (is.na(unit) || is.na(endpoint_value) || is.na(conclusion)) {
    return(.uncalibrated_result(
      unit, endpoint_value, conclusion,
      "uncalibrated", "Malformed calibration key"
    ))
  }

  matches <- registry[
    registry$calibration_unit == unit &
      registry$endpoint == endpoint_value &
      registry$conclusion_type == conclusion,
    , drop = FALSE
  ]
  if (nrow(matches) != 1L) {
    return(.uncalibrated_result(
      unit, endpoint_value, conclusion,
      "uncalibrated", "No exact method-specific calibration entry"
    ))
  }
  row <- matches[1L, , drop = FALSE]

  if (!identical(as.character(row$status), "validated_method_specific")) {
    return(.registry_result(row, status = as.character(row$status),
                            applicable = FALSE))
  }
  if (!.is_default_calibration_design(weights, max_removal_pct)) {
    result <- .registry_result(
      row, status = "uncalibrated", applicable = FALSE,
      reason = "Observed score configuration is outside the validated design"
    )
    result$version <- "taxonomy-2026-1"
    result$source <- NA_character_
    return(result)
  }

  .registry_result(row, status = "validated_method_specific", applicable = TRUE)
}

score_label_from_calibration <- function(score, calibration) {
  if (!is.list(calibration) || !isTRUE(calibration$applicable) ||
      !is.numeric(score) || length(score) != 1L || !is.finite(score)) {
    return(NA_character_)
  }
  cutoff_fragile <- calibration$cutoff_fragile
  cutoff_robust <- calibration$cutoff_robust
  if (!is.numeric(cutoff_fragile) || length(cutoff_fragile) != 1L ||
      !is.finite(cutoff_fragile) ||
      !is.numeric(cutoff_robust) || length(cutoff_robust) != 1L ||
      !is.finite(cutoff_robust) || cutoff_fragile >= cutoff_robust) {
    return(NA_character_)
  }
  if (score > cutoff_robust) return("Robust")
  if (score > cutoff_fragile) return("Moderately Robust")
  "Fragile"
}

# Attach method-specific calibration metadata after a model/TOST wrapper has
# identified its exact endpoint and observed conclusion.  The shared scoring
# engine deliberately does not infer categorical bands: model and TOST methods
# are currently uncalibrated, while unsuccessful conclusions are inapplicable.
attach_result_calibration <- function(out, calibration_unit, endpoint,
                                      conclusion_type) {
  if (!is.list(out)) {
    stop("result must be a list", call. = FALSE)
  }
  calibration <- resolve_result_calibration(
    calibration_unit = calibration_unit,
    endpoint = endpoint,
    conclusion_type = conclusion_type,
    weights = out$weights,
    max_removal_pct = out$max_removal_pct
  )
  out$calibration <- calibration
  out$interpretation_label <- score_label_from_calibration(
    out$metrics$overall_robustness, calibration
  )
  align_robustness_result_aliases(out, style = "model")
}
