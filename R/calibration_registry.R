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
