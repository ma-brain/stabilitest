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
  if (length(test_type) != 1L || is.na(test_type) ||
      !test_type %in% names(units)) {
    stop("unknown calibration test type", call. = FALSE)
  }
  unname(units[[test_type]])
}

calibration_unit_for_model <- function(engine, family = NULL) {
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
  if (length(endpoint) != 1L || is.na(endpoint) ||
      !endpoint %in% names(units)) {
    stop("unknown TOST calibration endpoint", call. = FALSE)
  }
  unname(units[[endpoint]])
}

validate_calibration_registry <- function(registry) {
  if (!is.data.frame(registry) ||
      !setequal(names(registry), .calibration_registry_columns)) {
    stop(
      paste(
        "calibration registry must contain exactly the required columns:",
        paste(.calibration_registry_columns, collapse = ", ")
      ),
      call. = FALSE
    )
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

.parse_calibration_cutoff <- function(values) {
  missing <- is.na(values) | !nzchar(trimws(as.character(values)))
  parsed <- suppressWarnings(as.numeric(as.character(values)))
  if (any(!missing & is.na(parsed))) {
    stop("calibration cutoffs must be numeric or missing", call. = FALSE)
  }
  parsed
}

load_calibration_registry <- function(path = NULL) {
  if (is.null(path)) {
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
  registry
}
