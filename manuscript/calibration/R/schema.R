# Common schemas for calibration scenario registries and replicate artifacts.

CALIBRATION_SCENARIO_COLUMNS <- c(
  "scenario_id", "analysis_family", "endpoint", "design_layer",
  "data_generator", "primary_adapter", "robustness_adapter", "truth_class",
  "target_conclusion", "sample_size", "n_boot", "max_removal_pct",
  "training_split", "scenario_seed", "parameters"
)

CALIBRATION_REPLICATE_COLUMNS <- c(
  "scenario_id", "replicate_id", "analysis_family", "endpoint", "design_layer",
  "truth_class", "target_conclusion", "screening_conclusion", "selected",
  "analysis_conclusion", "original_p", "effective_p", "jackknife_stability",
  "fragility_component", "fragility_k", "fragility_pct",
  "bootstrap_reproducibility", "overall_score", "assigned_label", "n",
  "replicate_seed", "bootstrap_seed", "runtime_seconds", "status",
  "failure_stage", "failure_class", "failure_message"
)

.schema_abort <- function(message) {
  stop(message, call. = FALSE)
}

.require_exact_columns <- function(x, required, artifact_name) {
  if (!is.data.frame(x)) {
    .schema_abort(sprintf("%s must be a data frame or tibble", artifact_name))
  }

  missing <- setdiff(required, names(x))
  extra <- setdiff(names(x), required)
  if (length(missing) > 0L) {
    .schema_abort(sprintf(
      "missing required %s columns: %s",
      artifact_name,
      paste(missing, collapse = ", ")
    ))
  }
  if (length(extra) > 0L) {
    .schema_abort(sprintf(
      "unexpected %s columns: %s",
      artifact_name,
      paste(extra, collapse = ", ")
    ))
  }
  if (anyDuplicated(names(x))) {
    .schema_abort(sprintf("%s columns must have unique names", artifact_name))
  }
}

.is_scalar_integer <- function(x, positive = FALSE) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    floor(x) == x && (!positive || x > 0)
}

.is_missing_value <- function(value) {
  if (is.null(value)) {
    return(TRUE)
  }
  if (is.list(value)) {
    return(all(vapply(value, .is_missing_value, logical(1))))
  }
  missing <- is.na(value)
  if (is.numeric(value)) {
    missing <- missing & !is.nan(value)
  }
  all(missing)
}

.is_optional_nonnegative_integer <- function(value) {
  if (length(value) != 1L) {
    return(FALSE)
  }
  if (is.na(value)) {
    return(!(is.numeric(value) && is.nan(value)))
  }
  .is_scalar_integer(value, positive = FALSE) && value >= 0
}

.assert_numeric_range <- function(x, name, lower = -Inf, upper = Inf,
                                  lower_open = FALSE, upper_open = FALSE,
                                  rows = seq_along(x)) {
  if (!is.numeric(x)) {
    .schema_abort(sprintf("%s must be numeric", name))
  }
  values <- x[rows]
  lower_bad <- if (lower_open) values <= lower else values < lower
  upper_bad <- if (upper_open) values >= upper else values > upper
  if (any(!is.finite(values)) || any(lower_bad | upper_bad)) {
    .schema_abort(sprintf("%s must contain finite values in the required range", name))
  }
}

#' Construct one row in the common completed-replicate schema.
#'
#' Values are intentionally not coerced.  Every supplied field must be a
#' scalar, except that a length-one list is retained as a list-column element.
#' Omitted fields are explicit `NA` values and unknown fields are rejected.
new_calibration_replicate <- function(...) {
  values <- list(...)
  value_names <- names(values)
  if (is.null(value_names) || anyNA(value_names) || any(!nzchar(value_names))) {
    .schema_abort("new_calibration_replicate requires named values")
  }
  if (anyDuplicated(value_names)) {
    .schema_abort("new_calibration_replicate values must have unique names")
  }

  unknown <- setdiff(value_names, CALIBRATION_REPLICATE_COLUMNS)
  if (length(unknown) > 0L) {
    .schema_abort(sprintf(
      "unknown calibration replicate columns: %s",
      paste(unknown, collapse = ", ")
    ))
  }

  output <- setNames(vector("list", length(CALIBRATION_REPLICATE_COLUMNS)),
                     CALIBRATION_REPLICATE_COLUMNS)
  for (column in CALIBRATION_REPLICATE_COLUMNS) {
    if (!column %in% value_names) {
      output[[column]] <- NA
      next
    }
    value <- values[[column]]
    if (is.null(value) || (!is.list(value) && length(value) != 1L)) {
      .schema_abort(sprintf("%s must be a scalar or list value", column))
    }
    if (is.list(value)) {
      # A list is one value for this one-row constructor, even when the
      # contained diagnostic has several named fields.
      output[[column]] <- list(value)
    } else {
      output[[column]] <- value
    }
  }

  tibble::as_tibble(output, .name_repair = "check_unique")
}

#' Validate the frozen calibration scenario registry.
validate_calibration_scenarios <- function(x) {
  .require_exact_columns(x, CALIBRATION_SCENARIO_COLUMNS, "scenario")
  if (nrow(x) < 1L) {
    .schema_abort("calibration scenario registry must contain at least one row")
  }

  ids <- x$scenario_id
  if (!is.character(ids) || anyNA(ids) || any(!nzchar(ids))) {
    .schema_abort("scenario_id must contain non-empty character values")
  }
  if (anyDuplicated(ids)) {
    .schema_abort("scenario_id values must be unique")
  }

  if (!is.character(x$design_layer) || anyNA(x$design_layer) ||
      any(!x$design_layer %in% c("core", "stress", "validation"))) {
    .schema_abort("design_layer must be one of core, stress, or validation")
  }
  if (!is.numeric(x$n_boot) || any(!vapply(x$n_boot, .is_scalar_integer, logical(1), positive = TRUE))) {
    .schema_abort("n_boot must contain positive integer values")
  }
  if (!is.numeric(x$sample_size) ||
      any(!vapply(x$sample_size, .is_scalar_integer, logical(1), positive = TRUE))) {
    .schema_abort("sample_size must contain positive integer values")
  }
  if (!is.numeric(x$scenario_seed) ||
      any(!vapply(x$scenario_seed, .is_scalar_integer, logical(1), positive = FALSE))) {
    .schema_abort("scenario_seed must contain finite integer values")
  }
  .assert_numeric_range(x$max_removal_pct, "max_removal_pct", 0, 1,
                        lower_open = TRUE)
  .assert_numeric_range(x$training_split, "training_split", 0, 1,
                        lower_open = TRUE, upper_open = TRUE)

  character_columns <- c(
    "analysis_family", "endpoint", "data_generator", "primary_adapter",
    "robustness_adapter", "truth_class", "target_conclusion"
  )
  for (column in character_columns) {
    if (!is.character(x[[column]]) || anyNA(x[[column]]) || any(!nzchar(x[[column]]))) {
      .schema_abort(sprintf("%s must contain non-empty character values", column))
    }
  }
  if (!is.list(x$parameters) ||
      !all(vapply(x$parameters, is.list, logical(1)))) {
    .schema_abort("parameters must be a list-column containing lists")
  }
  TRUE
}

#' Validate completed and failed calibration replicate artifacts.
validate_calibration_replicates <- function(x) {
  .require_exact_columns(x, CALIBRATION_REPLICATE_COLUMNS, "replicate")

  if (!is.character(x$scenario_id) || anyNA(x$scenario_id) || any(!nzchar(x$scenario_id))) {
    .schema_abort("replicate scenario_id must contain non-empty character values")
  }
  if (!is.numeric(x$replicate_id) ||
      any(!vapply(x$replicate_id, .is_scalar_integer, logical(1), positive = TRUE))) {
    .schema_abort("replicate_id must contain positive integer values")
  }
  if (anyDuplicated(data.frame(x$scenario_id, x$replicate_id))) {
    .schema_abort("scenario_id/replicate_id pairs must be unique and non-missing")
  }

  metadata_columns <- c(
    "analysis_family", "endpoint", "design_layer", "truth_class", "target_conclusion"
  )
  for (column in metadata_columns) {
    if (!is.character(x[[column]]) || anyNA(x[[column]]) || any(!nzchar(x[[column]]))) {
      .schema_abort(sprintf("%s must contain non-empty character values", column))
    }
  }
  if (any(!x$design_layer %in% c("core", "stress", "validation"))) {
    .schema_abort("design_layer must be one of core, stress, or validation")
  }

  valid_statuses <- c("completed", "failed")
  if (!is.character(x$status) || anyNA(x$status) || any(!x$status %in% valid_statuses)) {
    .schema_abort("status must be completed or failed")
  }
  if (!is.logical(x$selected)) {
    .schema_abort("selected must be logical")
  }
  if (!is.character(x$screening_conclusion)) {
    .schema_abort("screening_conclusion must be character")
  }
  if (any(!is.na(x$screening_conclusion) & !nzchar(x$screening_conclusion))) {
    .schema_abort("screening_conclusion must be non-empty when present")
  }
  if (!is.character(x$assigned_label)) {
    .schema_abort("assigned_label must be character")
  }
  if (any(!is.na(x$assigned_label) & !nzchar(x$assigned_label))) {
    .schema_abort("assigned_label must be non-empty when present")
  }
  if (!is.list(x$analysis_conclusion) &&
      !is.character(x$analysis_conclusion) &&
      !is.logical(x$analysis_conclusion)) {
    .schema_abort("analysis_conclusion must be a list, character, or logical column")
  }
  if (is.list(x$analysis_conclusion) && any(!vapply(
    x$analysis_conclusion,
    function(value) is.null(value) || is.list(value) || .is_missing_value(value),
    logical(1)
  ))) {
    .schema_abort("analysis_conclusion list values must be lists or missing")
  }

  failure_fields <- c("failure_stage", "failure_class", "failure_message")
  for (column in failure_fields) {
    if (!is.character(x[[column]])) {
      .schema_abort(sprintf("%s must be character", column))
    }
  }

  completed <- x$status == "completed"
  metric_columns <- c(
    "original_p", "effective_p", "jackknife_stability", "fragility_component",
    "fragility_k", "fragility_pct", "bootstrap_reproducibility", "overall_score",
    "n", "replicate_seed", "bootstrap_seed", "runtime_seconds"
  )
  for (column in metric_columns) {
    if (!is.numeric(x[[column]])) {
      .schema_abort(sprintf("%s must be numeric", column))
    }
    values <- x[[column]][completed]
    if (length(values) > 0L && any(!is.finite(values))) {
      .schema_abort(sprintf("successful %s values must be finite", column))
    }
  }

  if (any(completed & is.na(x$selected))) {
    .schema_abort("selected must be non-missing for completed replicates")
  }
  integer_metrics <- c("fragility_k", "n", "replicate_seed", "bootstrap_seed")
  for (column in integer_metrics) {
    values <- x[[column]][completed]
    positive <- column == "n"
    if (length(values) > 0L &&
        any(vapply(
          values,
          function(value) !.is_scalar_integer(value, positive = positive) ||
            (column %in% c("replicate_seed", "bootstrap_seed") && value < 0),
          logical(1)
        ))) {
      .schema_abort(sprintf("successful %s values must be integers", column))
    }
  }
  # Exact tests can return p == 1 with a microscopic float overshoot (1 + eps).
  # Clamp completed p-values into the unit interval before range checks so
  # otherwise-valid Fisher / prop replicates are not rejected.
  if (any(completed)) {
    x$original_p[completed] <- pmin(1, pmax(0, as.numeric(x$original_p[completed])))
    x$effective_p[completed] <- pmin(1, pmax(0, as.numeric(x$effective_p[completed])))
  }
  .assert_numeric_range(x$original_p, "original_p", 0, 1, rows = which(completed))
  .assert_numeric_range(x$effective_p, "effective_p", 0, 1, rows = which(completed))
  .assert_numeric_range(x$jackknife_stability, "jackknife_stability", 0, 100,
                        rows = which(completed))
  .assert_numeric_range(x$fragility_component, "fragility_component", 0, 100,
                        rows = which(completed))
  .assert_numeric_range(x$fragility_k, "fragility_k", 0, Inf,
                        rows = which(completed))
  .assert_numeric_range(x$fragility_pct, "fragility_pct", 0, 100,
                        rows = which(completed))
  .assert_numeric_range(x$bootstrap_reproducibility, "bootstrap_reproducibility", 0, 100,
                        rows = which(completed))
  .assert_numeric_range(x$overall_score, "overall_score", 0, 100,
                        rows = which(completed))
  .assert_numeric_range(x$n, "n", 1, Inf, rows = which(completed))
  .assert_numeric_range(x$runtime_seconds, "runtime_seconds", 0, Inf,
                        rows = which(completed))

  if (any(completed & (!is.na(x$failure_stage) | !is.na(x$failure_class) |
                      !is.na(x$failure_message)))) {
    .schema_abort("completed replicates cannot contain failure details")
  }
  failed <- !completed
  if (any(failed & !is.na(x$selected))) {
    .schema_abort("failed replicates must have selected set to NA")
  }
  failed_analysis_columns <- c(
    "original_p", "effective_p", "jackknife_stability", "fragility_component",
    "fragility_k", "fragility_pct", "bootstrap_reproducibility", "overall_score",
    "assigned_label", "analysis_conclusion"
  )
  for (column in failed_analysis_columns) {
    values <- x[[column]][failed]
    if (length(values) > 0L && any(!vapply(values, .is_missing_value, logical(1)))) {
      .schema_abort(sprintf("failed replicates cannot contain %s", column))
    }
  }
  for (column in integer_metrics) {
    values <- x[[column]][failed]
    positive <- column == "n"
    if (length(values) > 0L && any(vapply(
      values,
      function(value) {
        if (is.na(value)) {
          return(is.numeric(value) && is.nan(value))
        }
        !.is_scalar_integer(value, positive = positive) ||
          (column %in% c("replicate_seed", "bootstrap_seed") && value < 0)
      },
      logical(1)
    ))) {
      .schema_abort(sprintf("failed %s values must be missing or valid integers", column))
    }
  }
  failed_runtime <- x$runtime_seconds[failed]
  if (length(failed_runtime) > 0L && any(vapply(
    failed_runtime,
    function(value) {
      if (is.na(value)) {
        return(is.numeric(value) && is.nan(value))
      }
      !is.finite(value) || value < 0
    },
    logical(1)
  ))) {
    .schema_abort("failed runtime_seconds values must be missing or finite non-negative values")
  }
  for (column in failure_fields) {
    values <- x[[column]][failed]
    if (length(values) > 0L && (!is.character(values) || anyNA(values) || any(!nzchar(values)))) {
      .schema_abort(sprintf("failed replicates require non-empty %s", column))
    }
  }
  TRUE
}

.scenario_row_values <- function(scenario) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) {
      .schema_abort("scenario must contain exactly one row")
    }
    return(lapply(scenario, function(column) column[[1L]]))
  }
  if (is.list(scenario) && !is.null(names(scenario))) {
    return(scenario)
  }
  .schema_abort("scenario must be a one-row data frame or named list")
}

#' Construct an auditable failed-replicate row from a scenario and condition.
new_calibration_failure <- function(scenario, replicate_id, stage, condition,
                                    replicate_seed = NA_integer_,
                                    bootstrap_seed = NA_integer_) {
  scenario_values <- .scenario_row_values(scenario)
  missing <- setdiff(
    c("scenario_id", "analysis_family", "endpoint", "design_layer",
      "truth_class", "target_conclusion"),
    names(scenario_values)
  )
  if (length(missing) > 0L) {
    .schema_abort(sprintf("scenario is missing required fields: %s", paste(missing, collapse = ", ")))
  }
  required_metadata <- c(
    "scenario_id", "analysis_family", "endpoint", "design_layer",
    "truth_class", "target_conclusion"
  )
  for (field in required_metadata) {
    value <- scenario_values[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      .schema_abort(sprintf("scenario %s must be a non-empty character value", field))
    }
  }
  if (!scenario_values$design_layer %in% c("core", "stress", "validation")) {
    .schema_abort("scenario design_layer must be one of core, stress, or validation")
  }
  if (!.is_scalar_integer(replicate_id, positive = TRUE)) {
    .schema_abort("replicate_id must be a positive integer")
  }
  if (!is.character(stage) || length(stage) != 1L || is.na(stage) || !nzchar(stage)) {
    .schema_abort("failure stage must be a non-empty character value")
  }

  if (inherits(condition, "condition")) {
    failure_class <- class(condition)[[1L]]
    failure_message <- conditionMessage(condition)
  } else if (is.character(condition) && length(condition) == 1L && !is.na(condition)) {
    failure_class <- "error"
    failure_message <- condition
  } else {
    .schema_abort("condition must be a condition object or one message string")
  }
  if (!nzchar(failure_message)) {
    failure_message <- failure_class
  }
  for (seed in c("replicate_seed", "bootstrap_seed")) {
    value <- get(seed)
    if (!.is_optional_nonnegative_integer(value)) {
      .schema_abort(sprintf("%s must be a non-negative integer or NA", seed))
    }
  }

  new_calibration_replicate(
    scenario_id = scenario_values$scenario_id,
    replicate_id = replicate_id,
    analysis_family = scenario_values$analysis_family,
    endpoint = scenario_values$endpoint,
    design_layer = scenario_values$design_layer,
    truth_class = scenario_values$truth_class,
    target_conclusion = scenario_values$target_conclusion,
    screening_conclusion = NA_character_,
    selected = NA,
    analysis_conclusion = NA,
    original_p = NA_real_,
    effective_p = NA_real_,
    jackknife_stability = NA_real_,
    fragility_component = NA_real_,
    fragility_k = NA_real_,
    fragility_pct = NA_real_,
    bootstrap_reproducibility = NA_real_,
    overall_score = NA_real_,
    assigned_label = NA_character_,
    n = if (!is.null(scenario_values$sample_size)) scenario_values$sample_size else NA_real_,
    replicate_seed = replicate_seed,
    bootstrap_seed = bootstrap_seed,
    runtime_seconds = NA_real_,
    status = "failed",
    failure_stage = stage,
    failure_class = failure_class,
    failure_message = failure_message
  )
}
