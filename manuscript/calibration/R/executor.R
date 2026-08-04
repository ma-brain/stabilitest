# Full robustness execution and audit logging for selected calibration rows.

if (!exists("%||%", mode = "function")) `%||%` <- function(x, y) if (is.null(x)) y else x

.executor_abort <- function(message) stop(message, call. = FALSE)
.executor_replicate_seed <- function(value, id) replicate_seed(value, id)
.executor_bootstrap_seed <- function(value) bootstrap_seed(value)
.executor_timeout_condition <- function(error) {
  inherits(error, c("elapsedTimeLimit", "timeLimit")) ||
    grepl("elapsed.*time|time[ -]?limit|timeout", conditionMessage(error), ignore.case = TRUE)
}

.executor_scenario_values <- function(scenario) {
  if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) .executor_abort("scenario must contain exactly one row")
    return(lapply(scenario, function(value) value[[1L]]))
  }
  if (is.list(scenario) && !is.null(names(scenario))) return(scenario)
  .executor_abort("scenario must be a one-row data frame or named list")
}

.executor_value <- function(x, paths, default = NULL) {
  for (path in paths) {
    value <- x
    ok <- TRUE
    for (name in path) {
      if (is.null(value) || is.null(names(value)) || !name %in% names(value)) {
        ok <- FALSE
        break
      }
      value <- value[[name]]
    }
    if (ok && length(value) > 0L) {
      if (is.data.frame(value) && nrow(value) == 1L) value <- value[[1L]]
      if (length(value) == 1L) return(value[[1L]])
    }
  }
  default
}

.executor_condition <- function(class = "error", message = "calibration execution failed",
                                stage = NULL) {
  condition <- structure(list(message = as.character(message)[[1L]]),
                         class = c(class, "error", "condition"))
  if (!is.null(stage)) attr(condition, "failure_stage") <- stage
  condition
}

.executor_nonempty <- function(value, default) {
  if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) value else default
}

.executor_run_call <- function(fun, data, scenario, n_boot, seed) {
  if (!is.function(fun)) .executor_abort("adapter robustness function is missing")
  formals <- names(formals(fun))
  dots <- "..." %in% formals
  args <- list(data = data, scenario = scenario)
  if (dots || "n_boot" %in% formals) args$n_boot <- n_boot
  if (dots || "seed" %in% formals) args$seed <- seed
  do.call(fun, args)
}

.executor_generate <- function(adapter, scenario, seed) {
  generator <- adapter$generate %||% adapter$generate_data
  if (!is.function(generator)) {
    .executor_abort("adapter data generator is missing")
  }
  formals <- names(formals(generator))
  args <- list(scenario = scenario)
  if ("seed" %in% formals || "..." %in% formals) args$seed <- seed
  generated <- do.call(generator, args)
  if (is.list(generated) && !is.null(generated$data)) generated$data else generated
}

.executor_conclusion <- function(x) {
  value <- .executor_value(x, list(
    c("conclusion"), c("original_significant"), c("significant"),
    c("analysis_conclusion"), c("analysis", "conclusion")
  ))
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(if (value) "significant" else "non_significant")
  }
  if (is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)) value else NA_character_
}

.executor_metric <- function(result, names, default = NA_real_) {
  paths <- lapply(names, function(name) c("metrics", name))
  paths <- c(paths, lapply(names, function(name) c("robustness_metrics", name)),
             lapply(names, function(name) c(name)))
  .executor_value(result, paths, default)
}

.executor_failure_row <- function(scenario, replicate_id, stage, condition,
                                  replicate_seed, bootstrap_seed,
                                  runtime_seconds = NA_real_) {
  row <- new_calibration_failure(
    scenario, replicate_id = replicate_id, stage = stage, condition = condition,
    replicate_seed = replicate_seed, bootstrap_seed = bootstrap_seed
  )
  if (!is.na(runtime_seconds)) row$runtime_seconds <- as.numeric(runtime_seconds)
  row
}

.executor_validate_success <- function(result, data) {
  metric_values <- list(
    original_p = .executor_value(result, list(c("original_p"), c("p_value"), c("p"))),
    effective_p = .executor_value(result, list(c("effective_p"), c("p_eff"), c("original_p"), c("p_value"), c("p"))),
    jackknife_stability = .executor_metric(result, "jackknife_conclusion_stability"),
    fragility_component = .executor_metric(result, c("worstcase_fragility_component", "fragility_component")),
    fragility_k = .executor_metric(result, c("worstcase_fragility_k", "fragility_k")),
    fragility_pct = .executor_metric(result, c("worstcase_fragility_pct", "fragility_pct")),
    bootstrap_reproducibility = .executor_metric(result, "bootstrap_reproducibility"),
    overall_score = .executor_metric(result, c("overall_robustness", "overall_score"))
  )
  for (name in names(metric_values)) {
    value <- metric_values[[name]]
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
      return(list(ok = FALSE, condition = .executor_condition(
        "non_finite_metric", sprintf("%s is missing or non-finite", name)
      )))
    }
  }
  metric_values$fragility_k <- as.integer(metric_values$fragility_k)
  n_value <- .executor_value(result, list(c("n"), c("sample_info", "n")), nrow(data))
  if (!is.numeric(n_value) || length(n_value) != 1L || !is.finite(n_value) ||
      floor(n_value) != n_value || n_value < 1) {
    return(list(ok = FALSE, condition = .executor_condition(
      "non_finite_metric", "n is missing or invalid"
    )))
  }
  conclusion <- if (!is.null(result$analysis_conclusion)) result$analysis_conclusion else
    .executor_value(result, list(c("conclusion"), c("original_significant")))
  if (is.null(conclusion) || length(conclusion) == 0L || (length(conclusion) == 1L && is.na(conclusion))) {
    conclusion <- .executor_conclusion(result)
  }
  if (length(conclusion) == 1L && is.logical(conclusion)) {
    conclusion <- if (conclusion) "significant" else "non_significant"
  }
  if (is.null(conclusion) || length(conclusion) == 0L || (length(conclusion) == 1L && is.na(conclusion))) {
    conclusion <- list(significant = isTRUE(.executor_conclusion(result) == "significant"))
  }
  label <- .executor_value(result, list(c("assigned_label"), c("interpretation_label"), c("robustness_interpretation")), NA_character_)
  if (is.na(label) || !nzchar(as.character(label))) label <- "Unassigned"
  list(ok = TRUE, metrics = metric_values, n = as.integer(n_value),
       conclusion = conclusion, label = as.character(label))
}

#' Execute one pre-selected calibration replicate and return one audit row.
run_selected_replicate <- function(scenario, adapter, replicate_id, data = NULL,
                                   replicate_seed = NULL, screening = NULL,
                                   selected = TRUE, n_boot = NULL,
                                   bootstrap_seed = NULL, generated = NULL,
                                   timeout_seconds = NULL) {
  values <- .executor_scenario_values(scenario)
  if (is.null(replicate_seed)) replicate_seed <- .executor_replicate_seed(values$scenario_seed, replicate_id)
  if (is.null(bootstrap_seed)) bootstrap_seed <- .executor_bootstrap_seed(replicate_seed)
  replicate_seed <- as.integer(replicate_seed); bootstrap_seed <- as.integer(bootstrap_seed)
  if (is.null(data) && !is.null(generated)) data <- if (is.list(generated) && !is.null(generated$data)) generated$data else generated

  start <- proc.time()[["elapsed"]]
  current_stage <- "generation"
  limit_set <- !is.null(timeout_seconds)
  if (limit_set) setTimeLimit(elapsed = timeout_seconds, transient = TRUE)
  on.exit(if (limit_set) setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
  result <- tryCatch({
    if (is.null(data)) data <- .executor_generate(adapter, scenario, replicate_seed)
    if (is.null(data)) stop("generated data is missing", call. = FALSE)
    current_stage <- "screening"
    if (is.null(screening)) {
      if (!is.function(adapter$primary_decision)) stop("adapter screening function is missing", call. = FALSE)
      screening <- adapter$primary_decision(data, scenario)
    }
    screen_status <- .executor_value(screening, list(c("status")), "completed")
    # Public adapters use `status = "ok"` for a successful primary fit,
    # whereas the screening stage itself records `completed`.  A selected
    # replicate reuses the adapter result, so both success markers must be
    # accepted here; otherwise every model replicate is falsely audited as a
    # screening failure during the pilot/full hand-off.
    if (!screen_status %in% c("completed", "ok")) {
      cls <- .executor_nonempty(.executor_value(screening, list(c("failure_class"))), "screening_failure")
      msg <- .executor_nonempty(.executor_value(screening, list(c("failure_message"))), "screening did not complete")
      stage <- .executor_nonempty(.executor_value(screening, list(c("failure_stage"))), "screening")
      return(.executor_failure_row(values, replicate_id, as.character(stage),
                                   .executor_condition(as.character(cls), msg),
                                   replicate_seed, bootstrap_seed,
                                   proc.time()[["elapsed"]] - start))
    }
    screening_conclusion <- .executor_conclusion(screening)
    if (is.na(screening_conclusion)) stop("screening conclusion is missing", call. = FALSE)
    current_stage <- "robustness"
    n_boot_value <- if (is.null(n_boot)) values$n_boot %||% 1000L else n_boot
    full <- .executor_run_call(adapter$run_robustness, data, scenario, n_boot_value, bootstrap_seed)
    full_status <- .executor_value(full, list(c("status")), "completed")
    if (!identical(full_status, "completed")) {
      cls <- .executor_nonempty(.executor_value(full, list(c("failure_class"))), .executor_nonempty(as.character(full_status), "analysis_failure"))
      msg <- .executor_nonempty(.executor_value(full, list(c("failure_message"))), "robustness analysis did not complete")
      stage <- .executor_nonempty(.executor_value(full, list(c("failure_stage"))), "robustness")
      return(.executor_failure_row(values, replicate_id, as.character(stage),
                                   .executor_condition(as.character(cls), msg),
                                   replicate_seed, bootstrap_seed,
                                   proc.time()[["elapsed"]] - start))
    }
    validated <- .executor_validate_success(full, data)
    if (!validated$ok) stop(validated$condition)
    runtime <- proc.time()[["elapsed"]] - start
    new_calibration_replicate(
      scenario_id = values$scenario_id, replicate_id = replicate_id,
      analysis_family = values$analysis_family, endpoint = values$endpoint,
      design_layer = values$design_layer, truth_class = values$truth_class,
      target_conclusion = values$target_conclusion,
      screening_conclusion = screening_conclusion, selected = isTRUE(selected),
      analysis_conclusion = validated$conclusion,
      original_p = validated$metrics$original_p, effective_p = validated$metrics$effective_p,
      jackknife_stability = validated$metrics$jackknife_stability,
      fragility_component = validated$metrics$fragility_component,
      fragility_k = validated$metrics$fragility_k, fragility_pct = validated$metrics$fragility_pct,
      bootstrap_reproducibility = validated$metrics$bootstrap_reproducibility,
      overall_score = validated$metrics$overall_score, assigned_label = validated$label,
      n = validated$n, replicate_seed = replicate_seed, bootstrap_seed = bootstrap_seed,
      runtime_seconds = runtime, status = "completed", failure_stage = NA_character_,
      failure_class = NA_character_, failure_message = NA_character_
    )
  }, error = function(error) {
    stage <- if (.executor_timeout_condition(error)) "timeout" else if (
      inherits(error, "non_finite_metric")
    ) "metric_validation" else if (inherits(error, "subset_failure")) "subset" else if (
      inherits(error, "screening_failure")
    ) "screening" else current_stage
    cls <- class(error)[[1L]]
    if (identical(cls, "simpleError")) cls <- "error"
    condition <- if (identical(class(error)[[1L]], "simpleError")) {
      .executor_condition(cls, conditionMessage(error), stage = stage)
    } else error
    .executor_failure_row(values, replicate_id, stage, condition, replicate_seed, bootstrap_seed,
                          proc.time()[["elapsed"]] - start)
  })
  result
}

.executor_record <- function(x, scenario, replicate_id, master_seed) {
  if (is.list(x) && !is.null(x$data)) {
    return(list(replicate_id = replicate_id, data = x$data,
                replicate_seed = x$replicate_seed %||% .executor_replicate_seed(scenario$scenario_seed, replicate_id),
                screening = x$screening))
  }
  list(replicate_id = replicate_id, data = x,
       replicate_seed = .executor_replicate_seed(scenario$scenario_seed, replicate_id), screening = NULL)
}

#' Execute all selected replicates for a scenario, optionally using fork workers.
run_full_scenario <- function(scenario, adapter, selected = NULL, replicate_ids = NULL,
                              master_seed = 1L, workers = 1L, checkpoint_root = NULL,
                              checkpoint_batch = 25L, resume = FALSE,
                              n_boot = NULL, timeout_seconds = NULL,
                              manifest_hash = "executor-v1") {
  values <- .executor_scenario_values(scenario)
  if (is.null(replicate_ids)) {
    if (is.null(selected)) .executor_abort("replicate_ids or selected records are required")
    if (is.data.frame(selected) && "replicate_id" %in% names(selected)) replicate_ids <- selected$replicate_id
    else replicate_ids <- seq_along(selected)
  }
  replicate_ids <- as.integer(replicate_ids)
  if (length(replicate_ids) < 1L || anyNA(replicate_ids) || any(replicate_ids < 1L) || anyDuplicated(replicate_ids)) {
    .executor_abort("replicate_ids must be unique positive integers")
  }
  workers <- as.integer(workers)
  if (length(workers) != 1L || is.na(workers) || workers < 1L) .executor_abort("workers must be a positive integer")
  target_n <- length(replicate_ids)
  path <- if (!is.null(checkpoint_root)) checkpoint_path(checkpoint_root, values$scenario_id, "full") else NULL
  existing <- NULL
  if (isTRUE(resume) && !is.null(path) && file.exists(path)) {
    existing <- tryCatch(read_checkpoint(path, manifest_hash)$replicates,
                         error = function(error) NULL)
    existing <- if (!is.null(existing)) {
      tryCatch({ validate_calibration_replicates(existing); existing },
               error = function(error) NULL)
    } else NULL
    ids_match <- !is.null(existing) && nrow(existing) == target_n &&
      identical(sort(as.integer(existing$replicate_id)), sort(replicate_ids)) &&
      all(existing$scenario_id == values$scenario_id)
    if (!is.null(existing) && ids_match && checkpoint_complete(path, manifest_hash, target_n)) {
      return(existing[match(replicate_ids, existing$replicate_id), , drop = FALSE])
    }
  }
  scenario_seed_value <- values$scenario_seed %||% scenario_seed(values$scenario_id, master_seed)
  records <- lapply(seq_along(replicate_ids), function(index) {
    id <- replicate_ids[[index]]
    item <- if (is.null(selected)) NULL else if (is.data.frame(selected)) selected[index, , drop = FALSE] else selected[[index]]
    if (is.data.frame(item)) {
      item <- lapply(item, function(value) if (is.list(value)) value[[1L]] else value[[1L]])
    }
    if (is.null(item)) item <- list()
    data <- item$data %||% NULL
    list(replicate_id = id, data = data, replicate_seed = item$replicate_seed %||% .executor_replicate_seed(scenario_seed_value, id), screening = item$screening %||% NULL)
  })
  run_one <- function(record) run_selected_replicate(
    scenario, adapter, record$replicate_id, data = record$data,
    replicate_seed = record$replicate_seed, screening = record$screening,
    n_boot = n_boot, timeout_seconds = timeout_seconds
  )
  indexes <- seq_along(records)
  rows <- vector("list", length(indexes))
  if (!is.null(existing) && nrow(existing) > 0L) {
    for (i in seq_len(nrow(existing))) {
      match_index <- match(existing$replicate_id[[i]], replicate_ids)
      if (!is.na(match_index)) rows[[match_index]] <- existing[i, , drop = FALSE]
    }
  }
  batches <- split(indexes, ceiling(indexes / max(1L, as.integer(checkpoint_batch))))
  for (batch in batches) {
    todo <- batch[vapply(rows[batch], is.null, logical(1L))]
    computed <- if (length(todo) == 0L) list() else if (workers == 1L) lapply(records[todo], run_one) else {
      parallel::mclapply(records[todo], run_one, mc.cores = workers, mc.preschedule = FALSE)
    }
    if (length(todo) > 0L) rows[todo] <- computed
    if (!is.null(path)) {
      partial <- tibble::as_tibble(do.call(rbind, lapply(rows[seq_len(max(batch))], as.data.frame)))
      write_checkpoint(list(target_n = nrow(partial), replicates = partial), path, manifest_hash)
    }
  }
  output <- tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame)))
  validate_calibration_replicates(output)
  output
}
