# Deterministic first-stage screening and stratified replicate selection.

.screen_abort <- function(message) {
  stop(message, call. = FALSE)
}

.screen_scalar_integer <- function(value, name, positive = FALSE) {
  ok <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && floor(value) == value &&
    value >= -.Machine$integer.max && value <= .Machine$integer.max
  if (positive) ok <- ok && value > 0
  if (!ok) .screen_abort(sprintf("%s must be one %s integer", name,
                                 if (positive) "positive" else "finite"))
  as.integer(value)
}

.screen_scenario_value <- function(scenario, field, default = NULL) {
  value <- if (is.data.frame(scenario)) {
    if (!field %in% names(scenario) || nrow(scenario) < 1L) NULL else scenario[[field]][[1L]]
  } else if (is.list(scenario)) {
    scenario[[field]]
  } else NULL
  if (is.null(value)) default else value
}

.screen_scenario_metadata <- function(scenario) {
  required <- c("scenario_id", "truth_class", "target_conclusion")
  values <- vapply(required, function(field) {
    value <- .screen_scenario_value(scenario, field)
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }, logical(1))
  if (!all(values)) {
    .screen_abort(sprintf("scenario must provide non-empty %s", paste(required[!values], collapse = ", ")))
  }
  list(
    scenario_id = as.character(.screen_scenario_value(scenario, "scenario_id")),
    analysis_family = as.character(.screen_scenario_value(scenario, "analysis_family", "unknown")),
    endpoint = as.character(.screen_scenario_value(scenario, "endpoint", "unknown")),
    design_layer = as.character(.screen_scenario_value(scenario, "design_layer", "core")),
    truth_class = as.character(.screen_scenario_value(scenario, "truth_class")),
    target_conclusion = as.character(.screen_scenario_value(scenario, "target_conclusion")),
    n = as.numeric(.screen_scenario_value(scenario, "sample_size", NA_real_))
  )
}

.screen_call <- function(fun, scenario, data = NULL, seed = NULL, kind = c("generator", "primary")) {
  kind <- match.arg(kind)
  if (!is.function(fun)) .screen_abort(sprintf("adapter %s must be a function", kind))
  f_names <- names(formals(fun))
  has_dots <- "..." %in% f_names
  if (kind == "generator") {
    if (has_dots || "scenario" %in% f_names) {
      return(fun(scenario = scenario, seed = seed))
    }
    if ("seed" %in% f_names) return(fun(seed = seed))
    return(fun(scenario, seed))
  }
  if (has_dots || "scenario" %in% f_names) return(fun(data = data, scenario = scenario))
  if ("data" %in% f_names) return(fun(data = data))
  fun(data)
}

.screen_adapter_function <- function(adapter, candidates, name) {
  if (!is.list(adapter)) .screen_abort("adapter must be a named list containing generator and primary functions")
  for (candidate in candidates) {
    value <- adapter[[candidate]]
    if (is.function(value)) return(value)
  }
  .screen_abort(sprintf("adapter is missing a %s function", name))
}

.screen_priority <- function(replicate_seed, replicate_id, scenario_id) {
  if (exists(".calibration_hash_integer", mode = "function", inherits = TRUE)) {
    return(.calibration_hash_integer("screen-priority", scenario_id, replicate_seed, replicate_id))
  }
  # Keep screening independently sourceable while retaining the same stable
  # integer arithmetic used by seeds.R.
  text <- paste("screen-priority", scenario_id, replicate_seed, replicate_id, sep = "\u001f")
  hash <- 104729
  for (byte in utf8ToInt(enc2utf8(text))) hash <- (hash * 131 + byte) %% 2147483647
  as.integer(hash + 1L)
}

.screen_replicate_seed <- function(scenario_seed, replicate_id) {
  if (exists("replicate_seed", mode = "function", inherits = TRUE)) {
    return(replicate_seed(scenario_seed, replicate_id))
  }
  if (exists(".calibration_hash_integer", mode = "function", inherits = TRUE)) {
    return(.calibration_hash_integer("replicate", scenario_seed, replicate_id))
  }
  .screen_priority(scenario_seed, replicate_id, "replicate")
}

.screen_failure <- function(error, stage = "screening") {
  list(
    status = "failed", conclusion = NA_character_, screening_conclusion = NA_character_,
    failure_stage = stage,
    failure_class = if (inherits(error, "condition")) class(error)[[1L]] else "error",
    failure_message = if (inherits(error, "condition")) conditionMessage(error) else as.character(error)
  )
}

.screen_conclusion <- function(output) {
  if (!is.list(output)) .screen_abort("screening adapter must return a list")
  if (identical(output$status, "failed")) {
    failure <- if (is.null(output$failure_message)) "screening adapter reported failure" else output$failure_message
    stop(structure(list(message = as.character(failure), call = NULL),
                   class = c("screening_adapter_failure", "error", "condition")))
  }
  value <- output$conclusion
  if (is.null(value)) value <- output$screening_conclusion
  if (is.null(value)) value <- output$significant
  if (is.null(value)) value <- output$original_significant
  if (is.null(value)) .screen_abort("screening adapter returned no conclusion")
  if (length(value) != 1L || is.na(value)) .screen_abort("screening conclusion must be one non-missing value")
  if (is.logical(value)) return(if (isTRUE(value)) "significant" else "non_significant")
  if (!is.character(value) || !nzchar(value)) .screen_abort("screening conclusion must be character or logical")
  as.character(value)
}

.screen_split_name <- function(name) {
  name <- as.character(name)
  for (separator in c("::", "|", "/", ":")) {
    pieces <- strsplit(name, separator, fixed = TRUE)[[1L]]
    if (length(pieces) == 2L && all(nzchar(pieces))) {
      return(setNames(as.list(pieces), c("truth_class", "screening_conclusion")))
    }
  }
  # Common compact form for the frozen truth vocabulary.
  truths <- c("null", "borderline", "clear")
  hit <- truths[startsWith(name, paste0(truths, "_"))]
  if (length(hit) == 1L) {
    return(list(truth_class = hit, screening_conclusion = sub("^[^_]+_", "", name)))
  }
  .screen_abort(sprintf("target stratum '%s' must encode truth class and conclusion", name))
}

.screen_targets <- function(targets) {
  if (is.null(targets)) .screen_abort("targets must define at least one stratum")
  rows <- list()
  add <- function(truth, conclusion, target) {
    if (length(truth) != 1L || is.na(truth) || !nzchar(truth) ||
        length(conclusion) != 1L || is.na(conclusion) || !nzchar(conclusion)) {
      .screen_abort("target strata require non-empty truth class and conclusion")
    }
    target <- .screen_scalar_integer(target, "stratum target", positive = FALSE)
    if (target < 0L) .screen_abort("stratum targets must be non-negative")
    rows[[length(rows) + 1L]] <<- data.frame(
      truth_class = as.character(truth), screening_conclusion = as.character(conclusion),
      target = target, stringsAsFactors = FALSE
    )
  }

  if (is.data.frame(targets)) {
    truth_name <- intersect(names(targets), c("truth_class", "truth"))[1L]
    conclusion_name <- intersect(names(targets), c("screening_conclusion", "conclusion"))[1L]
    target_name <- intersect(names(targets), c("target", "quota", "n"))[1L]
    if (is.na(truth_name) || is.na(conclusion_name) || is.na(target_name)) {
      .screen_abort("target data frame must contain truth, conclusion, and target columns")
    }
    for (i in seq_len(nrow(targets))) add(targets[[truth_name]][[i]], targets[[conclusion_name]][[i]], targets[[target_name]][[i]])
  } else if (is.atomic(targets) && !is.null(names(targets))) {
    for (i in seq_along(targets)) {
      pieces <- .screen_split_name(names(targets)[[i]])
      add(pieces$truth_class, pieces$screening_conclusion, targets[[i]])
    }
  } else if (is.list(targets) && !is.null(names(targets))) {
    for (truth in names(targets)) {
      value <- targets[[truth]]
      if (is.atomic(value) && !is.null(names(value))) {
        for (conclusion in names(value)) add(truth, conclusion, value[[conclusion]])
      } else if (length(value) == 1L) {
        pieces <- .screen_split_name(truth)
        add(pieces$truth_class, pieces$screening_conclusion, value[[1L]])
      } else {
        .screen_abort("nested target lists must name each conclusion")
      }
    }
  } else {
    .screen_abort("targets must be a named vector, nested list, or data frame")
  }
  out <- do.call(rbind, rows)
  out$stratum <- paste(out$truth_class, out$screening_conclusion, sep = "::")
  if (anyDuplicated(out$stratum)) {
    out <- stats::aggregate(target ~ stratum + truth_class + screening_conclusion, out, sum)
  }
  rownames(out) <- NULL
  out
}

#' Select deterministic quotas from completed screening results.
#' @export
select_stratified_replicates <- function(screened, targets) {
  if (!is.data.frame(screened)) .screen_abort("screened must be a data frame")
  required <- c("truth_class", "screening_conclusion", "status", "replicate_id", "priority")
  missing <- setdiff(required, names(screened))
  if (length(missing) > 0L) .screen_abort(sprintf("screened is missing columns: %s", paste(missing, collapse = ", ")))
  if (!"selected" %in% names(screened)) screened$selected <- FALSE
  if (!is.logical(screened$selected)) .screen_abort("screened selected column must be logical")
  target_table <- .screen_targets(targets)
  if (!"stratum" %in% names(screened)) {
    screened$stratum <- ifelse(
      is.na(screened$screening_conclusion), NA_character_,
      paste(screened$truth_class, screened$screening_conclusion, sep = "::")
    )
  }
  screened$selected[] <- FALSE
  missing_rows <- list()
  for (i in seq_len(nrow(target_table))) {
    row <- target_table[i, , drop = FALSE]
    candidates <- which(
      screened$status == "completed" & !is.na(screened$screening_conclusion) &
        screened$truth_class == row$truth_class &
        screened$screening_conclusion == row$screening_conclusion
    )
    candidates <- candidates[order(screened$priority[candidates], screened$replicate_id[candidates])]
    take <- min(length(candidates), row$target)
    if (take > 0L) screened$selected[candidates[seq_len(take)]] <- TRUE
    if (take < row$target) {
      missing_rows[[length(missing_rows) + 1L]] <- list(
        stratum = row$stratum, available = length(candidates),
        target = row$target, needed = row$target - take
      )
    }
  }
  selected <- screened[screened$selected, , drop = FALSE]
  attr(selected, "status") <- if (length(missing_rows) == 0L) "complete" else "incomplete"
  attr(selected, "missing") <- missing_rows
  attr(selected, "targets") <- target_table
  attr(selected, "counts") <- table(screened$stratum[screened$selected])
  selected
}

#' Screen generated datasets and select deterministic truth-by-conclusion quotas.
#' @export
screen_scenario <- function(scenario, adapter, target_by_stratum, max_draws,
                            workers = 1L, checkpoint_root = NULL) {
  metadata <- .screen_scenario_metadata(scenario)
  max_draws <- .screen_scalar_integer(max_draws, "max_draws", positive = TRUE)
  workers <- .screen_scalar_integer(workers, "workers", positive = TRUE)
  scenario_seed_value <- .screen_scenario_value(scenario, "scenario_seed")
  scenario_seed_value <- .screen_scalar_integer(scenario_seed_value, "scenario_seed", positive = FALSE)
  if (!is.null(checkpoint_root) && (!is.character(checkpoint_root) || length(checkpoint_root) != 1L || is.na(checkpoint_root) || !nzchar(checkpoint_root))) {
    .screen_abort("checkpoint_root must be NULL or one non-empty path")
  }
  generator <- .screen_adapter_function(adapter, c("generate", "generate_data", "generator"), "generator")
  primary <- .screen_adapter_function(adapter, c("primary_decision", "screen", "screening"), "primary")
  ids <- seq_len(max_draws)
  one <- function(replicate_id) {
    seed <- .screen_replicate_seed(scenario_seed_value, replicate_id)
    priority <- .screen_priority(seed, replicate_id, metadata$scenario_id)
    generated <- tryCatch(.screen_call(generator, scenario, seed = seed, kind = "generator"),
                          error = function(e) e)
    if (inherits(generated, "condition")) {
      failure <- .screen_failure(generated, "generation")
      return(c(list(
        scenario_id = metadata$scenario_id, replicate_id = as.integer(replicate_id),
        truth_class = metadata$truth_class, target_conclusion = metadata$target_conclusion,
        replicate_seed = seed, priority = priority, data = NULL, screening = NULL
      ), failure))
    }
    output <- tryCatch(.screen_call(primary, scenario, data = generated, kind = "primary"),
                       error = function(e) e)
    if (inherits(output, "condition")) {
      failure <- .screen_failure(output, "screening")
      return(c(list(
        scenario_id = metadata$scenario_id, replicate_id = as.integer(replicate_id),
        truth_class = metadata$truth_class, target_conclusion = metadata$target_conclusion,
        replicate_seed = seed, priority = priority, data = generated, screening = NULL
      ), failure))
    }
    conclusion <- tryCatch(.screen_conclusion(output), error = function(e) e)
    if (inherits(conclusion, "condition")) {
      failure <- .screen_failure(conclusion, "screening")
      return(c(list(
        scenario_id = metadata$scenario_id, replicate_id = as.integer(replicate_id),
        truth_class = metadata$truth_class, target_conclusion = metadata$target_conclusion,
        replicate_seed = seed, priority = priority, data = generated, screening = output
      ), failure))
    }
    list(
      scenario_id = metadata$scenario_id, replicate_id = as.integer(replicate_id),
      truth_class = metadata$truth_class, target_conclusion = metadata$target_conclusion,
      replicate_seed = seed, priority = priority, data = generated, screening = output,
      status = "completed", conclusion = conclusion, screening_conclusion = conclusion,
      failure_stage = NA_character_, failure_class = NA_character_, failure_message = NA_character_
    )
  }
  if (workers > 1L && .Platform$OS.type != "windows" && length(ids) > 1L) {
    draws <- parallel::mclapply(ids, one, mc.cores = min(workers, length(ids)), mc.preschedule = TRUE)
  } else draws <- lapply(ids, one)
  screened <- tibble::tibble(
    scenario_id = vapply(draws, `[[`, character(1), "scenario_id"),
    replicate_id = as.integer(vapply(draws, `[[`, integer(1), "replicate_id")),
    truth_class = vapply(draws, `[[`, character(1), "truth_class"),
    target_conclusion = vapply(draws, `[[`, character(1), "target_conclusion"),
    replicate_seed = as.integer(vapply(draws, `[[`, integer(1), "replicate_seed")),
    priority = as.integer(vapply(draws, `[[`, integer(1), "priority")),
    status = vapply(draws, `[[`, character(1), "status"),
    screening_conclusion = vapply(draws, `[[`, character(1), "screening_conclusion"),
    failure_stage = vapply(draws, `[[`, character(1), "failure_stage"),
    failure_class = vapply(draws, `[[`, character(1), "failure_class"),
    failure_message = vapply(draws, `[[`, character(1), "failure_message"),
    data = lapply(draws, `[[`, "data"),
    screening = lapply(draws, `[[`, "screening"),
    selected = FALSE
  )
  selection <- select_stratified_replicates(screened, target_by_stratum)
  selected_ids <- selection$replicate_id
  screened$selected <- screened$replicate_id %in% selected_ids
  list(
    scenario_id = metadata$scenario_id,
    screened = screened,
    selected = selection,
    selected_ids = selected_ids,
    denominator = nrow(screened),
    completed_screening = sum(screened$status == "completed"),
    failed_screening = sum(screened$status == "failed"),
    status = attr(selection, "status"),
    missing = attr(selection, "missing"),
    targets = attr(selection, "targets"),
    workers = workers,
    checkpoint_root = checkpoint_root
  )
}
