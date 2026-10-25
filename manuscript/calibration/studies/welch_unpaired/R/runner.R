# Quota-based, resumable runner for the prospective Welch calibration study.

.welch_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, rows)
  if (!length(rows)) return(data.frame())
  result <- rows[[1L]]
  if (length(rows) > 1L) {
    for (i in 2:length(rows)) result <- rbind(result, rows[[i]])
  }
  rownames(result) <- NULL
  result
}

.welch_safe_id <- function(x) gsub("[^A-Za-z0-9_.-]", "_", x)

# Shared by the tools/ scripts: read `--study-version` (default 2026-2).
welch_tool_study_version <- function(args = commandArgs(trailingOnly = TRUE)) {
  index <- which(args == "--study-version")
  if (!length(index)) return("2026-2")
  if (index[[1L]] == length(args)) {
    stop("--study-version requires a value", call. = FALSE)
  }
  version <- args[[index[[1L]] + 1L]]
  if (!version %in% WELCH_STUDY_VERSIONS) {
    stop(sprintf("--study-version must be one of: %s",
                 paste(WELCH_STUDY_VERSIONS, collapse = ", ")), call. = FALSE)
  }
  version
}

# Scenario IDs carry the study version (`WTR3-` etc. for 2026-3), so a run can
# never mix scenario tables across studies.
.welch_check_scenario_version <- function(scenario_ids, study_version) {
  is_v3 <- grepl("^W(TR|VA|ST)3-", scenario_ids)
  ok <- if (identical(study_version, "2026-3")) all(is_v3) else !any(is_v3)
  if (!ok) {
    stop(sprintf("scenario IDs do not belong to Welch study %s", study_version),
         call. = FALSE)
  }
  invisible(TRUE)
}

select_welch_scenarios <- function(scenarios, layer = c("training", "validation")) {
  layer <- match.arg(layer)
  selected <- scenarios[scenarios$design_layer == layer, , drop = FALSE]
  if (!nrow(selected)) {
    stop(sprintf("no Welch scenarios found for %s layer", layer), call. = FALSE)
  }
  selected[order(selected$scenario_id), , drop = FALSE]
}

WELCH_STUDY_VERSIONS <- c("2026-2", "2026-3")

# Study 2026-2 keeps its original `artifacts/` root; later studies write to
# `artifacts/<version>/` so frozen 2026-2 evidence is never overwritten.
welch_artifact_root <- function(study_root, study_version = "2026-2") {
  study_version <- match.arg(study_version, WELCH_STUDY_VERSIONS)
  if (identical(study_version, "2026-2")) {
    return(file.path(study_root, "artifacts"))
  }
  file.path(study_root, "artifacts", study_version)
}

.welch_smoke_root <- function(study_root, study_version = "2026-2") {
  study_version <- match.arg(study_version, WELCH_STUDY_VERSIONS)
  root <- file.path(study_root, "outputs", "smoke")
  if (identical(study_version, "2026-2")) root else file.path(root, study_version)
}

welch_layer_output_dir <- function(study_root, mode, layer,
                                   study_version = "2026-2") {
  if (identical(mode, "smoke")) {
    return(file.path(.welch_smoke_root(study_root, study_version), layer))
  }
  file.path(welch_artifact_root(study_root, study_version), layer)
}

welch_layer_checkpoint_dir <- function(study_root, mode, layer,
                                       study_version = "2026-2") {
  if (identical(mode, "smoke")) {
    return(file.path(.welch_smoke_root(study_root, study_version), layer,
                     "checkpoints"))
  }
  file.path(welch_artifact_root(study_root, study_version), "checkpoints", layer)
}

validate_welch_run_authority <- function(mode = c("smoke", "production"),
                                         layer = c("training", "validation"),
                                         candidate_hash = NULL,
                                         study_root,
                                         study_version = "2026-2") {
  mode <- match.arg(mode)
  layer <- match.arg(layer)
  if (identical(layer, "validation")) {
    if (is.null(candidate_hash) || length(candidate_hash) != 1L ||
        is.na(candidate_hash) || !nzchar(candidate_hash)) {
      stop("validation requires an explicit frozen candidate hash",
           call. = FALSE)
    }
    if (identical(mode, "production")) {
      hash_path <- file.path(
        welch_artifact_root(study_root, study_version),
        "summaries", "candidate-hash.txt"
      )
      if (!file.exists(hash_path)) {
        stop("frozen candidate hash file is missing", call. = FALSE)
      }
      frozen <- trimws(readLines(hash_path, warn = FALSE)[[1L]])
      if (!identical(candidate_hash, frozen)) {
        stop("candidate hash does not match the committed freeze",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

read_welch_results <- function(study_root,
                               layer = c("training", "validation"),
                               authority = c("training", "validation"),
                               study_version = "2026-2") {
  layer <- match.arg(layer)
  authority <- match.arg(authority)
  if (identical(authority, "training") && identical(layer, "validation")) {
    stop("training authority cannot read validation score files",
         call. = FALSE)
  }
  path <- file.path(
    welch_artifact_root(study_root, study_version), layer, "completed.rds"
  )
  if (!file.exists(path)) {
    stop(sprintf("Welch %s completed score file is missing", layer),
         call. = FALSE)
  }
  readRDS(path)
}

.new_welch_ledger_row <- function(scenario, replicate_id, replicate_seed,
                                  p_value = NA_real_, significant = NA,
                                  status = c("screened", "failed"),
                                  message = NA_character_) {
  status <- match.arg(status)
  data.frame(
    scenario_id = as.character(scenario$scenario_id[[1L]]),
    replicate_id = as.integer(replicate_id),
    replicate_seed = as.integer(replicate_seed),
    p_value = as.numeric(p_value),
    significant = as.logical(significant),
    stage = "screening",
    status = status,
    message = as.character(message),
    stringsAsFactors = FALSE
  )
}

.welch_scenario_occupancy <- function(scenario, state, quota, draw_cap) {
  attempted <- nrow(state$ledger)
  screening_failed <- sum(state$ledger$status == "failed")
  screened <- sum(state$ledger$status == "screened")
  screened_significant <- sum(
    state$ledger$status == "screened" & state$ledger$significant,
    na.rm = TRUE
  )
  excluded <- sum(
    state$ledger$status == "screened" & !state$ledger$significant,
    na.rm = TRUE
  )
  completed <- nrow(state$completed)
  analysis_failed <- nrow(state$failures) - screening_failed
  analysis_failed <- max(0L, analysis_failed)
  pending <- screened_significant - completed - analysis_failed
  failure_rate <- if (screened_significant > 0L) {
    analysis_failed / screened_significant
  } else {
    0
  }
  data.frame(
    scenario_id = as.character(scenario$scenario_id[[1L]]),
    design_layer = as.character(scenario$design_layer[[1L]]),
    truth = as.character(scenario$truth[[1L]]),
    attempted = as.integer(attempted),
    screened = as.integer(screened),
    screened_significant = as.integer(screened_significant),
    completed = as.integer(completed),
    failed = as.integer(nrow(state$failures)),
    screening_failed = as.integer(screening_failed),
    analysis_failed = as.integer(analysis_failed),
    excluded = as.integer(excluded),
    pending = as.integer(pending),
    quota = as.integer(quota),
    draw_cap = as.integer(draw_cap),
    failure_rate = as.numeric(failure_rate),
    quota_met = completed >= quota,
    occupancy_pass = completed >= quota && failure_rate <= 0.05,
    stringsAsFactors = FALSE
  )
}

.new_welch_runner_state <- function(scenario_id, mode) {
  list(
    scenario_id = scenario_id,
    mode = mode,
    ledger = data.frame(),
    completed = data.frame(),
    failures = data.frame()
  )
}

.save_welch_checkpoint <- function(state, checkpoint_path) {
  dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(checkpoint_path, ".tmp")
  saveRDS(state, temporary, version = 3)
  if (!file.rename(temporary, checkpoint_path)) {
    stop("unable to atomically write Welch checkpoint", call. = FALSE)
  }
  invisible(checkpoint_path)
}

run_welch_scenario <- function(scenario,
                               mode = c("smoke", "production"),
                               quota = NULL,
                               draw_cap = NULL,
                               output_dir,
                               checkpoint_dir,
                               adapter = welch_adapter(),
                               workers = 1L,
                               resume = TRUE) {
  mode <- match.arg(mode)
  if (!is.data.frame(scenario) || nrow(scenario) != 1L) {
    stop("run_welch_scenario requires one scenario row", call. = FALSE)
  }
  quota <- as.integer(if (is.null(quota)) {
    if (identical(mode, "smoke")) 5L else scenario$replicate_quota[[1L]]
  } else quota)
  draw_cap <- as.integer(if (is.null(draw_cap)) {
    if (identical(mode, "smoke")) 1000L else scenario$draw_cap[[1L]]
  } else draw_cap)
  n_boot <- if (identical(mode, "smoke")) 20L else 1000L
  production <- identical(mode, "production")
  workers <- max(1L, as.integer(workers))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

  scenario_id <- as.character(scenario$scenario_id[[1L]])
  checkpoint_path <- file.path(
    checkpoint_dir, paste0(.welch_safe_id(scenario_id), ".rds")
  )
  state <- if (isTRUE(resume) && file.exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    .new_welch_runner_state(scenario_id, mode)
  }
  if (!identical(state$scenario_id, scenario_id) || !identical(state$mode, mode)) {
    stop("checkpoint authority does not match the requested scenario/mode",
         call. = FALSE)
  }
  if (nrow(state$ledger) && anyDuplicated(state$ledger$replicate_id)) {
    stop("checkpoint contains duplicate screening replicate IDs", call. = FALSE)
  }
  if (nrow(state$completed) && anyDuplicated(state$completed$replicate_id)) {
    stop("checkpoint contains duplicate completed replicate IDs", call. = FALSE)
  }

  while (nrow(state$completed) < quota && nrow(state$ledger) < draw_cap) {
    needed <- quota - nrow(state$completed)
    target_batch <- min(max(needed, 1L), 25L)
    selected_ids <- integer()

    while (length(selected_ids) < target_batch && nrow(state$ledger) < draw_cap) {
      replicate_id <- if (nrow(state$ledger)) {
        max(state$ledger$replicate_id) + 1L
      } else {
        1L
      }
      replicate_seed <- welch_replicate_seed(scenario$seed[[1L]], replicate_id)
      generated <- tryCatch(
        generate_welch_data(scenario, replicate_id),
        error = function(error) error
      )
      if (inherits(generated, "error")) {
        state$ledger <- .welch_bind_rows(list(
          state$ledger,
          .new_welch_ledger_row(
            scenario, replicate_id, replicate_seed, status = "failed",
            message = conditionMessage(generated)
          )
        ))
        state$failures <- .welch_bind_rows(list(
          state$failures,
          new_welch_score_row(
            scenario, replicate_id, n_boot = n_boot, status = "failed",
            failure_stage = "generation",
            failure_class = class(generated)[[1L]],
            failure_message = conditionMessage(generated)
          )
        ))
        next
      }
      decision <- tryCatch(
        adapter$primary_decision(generated, scenario),
        error = function(error) error
      )
      if (inherits(decision, "error")) {
        state$ledger <- .welch_bind_rows(list(
          state$ledger,
          .new_welch_ledger_row(
            scenario, replicate_id, replicate_seed, status = "failed",
            message = conditionMessage(decision)
          )
        ))
        state$failures <- .welch_bind_rows(list(
          state$failures,
          new_welch_score_row(
            scenario, replicate_id, n_boot = n_boot, status = "failed",
            failure_stage = "screening",
            failure_class = class(decision)[[1L]],
            failure_message = conditionMessage(decision)
          )
        ))
        next
      }
      state$ledger <- .welch_bind_rows(list(
        state$ledger,
        .new_welch_ledger_row(
          scenario, replicate_id, replicate_seed,
          p_value = decision$p_value,
          significant = decision$significant,
          status = "screened"
        )
      ))
      if (isTRUE(decision$significant)) {
        selected_ids <- c(selected_ids, replicate_id)
      }
    }

    if (length(selected_ids)) {
      score_one <- function(replicate_id) {
        generated <- tryCatch(
          generate_welch_data(scenario, replicate_id),
          error = function(error) error
        )
        if (inherits(generated, "error")) {
          return(new_welch_score_row(
            scenario, replicate_id, n_boot = n_boot, status = "failed",
            failure_stage = "generation",
            failure_class = class(generated)[[1L]],
            failure_message = conditionMessage(generated)
          ))
        }
        tryCatch(
          adapter$score(
            generated, scenario, replicate_id = replicate_id,
            n_boot = n_boot, production = production
          ),
          error = function(error) new_welch_score_row(
            scenario, replicate_id, n_boot = n_boot, status = "failed",
            failure_stage = "adapter",
            failure_class = class(error)[[1L]],
            failure_message = conditionMessage(error)
          )
        )
      }
      scored <- if (workers > 1L && length(selected_ids) > 1L &&
                    .Platform$OS.type != "windows") {
        parallel::mclapply(selected_ids, score_one, mc.cores = workers,
                           mc.preschedule = FALSE)
      } else {
        lapply(selected_ids, score_one)
      }
      scored <- .welch_bind_rows(scored)
      completed <- scored[scored$status == "completed", , drop = FALSE]
      failed <- scored[scored$status == "failed", , drop = FALSE]
      state$completed <- .welch_bind_rows(list(state$completed, completed))
      state$failures <- .welch_bind_rows(list(state$failures, failed))
    }
    .save_welch_checkpoint(state, checkpoint_path)
  }

  if (nrow(state$completed)) {
    state$completed <- state$completed[
      order(state$completed$replicate_id), , drop = FALSE
    ]
  }
  if (nrow(state$failures)) {
    state$failures <- state$failures[
      order(state$failures$replicate_id), , drop = FALSE
    ]
  }
  .save_welch_checkpoint(state, checkpoint_path)
  list(
    completed = state$completed,
    failures = state$failures,
    ledger = state$ledger,
    occupancy = .welch_scenario_occupancy(scenario, state, quota, draw_cap),
    checkpoint_path = checkpoint_path
  )
}

run_welch_layer <- function(study_root,
                            project_root,
                            mode = c("smoke", "production"),
                            layer = c("training", "validation"),
                            candidate_hash = NULL,
                            workers = 1L,
                            resume = TRUE,
                            scenarios = NULL,
                            adapter = welch_adapter(),
                            study_version = "2026-2") {
  mode <- match.arg(mode)
  layer <- match.arg(layer)
  study_version <- match.arg(study_version, WELCH_STUDY_VERSIONS)
  validate_welch_run_authority(mode, layer, candidate_hash, study_root,
                               study_version = study_version)
  if (is.null(scenarios)) {
    scenarios <- utils::read.csv(
      file.path(welch_artifact_root(study_root, study_version),
                "design", "resolved-scenarios.csv"),
      stringsAsFactors = FALSE
    )
  }
  selected <- select_welch_scenarios(scenarios, layer)
  .welch_check_scenario_version(selected$scenario_id, study_version)
  output_dir <- welch_layer_output_dir(study_root, mode, layer, study_version)
  checkpoint_dir <- welch_layer_checkpoint_dir(study_root, mode, layer,
                                               study_version)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()

  results <- vector("list", nrow(selected))
  for (i in seq_len(nrow(selected))) {
    scenario <- selected[i, , drop = FALSE]
    message(sprintf(
      "[%d/%d] %s (%s)", i, nrow(selected), scenario$scenario_id[[1L]], mode
    ))
    results[[i]] <- run_welch_scenario(
      scenario,
      mode = mode,
      output_dir = output_dir,
      checkpoint_dir = checkpoint_dir,
      adapter = adapter,
      workers = workers,
      resume = resume
    )
    print(results[[i]]$occupancy)
  }

  completed <- .welch_bind_rows(lapply(results, `[[`, "completed"))
  failures <- .welch_bind_rows(lapply(results, `[[`, "failures"))
  ledger <- .welch_bind_rows(lapply(results, `[[`, "ledger"))
  occupancy <- .welch_bind_rows(lapply(results, `[[`, "occupancy"))
  saveRDS(completed, file.path(output_dir, "completed.rds"), version = 3)
  saveRDS(failures, file.path(output_dir, "failures.rds"), version = 3)
  saveRDS(ledger, file.path(output_dir, "screening-ledger.rds"), version = 3)
  utils::write.csv(occupancy, file.path(output_dir, "occupancy.csv"),
                   row.names = FALSE)

  output_files <- c(
    completed = file.path(output_dir, "completed.rds"),
    failures = file.path(output_dir, "failures.rds"),
    screening_ledger = file.path(output_dir, "screening-ledger.rds"),
    occupancy = file.path(output_dir, "occupancy.csv")
  )
  manifest <- list(
    study = sprintf("prospective Welch study %s", study_version),
    study_version = study_version,
    mode = mode,
    layer = layer,
    candidate_hash = candidate_hash,
    started_at_utc = format(started, tz = "UTC", usetz = TRUE),
    finished_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    runtime_seconds = unname(as.numeric(difftime(Sys.time(), started, units = "secs"))),
    package_version = unname(read.dcf(file.path(project_root, "DESCRIPTION"))[1L, "Version"]),
    r_version = R.version.string,
    workers = as.integer(workers),
    n_boot = if (identical(mode, "smoke")) 20L else 1000L,
    scenario_ids = selected$scenario_id,
    required_cells = nrow(selected),
    completed_cells = sum(occupancy$quota_met),
    occupancy_failures = sum(!occupancy$occupancy_pass),
    attempted = sum(occupancy$attempted),
    screened = sum(occupancy$screened),
    screened_significant = sum(occupancy$screened_significant),
    completed = sum(occupancy$completed),
    failed = sum(occupancy$failed),
    excluded = sum(occupancy$excluded),
    silent_drops = sum(occupancy$attempted) -
      sum(occupancy$screened) - sum(occupancy$screening_failed),
    output_dir = normalizePath(output_dir, mustWork = TRUE),
    output_sha256 = as.list(vapply(
      output_files, digest::digest, character(1), file = TRUE, algo = "sha256"
    ))
  )
  jsonlite::write_json(
    manifest, file.path(output_dir, "manifest.json"),
    auto_unbox = TRUE, pretty = TRUE, null = "null"
  )
  invisible(list(
    completed = completed,
    failures = failures,
    ledger = ledger,
    occupancy = occupancy,
    manifest = manifest,
    output_dir = output_dir
  ))
}
