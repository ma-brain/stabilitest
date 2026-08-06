#!/usr/bin/env Rscript

# Score-only pilot go/no-go recorder for lm_ancova_v2 (Track A / Gate A).
# Computes sealed separation metrics and writes SCORE_PILOT_GATE.json.
# Does not search the categorical cutoff L.

`%||%` <- function(left, right) if (is.null(left)) right else left

.ANCOVA_V2_SCORE_PILOT_THRESHOLDS <- list(
  delta = list(go = 20, marginal_low = 15),
  overlap = list(go = 0.10, marginal_high = 0.20),
  auc = list(go = 0.75, marginal_low = 0.70)
)

.ancova_v2_score_pilot_abort <- function(message) {
  stop(message, call. = FALSE)
}

.ancova_v2_score_pilot_eligible <- function(data) {
  if (!is.data.frame(data)) {
    .ancova_v2_score_pilot_abort("score pilot data must be a data frame")
  }
  required <- c("truth_class", "overall_score")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    .ancova_v2_score_pilot_abort(
      sprintf("missing columns: %s", paste(missing, collapse = ", "))
    )
  }
  if ("analysis_conclusion" %in% names(data)) {
    conclusion <- tolower(gsub("[- ]", "_", as.character(data$analysis_conclusion)))
    data <- data[conclusion == "significant", , drop = FALSE]
  }
  if ("status" %in% names(data)) {
    data <- data[is.na(data$status) | data$status == "completed", , drop = FALSE]
  }
  if ("diagnostic_only" %in% names(data)) {
    diag <- data$diagnostic_only
    data <- data[is.na(diag) | !as.logical(diag), , drop = FALSE]
  }
  if ("design_layer" %in% names(data)) {
    data <- data[
      is.na(data$design_layer) | as.character(data$design_layer) != "stress",
      , drop = FALSE
    ]
  }
  truth <- as.character(data$truth_class)
  data <- data[truth %in% c("null", "clear"), , drop = FALSE]
  if (!nrow(data)) {
    .ancova_v2_score_pilot_abort(
      "no eligible significant ANCOVA v2 null/clear pilot replicates"
    )
  }
  if (any(!is.finite(as.numeric(data$overall_score)))) {
    .ancova_v2_score_pilot_abort("overall_score must contain finite values")
  }
  data
}

# Mann-Whitney AUC with clear as the positive class (higher score favors clear).
.ancova_v2_score_pilot_auc <- function(score, is_clear) {
  keep <- is.finite(score) & !is.na(is_clear)
  score <- as.numeric(score[keep])
  is_clear <- as.logical(is_clear[keep])
  n_clear <- sum(is_clear)
  n_null <- sum(!is_clear)
  if (!n_clear || !n_null) return(NA_real_)
  ranks <- rank(score, ties.method = "average")
  (sum(ranks[is_clear]) - n_clear * (n_clear + 1) / 2) / (n_clear * n_null)
}

.ancova_v2_score_pilot_band_delta <- function(delta) {
  if (!isTRUE(is.finite(delta))) return("hard_no_go")
  thr <- .ANCOVA_V2_SCORE_PILOT_THRESHOLDS$delta
  if (delta >= thr$go) "go" else if (delta >= thr$marginal_low) "marginal" else "hard_no_go"
}

.ancova_v2_score_pilot_band_overlap <- function(overlap) {
  if (!isTRUE(is.finite(overlap))) return("hard_no_go")
  thr <- .ANCOVA_V2_SCORE_PILOT_THRESHOLDS$overlap
  if (overlap <= thr$go) {
    "go"
  } else if (overlap <= thr$marginal_high) {
    "marginal"
  } else {
    "hard_no_go"
  }
}

.ancova_v2_score_pilot_band_auc <- function(auc) {
  if (!isTRUE(is.finite(auc))) return("hard_no_go")
  thr <- .ANCOVA_V2_SCORE_PILOT_THRESHOLDS$auc
  if (auc >= thr$go) "go" else if (auc >= thr$marginal_low) "marginal" else "hard_no_go"
}

ancova_v2_score_pilot_metrics <- function(data, compute_auc = TRUE) {
  data <- .ancova_v2_score_pilot_eligible(data)
  truth <- as.character(data$truth_class)
  null_scores <- as.numeric(data$overall_score[truth == "null"])
  clear_scores <- as.numeric(data$overall_score[truth == "clear"])
  if (!length(null_scores) || !length(clear_scores)) {
    .ancova_v2_score_pilot_abort(
      "score pilot requires both null and clear significant rows"
    )
  }
  median_null <- stats::median(null_scores)
  median_clear <- stats::median(clear_scores)
  delta <- median_clear - median_null
  overlap <- mean(null_scores > median_clear)
  auc <- if (isTRUE(compute_auc)) {
    .ancova_v2_score_pilot_auc(data$overall_score, truth == "clear")
  } else {
    NA_real_
  }
  list(
    delta = as.numeric(delta),
    overlap = as.numeric(overlap),
    auc = as.numeric(auc),
    auc_computed = isTRUE(compute_auc),
    median_null = as.numeric(median_null),
    median_clear = as.numeric(median_clear),
    n_null = as.integer(length(null_scores)),
    n_clear = as.integer(length(clear_scores)),
    n_total = as.integer(nrow(data))
  )
}

ancova_v2_score_pilot_metric_bands <- function(delta, overlap, auc = NA_real_,
                                              auc_computed = FALSE) {
  bands <- list(
    delta = .ancova_v2_score_pilot_band_delta(delta),
    overlap = .ancova_v2_score_pilot_band_overlap(overlap)
  )
  if (isTRUE(auc_computed)) {
    bands$auc <- .ancova_v2_score_pilot_band_auc(auc)
  } else {
    bands$auc <- NULL
  }
  bands
}

ancova_v2_score_pilot_decide <- function(metrics, clear_power = 0.90,
                                         already_escalated = FALSE) {
  if (!is.list(metrics)) {
    .ancova_v2_score_pilot_abort("metrics must be a list from ancova_v2_score_pilot_metrics")
  }
  clear_power <- as.numeric(clear_power)
  if (!isTRUE(is.finite(clear_power)) || !(clear_power %in% c(0.90, 0.95))) {
    .ancova_v2_score_pilot_abort("clear_power must be 0.90 or 0.95")
  }
  bands <- ancova_v2_score_pilot_metric_bands(
    delta = metrics$delta,
    overlap = metrics$overlap,
    auc = metrics$auc %||% NA_real_,
    auc_computed = isTRUE(metrics$auc_computed)
  )
  statuses <- unlist(bands, use.names = FALSE)
  has_hard <- any(statuses == "hard_no_go")
  has_marginal <- any(statuses == "marginal")
  all_go <- length(statuses) > 0L && all(statuses == "go")

  decision <- "no_go"
  pass <- FALSE
  escalate <- FALSE
  recommended_clear_power <- NA_real_
  frozen_clear_power <- NA_real_

  if (isTRUE(all_go)) {
    decision <- "go"
    pass <- TRUE
    frozen_clear_power <- clear_power
  } else if (isTRUE(has_hard)) {
    decision <- "no_go"
  } else if (isTRUE(has_marginal)) {
    if (isTRUE(already_escalated) || isTRUE(all.equal(clear_power, 0.95))) {
      decision <- "no_go"
    } else {
      decision <- "escalate_clear_power"
      escalate <- TRUE
      recommended_clear_power <- 0.95
    }
  }

  list(
    pass = pass,
    decision = decision,
    clear_power_evaluated = clear_power,
    frozen_clear_power = frozen_clear_power,
    recommended_clear_power = recommended_clear_power,
    escalate = escalate,
    already_escalated = isTRUE(already_escalated),
    searched_L = FALSE,
    bands = bands,
    metrics = metrics,
    thresholds = .ANCOVA_V2_SCORE_PILOT_THRESHOLDS
  )
}

ancova_v2_score_pilot_quantiles_by_n <- function(data) {
  data <- .ancova_v2_score_pilot_eligible(data)
  if (!("n" %in% names(data))) {
    .ancova_v2_score_pilot_abort("by-n quantiles require an n column")
  }
  components <- c(
    "overall_score",
    "fragility_component",
    "bootstrap_reproducibility",
    "jackknife_stability"
  )
  components <- components[components %in% names(data)]
  if (!length(components)) {
    .ancova_v2_score_pilot_abort("no score/component columns available for quantiles")
  }

  rows <- list()
  for (n_val in sort(unique(as.integer(data$n)))) {
    for (truth in c("null", "clear")) {
      selected <- as.integer(data$n) == n_val & as.character(data$truth_class) == truth
      if (!any(selected)) next
      for (component in components) {
        values <- as.numeric(data[[component]][selected])
        values <- values[is.finite(values)]
        if (!length(values)) next
        qs <- stats::quantile(values, probs = c(0.25, 0.5, 0.75), names = FALSE, type = 7)
        rows[[length(rows) + 1L]] <- data.frame(
          n = as.integer(n_val),
          truth_class = truth,
          component = component,
          n_obs = as.integer(length(values)),
          q25 = as.numeric(qs[[1L]]),
          median = as.numeric(qs[[2L]]),
          q75 = as.numeric(qs[[3L]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      n = integer(), truth_class = character(), component = character(),
      n_obs = integer(), q25 = numeric(), median = numeric(), q75 = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.ancova_v2_score_pilot_code_commit <- function() {
  out <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE),
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  if (!length(out) || !nzchar(out[[1L]])) NA_character_ else as.character(out[[1L]])
}

ancova_v2_write_score_pilot_gate <- function(data, output,
                                             clear_power = 0.90,
                                             compute_auc = TRUE,
                                             already_escalated = FALSE) {
  metrics <- ancova_v2_score_pilot_metrics(data, compute_auc = compute_auc)
  decision <- ancova_v2_score_pilot_decide(
    metrics,
    clear_power = clear_power,
    already_escalated = already_escalated
  )
  quantiles <- tryCatch(
    ancova_v2_score_pilot_quantiles_by_n(data),
    error = function(e) NULL
  )

  stamp <- list(
    gate = "score_pilot",
    calibration_unit = "lm_ancova_v2",
    pass = decision$pass,
    decision = decision$decision,
    clear_power_evaluated = decision$clear_power_evaluated,
    frozen_clear_power = decision$frozen_clear_power,
    recommended_clear_power = decision$recommended_clear_power,
    escalate = decision$escalate,
    already_escalated = decision$already_escalated,
    searched_L = FALSE,
    metrics = decision$metrics,
    bands = decision$bands,
    thresholds = decision$thresholds,
    quantiles_by_n = quantiles,
    code_commit = .ancova_v2_score_pilot_code_commit(),
    recorded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
  )

  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    stamp,
    path = output,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    dataframe = "rows"
  )
  invisible(stamp)
}

.ancova_v2_score_pilot_study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "lm_ancova_v2"),
    mustWork = TRUE
  )
}

.ancova_v2_score_pilot_read_input <- function(path) {
  if (!file.exists(path)) {
    .ancova_v2_score_pilot_abort(sprintf("pilot input not found: %s", path))
  }
  if (dir.exists(path)) {
    rds_files <- list.files(path, pattern = "[.]rds$", recursive = TRUE, full.names = TRUE)
    if (!length(rds_files)) {
      .ancova_v2_score_pilot_abort(
        sprintf("no .rds pilot artifacts under: %s", path)
      )
    }
    parts <- lapply(rds_files, function(f) {
      obj <- readRDS(f)
      if (is.data.frame(obj)) {
        obj
      } else if (is.list(obj) && is.data.frame(obj$payload$replicates)) {
        obj$payload$replicates
      } else if (is.list(obj) && is.data.frame(obj$replicates)) {
        obj$replicates
      } else {
        NULL
      }
    })
    parts <- Filter(Negate(is.null), parts)
    if (!length(parts)) {
      .ancova_v2_score_pilot_abort(
        sprintf("no replicate data frames found under: %s", path)
      )
    }
    return(do.call(rbind, parts))
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) {
    obj <- readRDS(path)
    if (is.data.frame(obj)) return(obj)
    if (is.list(obj) && is.data.frame(obj$payload$replicates)) {
      return(obj$payload$replicates)
    }
    if (is.list(obj) && is.data.frame(obj$replicates)) return(obj$replicates)
    # Shared runner artifact: list(screen=..., analyse=list(<replicate tibbles>))
    if (is.list(obj) && is.list(obj$analyse)) {
      parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, obj$analyse)
      if (length(parts)) return(do.call(rbind, parts))
    }
    .ancova_v2_score_pilot_abort("RDS input did not contain a replicate data frame")
  }
  if (identical(ext, "csv")) {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }
  .ancova_v2_score_pilot_abort(
    sprintf("unsupported pilot input type: %s", path)
  )
}

.ancova_v2_score_pilot_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  study_root <- .ancova_v2_score_pilot_study_root()
  input <- NULL
  output <- file.path(study_root, "artifacts", "summaries", "SCORE_PILOT_GATE.json")
  clear_power <- 0.90
  compute_auc <- TRUE
  already_escalated <- FALSE

  if (length(args)) {
    if ("--input" %in% args) {
      input <- args[[which(args == "--input") + 1L]]
    }
    if ("--output" %in% args) {
      output <- args[[which(args == "--output") + 1L]]
    }
    if ("--clear-power" %in% args) {
      clear_power <- as.numeric(args[[which(args == "--clear-power") + 1L]])
    }
    if ("--no-auc" %in% args) {
      compute_auc <- FALSE
    }
    if ("--already-escalated" %in% args) {
      already_escalated <- TRUE
    }
  }

  if (is.null(input)) {
    candidates <- c(
      file.path(study_root, "artifacts", "raw", "pilot"),
      file.path(study_root, "outputs", "score-pilot"),
      file.path(study_root, "artifacts", "pilot")
    )
    input <- candidates[dir.exists(candidates) | file.exists(candidates)][1]
    if (is.na(input) || !nzchar(input %||% "")) {
      .ancova_v2_score_pilot_abort(
        paste(
          "no pilot input found; pass --input <rds|csv|dir>",
          "after score-only pilot compute"
        )
      )
    }
  }

  data <- .ancova_v2_score_pilot_read_input(input)
  stamp <- ancova_v2_write_score_pilot_gate(
    data = data,
    output = output,
    clear_power = clear_power,
    compute_auc = compute_auc,
    already_escalated = already_escalated
  )
  message(sprintf(
    "score pilot gate: decision=%s pass=%s frozen_clear_power=%s output=%s",
    stamp$decision,
    stamp$pass,
    stamp$frozen_clear_power %||% NA,
    normalizePath(output, mustWork = FALSE)
  ))
  invisible(stamp)
}

if (sys.nframe() == 0L) {
  .ancova_v2_score_pilot_main()
}
