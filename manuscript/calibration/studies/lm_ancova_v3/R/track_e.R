# Track E violation-detection metrics (frozen Phase 1 SAP).
# Pooled ΔAUC = AUC_score − AUC_p among significant clear rows.
# Score orientation pre-specified: clean > violated (clean = positive class).
# p orientation (−log10 p): empirically whichever favors p.

.LM_ANCOVA_V3_TRACK_E_GATE_DELTA <- 0.10
.LM_ANCOVA_V3_TRACK_E_BOOT_B <- 1000L
.LM_ANCOVA_V3_TRACK_E_BOOT_SEED <- 20260807L

#' Mann-Whitney / Wilcoxon AUC; higher marker favors the positive class.
lm_ancova_v3_mann_whitney_auc <- function(marker, is_positive) {
  keep <- is.finite(marker) & !is.na(is_positive)
  marker <- as.numeric(marker[keep])
  is_positive <- as.logical(is_positive[keep])
  n_pos <- sum(is_positive)
  n_neg <- sum(!is_positive)
  if (!n_pos || !n_neg) return(NA_real_)
  ranks <- rank(marker, ties.method = "average")
  (sum(ranks[is_positive]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

#' Orient −log10 p so AUC favors p (conservative for the score-add claim).
lm_ancova_v3_orient_p_marker <- function(neglog_p, is_clean) {
  auc_as_is <- lm_ancova_v3_mann_whitney_auc(neglog_p, is_clean)
  auc_inv <- lm_ancova_v3_mann_whitney_auc(-neglog_p, is_clean)
  if (!is.finite(auc_as_is) && !is.finite(auc_inv)) {
    return(list(
      marker = neglog_p, auc = NA_real_, direction = "undefined"
    ))
  }
  if (!is.finite(auc_as_is) ||
      (is.finite(auc_inv) && auc_inv > auc_as_is)) {
    list(marker = -neglog_p, auc = auc_inv, direction = "inverted")
  } else {
    list(marker = neglog_p, auc = auc_as_is, direction = "as_is")
  }
}

.lm_ancova_v3_track_e_prepare <- function(replicates) {
  if (!is.data.frame(replicates)) {
    stop("Track E replicates must be a data frame", call. = FALSE)
  }
  required <- c("overall_score", "original_p", "truth_class")
  missing <- setdiff(required, names(replicates))
  if (length(missing)) {
    stop(
      sprintf("Track E replicates missing columns: %s", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
  data <- replicates
  if ("analysis_conclusion" %in% names(data)) {
    conclusion <- tolower(gsub("[- ]", "_", as.character(data$analysis_conclusion)))
    data <- data[conclusion == "significant", , drop = FALSE]
  }
  if ("status" %in% names(data)) {
    data <- data[is.na(data$status) | as.character(data$status) == "completed", , drop = FALSE]
  }
  if ("diagnostic_only" %in% names(data)) {
    diag <- data$diagnostic_only
    data <- data[is.na(diag) | !as.logical(diag), , drop = FALSE]
  }
  truth <- as.character(data$truth_class)
  data <- data[truth == "clear", , drop = FALSE]

  if (!("violation_type" %in% names(data))) {
    data$violation_type <- NA_character_
  }
  vt <- as.character(data$violation_type)
  vt[is.na(vt) | !nzchar(vt)] <- NA_character_
  data$violation_type <- vt
  data$is_clean <- is.na(data$violation_type)
  data$is_violated <- !data$is_clean

  if (!nrow(data) || !any(data$is_clean) || !any(data$is_violated)) {
    stop(
      "Track E requires significant clear clean and violated rows",
      call. = FALSE
    )
  }
  data$neglog_p <- -log10(pmax(as.numeric(data$original_p), .Machine$double.xmin))
  data
}

.lm_ancova_v3_track_e_delta <- function(data, p_direction = NULL) {
  score_auc <- lm_ancova_v3_mann_whitney_auc(data$overall_score, data$is_clean)
  if (is.null(p_direction)) {
    oriented <- lm_ancova_v3_orient_p_marker(data$neglog_p, data$is_clean)
  } else if (identical(p_direction, "inverted")) {
    oriented <- list(
      marker = -data$neglog_p,
      auc = lm_ancova_v3_mann_whitney_auc(-data$neglog_p, data$is_clean),
      direction = "inverted"
    )
  } else {
    oriented <- list(
      marker = data$neglog_p,
      auc = lm_ancova_v3_mann_whitney_auc(data$neglog_p, data$is_clean),
      direction = "as_is"
    )
  }
  list(
    auc_score = as.numeric(score_auc),
    auc_p = as.numeric(oriented$auc),
    delta_auc = as.numeric(score_auc) - as.numeric(oriented$auc),
    p_direction = oriented$direction
  )
}

.lm_ancova_v3_track_e_cluster_ids <- function(data) {
  # One cluster per clean sample-size cell (matched clean + its violations).
  if ("sample_size" %in% names(data) && any(!is.na(data$sample_size))) {
    return(as.character(as.integer(data$sample_size)))
  }
  if ("matched_clean_id" %in% names(data)) {
    ids <- as.character(data$matched_clean_id)
    ids[data$is_clean] <- as.character(data$scenario_id[data$is_clean])
    return(ids)
  }
  if ("scenario_id" %in% names(data)) {
    return(as.character(data$scenario_id))
  }
  rep("all", nrow(data))
}

.lm_ancova_v3_track_e_bootstrap <- function(data, delta_fun, B = 1000L, seed = 20260807L) {
  clusters <- .lm_ancova_v3_track_e_cluster_ids(data)
  uniq <- unique(clusters)
  if (!length(uniq)) {
    return(c(ci_lower = NA_real_, ci_upper = NA_real_))
  }
  set.seed(as.integer(seed))
  boots <- numeric(as.integer(B))
  for (b in seq_len(as.integer(B))) {
    draw <- sample(uniq, length(uniq), replace = TRUE)
    pieces <- vector("list", length(draw))
    for (i in seq_along(draw)) {
      pieces[[i]] <- data[clusters == draw[[i]], , drop = FALSE]
    }
    boot_data <- do.call(rbind, pieces)
    if (!any(boot_data$is_clean) || !any(boot_data$is_violated)) {
      boots[[b]] <- NA_real_
    } else {
      boots[[b]] <- delta_fun(boot_data)$delta_auc
    }
  }
  boots <- boots[is.finite(boots)]
  if (!length(boots)) {
    return(c(ci_lower = NA_real_, ci_upper = NA_real_))
  }
  qs <- stats::quantile(boots, probs = c(0.025, 0.975), names = FALSE, type = 7)
  c(ci_lower = unname(qs[[1L]]), ci_upper = unname(qs[[2L]]))
}

#' Analyse Track E ΔAUC with the frozen gate and cluster bootstrap.
analyse_lm_ancova_v3_track_e <- function(replicates,
                                         cluster_B = .LM_ANCOVA_V3_TRACK_E_BOOT_B,
                                         cluster_seed = .LM_ANCOVA_V3_TRACK_E_BOOT_SEED) {
  data <- .lm_ancova_v3_track_e_prepare(replicates)
  pooled_point <- .lm_ancova_v3_track_e_delta(data)
  p_dir <- pooled_point$p_direction

  ci <- .lm_ancova_v3_track_e_bootstrap(
    data,
    delta_fun = function(d) .lm_ancova_v3_track_e_delta(d, p_direction = p_dir),
    B = cluster_B,
    seed = cluster_seed
  )

  vtypes <- sort(unique(data$violation_type[!is.na(data$violation_type)]))
  per_rows <- vector("list", length(vtypes))
  for (i in seq_along(vtypes)) {
    vt <- vtypes[[i]]
    sub <- data[data$is_clean | data$violation_type == vt, , drop = FALSE]
    metrics <- .lm_ancova_v3_track_e_delta(sub, p_direction = p_dir)
    per_rows[[i]] <- data.frame(
      violation_type = vt,
      auc_score = metrics$auc_score,
      auc_p = metrics$auc_p,
      delta_auc = metrics$delta_auc,
      stringsAsFactors = FALSE
    )
  }
  per_violation <- if (length(per_rows)) {
    do.call(rbind, per_rows)
  } else {
    data.frame(
      violation_type = character(),
      auc_score = numeric(),
      auc_p = numeric(),
      delta_auc = numeric(),
      stringsAsFactors = FALSE
    )
  }

  confirmed <- isTRUE(
    is.finite(pooled_point$delta_auc) &&
      pooled_point$delta_auc >= .LM_ANCOVA_V3_TRACK_E_GATE_DELTA &&
      is.finite(ci[["ci_lower"]]) &&
      ci[["ci_lower"]] > 0
  )
  verdict <- if (confirmed) "confirmed" else "not confirmed"

  list(
    pooled = list(
      auc_score = pooled_point$auc_score,
      auc_p = pooled_point$auc_p,
      delta_auc = pooled_point$delta_auc,
      p_direction = pooled_point$p_direction,
      ci_lower = unname(ci[["ci_lower"]]),
      ci_upper = unname(ci[["ci_upper"]]),
      cluster_B = as.integer(cluster_B),
      cluster_seed = as.integer(cluster_seed)
    ),
    per_violation = per_violation,
    confirmed = confirmed,
    verdict = verdict,
    gate = list(
      delta_min = .LM_ANCOVA_V3_TRACK_E_GATE_DELTA,
      ci_lower_gt = 0
    )
  )
}
