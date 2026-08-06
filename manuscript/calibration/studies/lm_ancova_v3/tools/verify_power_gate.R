#!/usr/bin/env Rscript

# Primary-test-only power check for lm_ancova_v3 Track E Phase 1.
# Clean clear cells: gated at tolerance 0.02 vs nominal 0.90.
# Violated clear cells: descriptive empirical significance rates only (no gate).
# Diagnostic null pairs: not part of this check.

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "lm_ancova_v3"),
    mustWork = TRUE
  )
}

.project_root <- function(study_root = .study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

`%||%` <- function(left, right) if (is.null(left)) right else left

args <- commandArgs(trailingOnly = TRUE)
draws <- 10000L
seed <- 20260807L
tol_power <- 0.02
out <- file.path(.study_root(), "artifacts", "summaries", "power-verification.csv")
out_viol <- file.path(
  .study_root(), "artifacts", "summaries", "power-verification-violated.csv"
)

if (length(args)) {
  if ("--draws" %in% args) {
    draws <- as.integer(args[[which(args == "--draws") + 1L]])
  }
  if ("--seed" %in% args) {
    seed <- as.integer(args[[which(args == "--seed") + 1L]])
  }
  if ("--output" %in% args) {
    out <- args[[which(args == "--output") + 1L]]
  }
}

study_root <- .study_root()
project_root <- .project_root(study_root)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_lm_ancova_v3_study(project_root = project_root, envir = env)

scenarios <- env$lm_ancova_v3_scenarios()
is_diag <- vapply(
  scenarios$parameters,
  function(p) isTRUE(p$diagnostic_only),
  logical(1)
)
is_viol <- vapply(
  scenarios$parameters,
  function(p) {
    vt <- p$violation_type
    !is.null(vt) && nzchar(as.character(vt))
  },
  logical(1)
)

clean <- scenarios[!is_diag & !is_viol & scenarios$truth_class == "clear", , drop = FALSE]
violated <- scenarios[!is_diag & is_viol & scenarios$truth_class == "clear", , drop = FALSE]
clean <- clean[order(clean$scenario_id), , drop = FALSE]
violated <- violated[order(violated$scenario_id), , drop = FALSE]

verify_one <- function(scenario, i, target, tol, gated) {
  scenario_id <- as.character(scenario$scenario_id[[1L]])
  scenario_seed <- as.integer(seed + i - 1L)
  verification <- env$verify_ancova_power(
    scenario,
    draws = draws,
    seed = scenario_seed
  )
  # Use v3 generator path for allocation_2to1 fixed-effect cells.
  # verify_ancova_power calls generate_lm_ancova directly; for allocation_2to1
  # re-run with generate_lm_ancova_v3 so the clean-solved effect is used.
  generator <- scenario$parameters[[1L]]$generator
  if (identical(as.character(generator$stress %||% ""), "allocation_2to1")) {
    set.seed(as.integer(scenario_seed))
    replicate_seeds <- sample.int(.Machine$integer.max, draws)
    analysis <- scenario$parameters[[1L]]$analysis
    alpha <- as.numeric(analysis$alpha %||% 0.05)
    term <- as.character(analysis$term %||% "treatmentB")
    formula <- stats::as.formula(
      analysis$formula %||% "outcome ~ treatment + baseline"
    )
    significant <- logical(draws)
    for (j in seq_len(draws)) {
      generated <- env$generate_lm_ancova_v3(scenario, seed = replicate_seeds[[j]])
      fit <- stats::lm(formula, data = generated$data)
      ct <- stats::coef(summary(fit))
      if (!term %in% rownames(ct)) {
        significant[[j]] <- FALSE
        next
      }
      significant[[j]] <- ct[term, "Pr(>|t|)"] < alpha
    }
    achieved <- mean(significant)
    used_v3_generator <- TRUE
  } else {
    achieved <- as.numeric(verification$achieved_power)
    used_v3_generator <- FALSE
  }
  abs_error <- abs(achieved - target)
  ok <- if (isTRUE(gated)) abs_error <= tol else NA
  data.frame(
    scenario_id = scenario_id,
    design_layer = as.character(scenario$design_layer[[1L]]),
    truth_class = as.character(scenario$truth_class[[1L]]),
    violation_type = as.character(
      scenario$parameters[[1L]]$violation_type %||% NA_character_
    ),
    sample_size = as.integer(scenario$sample_size[[1L]]),
    draws = as.integer(draws),
    seed = scenario_seed,
    target = target,
    achieved = achieved,
    abs_error = abs_error,
    tolerance = if (isTRUE(gated)) tol else NA_real_,
    pass = ok,
    gated = isTRUE(gated),
    used_v3_generator = used_v3_generator,
    used_robustness_score = FALSE,
    stringsAsFactors = FALSE
  )
}

clean_rows <- vector("list", nrow(clean))
failures <- character()
for (i in seq_len(nrow(clean))) {
  row <- verify_one(clean[i, , drop = FALSE], i, target = 0.90, tol = tol_power, gated = TRUE)
  if (!isTRUE(row$pass[[1L]])) {
    failures <- c(
      failures,
      sprintf(
        "%s: achieved=%.4f target=0.90 tol=%.2f",
        row$scenario_id[[1L]], row$achieved[[1L]], tol_power
      )
    )
  }
  clean_rows[[i]] <- row
  message(sprintf(
    "[clean] %s achieved=%.4f pass=%s",
    row$scenario_id[[1L]], row$achieved[[1L]], row$pass[[1L]]
  ))
}

viol_rows <- vector("list", nrow(violated))
for (i in seq_len(nrow(violated))) {
  # Seeds continue after clean block so streams stay disjoint.
  row <- verify_one(
    violated[i, , drop = FALSE],
    i + nrow(clean),
    target = 0.90,
    tol = tol_power,
    gated = FALSE
  )
  viol_rows[[i]] <- row
  message(sprintf(
    "[violated/descriptive] %s achieved=%.4f",
    row$scenario_id[[1L]], row$achieved[[1L]]
  ))
}

clean_df <- do.call(rbind, clean_rows)
viol_df <- do.call(rbind, viol_rows)
utils::write.csv(clean_df, out, row.names = FALSE)
utils::write.csv(viol_df, out_viol, row.names = FALSE)

log_path <- sub("\\.csv$", ".log", out)
sink(log_path)
cat("lm_ancova_v3 Track E power check\n")
cat(sprintf("draws=%d seed=%d tol_power=%.2f\n", draws, seed, tol_power))
cat(sprintf("clean_gated=%d violated_descriptive=%d\n", nrow(clean_df), nrow(viol_df)))
cat(sprintf("n_fail_clean=%d\n", length(failures)))
if (length(failures)) {
  cat("FAILURES:\n")
  cat(paste0("- ", failures, collapse = "\n"), "\n")
}
sink()

message("Wrote ", out)
message("Wrote ", out_viol)
message("Wrote ", log_path)

if (length(failures)) {
  quit(save = "no", status = 1L)
}
message("Power check PASSED for all clean cells.")
