#!/usr/bin/env Rscript

# Resolve frozen effects and verify core power using primary Welch decisions
# only. This script never calls stabilitest or computes robustness scores.

.study_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1L && file.exists(file_arg)) {
    return(normalizePath(dirname(dirname(file_arg)), mustWork = TRUE))
  }
  normalizePath(
    file.path("manuscript", "calibration", "studies", "welch_unpaired"),
    mustWork = TRUE
  )
}

.project_root <- function(study_root = .study_root()) {
  normalizePath(file.path(study_root, "..", "..", "..", ".."), mustWork = TRUE)
}

.file_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}

args <- commandArgs(trailingOnly = TRUE)
draws <- 2000L
if ("--draws" %in% args) {
  value_index <- which(args == "--draws") + 1L
  if (value_index > length(args)) stop("--draws requires a value", call. = FALSE)
  draws <- as.integer(args[[value_index]])
}
if (is.na(draws) || draws < 100L) {
  stop("power verification requires at least 100 draws", call. = FALSE)
}

study_root <- .study_root()
project_root <- .project_root(study_root)
env <- new.env(parent = globalenv())
sys.source(file.path(study_root, "R", "load_study.R"), envir = env)
env$load_welch_study(project_root = project_root, envir = env)

resolved <- env$welch_scenarios()
for (i in seq_len(nrow(resolved))) {
  if (is.na(resolved$delta[[i]])) {
    resolved$delta[[i]] <- env$solve_welch_delta(
      target_power = resolved$target_power[[i]],
      n1 = resolved$n_group1[[i]],
      n2 = resolved$n_group2[[i]],
      sd1 = resolved$sd_group1[[i]],
      sd2 = resolved$sd_group2[[i]],
      alpha = resolved$alpha[[i]]
    )
  }
}

design_dir <- file.path(study_root, "artifacts", "design")
dir.create(design_dir, recursive = TRUE, showWarnings = FALSE)
resolved_path <- file.path(design_dir, "resolved-scenarios.csv")
power_path <- file.path(design_dir, "power-verification.csv")
manifest_path <- file.path(design_dir, "design-manifest.json")
utils::write.csv(resolved, resolved_path, row.names = FALSE, na = "")

core <- resolved[
  resolved$design_layer %in% c("training", "validation"), , drop = FALSE
]
verification_master_seed <- 202650001L
rows <- vector("list", nrow(core))
for (i in seq_len(nrow(core))) {
  scenario <- core[i, , drop = FALSE]
  scenario$seed <- verification_master_seed + i - 1L
  decisions <- logical(draws)
  analysis_failures <- 0L
  for (replicate_id in seq_len(draws)) {
    generated <- env$generate_welch_data(scenario, replicate_id)
    x1 <- generated$value[generated$group == "group1"]
    x2 <- generated$value[generated$group == "group2"]
    p_value <- tryCatch(
      stats::t.test(x1, x2, var.equal = FALSE)$p.value,
      error = function(error) {
        analysis_failures <<- analysis_failures + 1L
        NA_real_
      }
    )
    decisions[[replicate_id]] <- is.finite(p_value) &&
      p_value < scenario$alpha[[1L]]
  }

  target <- if (identical(scenario$truth[[1L]], "null")) {
    scenario$alpha[[1L]]
  } else {
    scenario$target_power[[1L]]
  }
  achieved <- mean(decisions)
  passed <- analysis_failures == 0L && abs(achieved - target) <= 0.03
  rows[[i]] <- data.frame(
    scenario_id = scenario$scenario_id[[1L]],
    design_layer = scenario$design_layer[[1L]],
    truth = scenario$truth[[1L]],
    n_group1 = scenario$n_group1[[1L]],
    n_group2 = scenario$n_group2[[1L]],
    sd_group1 = scenario$sd_group1[[1L]],
    sd_group2 = scenario$sd_group2[[1L]],
    delta = scenario$delta[[1L]],
    target_power = target,
    simulated_power = achieved,
    abs_error = abs(achieved - target),
    tolerance = 0.03,
    draws = draws,
    verification_seed = scenario$seed[[1L]],
    analysis_failures = analysis_failures,
    used_robustness_score = FALSE,
    power_gate_pass = passed,
    stringsAsFactors = FALSE
  )
  message(sprintf(
    "[%d/%d] %s power %.4f target %.2f %s",
    i, nrow(core), scenario$scenario_id[[1L]], achieved, target,
    if (passed) "PASS" else "FAIL"
  ))
}

power <- do.call(rbind, rows)
utils::write.csv(power, power_path, row.names = FALSE)

input_files <- c(
  sap = file.path(study_root, "CALIBRATION_SAP.md"),
  scenarios = file.path(study_root, "config", "scenarios.R"),
  power = file.path(study_root, "R", "power.R"),
  generator = file.path(study_root, "R", "generator.R"),
  verifier = file.path(study_root, "tools", "verify_power_gate.R")
)
manifest <- list(
  study = "prospective Welch study 2026-2",
  calibration_unit = "welch_unpaired",
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  git_head_at_generation = system2(
    "git", c("-C", project_root, "rev-parse", "HEAD"), stdout = TRUE
  ),
  r_version = R.version.string,
  power_method = "two-sided noncentral-t Welch-Satterthwaite approximation",
  verification_method = "primary Welch test decisions only",
  verification_draws_per_core_row = draws,
  verification_master_seed = verification_master_seed,
  power_tolerance = 0.03,
  core_rows = nrow(power),
  failed_core_rows = sum(!power$power_gate_pass),
  used_robustness_score = any(power$used_robustness_score),
  input_sha256 = as.list(vapply(input_files, .file_sha256, character(1))),
  output_sha256 = list(
    resolved_scenarios = .file_sha256(resolved_path),
    power_verification = .file_sha256(power_path)
  )
)
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

message(sprintf(
  "Wrote %s (%d core rows; %d failures)",
  design_dir, nrow(power), sum(!power$power_gate_pass)
))

if (any(!power$power_gate_pass) || any(power$used_robustness_score)) {
  quit(status = 1L)
}
