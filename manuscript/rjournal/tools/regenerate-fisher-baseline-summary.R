#!/usr/bin/env Rscript

# Descriptive comparison of the validated Fisher cutoff with plain p-value
# rules on the published Fisher study rows. Nothing is refit: the score cutoff
# (58) is the frozen candidate, and the matched p-value rule is chosen on
# TRAINING rows only (grid point whose training false reassurance is closest
# to the score's). The held-out rows were opened by the original study; here
# they are read descriptively, never for any gate or selection.

suppressPackageStartupMessages(library(stabilitest))

.script_path <- normalizePath(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE),
                             value = TRUE)[[1L]]),
  mustWork = TRUE
)
.project_root <- normalizePath(file.path(dirname(.script_path), "..", "..", ".."),
                               mustWork = TRUE)
.published_dir <- file.path(
  .project_root, "manuscript", "calibration", "studies", "binary_proportion",
  "published"
)
.source_path <- file.path(.published_dir, "output-hashes.txt")
.output_dir <- file.path(.project_root, "manuscript", "rjournal", "artifacts",
                         "fisher-p-baseline")

t_start <- Sys.time()
cutoff <- 58
p_grid <- c(0.04, 0.03, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001)
boot_B <- 1000L
boot_seed <- 2026102501L

load_rows <- function(file) {
  d <- readRDS(file.path(.published_dir, file))
  d <- d[d$truth_class %in% c("null", "clear") & is.finite(d$overall_score) &
           d$original_p < 0.05, , drop = FALSE]
  d
}
training <- load_rows("completed_training.rds")
validation <- load_rows("completed_validation.rds")

rates <- function(d, label) {
  c(false_reassurance = mean(label[d$truth_class == "null"]),
    clear_identification = mean(label[d$truth_class == "clear"]))
}
auc <- function(x, truth) {
  a <- x[truth == "clear"]
  b <- x[truth == "null"]
  mean(outer(a, b, ">") + 0.5 * outer(a, b, "=="))
}

train_score <- rates(training, training$overall_score > cutoff)
grid_fr <- vapply(p_grid, function(q) {
  rates(training, training$original_p <= q)[["false_reassurance"]]
}, numeric(1))
q_matched <- p_grid[which.min(abs(grid_fr - train_score[["false_reassurance"]]))]

# Paired scenario-cluster bootstrap of the clear-identification difference.
cluster_gain <- function(d) {
  clear <- d[d$truth_class == "clear", , drop = FALSE]
  stat <- function(rows) {
    mean(rows$overall_score > cutoff) - mean(rows$original_p <= q_matched)
  }
  ids <- unique(clear$scenario_id)
  set.seed(boot_seed)
  draws <- vapply(seq_len(boot_B), function(i) {
    pick <- sample(ids, length(ids), replace = TRUE)
    stat(do.call(rbind, lapply(pick, function(id) clear[clear$scenario_id == id, ])))
  }, numeric(1))
  c(gain = stat(clear), lower90 = unname(stats::quantile(draws, 0.05)),
    upper90 = unname(stats::quantile(draws, 0.95)))
}

one_set <- function(d, set) {
  s <- rates(d, d$overall_score > cutoff)
  p <- rates(d, d$original_p <= q_matched)
  g <- cluster_gain(d)
  data.frame(
    dataset = set,
    n_null = sum(d$truth_class == "null"),
    n_clear = sum(d$truth_class == "clear"),
    cutoff_score = cutoff,
    score_false_reassurance = s[["false_reassurance"]],
    score_clear_identification = s[["clear_identification"]],
    q_matched = q_matched,
    p_false_reassurance = p[["false_reassurance"]],
    p_clear_identification = p[["clear_identification"]],
    gain_clear_identification = g[["gain"]],
    gain_lower90 = g[["lower90"]],
    gain_upper90 = g[["upper90"]],
    auc_score = auc(d$overall_score, d$truth_class),
    auc_neg_log10_p = auc(-log10(d$original_p), d$truth_class),
    stringsAsFactors = FALSE
  )
}
summary <- rbind(one_set(training, "training"), one_set(validation, "held-out (descriptive)"))

grid_table <- do.call(rbind, lapply(p_grid, function(q) {
  r <- rates(training, training$original_p <= q)
  data.frame(q = q, false_reassurance = r[["false_reassurance"]],
             clear_identification = r[["clear_identification"]])
}))

dir.create(.output_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(summary, file.path(.output_dir, "fisher-p-baseline-summary.rds"), version = 3)
utils::write.csv(summary, file.path(.output_dir, "fisher-p-baseline-summary.csv"),
                 row.names = FALSE)
saveRDS(grid_table, file.path(.output_dir, "fisher-p-baseline-grid.rds"), version = 3)

registry <- stabilitest:::load_calibration_registry()
welch <- registry[registry$calibration_unit == "welch_unpaired", , drop = FALSE]
t_end <- Sys.time()
manifest <- list(
  artifact_role = "fisher_p_value_baseline_descriptive_comparison",
  generated_at = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
  runtime_seconds = as.numeric(difftime(t_end, t_start, units = "secs")),
  package_version = as.character(utils::packageVersion("stabilitest")),
  git_commit = system2("git", c("-C", .project_root, "rev-parse", "HEAD"),
                       stdout = TRUE),
  r_version = R.version.string,
  script_path = file.path("manuscript", "rjournal", "tools",
                          basename(.script_path)),
  script_sha256 = digest::digest(file = .script_path, algo = "sha256"),
  seed = boot_seed,
  source_study_verdict_hash = digest::digest(file = .source_path,
                                             algo = "sha256"),
  welch_calibration_status = welch$status,
  welch_label_emitted = identical(welch$status, "validated_method_specific"),
  matched_p_cutoff = q_matched,
  descriptive_only = TRUE
)
jsonlite::write_json(manifest, file.path(.output_dir, "manifest.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null", digits = 15)

print(t(summary))
print(grid_table)
message("Fisher p-value baseline summary written to ", .output_dir)
