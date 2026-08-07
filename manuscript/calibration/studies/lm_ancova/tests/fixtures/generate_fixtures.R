root <- normalizePath(".")
study <- file.path(root, "manuscript/calibration/studies/lm_ancova")
env <- new.env(parent = globalenv())
sys.source(file.path(study, "R/load_study.R"), envir = env)
env$load_lm_ancova_study(project_root = root, envir = env)

make_rows <- function(scenario_id, truth_class, scores, sample_size, baseline_r2) {
  data.frame(
    scenario_id = scenario_id,
    truth_class = truth_class,
    analysis_conclusion = "significant",
    overall_score = as.numeric(scores),
    sample_size = as.integer(sample_size),
    baseline_r2 = as.numeric(baseline_r2),
    status = "completed",
    stringsAsFactors = FALSE
  )
}

block <- function(prefix, n, r2, n_per, seed) {
  set.seed(seed)
  null_scores <- rep(c(10, 20, 30, 40, 45, 48, 49, 50), length.out = n_per)
  bord_scores <- rep(c(51, 55, 58, 60, 62, 65, 68, 70), length.out = n_per)
  clear_scores <- rep(c(71, 75, 78, 80, 82, 85, 90, 95), length.out = n_per)
  null_scores <- pmin(50, null_scores)
  bord_scores <- pmin(70, pmax(51, bord_scores))
  clear_scores <- pmax(71, clear_scores)
  rbind(
    make_rows(paste0(prefix, "_null"), "null", null_scores, n, r2),
    make_rows(paste0(prefix, "_bord"), "borderline", bord_scores, n, r2),
    make_rows(paste0(prefix, "_clear"), "clear", clear_scores, n, r2)
  )
}

training <- rbind(
  block("train40", 40L, 0.10, 40L, 101L),
  block("train80", 80L, 0.40, 40L, 102L),
  block("train160", 160L, 0.70, 40L, 103L)
)
validation <- rbind(
  block("val60", 60L, 0.25, 100L, 201L),
  block("val120", 120L, 0.55, 100L, 202L),
  block("val240", 240L, 0.25, 100L, 203L)
)

fit <- env$fit_lm_ancova_cutoffs(training)
stopifnot(identical(fit$status, "candidate"), identical(fit$cutoffs, c(50L, 70L)))
message("training cutoffs: ", paste(fit$cutoffs, collapse = "/"))

dir.create(file.path(study, "tests/fixtures"), showWarnings = FALSE, recursive = TRUE)
saveRDS(training, file.path(study, "tests/fixtures/training-replicates.rds"), version = 2)
saveRDS(validation, file.path(study, "tests/fixtures/validation-replicates.rds"), version = 2)
message("wrote fixtures")
