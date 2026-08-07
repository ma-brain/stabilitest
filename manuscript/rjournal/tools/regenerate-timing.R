# ==============================================================================
# R Journal evidence artifact: performance/scalability table (Section
# "Performance"), per docs/plans/2026-08-06-r-journal-submission-plan.md
# Task 3.3.
#
# Benchmarks robustness_analysis() (Welch), robustness_lm(), robustness_glm(),
# robustness_surv(), and robustness_tost() at two sample sizes per arm using
# the installed package and its documented defaults (n_boot = 1000).
#
# The Cox benchmark uses freshly simulated synthetic survival data (fixed
# seed, generated in this script) because no survival dataset ships with the
# package; it exists only to time robustness_surv(), not to support any
# substantive claim.
# ==============================================================================

suppressPackageStartupMessages({
  library(stabilitest)
  library(survival)
})

OUTPUT_DIR <- file.path("manuscript", "rjournal", "artifacts", "timing")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

SIZES <- c(50L, 150L)   # patients per arm
SEED <- 20260807L

time_call <- function(expr) {
  gc(FALSE)
  t0 <- Sys.time()
  force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

make_welch_data <- function(n) {
  set.seed(SEED)
  list(g1 = rnorm(n, 0.5, 1), g2 = rnorm(n, 0, 1))
}

make_lm_data <- function(n) {
  set.seed(SEED)
  arm <- factor(rep(c("Placebo", "Active"), each = n))
  baseline <- rnorm(2 * n, 60, 10)
  change <- -0.4 * baseline + ifelse(arm == "Active", -8, 0) + rnorm(2 * n, 0, 12)
  data.frame(arm = arm, baseline = baseline, change = change)
}

make_glm_data <- function(n) {
  set.seed(SEED)
  arm <- factor(rep(c("Placebo", "Active"), each = n))
  linpred <- ifelse(arm == "Active", 0.9, -0.2)
  y <- rbinom(2 * n, 1, plogis(linpred))
  data.frame(arm = arm, y = y)
}

make_surv_data <- function(n) {
  set.seed(SEED)
  arm <- factor(rep(c("Placebo", "Active"), each = n))
  rate <- ifelse(arm == "Active", 0.02, 0.035)
  time <- rexp(2 * n, rate)
  censor_time <- runif(2 * n, 20, 80)
  event <- as.integer(time <= censor_time)
  time <- pmin(time, censor_time)
  data.frame(arm = arm, time = time, event = event)
}

results <- data.frame()

for (n in SIZES) {
  message(sprintf("n = %d per arm/group", n))

  d <- make_welch_data(n)
  t <- time_call(robustness_analysis(d$g1, d$g2, test_type = "t.test",
                                      n_boot = 1000, seed = 123))
  results <- rbind(results, data.frame(engine = "robustness_analysis (Welch)",
                                        n_per_arm = n, seconds = t))

  d <- make_lm_data(n)
  t <- time_call(robustness_lm(change ~ arm + baseline, data = d, term = "arm",
                                n_boot = 1000, seed = 123))
  results <- rbind(results, data.frame(engine = "robustness_lm (ANCOVA)",
                                        n_per_arm = n, seconds = t))

  d <- make_glm_data(n)
  t <- time_call(robustness_glm(y ~ arm, data = d, term = "arm",
                                 family = binomial(), n_boot = 1000, seed = 123))
  results <- rbind(results, data.frame(engine = "robustness_glm (binomial)",
                                        n_per_arm = n, seconds = t))

  d <- make_surv_data(n)
  t <- time_call(robustness_surv(Surv(time, event) ~ arm, data = d, term = "arm",
                                  n_boot = 1000, seed = 123))
  results <- rbind(results, data.frame(engine = "robustness_surv (Cox)",
                                        n_per_arm = n, seconds = t))

  d <- make_welch_data(n)
  t <- time_call(robustness_tost(d$g1, d$g2, type = "equivalence", endpoint = "mean",
                                  delta_L = -1, delta_U = 1, n_boot = 1000, seed = 123))
  results <- rbind(results, data.frame(engine = "robustness_tost (mean equivalence)",
                                        n_per_arm = n, seconds = t))
}

saveRDS(results, file.path(OUTPUT_DIR, "timing.rds"))
utils::write.csv(results, file.path(OUTPUT_DIR, "timing.csv"), row.names = FALSE)

manifest <- c(
  sprintf("generated_at: %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
  sprintf("package_version: %s", as.character(utils::packageVersion("stabilitest"))),
  sprintf("r_version: %s", R.version.string),
  sprintf("n_boot: %d", 1000L),
  sprintf("seed: %d", 123L),
  sprintf("sizes_per_arm: %s", paste(SIZES, collapse = ", ")),
  "note: robustness_surv benchmark uses synthetic simulated survival data (fixed seed in this script), timing only"
)
writeLines(manifest, file.path(OUTPUT_DIR, "manifest.txt"))

print(results, row.names = FALSE)
message("Timing artifact written to ", OUTPUT_DIR)
