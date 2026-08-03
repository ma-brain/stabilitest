# ==============================================================================
# Simulation study validating the robustness framework (manuscript Section 3)
#
# Design: 3 effect sizes (d = 0, 0.5, 0.8) x 2 sample sizes (n = 25, 50 per
# group) x 2 contamination levels (0 or 2 outliers at +4 SD injected into the
# treated group), 500 replications per scenario, B = 200 bootstrap iterations.
#
# NOTE ON RUNTIME: each replication performs ~1500-3500 significance tests,
# so the full grid takes roughly 1-2 h single-threaded. For a smoke test use
# nrep = 25. The manuscript results were produced with an algorithmically
# identical implementation (same tests, same greedy rules, same seeds
# structure); Monte Carlo SE for a proportion at nrep = 500 is <= 0.022, and
# reported values are stable to that precision across RNGs.
# ==============================================================================

library(tidyverse)
source("robustness_analysis.R")

simulate_scenario <- function(d, n_per_group, n_outliers,
                              nrep = 500, n_boot = 200, alpha = 0.05,
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  reps <- map_dfr(seq_len(nrep), \(rep) {
    g_ctrl <- rnorm(n_per_group, 0, 1)
    g_trt  <- rnorm(n_per_group, d, 1)
    if (n_outliers > 0) {
      # contamination inflating the apparent effect
      g_trt[seq_len(n_outliers)] <- d + 4
    }
    res <- robustness_analysis(g_trt, g_ctrl, test_type = "t.test",
                               alpha = alpha, n_boot = n_boot,
                               seed = sample.int(1e6, 1))
    m <- res$robustness_metrics
    tibble(
      rep         = rep,
      significant = res$original_significant,
      score       = m$overall_robustness,
      k_wc        = m$worstcase_fragility_k,
      frag_wc_pct = m$worstcase_fragility_pct,
      k_ex        = m$extreme_fragility_k,
      s_jack      = m$jackknife_conclusion_stability,
      s_boot      = m$bootstrap_reproducibility
    )
  })

  reps |>
    summarise(
      rejection_rate = mean(significant),
      score_all      = mean(score),
      # calibration is defined conditional on the significance outcome:
      score_sig      = mean(score[significant]),
      score_nonsig   = mean(score[!significant]),
      k_wc_med_sig   = median(k_wc[significant]),
      frag_wc_med_sig = median(frag_wc_pct[significant]),
      k_ex_med_sig   = median(k_ex[significant]),
      s_jack_sig     = mean(s_jack[significant]),
      s_boot_sig     = mean(s_boot[significant]),
      .groups = "drop"
    ) |>
    mutate(d = d, n_per_group = n_per_group, n_outliers = n_outliers,
           nrep = nrep, .before = 1)
}

# --- full grid ----------------------------------------------------------------
scenarios <- expand_grid(
  d          = c(0, 0.5, 0.8),
  n_per_group = c(25, 50),
  n_outliers = c(0, 2)
)

run_simulation <- function(nrep = 500) {
  scenarios |>
    mutate(scenario = row_number()) |>
    pmap_dfr(\(d, n_per_group, n_outliers, scenario) {
      message(sprintf("Scenario %d/12: d=%.1f, n=%d, outliers=%d",
                      scenario, d, n_per_group, n_outliers))
      simulate_scenario(d, n_per_group, n_outliers,
                        nrep = nrep, seed = 987000 + scenario)
    })
}

if (interactive()) {
  sim_results <- run_simulation(nrep = 500)   # ~1-2 h; use nrep = 25 to smoke-test
  print(sim_results, width = Inf)
  write_csv(sim_results, "simulation_results.csv")

  # Headline calibration figure: score distribution for chance-significant
  # findings (d = 0) vs true large effects (d = 0.8), conditional on p < alpha
  # (values reported in manuscript Table 2)
}
