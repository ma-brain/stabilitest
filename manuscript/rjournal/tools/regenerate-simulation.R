# ==============================================================================
# R Journal evidence artifact: simulation study (manuscript Section "Calibration
# policy and evidence" / simulation summary figure).
#
# Rewritten from manuscript/simulation_study.R to use ONLY the installed
# stabilitest package (library(stabilitest); no pkgload::load_all) and base R
# (no tidyverse), per docs/plans/2026-08-06-r-journal-submission-plan.md Task 3.
#
# Design (unchanged from the original manuscript Section 3 grid): 3 effect
# sizes (d = 0, 0.5, 0.8) x 2 sample sizes (n = 25, 50 per group) x 2
# contamination levels (0 or 2 outliers at +4 SD injected into the treated
# group) = 12 scenarios, 500 replications per scenario by default, B = 200
# bootstrap iterations, test_type = "t.test" (Welch).
#
# Unlike the original script, this one saves the FULL replicate-level results
# per scenario (not just scenario summaries), because the article's simulation
# figure (score distributions, null vs d = 0.8) needs the raw distribution.
#
# NOTE ON RUNTIME: the full grid (500 reps/scenario) takes roughly 1-2 h
# single-threaded on a laptop-class machine. Use --smoke (nrep = 5, n_boot =
# 20) to verify the pipeline runs end to end in seconds.
# ==============================================================================

suppressPackageStartupMessages(library(stabilitest))

MASTER_SEED <- 20260807L

.script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1L) {
    return(normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE))
  }
  NA_character_
}

.usage <- function() {
  paste(
    "Usage: Rscript manuscript/rjournal/tools/regenerate-simulation.R [options]",
    "",
    "Options:",
    "  --nrep N        replications per scenario (default: 500)",
    "  --n-boot B      bootstrap iterations (default: 200)",
    "  --output-dir P  destination directory (default: manuscript/rjournal/artifacts/simulation)",
    "  --smoke         nrep=5, n_boot=20, writes to <output-dir>-smoke",
    "  --help          show this message without running",
    sep = "\n"
  )
}

.parse_positive_integer <- function(value, option) {
  parsed <- suppressWarnings(as.numeric(value))
  if (length(parsed) != 1L || is.na(parsed) || !is.finite(parsed) ||
      parsed < 1 || parsed != floor(parsed)) {
    stop(sprintf("%s must be a positive integer", option), call. = FALSE)
  }
  as.integer(parsed)
}

.parse_args <- function(args) {
  value_options <- c("--nrep", "--n-boot", "--output-dir")
  flag_options <- c("--smoke", "--help")
  known <- c(value_options, flag_options)
  seen <- character()
  values <- list()
  i <- 1L
  while (i <= length(args)) {
    opt <- args[[i]]
    if (!opt %in% known) stop(sprintf("Unknown option: %s", opt), call. = FALSE)
    if (opt %in% seen) stop(sprintf("%s may only be supplied once", opt), call. = FALSE)
    seen <- c(seen, opt)
    if (opt %in% value_options) {
      if (i == length(args) || startsWith(args[[i + 1L]], "--")) {
        stop(sprintf("%s requires a value", opt), call. = FALSE)
      }
      values[[opt]] <- args[[i + 1L]]
      i <- i + 2L
    } else {
      values[[opt]] <- TRUE
      i <- i + 1L
    }
  }

  help <- "--help" %in% seen
  smoke <- "--smoke" %in% seen
  if (smoke && any(c("--nrep", "--n-boot") %in% seen)) {
    stop("--smoke cannot be combined with --nrep or --n-boot", call. = FALSE)
  }

  nrep <- if (smoke) 5L else if ("--nrep" %in% seen) {
    .parse_positive_integer(values[["--nrep"]], "--nrep")
  } else 500L
  n_boot <- if (smoke) 20L else if ("--n-boot" %in% seen) {
    .parse_positive_integer(values[["--n-boot"]], "--n-boot")
  } else 200L

  default_dir <- file.path("manuscript", "rjournal", "artifacts",
                            if (smoke) "simulation-smoke" else "simulation")
  output_dir <- if ("--output-dir" %in% seen) values[["--output-dir"]] else default_dir

  list(nrep = nrep, n_boot = n_boot, output_dir = output_dir,
       smoke = smoke, help = help)
}

scenarios <- expand.grid(
  d = c(0, 0.5, 0.8),
  n_per_group = c(25L, 50L),
  n_outliers = c(0L, 2L),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
scenarios <- scenarios[order(scenarios$d, scenarios$n_per_group, scenarios$n_outliers), ]
scenarios$scenario <- seq_len(nrow(scenarios))
rownames(scenarios) <- NULL

simulate_scenario <- function(d, n_per_group, n_outliers, scenario_id,
                               nrep, n_boot, alpha = 0.05, seed) {
  set.seed(seed)
  cols <- c("rep", "significant", "score", "k_wc", "frag_wc_pct", "k_ex",
            "s_jack", "s_boot", "original_p")
  out <- as.data.frame(matrix(NA_real_, nrow = nrep, ncol = length(cols)))
  names(out) <- cols

  for (rep in seq_len(nrep)) {
    g_ctrl <- rnorm(n_per_group, 0, 1)
    g_trt  <- rnorm(n_per_group, d, 1)
    if (n_outliers > 0) {
      g_trt[seq_len(n_outliers)] <- d + 4
    }
    rep_seed <- sample.int(1e6, 1)
    res <- robustness_analysis(g_trt, g_ctrl, test_type = "t.test",
                                alpha = alpha, n_boot = n_boot,
                                seed = rep_seed)
    m <- res$robustness_metrics
    out$rep[rep]         <- rep
    out$significant[rep] <- as.numeric(res$original_significant)
    out$score[rep]       <- m$overall_robustness
    out$k_wc[rep]         <- m$worstcase_fragility_k
    out$frag_wc_pct[rep]  <- m$worstcase_fragility_pct
    out$k_ex[rep]         <- m$extreme_fragility_k
    out$s_jack[rep]       <- m$jackknife_conclusion_stability
    out$s_boot[rep]       <- m$bootstrap_reproducibility
    out$original_p[rep]   <- res$original_p
  }

  out$significant <- as.logical(out$significant)
  out$scenario <- scenario_id
  out$d <- d
  out$n_per_group <- n_per_group
  out$n_outliers <- n_outliers
  out$scenario_seed <- seed
  out
}

summarise_scenario <- function(reps) {
  sig <- reps$significant
  data.frame(
    scenario        = reps$scenario[1],
    d               = reps$d[1],
    n_per_group     = reps$n_per_group[1],
    n_outliers      = reps$n_outliers[1],
    nrep            = nrow(reps),
    n_boot          = NA_integer_,
    scenario_seed   = reps$scenario_seed[1],
    rejection_rate  = mean(sig),
    score_all       = mean(reps$score),
    score_all_sd    = stats::sd(reps$score),
    score_sig       = mean(reps$score[sig]),
    score_nonsig    = mean(reps$score[!sig]),
    k_wc_med_sig    = stats::median(reps$k_wc[sig]),
    frag_wc_med_sig = stats::median(reps$frag_wc_pct[sig]),
    k_ex_med_sig    = stats::median(reps$k_ex[sig]),
    s_jack_sig      = mean(reps$s_jack[sig]),
    s_boot_sig      = mean(reps$s_boot[sig])
  )
}

run <- function(opts) {
  dir.create(opts$output_dir, recursive = TRUE, showWarnings = FALSE)
  scenario_count <- nrow(scenarios)
  summaries <- vector("list", scenario_count)
  scenario_seeds <- integer(scenario_count)
  t_start <- Sys.time()

  for (idx in seq_len(scenario_count)) {
    sc <- scenarios[idx, ]
    scenario_seed <- MASTER_SEED + sc$scenario
    scenario_seeds[idx] <- scenario_seed
    message(sprintf(
      "[%s] Scenario %d/%d: d=%.1f, n=%d, outliers=%d (seed=%d)",
      format(Sys.time(), "%H:%M:%S"), idx, scenario_count,
      sc$d, sc$n_per_group, sc$n_outliers, scenario_seed
    ))
    reps <- simulate_scenario(sc$d, sc$n_per_group, sc$n_outliers, sc$scenario,
                               nrep = opts$nrep, n_boot = opts$n_boot,
                               seed = scenario_seed)
    saveRDS(reps, file.path(opts$output_dir, sprintf("scenario-%02d.rds", sc$scenario)))
    summary_row <- summarise_scenario(reps)
    summary_row$n_boot <- opts$n_boot
    summaries[[idx]] <- summary_row
  }

  t_end <- Sys.time()
  summary_df <- do.call(rbind, summaries)
  saveRDS(summary_df, file.path(opts$output_dir, "simulation-summary.rds"))
  utils::write.csv(summary_df, file.path(opts$output_dir, "simulation-summary.csv"),
                    row.names = FALSE)

  script_path <- .script_path()
  manifest <- list(
    generated_at_start = format(t_start, "%Y-%m-%dT%H:%M:%S%z"),
    generated_at_end   = format(t_end, "%Y-%m-%dT%H:%M:%S%z"),
    runtime_seconds    = as.numeric(difftime(t_end, t_start, units = "secs")),
    package_version    = as.character(utils::packageVersion("stabilitest")),
    r_version          = R.version.string,
    platform           = R.version$platform,
    master_seed        = MASTER_SEED,
    nrep_per_scenario  = opts$nrep,
    n_boot             = opts$n_boot,
    scenario_count     = scenario_count,
    scenario_seeds     = as.list(setNames(scenario_seeds, sprintf("scenario_%02d", scenarios$scenario))),
    script_path        = script_path,
    script_md5         = if (!is.na(script_path)) {
      tryCatch(as.character(tools::md5sum(script_path)), error = function(e) NA_character_)
    } else NA_character_,
    smoke              = opts$smoke
  )
  manifest_json <- .to_json(manifest)
  writeLines(manifest_json, file.path(opts$output_dir, "manifest.json"))

  message(sprintf("Done in %.1f minutes. Artifacts in %s",
                   manifest$runtime_seconds / 60, opts$output_dir))
  invisible(summary_df)
}

# Minimal dependency-free JSON writer (avoids requiring jsonlite for this
# reproduction path).
.to_json <- function(x, indent = 0) {
  pad <- strrep("  ", indent)
  pad1 <- strrep("  ", indent + 1)
  if (is.list(x) && !is.null(names(x))) {
    items <- vapply(names(x), function(nm) {
      sprintf('%s"%s": %s', pad1, nm, .to_json(x[[nm]], indent + 1))
    }, character(1))
    paste0("{\n", paste(items, collapse = ",\n"), "\n", pad, "}")
  } else if (is.list(x)) {
    items <- vapply(x, .to_json, character(1), indent = indent + 1)
    paste0("[", paste(items, collapse = ", "), "]")
  } else if (is.character(x)) {
    if (length(x) == 1L) {
      if (is.na(x)) "null" else sprintf('"%s"', gsub('"', '\\\\"', x))
    } else {
      paste0("[", paste(sprintf('"%s"', x), collapse = ", "), "]")
    }
  } else if (is.logical(x)) {
    tolower(as.character(x))
  } else {
    if (length(x) == 1L) {
      if (is.na(x)) "null" else format(x, scientific = FALSE)
    } else {
      paste0("[", paste(format(x, scientific = FALSE), collapse = ", "), "]")
    }
  }
}

.is_direct <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_arg) == 1L && grepl("regenerate-simulation\\.R$", file_arg)
}

if (.is_direct()) {
  opts <- .parse_args(commandArgs(trailingOnly = TRUE))
  if (opts$help) {
    cat(.usage(), "\n")
  } else {
    run(opts)
  }
}
