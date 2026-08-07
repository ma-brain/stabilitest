# Mini-study: component behavior on binary data (fisher_exact), to choose
# calibration weights for the proportions family. 40 significant replicates
# per truth class per cell; components from the actual package engine.
suppressMessages(devtools::load_all("/Users/Marius/Github/stabilitest", quiet = TRUE))

cells <- list(
  list(n = 50, p0 = 0.25, p1 = 0.613),   # mid cell, projRI 0.764
  list(n = 25, p0 = 0.25, p1 = 0.756)    # knife-edge cell, projRI 0.702
)
N_SIG <- 40; N_BOOT <- 300; MAX_DRAWS <- 20000

rows <- list()
set.seed(20260806)
for (cell in cells) {
  for (cls in c("null", "clear")) {
    p1 <- if (cls == "null") cell$p0 else cell$p1
    got <- 0L; draws <- 0L
    while (got < N_SIG && draws < MAX_DRAWS) {
      draws <- draws + 1L
      g1 <- rbinom(cell$n, 1, p1)      # treatment
      g2 <- rbinom(cell$n, 1, cell$p0) # control
      tab <- matrix(c(sum(g1), cell$n - sum(g1), sum(g2), cell$n - sum(g2)), 2)
      if (fisher.test(tab)$p.value > 0.05) next
      got <- got + 1L
      res <- robustness_analysis(g1, g2, test_type = "fisher",
                                 n_boot = N_BOOT, seed = 1000 + draws)
      m <- res$robustness_metrics
      rows[[length(rows) + 1L]] <- data.frame(
        n = cell$n, cls = cls, p = res$original_p,
        J = m$jackknife_conclusion_stability,
        Fg = m$worstcase_fragility_component,
        B = m$bootstrap_reproducibility)
    }
    cat(sprintf("cell n=%d %s: %d significant from %d draws\n",
                cell$n, cls, got, draws))
  }
}
d <- do.call(rbind, rows)
saveRDS(d, "/private/tmp/claude-502/-Users-Marius-Github-stabilitest/a4ac089e-a616-4201-9fce-ec45b3f59919/scratchpad/prop_weight_ministudy.rds")

auc <- function(x, y) { r <- rank(c(x, y)); n1 <- length(x)
  (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * length(y)) }

cat("\n=== Jackknife saturation P(J = 100) ===\n")
for (nv in unique(d$n)) for (cc in c("null", "clear"))
  cat(sprintf("n=%2d %-5s  P(J=100)=%.2f  median J=%.1f\n", nv, cc,
      mean(d$J[d$n == nv & d$cls == cc] >= 100),
      median(d$J[d$n == nv & d$cls == cc])))

cat("\n=== Spearman with -log10(p) (pooled significant) ===\n")
lp <- -log10(d$p)
cat(sprintf("J=%.3f  F=%.3f  B=%.3f\n",
    cor(d$J, lp, method = "spearman"), cor(d$Fg, lp, method = "spearman"),
    cor(d$B, lp, method = "spearman")))

cat("\n=== AUC null vs clear (pooled and per n) for candidate weightings ===\n")
W <- list("0.5F/0.5B"   = c(0, .5, .5),
          "0.4J/0.4F/0.2B" = c(.4, .4, .2),
          "0.45/0.45/0.1"  = c(.1, .45, .45),
          "F only"      = c(0, 1, 0),
          "B only"      = c(0, 0, 1),
          "J only"      = c(1, 0, 0),
          "p itself"    = NULL)
for (nm in names(W)) {
  w <- W[[nm]]
  S <- if (is.null(w)) lp else w[1] * d$J + w[2] * d$Fg + w[3] * d$B
  a_all <- auc(S[d$cls == "clear"], S[d$cls == "null"])
  as_n <- sapply(unique(d$n), function(nv) {
    i <- d$n == nv
    auc(S[i & d$cls == "clear"], S[i & d$cls == "null"]) })
  nd <- length(unique(round(S[d$cls == "null"], 6)))
  cat(sprintf("%-15s AUC pooled=%.3f  per-n=%s  distinct null scores=%d\n",
      nm, a_all, paste(sprintf("%.3f", as_n), collapse = "/"), nd))
}
cat("\nnote: weights for 0.45/0.45/0.1 are (J=0.1, F=0.45, B=0.45)\n")
