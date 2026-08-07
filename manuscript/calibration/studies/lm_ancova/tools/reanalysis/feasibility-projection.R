# Follow-up 1: incremental value of components beyond -log10(p)
# Follow-up 2: exact noncentral-t projection of 2-band feasibility
#              (p-value-equivalent score) for clear power 0.90/0.95/0.99
# Evidence base for docs/plans/2026-08-06-lm-ancova-v3-design.md.
# Run from the repository root: Rscript manuscript/calibration/studies/lm_ancova/tools/reanalysis/feasibility-projection.R

d <- readRDS("manuscript/calibration/studies/lm_ancova/artifacts/raw/training/completed_training_core.rds")
d <- d[d$status == "completed" & d$analysis_conclusion == "significant", ]
J <- d$jackknife_stability; Fg <- d$fragility_component
B <- d$bootstrap_reproducibility; cls <- d$truth_class; nn <- d$n
lp <- -log10(d$original_p)

auc <- function(x, y) {
  r <- rank(c(x, y)); n1 <- length(x)
  (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * length(y))
}

cat("=== Incremental AUC beyond p (null vs clear, pooled + per n) ===\n")
sub <- cls %in% c("null", "clear")
y <- as.integer(cls[sub] == "clear")
X <- data.frame(lp = lp[sub], J = J[sub], Fg = Fg[sub], B = B[sub],
                n = nn[sub], y = y)
m1 <- glm(y ~ lp, family = binomial, data = X)
m2 <- glm(y ~ lp + J + Fg + B, family = binomial, data = X)
m3 <- glm(y ~ lp * factor(n) + J + Fg + B, family = binomial, data = X)
p1 <- predict(m1); p2 <- predict(m2); p3 <- predict(m3)
cat(sprintf("AUC p alone           = %.4f\n", auc(p1[y == 1], p1[y == 0])))
cat(sprintf("AUC p + components    = %.4f\n", auc(p2[y == 1], p2[y == 0])))
cat(sprintf("AUC p*n + components  = %.4f\n", auc(p3[y == 1], p3[y == 0])))

cat("\n=== Exact projection: RI at FR-safe threshold, p-banding ===\n")
cat("FR-safe alpha' = 0.05 * 0.05 = 0.0025 (point constraint binding)\n")
proj <- function(n, power_target, alpha = 0.05) {
  df <- n - 3
  tcrit <- qt(1 - alpha / 2, df)
  # ncp for the clear class at this power target
  f <- function(ncp) pt(-tcrit, df, ncp) + 1 - pt(tcrit, df, ncp) - power_target
  ncp <- uniroot(f, c(0, 20))$root
  tstar <- qt(1 - 0.00125, df)          # |T| threshold: P(|T|>t*|null)=0.0025
  ri <- pt(-tstar, df, ncp) + 1 - pt(tstar, df, ncp)
  c(ncp = ncp, tstar = tstar, ri = ri)
}
for (pw in c(0.90, 0.95, 0.99)) {
  cat(sprintf("clear power %.2f:\n", pw))
  for (n in c(40, 60, 80, 120, 160, 240)) {
    pr <- proj(n, pw)
    cat(sprintf("  n=%3d  ncp=%.2f  RI at FR-safe threshold = %.3f %s\n",
                n, pr["ncp"], pr["ri"],
                ifelse(pr["ri"] >= 0.70, " >= 0.70 PASS", " < 0.70 fail")))
  }
}

cat("\n=== Sanity: empirical p-banding RI at FR-safe cut (v1 data, power 0.90) ===\n")
for (nv in c(40, 80, 160)) {
  pn <- d$original_p[cls == "null" & nn == nv]
  pc <- d$original_p[cls == "clear" & nn == nv]
  thr <- quantile(pn, 0.05)   # 5% of significant nulls below this p
  cat(sprintf("n=%3d  empirical p* = %.5f  RI = %.3f (projection %.3f)\n",
              nv, thr, mean(pc <= thr), proj(nv, 0.90)["ri"]))
}

cat("\n=== Required AUC for gates (binormal, equal variance) ===\n")
mu <- qnorm(0.95) + qnorm(0.70)
cat(sprintf("mu = z(.95)+z(.70) = %.3f -> required AUC = %.3f\n",
            mu, pnorm(mu / sqrt(2))))
cat(sprintf("observed best AUC (any weighting, incl. n): ~0.894\n"))
