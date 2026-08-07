# Exact feasibility projection for two-band calibration of 2x2 proportion tests.
# No simulation: enumerate all tables (x0, x1), x_g ~ Bin(n, p_g), balanced arms.
# Question: at FR-safe threshold (FR <= 0.05 among significant nulls), what is
# the projected Not-fragile identification RI among significant clear rows,
# for a p-monotone score (the ANCOVA lesson: the composite is ~monotone in p)?
# Gate for bands: RI >= 0.70.

fisher_p_matrix <- function(n) {
  # p-value of fisher.test for every table (x0, x1), 0..n each
  P <- matrix(NA_real_, n + 1, n + 1)
  for (x0 in 0:n) for (x1 in 0:n) {
    m <- matrix(c(x1, n - x1, x0, n - x0), 2)
    P[x0 + 1, x1 + 1] <- fisher.test(m)$p.value
  }
  P
}

chisq_p_matrix <- function(n) {
  P <- matrix(NA_real_, n + 1, n + 1)
  for (x0 in 0:n) for (x1 in 0:n) {
    m <- matrix(c(x1, n - x1, x0, n - x0), 2)
    P[x0 + 1, x1 + 1] <- tryCatch(
      suppressWarnings(chisq.test(m, correct = TRUE)$p.value),
      error = function(e) NA_real_)
  }
  P[is.na(P)] <- 1
  P
}

weights_mat <- function(n, p0, p1) outer(dbinom(0:n, n, p0), dbinom(0:n, n, p1))

exact_power <- function(P, n, p0, p1, alpha = 0.05) {
  sum(weights_mat(n, p0, p1)[P <= alpha])
}

solve_p1 <- function(P, n, p0, target, alpha = 0.05) {
  f <- function(p1) exact_power(P, n, p0, p1, alpha) - target
  if (f(0.999) < 0) return(NA_real_)
  uniroot(f, c(p0 + 1e-6, 0.999), tol = 1e-6)$root
}

# FR-safe threshold and projected RI on the p-scale (score ~ monotone in p)
project_ri <- function(P, n, p0, p1, alpha = 0.05, fr_max = 0.05) {
  sig <- P <= alpha
  wN <- weights_mat(n, p0, p0); wC <- weights_mat(n, p0, p1)
  pN <- P[sig]; wNs <- wN[sig] / sum(wN[sig])   # null | significant
  pC <- P[sig]; wCs <- wC[sig] / sum(wC[sig])   # clear | significant
  # candidate thresholds: the discrete atoms; find largest p* with
  # P(p < p* | null sig) <= fr_max
  atoms <- sort(unique(pN))
  fr <- vapply(atoms, function(t) sum(wNs[pN < t]), numeric(1))
  ok <- atoms[fr <= fr_max]
  if (!length(ok)) return(c(pstar = NA, fr = NA, ri = NA))
  pstar <- max(ok)
  c(pstar = pstar,
    fr = sum(wNs[pN < pstar]),
    ri = sum(wCs[pC < pstar]))
}

cat("=== Exact projection: Fisher (top) / chisq+correction (bottom) ===\n")
cat("clear power 0.90, alpha 0.05, balanced arms; RI gate >= 0.70\n\n")
for (test in c("fisher", "chisq")) {
  cat(sprintf("--- %s ---\n", test))
  for (n in c(25, 50, 100, 200)) {
    P <- if (test == "fisher") fisher_p_matrix(n) else chisq_p_matrix(n)
    for (p0 in c(0.10, 0.25, 0.50)) {
      p1 <- solve_p1(P, n, p0, 0.90)
      if (is.na(p1)) { cat(sprintf("n=%3d p0=%.2f: 0.90 power unreachable\n", n, p0)); next }
      pr <- project_ri(P, n, p0, p1)
      # also type-I of the test itself (conservativeness) among all nulls
      t1 <- exact_power(P, n, p0, p0)
      cat(sprintf(
        "n=%3d/arm p0=%.2f p1=%.3f  typeI=%.4f  p*=%.5f  FR=%.3f  projRI=%.3f %s\n",
        n, p0, p1, t1, pr["pstar"], pr["fr"], pr["ri"],
        ifelse(!is.na(pr["ri"]) && pr["ri"] >= 0.70, "PASS", "fail")))
    }
  }
}

cat("\n=== Same, clear power 0.95 (escalation), Fisher only ===\n")
for (n in c(50, 100, 200)) {
  P <- fisher_p_matrix(n)
  for (p0 in c(0.10, 0.25, 0.50)) {
    p1 <- solve_p1(P, n, p0, 0.95)
    if (is.na(p1)) next
    pr <- project_ri(P, n, p0, p1)
    cat(sprintf("n=%3d/arm p0=%.2f p1=%.3f  projRI=%.3f %s\n",
        n, p0, p1, pr["ri"],
        ifelse(!is.na(pr["ri"]) && pr["ri"] >= 0.70, "PASS", "fail")))
  }
}
