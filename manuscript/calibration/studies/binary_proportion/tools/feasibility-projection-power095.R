fisher_p_matrix <- function(n) {
  P <- matrix(NA_real_, n + 1, n + 1)
  for (x0 in 0:n) for (x1 in 0:n)
    P[x0 + 1, x1 + 1] <- fisher.test(matrix(c(x1, n - x1, x0, n - x0), 2))$p.value
  P
}
chisq_p_matrix <- function(n) {
  P <- matrix(NA_real_, n + 1, n + 1)
  for (x0 in 0:n) for (x1 in 0:n)
    P[x0 + 1, x1 + 1] <- tryCatch(
      suppressWarnings(chisq.test(matrix(c(x1, n - x1, x0, n - x0), 2),
                                  correct = TRUE)$p.value),
      error = function(e) 1)
  P[!is.finite(P)] <- 1
  P
}
weights_mat <- function(n, p0, p1) outer(dbinom(0:n, n, p0), dbinom(0:n, n, p1))
exact_power <- function(P, n, p0, p1, a = 0.05) sum(weights_mat(n, p0, p1)[P <= a])
solve_p1 <- function(P, n, p0, target) {
  f <- function(p1) exact_power(P, n, p0, p1) - target
  if (f(0.999) < 0) return(NA_real_)
  uniroot(f, c(p0 + 1e-6, 0.999), tol = 1e-6)$root
}
project_ri <- function(P, n, p0, p1, a = 0.05, fr_max = 0.05) {
  sig <- P <= a
  wN <- weights_mat(n, p0, p0); wC <- weights_mat(n, p0, p1)
  pN <- P[sig]; wNs <- wN[sig] / sum(wN[sig])
  wCs <- wC[sig] / sum(wC[sig])
  atoms <- sort(unique(pN))
  fr <- vapply(atoms, function(t) sum(wNs[pN < t]), numeric(1))
  ok <- atoms[fr <= fr_max]
  if (!length(ok)) return(c(pstar = NA, fr = NA, ri = NA))
  pstar <- max(ok)
  c(pstar = pstar, fr = sum(wNs[pN < pstar]), ri = sum(wCs[pN < pstar]))
}

cat("=== clear power 0.95, n=25/arm ===\n")
for (test in c("fisher", "chisq")) {
  P <- if (test == "fisher") fisher_p_matrix(25) else chisq_p_matrix(25)
  for (p0 in c(0.10, 0.25, 0.50)) {
    p1 <- solve_p1(P, 25, p0, 0.95)
    if (is.na(p1)) { cat(sprintf("%-6s n= 25 p0=%.2f: unreachable\n", test, p0)); next }
    pr <- project_ri(P, 25, p0, p1)
    cat(sprintf("%-6s n= 25 p0=%.2f p1=%.3f projRI=%.3f %s\n",
                test, p0, p1, pr["ri"], ifelse(pr["ri"] >= 0.70, "PASS", "fail")))
  }
}
cat("\n=== chisq, clear power 0.95, n in {50,100,200} ===\n")
for (n in c(50, 100, 200)) {
  P <- chisq_p_matrix(n)
  for (p0 in c(0.10, 0.25, 0.50)) {
    p1 <- solve_p1(P, n, p0, 0.95)
    pr <- project_ri(P, n, p0, p1)
    cat(sprintf("chisq  n=%3d p0=%.2f p1=%.3f projRI=%.3f %s\n",
                n, p0, p1, pr["ri"], ifelse(pr["ri"] >= 0.70, "PASS", "fail")))
  }
}
