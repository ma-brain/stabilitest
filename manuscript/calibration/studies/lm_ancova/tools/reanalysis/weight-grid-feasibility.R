# Exploratory reanalysis of lm_ancova v1 training replicates (2700 rows).
# Evidence base for docs/plans/2026-08-06-lm-ancova-v3-design.md.
# Run from the repository root: Rscript manuscript/calibration/studies/lm_ancova/tools/reanalysis/weight-grid-feasibility.R
# Question 1: does ANY weight vector on (jackknife, fragility, bootstrap)
#             admit feasible 3-band cutoffs under the frozen v1 gates?
# Question 2: what is the best achievable 2-band (null vs clear) rule, and
#             does the frozen v2 0/0.5/0.5 score pass its own pilot gate?
# Question 3: where does the separation actually live (components, n-drift)?
# Exploration only -- uses v1 TRAINING rows; v1 held-out stays closed.

suppressMessages({
  d <- readRDS("manuscript/calibration/studies/lm_ancova/artifacts/raw/training/completed_training_core.rds")
})
d <- d[d$status == "completed" & d$analysis_conclusion == "significant", ]
cat("rows:", nrow(d), "\n")
print(table(d$truth_class, d$n))

J <- d$jackknife_stability
Fg <- d$fragility_component
B <- d$bootstrap_reproducibility
cls <- d$truth_class
nn <- d$n

# sanity: reconstruct v1 composite
v1 <- 0.4 * J + 0.4 * Fg + 0.2 * B
cat("max |v1 recon - overall_score|:", max(abs(v1 - d$overall_score)), "\n\n")

wilson_upper <- function(x, n, z = qnorm(0.95)) {
  p <- x / n
  c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  h <- z / (1 + z^2 / n) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c0 + h
}
wilson_lower <- function(x, n, z = qnorm(0.95)) {
  p <- x / n
  c0 <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  h <- z / (1 + z^2 / n) * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))
  c0 - h
}

auc <- function(x, y) {  # P(score_clear > score_null) via rank sum
  r <- rank(c(x, y)); n1 <- length(x); n2 <- length(y)
  (sum(r[seq_len(n1)]) - n1 * (n1 + 1) / 2) / (n1 * n2)
}

# ---- 3-band search over the weight simplex --------------------------------
# For score S and integer cutoffs L < U:
#   Fragile S<=L (null correct), Moderate L<S<=U (borderline), Robust S>U (clear)
# Gates (training-side, as in v1): FR pt<=0.05 & Wilson-up<=0.10;
#   RI pt>=0.70 & Wilson-lo>=0.60; bal acc>=0.70; each class acc>=0.60;
#   medians ordered.
grid_step <- 0.05
ws <- seq(0, 1, grid_step)
combos <- expand.grid(wj = ws, wf = ws)
combos$wb <- 1 - combos$wj - combos$wf
combos <- combos[combos$wb > -1e-9, ]
combos$wb[combos$wb < 0] <- 0

idx_null <- cls == "null"; idx_bor <- cls == "borderline"; idx_clr <- cls == "clear"
n_null <- sum(idx_null); n_bor <- sum(idx_bor); n_clr <- sum(idx_clr)

eval_weights <- function(wj, wf, wb, null_i, bor_i, clr_i, use_borderline = TRUE) {
  S <- wj * J + wf * Fg + wb * B
  # rescale to 0-100 support so integer cutoffs are meaningful regardless of w
  cuts <- 0:100
  Fn <- ecdf(S[null_i]); Fb <- if (use_borderline) ecdf(S[bor_i]) else NULL
  Fc <- ecdf(S[clr_i])
  nN <- sum(null_i); nB <- sum(bor_i); nC <- sum(clr_i)
  fn <- Fn(cuts); fc <- Fc(cuts)
  fb <- if (use_borderline) Fb(cuts) else rep(NA_real_, length(cuts))
  best <- NULL; best_ba <- -Inf; n_feas <- 0L
  med_ok <- if (use_borderline) {
    median(S[null_i]) < median(S[bor_i]) && median(S[bor_i]) < median(S[clr_i])
  } else TRUE
  for (Li in seq_along(cuts)) {
    L <- cuts[Li]
    fr <- 1 - fn[Li]                       # P(S > L | null)
    if (fr > 0.05) next
    fr_up <- wilson_upper(round(fr * nN), nN)
    if (fr_up > 0.10) next
    accN <- fn[Li]
    Uis <- which(cuts > L)
    ri <- 1 - fc[Uis]                      # P(S > U | clear)
    ri_ok <- ri >= 0.70 & wilson_lower(round(ri * nC), nC) >= 0.60
    accC <- ri
    if (use_borderline) {
      accB <- fb[Uis] - fb[Li]
      ba <- (accN + accB + accC) / 3
      ok <- ri_ok & accB >= 0.60 & accC >= 0.60 & accN >= 0.60 & ba >= 0.70 & med_ok
    } else {
      ba <- (accN + accC) / 2
      ok <- ri_ok
    }
    n_feas <- n_feas + sum(ok)
    if (any(ba[ok] > best_ba)) {
      k <- which(ok)[which.max(ba[ok])]
      best_ba <- ba[k]
      best <- c(L = L, U = cuts[Uis[k]], ba = ba[k], fr = fr,
                accN = accN, accB = if (use_borderline) fb[Uis[k]] - fb[Li] else NA,
                accC = accC[k])
    }
  }
  # also best unconstrained balanced accuracy (ignore gates) for reference
  ba_max <- -Inf
  for (Li in seq_along(cuts)) {
    Uis <- which(cuts > cuts[Li])
    if (!length(Uis)) next
    if (use_borderline) {
      ba <- (fn[Li] + (fb[Uis] - fb[Li]) + (1 - fc[Uis])) / 3
    } else {
      ba <- (fn[Li] + (1 - fc[Uis])) / 2
    }
    ba_max <- max(ba_max, max(ba))
  }
  list(best = best, n_feasible = n_feas, ba_unconstrained = ba_max, med_ok = med_ok)
}

cat("=== 3-band search over weight simplex (pooled, all n) ===\n")
res3 <- vector("list", nrow(combos))
for (i in seq_len(nrow(combos))) {
  res3[[i]] <- eval_weights(combos$wj[i], combos$wf[i], combos$wb[i],
                            idx_null, idx_bor, idx_clr, TRUE)
}
feas <- vapply(res3, function(r) r$n_feasible, integer(1))
bamax <- vapply(res3, function(r) r$ba_unconstrained, numeric(1))
cat("weight combos searched:", nrow(combos),
    "| combos with >=1 feasible (L,U):", sum(feas > 0), "\n")
top <- order(-bamax)[1:5]
for (k in top) {
  cat(sprintf("w=(J=%.2f,F=%.2f,B=%.2f)  best unconstrained bal.acc=%.3f\n",
              combos$wj[k], combos$wf[k], combos$wb[k], bamax[k]))
}

# ---- upper bound: optimal 1-D projection incl. n as covariate --------------
cat("\n=== Discriminant upper bounds (pooled) ===\n")
if (requireNamespace("MASS", quietly = TRUE)) {
  ld <- MASS::lda(x = cbind(J = J, Fg = Fg, B = B), grouping = factor(cls))
  S_ld <- as.numeric(cbind(J, Fg, B) %*% ld$scaling[, 1])
  ld2 <- MASS::lda(x = cbind(J = J, Fg = Fg, B = B, logn = log(nn)),
                   grouping = factor(cls))
  S_ld2 <- as.numeric(cbind(J, Fg, B, log(nn)) %*% ld2$scaling[, 1])
  for (nm in c("v1 0.4/0.4/0.2", "v2 0.5F/0.5B", "LDA(J,F,B)", "LDA(J,F,B,log n)")) {
    S <- switch(nm,
      "v1 0.4/0.4/0.2" = v1,
      "v2 0.5F/0.5B" = 0.5 * Fg + 0.5 * B,
      "LDA(J,F,B)" = S_ld,
      "LDA(J,F,B,log n)" = S_ld2)
    if (mean(S[idx_clr]) < mean(S[idx_null])) S <- -S
    a_nc <- auc(S[idx_clr], S[idx_null])
    a_nb <- auc(S[idx_bor], S[idx_null])
    a_bc <- auc(S[idx_clr], S[idx_bor])
    cat(sprintf("%-18s AUC null-vs-clear=%.3f  null-vs-bord=%.3f  bord-vs-clear=%.3f\n",
                nm, a_nc, a_nb, a_bc))
  }
}

# ---- per-n stratified analysis --------------------------------------------
cat("\n=== Per-n analysis: median score by class & n (v1 and v2 scores) ===\n")
v2 <- 0.5 * Fg + 0.5 * B
for (nv in sort(unique(nn))) {
  for (cc in c("null", "borderline", "clear")) {
    ii <- nn == nv & cls == cc
    cat(sprintf("n=%3d %-10s  v1: med=%5.1f  v2: med=%5.1f  J: med=%5.1f F: med=%5.1f B: med=%5.1f\n",
                nv, cc, median(v1[ii]), median(v2[ii]), median(J[ii]),
                median(Fg[ii]), median(B[ii])))
  }
}

cat("\n=== Per-n AUC null vs clear ===\n")
for (nv in sort(unique(nn))) {
  iN <- nn == nv & idx_null; iC <- nn == nv & idx_clr
  cat(sprintf("n=%3d  v1 AUC=%.3f  v2 AUC=%.3f  J=%.3f  F=%.3f  B=%.3f  p-value=%.3f\n",
      nv, auc(v1[iC], v1[iN]), auc(v2[iC], v2[iN]),
      auc(J[iC], J[iN]), auc(Fg[iC], Fg[iN]), auc(B[iC], B[iN]),
      auc(-log10(d$original_p[iC]), -log10(d$original_p[iN]))))
}

# ---- v2 pilot-gate preview on v1 training rows -----------------------------
cat("\n=== v2 (0.5F/0.5B) pilot-gate preview, pooled & by n ===\n")
gate_preview <- function(ii_null, ii_clr, tag) {
  Sn <- v2[ii_null]; Sc <- v2[ii_clr]
  delta <- median(Sc) - median(Sn)
  overlap <- mean(Sn > median(Sc))
  a <- auc(Sc, Sn)
  cat(sprintf("%-12s Delta=%5.1f (go>=20)  Overlap=%.3f (go<=0.10)  AUC=%.3f (go>=0.75)\n",
              tag, delta, overlap, a))
}
gate_preview(idx_null, idx_clr, "pooled")
for (nv in sort(unique(nn))) {
  gate_preview(nn == nv & idx_null, nn == nv & idx_clr, sprintf("n=%d", nv))
}

# ---- 2-band feasibility (null vs clear only), pooled and per-n -------------
cat("\n=== 2-band feasible L search (FR pt<=.05,Wup<=.10; RI pt>=.70,Wlo>=.60) ===\n")
two_band <- function(ii_null, ii_clr, S, tag) {
  Sn <- S[ii_null]; Sc <- S[ii_clr]
  nN <- length(Sn); nC <- length(Sc)
  best <- NULL
  for (L in 0:100) {
    fr <- mean(Sn > L)
    if (fr > 0.05 || wilson_upper(round(fr * nN), nN) > 0.10) next
    ri <- mean(Sc > L)
    if (ri < 0.70 || wilson_lower(round(ri * nC), nC) < 0.60) next
    if (is.null(best) || ri > best["ri"]) best <- c(L = L, fr = fr, ri = ri)
  }
  if (is.null(best)) {
    # report the least-bad L: max RI subject to FR constraint alone
    cand <- sapply(0:100, function(L) {
      fr <- mean(Sn > L)
      if (fr > 0.05 || wilson_upper(round(fr * nN), nN) > 0.10) return(NA_real_)
      mean(Sc > L)
    })
    if (all(is.na(cand))) {
      cat(sprintf("%-12s NO L passes even the FR constraint\n", tag))
    } else {
      k <- which.max(cand)
      cat(sprintf("%-12s infeasible; best RI at FR-safe L=%d is %.3f (need >=0.70)\n",
                  tag, k - 1, cand[k]))
    }
  } else {
    cat(sprintf("%-12s FEASIBLE: L=%d  FR=%.3f  RI=%.3f\n", tag, best["L"], best["fr"], best["ri"]))
  }
}
two_band(idx_null, idx_clr, v2, "v2 pooled")
for (nv in sort(unique(nn))) {
  two_band(nn == nv & idx_null, nn == nv & idx_clr, v2, sprintf("v2 n=%d", nv))
}
cat("--- same with v1 composite for reference ---\n")
two_band(idx_null, idx_clr, v1, "v1 pooled")

# ---- 3-band per-n search on best pooled weights and per-n weights ---------
cat("\n=== 3-band search restricted to each n stratum ===\n")
for (nv in sort(unique(nn))) {
  iN <- nn == nv & idx_null; iB <- nn == nv & idx_bor; iC <- nn == nv & idx_clr
  best_any <- NULL; best_ba <- -Inf; any_feas <- 0L
  for (i in seq_len(nrow(combos))) {
    r <- eval_weights(combos$wj[i], combos$wf[i], combos$wb[i], iN, iB, iC, TRUE)
    any_feas <- any_feas + (r$n_feasible > 0)
    if (r$ba_unconstrained > best_ba) {
      best_ba <- r$ba_unconstrained
      best_any <- combos[i, ]
    }
  }
  cat(sprintf("n=%3d  weight combos w/ feasible (L,U): %d | best unconstrained bal.acc=%.3f at w=(J=%.2f,F=%.2f,B=%.2f)\n",
              nv, any_feas, best_ba, best_any$wj, best_any$wf, best_any$wb))
}

# ---- component saturation / drift diagnostics ------------------------------
cat("\n=== Jackknife saturation: P(J = 100) by class & n ===\n")
for (nv in sort(unique(nn))) {
  cat(sprintf("n=%3d  null=%.2f  borderline=%.2f  clear=%.2f\n", nv,
      mean(J[nn == nv & idx_null] >= 100), mean(J[nn == nv & idx_bor] >= 100),
      mean(J[nn == nv & idx_clr] >= 100)))
}
cat("\n=== Correlation of components with -log10(p) (pooled significant rows) ===\n")
lp <- -log10(d$original_p)
cat(sprintf("cor(J,lp)=%.3f  cor(F,lp)=%.3f  cor(B,lp)=%.3f  cor(v2,lp)=%.3f\n",
            cor(J, lp), cor(Fg, lp), cor(B, lp), cor(v2, lp)))
cat(sprintf("Spearman: J=%.3f F=%.3f B=%.3f v2=%.3f\n",
            cor(J, lp, method = "spearman"), cor(Fg, lp, method = "spearman"),
            cor(B, lp, method = "spearman"), cor(v2, lp, method = "spearman")))
