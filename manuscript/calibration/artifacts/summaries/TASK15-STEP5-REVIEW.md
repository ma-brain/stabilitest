# Task 15 Step 5 — Independent freeze review

**Date:** 2026-08-05
**Worktree:** `.worktrees/calibration-task1` (`codex/calibration-task1`)
**Freeze commit reviewed:** `5d83d03` (production assemble → freeze → held-out, `validation_refit = FALSE`)
**Scope:** Synthesize independent code review and statistical review of the published all-uncalibrated freeze (`n_boot = 1000`).
**Out of scope:** Phase 6 package integration; any claim that Fragile / Moderate / Robust bands are empirically calibrated.

This note does **not** claim calibrated bands.

---

## Overall Step 5 verdict

| Question | Verdict |
|----------|---------|
| Is the freeze process sound (no refit, hashes, reasons)? | **Yes** |
| Is `uncalibrated` / `no_feasible_thresholds` for all 7 families scientifically credible? | **Yes** |
| Ship calibrated / validated score bands via Phase 6? | **No-go** |
| Ship an explicit all-uncalibrated registry via Phase 6? | **Conditional go** — only after provenance fixes, with docs/UI stating bands are not calibrated |
| Treat this freeze as SAP-complete publication evidence today? | **No** — provenance gaps remain |

**Synthesized gate:** Phase 6 must not proceed as “validated calibration.” The threshold *decision* (no feasible cutoffs) is correct; the scientific estimand and held-out design block calibrated shipping; provenance gaps block treating the freeze as SAP-complete even for an uncalibrated registry.

---

## Checklist (Step 5)

| Item | Result | Notes |
|------|--------|-------|
| Scenario balance | **Weak / incomplete** | Training covers null/borderline/clear for all families (uneven *n*). Held-out: only **two_sample** has null+borderline+clear with `stratum_complete=TRUE`; 6/7 families incomplete by catalog design. |
| Manifest hashes | **Pass** | Paired `scenario_manifest_hash = 11bc762c501dcd1cb9d1e40e607e6603` matches live scenarios; `reduced_fixture = FALSE`; listed MD5s in `output-hashes.txt` match; `candidate_hash = 3603f361…` ≠ `registry_hash = 6c8249be…` (reproduced by `calibration_analysis_from_files()`). |
| Failure exclusions | **Fail (provenance)** | Assemble keeps only `status == "completed"`; published manifests report `failed_replicates = 0` despite real production failures (e.g. `two_sample_stress_null_n20`: 369/1000 failed, ≫ 5%). |
| Uncertainty (Wilson) | **Pass** | One-sided Wilson (`qnorm(0.95)`) for FR upper / RI lower; **not** the binding failure mode (0 feasible cutoffs even without Wilson). |
| No-refit guarantee | **Pass** | Fit on training only; validate/evaluate held-out; manifests `validation_refit = FALSE`; train/val `scenario_id` overlap = 0. |
| Freeze conclusion coherence | **Pass** | All 7 families `uncalibrated` / `no_feasible_thresholds`; reason preserved (not overwritten as `training_fit_failed`). |

---

## Key freeze facts (do not reinterpret as calibration)

| Quantity | Value |
|----------|-------|
| Families | 7 / 7 `uncalibrated` |
| Reason | `no_feasible_thresholds` |
| Training completed replicates (assembled) | 28,722 |
| Validation completed replicates (assembled) | 10,759 |
| Held-out `stratum_complete` | **only `two_sample`** |
| Shared diagnostic bands in registry | 55 / 70 (not validated cutoffs) |
| Feasible L&lt;U under FR≤0.05 & RI≥0.70 | **0** families (with or without Wilson) |
| Sig-only counterfactual | Still 0 feasible for 6/7; **cox** only becomes a candidate (~62/67) |

### Training score medians by truth class (all conclusions)

| Family | Null median | Clear median | Ordering |
|--------|-------------|--------------|----------|
| binomial | 60.8 | 60.6 | ~tied |
| cox | 58.0 | 69.1 | clear &gt; null |
| lm | 59.6 | 59.8 | ~tied |
| poisson | 59.0 | 59.1 | ~tied |
| proportion | 81.0 | 63.0 | **inverted** |
| tost | 84.0 | 57.9 | **inverted** |
| two_sample | 68.6 | 83.9 | clear &gt; null |

Inversions and near-ties are driven in part by high-scoring true-null / non-significant (and TOST `not_equivalent`) rows that the non-significant registry marks as band-inapplicable.

---

## Findings by severity

### Critical

1. **No feasible calibrated cutoffs under the locked SAP** — Full integer grid yields zero FR≤0.05 & RI≥0.70 pairs for every family, with or without Wilson. All-uncalibrated is the correct publish outcome, not a freeze bug.
2. **FR fitting population vs band applicability mismatch** — FR is fit on all `truth_class == "null"` replicates, including stable non-significant / TOST `not_equivalent` conclusions that inflate FR and can invert null vs clear. That conflicts with `non-significant-registry.csv` (`bands_not_applicable`).
3. **Do not ship calibrated bands** — Even a sig-only / band-applicable counterfactual leaves six of seven families without feasible cutoffs; clear significant medians often sit in the moderate band (~59–67), so RI at 70 stays too low for several families.

### Important (provenance / release blockers for SAP-complete evidence)

4. **Published failure counts are zeroed by assemble** — Completed-only tables make `attempted == completed` and hide &gt;5% failure scenarios from manifests (SAP reporting gap).
5. **Stale pilot CSVs in the production hash ledger** — `pilot-*-summary.csv` (n_boot=50 era) are hashed beside production manifests and misread as production evidence.
6. **`unsupported` is hardcoded** — Not derived from missing checkpoints; silent skip risk beyond `binomial_stress_separation`.
7. **`CALIBRATION_SAP.md` still describes the reduced-fixture run** — Hash `f543a90c…` / 600×300 vs production `11bc762c…` / ~28.7k×10.8k.
8. **Held-out design incomplete for 6/7 families** — Family-specific acceptance could not be declared even if training had produced candidates.
9. **Assemble errors can be swallowed** — `system2` without status check can publish from stale assembled RDS.

### Medium / minor

10. Occupancy shortfalls (incomplete screening strata) are documented and secondary; they do not explain the empty feasible grid.
11. No unit test locking `no_feasible_thresholds` reason preservation; `calibration-registry.rds` published but not hashed; production failure / scenario-id invariants under-tested.

---

## Scientific credibility of all-uncalibrated

**Credible.** Under the locked policy and the published fitting population:

- Scores do not support safe categorical thresholds (separation insufficient or inverted).
- Wilson uncertainty is not what blocked calibration.
- The honest registry status is `uncalibrated` with reason `no_feasible_thresholds`.

This is a valid scientific outcome of Task 15 steps 2–4. It is **not** evidence that 55/70 (or any other pair) is empirically validated.

Deeper issue for any *future* calibrated claim: redefine the estimand so FR/RI are computed only on band-applicable conclusions, rebuild scenarios for strong clear effects and clean null-FP strata, and complete held-out truth balance per family — then re-run fit → held-out with no validation refit.

---

## Phase 6 go / no-go

| Path | Decision |
|------|----------|
| Phase 6: copy registry into package as **validated calibrated bands** | **No-go** |
| Phase 6: integrate **explicit uncalibrated** registry (NA cutoffs, no validated claims) | **Conditional go** after provenance fixes (items 4–9) |
| Start Phase 6 now without fixes | **No-go** |

Do **not** start Phase 6 from this Step 5 deliverable alone.

---

## Recommended next actions

### A. Provenance fixes (required before any Phase 6 uncalibrated ship)

1. Report real `attempted` / `failed` / exclusion rates from pre-assemble diagnostics (or a separate production failure summary hashed into the ledger).
2. Remove or clearly segregate pilot CSVs from the production hash ledger.
3. Derive `unsupported` / missing scenarios from checkpoints (assert completeness).
4. Update `CALIBRATION_SAP.md` Task 15 record to production hashes, replicate counts, and all-uncalibrated outcome.
5. Fail freeze if reassemble `system2` status ≠ 0; hash `calibration-registry.rds` or stop publishing it unpaired.
6. Document held-out incompleteness (6/7) explicitly in publish notes.

### B. Scientific redesign (required before any calibrated-band claim)

1. Redefine FR/RI estimands to band-applicable conclusions only (align with non-significant registry).
2. Rebuild scenarios: stronger clear effects (RI), cleaner null-FP strata; isolate sparse/high-score null stress cells that break ordering.
3. Complete held-out design: every family needs null+borderline+clear with *n*≥100 before family acceptance.
4. Re-run training fit → single held-out evaluate (still no refit). Expect possible cox-like candidates, not universal shared 55/70 validation.

### Priority guidance

- **Goal = calibrated bands:** redesign first (B), then re-freeze; provenance (A) still required on the new freeze.
- **Goal = honest uncalibrated package registry only:** fix provenance (A) first, then conditional Phase 6 with explicit uncalibrated messaging — **do not** claim calibrated bands.

---

## Review sources

- Code review agent: `f128df67-ed25-49d0-9d76-1b2ecb84daf3` — Ready for Phase 6 **with fixes** (provenance); threshold decision OK.
- Statistical review agent: `6a5b2013-8b1f-4ea1-8701-ee1f5c665983` — **No-go** for calibrated bands; conditional go only for explicit uncalibrated registry.

### Evidence anchors

- Published registry: all 7 × `uncalibrated` / `no_feasible_thresholds` / `status_public = uncalibrated`
- Manifests: `candidate_hash = 3603f3614eacfa9f95d294b7f56f977a`, `registry_hash = 6c8249beef62677967d2c51e0dd20caf`
- Assembled: 28,722 training / 10,759 validation completed replicates; train layers `core|stress`, val layer `validation`; train/val `scenario_id` overlap = 0
- Artifacts: `manuscript/calibration/published/` (`calibration-registry.csv`, manifests, `output-hashes.txt`); assembled replicates under `artifacts/raw/assembled/` (gitignored)
