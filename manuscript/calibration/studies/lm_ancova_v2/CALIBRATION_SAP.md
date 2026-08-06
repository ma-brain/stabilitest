# LM/ANCOVA v2 Calibration Statistical Analysis Plan (Track A / Gate A)

**Calibration unit:** `lm_ancova_v2`  
**Study path:** `manuscript/calibration/studies/lm_ancova_v2/`  
**Design:** `docs/plans/2026-08-06-lm-ancova-v2-design.md` (Track A locked)  
**Status:** Gate A protocol frozen. Score-only pilot, production training,
cutoff search, and held-out acceptance are not yet executed. Package labels
remain suppressed until a later Gate B integration of a validated (or
fail-closed uncalibrated) v2 decision. The v1 unit `lm_ancova` stays the
immutable uncalibrated historical record (`no_feasible_thresholds`; held-out
not opened).

## Scope

Bands apply only to eligible significant canonical 1-df ANCOVA treatment
effects from

```text
outcome ~ treatment + baseline
```

with a continuous response, a two-level treatment factor, exactly one continuous
baseline covariate, additive terms, complete cases, `alpha = 0.05`,
`n_boot = 1000`, frozen Track A weights
`fragility = 0.5`, `bootstrap = 0.5`, `jackknife = 0`, and
`max_removal_pct = 0.30`.

**Claim:** Fragile vs Not fragile (two bands only).  
**Rule:** Fragile if score ≤ L; else Not fragile.  
Only the single integer cutoff L is estimated after the sealed score pilot
passes. Multi-df joint terms and noncanonical LM results remain numeric-only.
Welch 55/70 is a Welch comparator, not an ANCOVA default or fallback.

### Score weights (frozen)

| Component | Weight |
| --- | --- |
| fragility | 0.5 |
| bootstrap | 0.5 |
| jackknife | 0 |

Alternate weights `0.45 / 0.45 / 0.10` may be considered only in a named
pilot comparison for continuity; they are not the frozen production default.
The locked v1 composite (`jackknife = 0.4`, `fragility = 0.4`,
`bootstrap = 0.2`) is always archived as a comparator column or parallel
summary and is **never** used to fit or accept v2 cutoffs.

## Scenario grids

### Training (core)

- `n ∈ {40, 80, 160}`
- baseline `R² ∈ {0.10, 0.40, 0.70}`
- truth ∈ {null, borderline, clear}
- 27 scenarios; seeds begin at `41001`
- **Fitting strata:** null + clear only

### Held-out validation

- `n ∈ {60, 120, 240}`
- baseline `R² ∈ {0.25, 0.55}`
- truth ∈ {null, borderline, clear}
- 18 scenarios; seeds begin at `42001`
- Acceptance uses null + clear only

### Stress (diagnostic only)

Allocation 2:1, heteroscedasticity, heavy tails, missing baseline,
nonlinear baseline, and treatment-by-baseline interaction. Seeds begin at
`43001`. Stress rows never fit cutoffs or enter acceptance.

### Truth and borderline policy

- Null: \(\beta = 0\) (type-I target 0.05).
- Borderline (~60% power): **diagnostic only** — generated for figures and
  diagnostics, excluded from cutoff search and acceptance gates.
- Clear: power-defined at the frozen clear-power choice (below).

Primary-test screening remains significant-only for band-fitting rows (false
reassurance is defined on significant nulls).

## Clear power choice rule

1. Default clear target power is `0.90`.
2. Escalate to `0.95` **only** via a recorded score-pilot decision when the
   0.90 pilot is marginal under the go/no-go formulas below.
3. Hard failure at 0.90 does not automatically burn a 0.95 pilot; stop at
   numeric-only unless the recorded decision explicitly escalates once.
4. The frozen clear-power value is written into the pilot gate stamp before
   any production cutoff search.

## Production execution freeze (defaults)

| Field | Value |
| --- | --- |
| Workers (default / host cap) | `4` |
| `n_boot` | `1000` |
| Max screen draws | `10000` |
| Screening quota | ≥ `100` significant completed results per required scenario |
| Failure limit | analysis failures ≤ 5% in any required scenario |
| `max_removal_pct` | `0.30` |

Required scenarios for quotas and acceptance are core/validation null and
clear rows (borderline and stress excluded). Block publication if any required
scenario misses quota or exceeds the failure limit. Full screening budget and
resume require matching scenario/code/option hashes.

## Score-only pilot go/no-go (sealed)

Run a **score-only pilot** before any categorical fitting and before opening
held-out scores. Inspect only the pre-registered summaries below.

### Metrics (formulas)

Let S be the v2 composite under frozen weights `0.5 / 0.5 / 0`, pooled
across completed significant core pilot rows in each truth class.

1. **Location gap:** median(clear) − median(null) on the v2 score S
   (\(\Delta = \mathrm{median}(S_{\mathrm{clear}}) - \mathrm{median}(S_{\mathrm{null}})\)).
2. **Overlap index:** P(null > median(clear)), estimated as the empirical mean
   of the indicator \(S_{\mathrm{null}} > \mathrm{median}(S_{\mathrm{clear}})\) on
   significant null rows.
3. **Optional AUC:** ROC AUC for null vs clear on S (higher score favors
   clear). If AUC is not computed in a given pilot stamp, it does not enter
   the go decision; if computed, it must meet its threshold.

Also archive class-wise component and composite quantiles by \(n\) (diagnostic;
not gate-forming).

### Go thresholds (conservative)

Chosen to be plausibly compatible with later FR ≤ 0.05 and Not-fragile
identification ≥ 0.70 gates, without searching \(L\) in the pilot.

| Metric | Go | Marginal (may escalate clear power once) | Hard no-go |
| --- | --- | --- | --- |
| \(\Delta\) | ≥ 20 | \(15 \le \Delta < 20\) | \(\Delta < 15\) |
| \(O\) | ≤ 0.10 | \(0.10 < O \le 0.20\) | \(O > 0.20\) |
| AUC (if computed) | ≥ 0.75 | \(0.70 \le \mathrm{AUC} < 0.75\) | \(\mathrm{AUC} < 0.70\) |

### Decision rule

- **Go at 0.90** if every required metric is Go (and optional AUC, if present,
  is Go).
- **Escalate once to 0.95** only if no metric is Hard no-go and at least one
  required metric is Marginal; re-evaluate the same formulas on the 0.95 clear
  target; Go only if all then meet Go thresholds. Record the escalation in the
  pilot stamp.
- **No-go** if any Hard no-go fires, or if the single 0.95 escalation still
  fails Go. Remain numeric-only; do not search \(L\); do not open held-out.

Machine-readable stamp (later task): `artifacts/summaries/SCORE_PILOT_GATE.json`.

## Cutoff search (training)

After a recorded Go:

- Search integer L in {0, ..., 100}.
- Fragile if score ≤ L; else Not fragile.
- Fit only on completed significant **null + clear** core rows (exclude
  borderline / `diagnostic_only` and stress).

Training feasibility:

- false reassurance (FR): share of significant nulls with score > L ≤ 0.05,
  with one-sided 95% Wilson upper ≤ 0.10;
- Not-fragile identification on clear: share of clear with score > L ≥ 0.70,
  with one-sided 95% Wilson lower ≥ 0.60.

Deterministic tie-breaks: highest clear identification, then greatest FR safety
margin (distance of FR upper bound below 0.10), then smallest L.

If no feasible L exists, publish `uncalibrated` / `no_feasible_threshold`
(or equivalent), leave held-out closed, and fail closed.

## Held-out acceptance

Evaluate the frozen L once without refit. Use the more conservative of
Wilson and scenario-cluster bootstrap bounds. Require the same FR and
Not-fragile identification operating characteristics as training, on
validation null + clear significant rows only. Failure leaves `lm_ancova_v2`
uncalibrated; no second candidate is allowed.

Cluster bootstrap: seed `20260806`, production default `B = 1000`. Stress rows
never accept.

## Provenance isolation from v1

- New unit `lm_ancova_v2`; do not overwrite the v1 `lm_ancova` registry
  provenance row.
- Scenario IDs, seeds, manifests, and hash ledgers are isolated
  (v2 seeds begin at `41001` / `42001` / `43001`).
- **Assertion:** v1 validation seeds and scenario IDs are absent from v2
  ledgers, manifests, and fitting/acceptance inputs. Quiet reuse of v1
  held-out replicates is forbidden.
- Comparator reporting of the locked v1 composite does not import v1
  validation rows.
- `pain_ancova_trial` remains prospectively frozen and non-calibrating; its
  case-study ID/seed must be absent from v2 calibration ledgers.

## Power verification

- Primary-test-only Monte Carlo before robustness scoring
- Independent seeds; no inspection of score distributions for the power gate
- Production gate: `draws = 10000`, master seed `20260806`, tolerance `0.02`
  of the frozen clear target (`0.90` or `0.95`) and borderline `0.60`; null
  type-I within `0.02` of `0.05`
- Unit tests may use 2,000 draws and tolerance `0.04`
- Borderline power checks may be retained as diagnostics even though borderline
  is excluded from cutoff fitting

## Canonical commands

```sh
Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode smoke --phase all --engine lm --workers 1 \
  --output /tmp/stabilitest-lm-ancova-v2-smoke

Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode pilot --phase all --engine lm --workers 4 \
  --output manuscript/calibration/studies/lm_ancova_v2/outputs/score-pilot

Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode full --phase all --engine lm --workers 4 --resume \
  --output manuscript/calibration/studies/lm_ancova_v2/artifacts/raw/training

Rscript manuscript/calibration/studies/lm_ancova_v2/run_calibration.R \
  --mode full --phase all --engine lm --workers 4 --resume \
  --validation-only \
  --output manuscript/calibration/studies/lm_ancova_v2/artifacts/raw/validation
```

## Publication policy

Compact published outputs (when executed) must include completed replicate
summaries, occupancy and failure tables, power verification, candidate and
validation diagnostics, method-specific registry CSV/RDS for `lm_ancova_v2`,
training/validation manifests, and the hash ledger. An unsuccessful calibration
remains a valid, publishable uncalibrated result. User-facing docs must state
that any ANCOVA bands are two-class and method-specific, not Welch 55/70.
