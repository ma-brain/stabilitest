# LM/ANCOVA Calibration Statistical Analysis Plan (Gate A)

**Calibration unit:** `lm_ancova`  
**Study path:** `manuscript/calibration/studies/lm_ancova/`  
**Status:** Gate A infrastructure frozen; active package registry remains
`uncalibrated` until Gate B.

## Gate A verification

| Field | Value |
| --- | --- |
| Reviewed commit | `96141f2f3df6c45d3cc04f5c5c9696f56473a3bc` |
| Date | 2026-08-06 |
| `git diff --check` | PASS |
| Documentation audit | PASS |
| Package `devtools::test()` | PASS (663) |
| Shared calibration tests | PASS |
| Study-local tests | PASS |
| ANCOVA vignette render | PASS |
| `roxygen2::roxygenise()` drift | none |
| `rcmdcheck --as-cran` | 0 errors, 0 warnings, accepted `New submission` NOTE only |

No source changes were required by verification. Machine-readable stamp:
`artifacts/summaries/GATE_A_REVIEW.json`. Fold the same hash into the pilot
manifest when Task 14 creates the pilot run.

## Scope

Bands apply only to eligible significant canonical 1-df ANCOVA treatment
effects from

```text
outcome ~ treatment + baseline
```

with a continuous response, a two-level treatment factor, exactly one continuous
baseline covariate, additive terms, complete cases, `alpha = 0.05`,
`n_boot = 1000`, weights `jackknife = 0.4`, `fragility = 0.4`,
`bootstrap = 0.2`, and `max_removal_pct = 0.30`. Score weights remain frozen;
only the two categorical cutoffs are estimated.

Multi-df joint terms and noncanonical LM results remain numeric-only. Welch
55/70 is a Welch comparator, not an ANCOVA default or fallback. Randomization
and prospective covariate choice are user responsibilities.

## Scenario grids

### Training (core)

- `n ∈ {40, 80, 160}`
- baseline `R² ∈ {0.10, 0.40, 0.70}`
- truth ∈ {null, borderline, clear}
- 27 scenarios; seeds begin at `31001`

### Held-out validation

- `n ∈ {60, 120, 240}`
- baseline `R² ∈ {0.25, 0.55}`
- truth ∈ {null, borderline, clear}
- 18 scenarios; seeds begin at `32001`

### Stress (diagnostic only)

Allocation 2:1, heteroscedasticity, heavy tails, missing baseline,
nonlinear baseline, and treatment-by-baseline interaction. Stress rows never
fit cutoffs or enter acceptance.

Truth strata are power-defined: null (`β = 0`), borderline (~60% power), and
clear (~90% power), solved from the noncentral t distribution. Score weights
remain frozen for the Gate A protocol.

## Power verification

- Primary-test-only Monte Carlo before robustness scoring
- Independent seeds; no inspection of score distributions
- Production gate: `draws = 10000`, master seed `20260806`, tolerance
  `0.02` of targets `0.60` / `0.90`; null type-I within `0.02` of `0.05`
- Unit tests may use 2,000 draws and tolerance `0.04`
- Frozen artifact: `artifacts/summaries/power-verification.csv`
  (sha256 `c47409b1698f6ad2180776599d28145d4416cd807f5dde4c82df8197372a9e90`;
  45/45 canonical core+validation scenarios PASS; no robustness scores)

## Production execution freeze

| Field | Value |
| --- | --- |
| Code commit | `433c9671a15882d80652b435305563aa0274b801` |
| Gate A reviewed commit | `96141f2f3df6c45d3cc04f5c5c9696f56473a3bc` |
| All-scenario hash | `5b97d1cdee060ac2c29f6f6256f75912` |
| Training (core+stress) hash | `784f035d4965f2f3a626af4f0a2ce2a3` |
| Core-only hash | `61af0d58e582e1e6a935d921db43f216` |
| Pilot scenario hash | `40d480b7c211ce1b7baa92d6c580162d` |
| Pilot commit | `0c2449d99e9c85be8cbb4b157dee9406eeb94f97` |
| `n_boot` | `1000` |
| Max screen draws | `10000` |
| Screening target | `100` significant per required scenario |
| Workers | `4` |
| Runtime projection | ~5.95s/replicate at `n_boot=1000`; ~4.5h core @1 worker / ~1.1h @4 |

Pilot (wiring only; scores not inspected): 27 core scenarios, 1922 completed
analyses, 0 analysis failures, 9 incomplete null screens under the pilot
`max_screen_draws=250` cap, validation not accessed. Compact stamp:
`artifacts/summaries/execution-freeze.json`.

## Screening quotas and failures

- Screen with the same coefficient test as `robustness_lm()`
- Eligible conclusion: significant only
- Target: at least 100 completed eligible results per held-out scenario
- Aggregate held-out truth-stratum target: at least 600
- Block publication if any required scenario misses quota
- Block publication if analysis failures exceed 5% in any required scenario
- Full screening budget and resume require matching scenario/code/option hashes

## Cutoff search

Search ordered integer pairs `(L, U)` on 0--100:

- score `≤ L`: Fragile
- `L < score ≤ U`: Moderately Robust
- score `> U`: Robust

Training feasibility:

- false reassurance ≤ 0.05 with one-sided 95% upper ≤ 0.10
- robust identification ≥ 0.70 with one-sided 95% lower ≥ 0.60
- balanced three-class accuracy ≥ 0.70
- each truth-class accuracy ≥ 0.60
- median scores ordered null < borderline < clear

Tie-breaks: highest balanced accuracy; highest minimum class accuracy;
greatest safety margin from FR/RI limits; deterministic lexicographic
`(L, U)`.

## Held-out acceptance

Evaluate the frozen candidate once without refit. Use the more conservative of
Wilson and scenario-cluster bootstrap bounds. Require balanced accuracy ≥ 0.70
with cluster lower ≥ 0.65, class accuracies ≥ 0.60, and ordered medians within
matched sample-size/prognostic-strength blocks. Failure leaves `lm_ancova`
uncalibrated; no second candidate is allowed.

Cluster bootstrap: seed `20260806`, draws as configured at analysis time
(production default `B = 1000`).

## Illustrative dataset provenance

| Field | Value |
| --- | --- |
| Dataset | `pain_ancova_trial` |
| Generator | `data-raw/pain_ancova_trial.R` |
| Seed | `20260806` |
| Dataset MD5 | `c1ec542b49bb3f2c05ca692cf3c33e47` |
| Freeze commit | `3c0f77a` |
| Case-study docs commit | `d74f9a8` |
| Worked bootstrap seed | `1408` |

The case-study ID/seed must be absent from calibration scenario IDs and seed
ledgers. The manuscript places the illustrative synthetic case study **after**
calibration results.

## Canonical commands

```sh
Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode smoke --phase all --engine lm --workers 1 \
  --output /tmp/stabilitest-lm-ancova-smoke

Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode pilot --phase all --engine lm --workers 1 \
  --output manuscript/calibration/studies/lm_ancova/artifacts/pilot

Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode full --phase all --engine lm --workers <N> --resume \
  --output manuscript/calibration/studies/lm_ancova/artifacts/raw/training

Rscript manuscript/calibration/studies/lm_ancova/run_calibration.R \
  --mode full --phase all --engine lm --workers <N> --resume \
  --validation-only \
  --output manuscript/calibration/studies/lm_ancova/artifacts/raw/validation

Rscript manuscript/calibration/studies/lm_ancova/tools/freeze_and_publish.R
```

## Publication hash targets

Compact published outputs must include completed replicate tables, full audit
rows, occupancy and failure summaries, power-verification summary, candidate
and validation diagnostics, method-specific registry CSV/RDS, training and
validation manifests, and the hash ledger. An unsuccessful calibration remains
a valid, publishable uncalibrated result.
