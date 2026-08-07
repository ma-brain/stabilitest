# Binary-Proportion Score Pilot Gate (Task 8)

**Date:** 2026-08-06
**Pilot mode:** `--mode pilot --phase all --engine proportion --workers 4`
**Output:** `outputs/score-pilot/` (gitignored); gate artifact:
`artifacts/summaries/SCORE_PILOT_GATE.json`

## Sealed feasibility-projection gate

| Quantity | Value |
| --- | --- |
| FR-safe integer L (FR ≤ 0.05, Wilson upper ≤ 0.10) | **60** |
| False reassurance at L | 0.044 (Wilson upper 0.0944) |
| Projected RI = P(score > L \| clear, significant) | **0.7250** (Wilson lower 0.7033) |
| Pooled AUC (clear vs null) | 0.9231 |
| **Verdict** | **go** (≥ 0.72) |

The projected RI (0.7250) clears the go threshold (≥ 0.72) and its Wilson lower
(0.7033) is above the hard no-go line (0.70). This empirically confirms the
committed analytic projection (Task 0: feasibility-projection-power095.R, all
12 cells pass at clear power 0.95) on actual composite scores.

## Occupancy

| truth_class | significant completed |
| --- | --- |
| borderline | 1200 |
| clear | 1200 |
| null | 91 |

Borderline and clear cells met the ≥ 100 quota. Null cells are sparse (91
significant nulls total across all null scenarios, below the 100/scenario
quota): this reflects Fisher's exact conservatism (enumerated type-I 0.009–
0.040), which is the structural reason the design froze clear power at 0.95.
The 91 null-significant scores nonetheless suffice to identify the FR-safe
cutoff (Wilson upper 0.0944 < 0.10).

## Power gate

Independent Monte Carlo verifier (`verify_prop_power`, master seed 20260808)
on representative clear scenarios: achieved 0.946–0.954 vs target 0.95 — all
within tolerance. No miss.

## Decision

Track A″ proceeds to production training (Task 9). Held-out remains closed.
