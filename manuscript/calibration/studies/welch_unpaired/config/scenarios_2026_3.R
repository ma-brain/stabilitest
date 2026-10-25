# Prospective Welch calibration scenario contract, relaxed-gates study 2026-3.
#
# Frozen with CALIBRATION_SAP_2026-3.md before any 2026-3 robustness score is
# produced. The core registry, stress rows, truth definitions, and quotas are
# those of study 2026-2; only identities and seed ranges change. Training,
# validation, and stress streams are disjoint from each other and from 2026-2.

WELCH3_SEED_OFFSET <- 100000L
WELCH3_ID_PREFIX <- c(WTR = "WTR3", WVA = "WVA3", WST = "WST3")

#' Return the frozen Welch 2026-3 calibration scenarios.
#'
#' Derived from the 2026-2 registry by a fixed rule: identifiers `WTR-`,
#' `WVA-`, `WST-` become `WTR3-`, `WVA3-`, `WST3-`, and every seed increases by
#' `WELCH3_SEED_OFFSET` (training 202710001, validation 202720001, stress
#' 202730001). The test suite asserts the resulting IDs and seeds.
welch_scenarios_2026_3 <- function() {
  rows <- welch_scenarios()
  prefix <- sub("-.*$", "", rows$scenario_id)
  stopifnot(all(prefix %in% names(WELCH3_ID_PREFIX)))
  rows$scenario_id <- paste0(
    unname(WELCH3_ID_PREFIX[prefix]),
    substring(rows$scenario_id, nchar(prefix) + 1L)
  )
  rows$seed <- rows$seed + WELCH3_SEED_OFFSET
  rownames(rows) <- NULL
  rows
}
