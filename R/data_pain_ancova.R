#' Synthetic ANCOVA pain trial (prospectively frozen illustration)
#'
#' Row-level synthetic companion to the Welch pain example. The dataset was
#' generated once from `data-raw/pain_ancova_trial.R` with seed `20260806` and
#' is **excluded** from ANCOVA calibration training, candidate selection, and
#' held-out validation. It is intended only for manuscript and vignette
#' illustration of the eligible `robustness_lm()` workflow.
#'
#' @format A data frame with 80 rows and 5 variables:
#' \describe{
#'   \item{subject_id}{Unique subject identifier.}
#'   \item{arm}{Two-level treatment factor with levels `Placebo`, `Active`.}
#'   \item{baseline_pain}{Continuous baseline pain score on a 0--100 scale.}
#'   \item{week12_pain}{Continuous Week 12 pain score on a 0--100 scale.}
#'   \item{change}{Derived descriptive change: `week12_pain - baseline_pain`.}
#' }
#' @source Simulated; see `data-raw/pain_ancova_trial.R`.
#' @seealso [robustness_lm()]
"pain_ancova_trial"
