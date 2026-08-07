#' Synthetic oncology response trial (prospectively frozen illustration)
#'
#' Row-level synthetic two-arm oncology responder trial. The dataset was
#' generated once from `data-raw/onc_response_trial.R` with seed `20260809L`
#' (control response rate 0.20, active 0.45) and is **excluded** from
#' binary-proportion calibration training, candidate selection, and held-out
#' validation. It is intended only for manuscript and vignette illustration of
#' the eligible `robustness_analysis(test_type = "fisher")` workflow.
#'
#' @format A data frame with 120 rows and 3 variables:
#' \describe{
#'   \item{subject_id}{Unique subject identifier.}
#'   \item{arm}{Two-level treatment factor with levels `Placebo`, `Active`
#'     (60 per arm).}
#'   \item{response}{Binary objective response, encoded as 0/1 integer.}
#' }
#' @source Simulated; see `data-raw/onc_response_trial.R`.
#' @seealso [robustness_analysis()]
"onc_response_trial"
