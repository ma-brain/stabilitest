#' Phase II analgesic trial: change from baseline in pain score (Week 12)
#'
#' Fixed synthetic dataset used in the manuscript case study. Pain measured
#' on a 0-100 scale; values are change from baseline (negative = improvement).
#' Includes one extreme responder (-52.0, treatment) and one minimal
#' responder (+3.0, placebo) to illustrate influence diagnostics.
#'
#' @format Two numeric vectors: `pain_treatment` (n = 28), `pain_placebo`
#'   (n = 27).
#' @source Simulated; see data-raw/pain_trial.R.
#' @name pain_trial
NULL

#' @rdname pain_trial
#' @export
pain_treatment <- c(-9.0, -8.4, -17.4, 1.4, -21.5, -26.6, -35.8, -12.6, -21.0,
                    -6.6, -9.5, -13.2, -36.8, -52.0, -25.2, 7.6, -34.0, -8.4,
                    -43.9, -35.4, -23.2, -10.0, -8.0, -18.0, -26.5, -9.8,
                    -29.9, -20.7)

#' @rdname pain_trial
#' @export
pain_placebo <- c(-4.4, -12.7, -8.9, -31.5, -12.0, -25.1, -13.0, -0.7, -19.9,
                  -1.0, -3.6, 6.4, -1.0, -2.6, -1.2, 23.0, -21.8, -5.3,
                  -25.5, -15.4, -9.7, 3.0, -3.3, -3.0, -26.1, -13.8, 2.2)
