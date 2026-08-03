#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import tibble
#' @importFrom purrr map_dfr
#' @importFrom ggplot2 ggplot aes geom_hline geom_line geom_point geom_histogram geom_vline labs theme_minimal theme
#' @importFrom stats t.test wilcox.test sd median quantile lm glm binomial
## usethis namespace: end
NULL

# dplyr NSE column names referenced in mutate/filter/aes/select
utils::globalVariables(c(
  "conclusion_match", "influential", "influential_delta",
  "k_removed", "label", "method", "p_value", "significant"
))
