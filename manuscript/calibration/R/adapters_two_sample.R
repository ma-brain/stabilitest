# Analysis-specific generator and adapters for two-sample calibration.

.two_sample_abort <- function(message, cause = NULL) {
  condition <- structure(
    list(message = as.character(message), call = NULL, cause = cause),
    class = c("two_sample_adapter_error", "error", "condition")
  )
  stop(condition)
}

.two_sample_validate_numeric <- function(value, name, integer = FALSE,
                                          positive = FALSE, lower = -Inf, upper = Inf) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) && is.finite(value)
  if (valid && integer) valid <- floor(value) == value
  if (valid && integer) valid <- value <= .Machine$integer.max
  if (valid && positive) valid <- value > 0
  if (valid) valid <- value >= lower && value <= upper
  if (!valid) .two_sample_abort(sprintf("%s must be a valid numeric%s", name,
                                        if (integer) " integer" else ""))
  unname(value)
}

.two_sample_alpha <- function(analysis, scenario) {
  value <- analysis$alpha %||% .two_sample_scenario_scalar(scenario, "alpha", 0.05)
  value <- .two_sample_validate_numeric(value, "alpha", lower = 0, upper = 1)
  if (!(value > 0 && value < 1)) .two_sample_abort("alpha must be strictly between 0 and 1")
  value
}

.two_sample_analysis <- function(scenario) {
  parameters <- if (is.data.frame(scenario)) {
    if (nrow(scenario) != 1L) .two_sample_abort("scenario must contain one row")
    scenario$parameters[[1L]]
  } else if (is.list(scenario)) {
    scenario$parameters %||% scenario
  } else {
    .two_sample_abort("scenario must be a list or one-row data frame")
  }
  if (!is.list(parameters)) parameters <- list()
  analysis <- parameters$analysis
  if (is.null(analysis)) analysis <- list()
  if (!is.list(analysis)) .two_sample_abort("scenario analysis settings must be a list")
  analysis
}

.two_sample_generator_settings <- function(scenario) {
  parameters <- if (is.data.frame(scenario)) {
    scenario$parameters[[1L]]
  } else if (is.list(scenario) && !is.null(scenario$parameters)) {
    scenario$parameters
  } else if (is.list(scenario) && !is.null(scenario$generator)) {
    scenario
  } else {
    NULL
  }
  if (is.null(parameters) || !is.list(parameters)) return(list())
  generator <- parameters$generator
  if (is.null(generator)) list() else generator
}

.two_sample_scenario_scalar <- function(scenario, field, default) {
  value <- if (is.data.frame(scenario)) {
    if (!field %in% names(scenario)) NULL else scenario[[field]][[1L]]
  } else if (is.list(scenario)) {
    scenario[[field]]
  } else NULL
  if (is.null(value)) return(default)
  if (length(value) != 1L || is.na(value)) {
    .two_sample_abort(sprintf("scenario %s must be one non-missing scalar", field))
  }
  value
}

`%||%` <- function(x, y) if (is.null(x)) y else x

.two_sample_test_type <- function(scenario) {
  analysis <- .two_sample_analysis(scenario)
  test_type <- analysis$test_type %||% analysis$test %||% "t.test"
  if (length(test_type) != 1L || !is.character(test_type) || is.na(test_type)) {
    .two_sample_abort("test_type must be one supported character value")
  }
  supported <- c("t.test", "paired.t.test", "wilcoxon", "wilcoxon.paired",
                 "brunner_munzel", "fisher", "chisq", "prop")
  if (!test_type %in% supported) {
    .two_sample_abort(sprintf("unsupported test_type '%s'", test_type))
  }
  test_type
}

.two_sample_correct <- function(analysis) {
  value <- analysis$correct %||% TRUE
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .two_sample_abort("correct must be one non-missing logical value")
  }
  value
}

.two_sample_data <- function(data) {
  if (is.data.frame(data)) {
    nms <- names(data)
    if (all(c("group1", "group2") %in% nms)) {
      return(list(group1 = data$group1, group2 = data$group2))
    }
    if (all(c("x", "y") %in% nms)) {
      return(list(group1 = data$x, group2 = data$y))
    }
    if (all(c("value", "group") %in% nms)) {
      groups <- split(data$value, data$group, drop = TRUE)
      if (length(groups) == 2L) {
        return(list(group1 = groups[[1L]], group2 = groups[[2L]]))
      }
    }
  }
  if (is.matrix(data) && ncol(data) == 2L) {
    return(list(group1 = data[, 1L], group2 = data[, 2L]))
  }
  if (is.list(data)) {
    group1 <- data$group1 %||% data$x %||% data$control
    group2 <- data$group2 %||% data$y %||% data$treatment
    if (!is.null(group1) && !is.null(group2)) {
      return(list(group1 = group1, group2 = group2))
    }
  }
  .two_sample_abort("data must provide group1/group2 (or x/y) vectors")
}

.two_sample_binary <- function(x, name) {
  if (is.logical(x)) return(as.numeric(x))
  if (!is.numeric(x) || anyNA(x) || !all(x %in% c(0, 1))) {
    .two_sample_abort(sprintf("%s must be binary 0/1 or logical", name))
  }
  as.numeric(x)
}

.two_sample_table <- function(g1, g2) {
  matrix(c(sum(g1), length(g1) - sum(g1),
           sum(g2), length(g2) - sum(g2)), nrow = 2L,
         dimnames = list(outcome = c("success", "failure"),
                         group = c("group1", "group2")))
}

.two_sample_primary_test <- function(g1, g2, test_type, alpha, correct) {
  if (test_type %in% c("fisher", "chisq", "prop")) {
    g1 <- .two_sample_binary(g1, "group1")
    g2 <- .two_sample_binary(g2, "group2")
  } else if (!is.numeric(g1) || !is.numeric(g2) || anyNA(g1) || anyNA(g2)) {
    .two_sample_abort("continuous groups must be numeric and non-missing")
  }
  result <- switch(test_type,
    "t.test" = stats::t.test(g1, g2),
    "paired.t.test" = {
      if (length(g1) != length(g2)) .two_sample_abort("paired tests require equal group lengths")
      stats::t.test(g1, g2, paired = TRUE)
    },
    "wilcoxon" = tryCatch(
      stats::wilcox.test(g1, g2, exact = FALSE, conf.int = TRUE),
      error = function(e) stats::wilcox.test(g1, g2, exact = FALSE)
    ),
    "wilcoxon.paired" = {
      if (length(g1) != length(g2)) .two_sample_abort("paired tests require equal group lengths")
      tryCatch(
        stats::wilcox.test(g1, g2, paired = TRUE, exact = FALSE, conf.int = TRUE),
        error = function(e) stats::wilcox.test(g1, g2, paired = TRUE, exact = FALSE)
      )
    },
    "brunner_munzel" = {
      bm <- get("brunner_munzel_test", envir = asNamespace("stabilitest"), inherits = FALSE)
      bm(g1, g2, alpha = alpha)
    },
    "fisher" = stats::fisher.test(.two_sample_table(g1, g2)),
    "chisq" = suppressWarnings(stats::chisq.test(.two_sample_table(g1, g2), correct = correct)),
    "prop" = suppressWarnings(stats::prop.test(
      c(sum(g1), sum(g2)), c(length(g1), length(g2)), correct = correct
    ))
  )
  p <- unname(result$p.value)
  if (!is.numeric(p) || length(p) != 1L || !is.finite(p)) {
    .two_sample_abort(sprintf("primary %s returned non-finite p", test_type))
  }
  list(p = p, significant = p < alpha)
}

#' Generate one deterministic two-sample calibration replicate.
#' @export
generate_two_sample <- function(scenario, seed = NULL) {
  settings <- .two_sample_generator_settings(scenario)
  if (is.null(seed)) seed <- .two_sample_scenario_scalar(scenario, "scenario_seed", 123L)
  seed <- .two_sample_validate_numeric(seed, "seed", integer = TRUE, lower = 0)
  old_seed <- if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", .GlobalEnv, inherits = FALSE)
  } else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old_seed, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)

  n_default <- settings$n_per_group %||% settings$n %||%
    .two_sample_scenario_scalar(scenario, "sample_size", 20L)
  n1 <- .two_sample_validate_numeric(
    settings$n_group1 %||% settings$n_control %||% n_default,
    "group size", integer = TRUE, positive = TRUE
  )
  n2 <- .two_sample_validate_numeric(
    settings$n_group2 %||% settings$n_treatment %||% n_default,
    "group size", integer = TRUE, positive = TRUE
  )
  if (n1 < 4L || n2 < 4L) .two_sample_abort("group sizes must each be at least 4")
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  paired <- isTRUE(settings$paired)
  if (paired && n1 != n2) {
    .two_sample_abort("paired designs require equal group sizes")
  }
  effect <- .two_sample_validate_numeric(settings$effect_size %||% settings$mean_difference %||% 0,
                                         "effect_size")
  sd1 <- .two_sample_validate_numeric(settings$sd_control %||% settings$sd %||% 1,
                                       "sd_control", positive = TRUE)
  sd2 <- .two_sample_validate_numeric(settings$sd_treatment %||% settings$sd %||% sd1,
                                       "sd_treatment", positive = TRUE)
  distribution <- settings$distribution %||% "normal"
  draw <- function(n, sd) {
    if (identical(distribution, "heavy_tailed") || identical(distribution, "t")) {
      stats::rt(n, df = 3) * sd / sqrt(3)
    } else {
      stats::rnorm(n, sd = sd)
    }
  }

  is_binary <- !is.null(settings$probability_control) ||
    !is.null(settings$probability_treatment) || identical(distribution, "binary")
  if (is_binary) {
    p1 <- .two_sample_validate_numeric(settings$probability_control %||% 0.5,
                                        "probability_control", lower = 0, upper = 1)
    p2 <- .two_sample_validate_numeric(settings$probability_treatment %||% p1,
                                        "probability_treatment", lower = 0, upper = 1)
    return(list(group1 = stats::rbinom(n1, 1L, p1), group2 = stats::rbinom(n2, 1L, p2)))
  }
  if (paired) {
    baseline <- draw(n1, sd1)
    group1 <- baseline
    group2 <- baseline + effect + draw(n1, sd2)
  } else {
    group1 <- draw(n1, sd1)
    group2 <- effect + draw(n2, sd2)
  }
  contamination <- .two_sample_validate_numeric(settings$contamination %||% 0,
                                                 "contamination", lower = 0, upper = 1)
  if (contamination > 0) {
    n_bad <- min(n2, floor(n2 * contamination))
    if (n_bad > 0L) {
      bad <- sample.int(n2, n_bad)
      group2[bad] <- group2[bad] + 8 * sd2
    }
  }
  list(group1 = group1, group2 = group2)
}

#' Build the two-sample calibration adapter.
#' @export
two_sample_adapter <- function() {
  primary_decision <- function(data, scenario) {
    test_type <- .two_sample_test_type(scenario)
    analysis <- .two_sample_analysis(scenario)
    alpha <- .two_sample_alpha(analysis, scenario)
    correct <- .two_sample_correct(analysis)
    groups <- .two_sample_data(data)
    tested <- tryCatch(
      .two_sample_primary_test(groups$group1, groups$group2, test_type, alpha, correct),
      error = function(error) .two_sample_abort(
        sprintf("primary %s failed: %s", test_type, conditionMessage(error)), error
      )
    )
    list(
      p = tested$p, p_value = tested$p, original_p = tested$p,
      conclusion = tested$significant, significant = tested$significant,
      original_significant = tested$significant, test_type = test_type,
      alpha = alpha
    )
  }

  run_robustness <- function(data, scenario, n_boot = NULL, seed = NULL) {
    test_type <- .two_sample_test_type(scenario)
    analysis <- .two_sample_analysis(scenario)
    groups <- .two_sample_data(data)
    alpha <- .two_sample_alpha(analysis, scenario)
    correct <- .two_sample_correct(analysis)
    if (is.null(n_boot)) n_boot <- .two_sample_scenario_scalar(scenario, "n_boot", 1000L)
    if (is.null(seed)) seed <- .two_sample_scenario_scalar(scenario, "scenario_seed", 123L)
    n_boot <- .two_sample_validate_numeric(n_boot, "n_boot", integer = TRUE, positive = TRUE)
    seed <- .two_sample_validate_numeric(seed, "seed", integer = TRUE, lower = 0)
    removal <- .two_sample_scenario_scalar(scenario, "max_removal_pct", 0.30)
    .two_sample_validate_numeric(removal, "max_removal_pct", lower = .Machine$double.eps, upper = 1)
    # Evaluate the exact public primary test first so degenerate cases fail with
    # the same explicit adapter condition as primary_decision().
    .two_sample_primary_test(groups$group1, groups$group2, test_type, alpha, correct)
    args <- list(
      group1 = groups$group1, group2 = groups$group2, test_type = test_type,
      alpha = alpha, n_boot = n_boot, max_removal_pct = removal,
      seed = seed, correct = correct
    )
    weights <- analysis$weights
    if (!is.null(weights)) args$weights <- weights
    output <- tryCatch(
      do.call(stabilitest::robustness_analysis, args),
      error = function(error) .two_sample_abort(
        sprintf("robustness %s failed: %s", test_type, conditionMessage(error)), error
      )
    )
    if (!is.numeric(output$original_p) || length(output$original_p) != 1L ||
        !is.finite(output$original_p)) {
      .two_sample_abort(sprintf("robustness %s returned non-finite p", test_type))
    }
    output
  }

  list(primary_decision = primary_decision, run_robustness = run_robustness)
}

# Convenience aliases used by calibration runners and downstream analyses.
new_two_sample_adapter <- two_sample_adapter
get_two_sample_adapter <- two_sample_adapter
make_two_sample_adapter <- two_sample_adapter
two_sample_adapters <- two_sample_adapter

# Named entry points used by the scenario registry.  They intentionally
# delegate to the same adapter object so primary and robustness analyses cannot
# drift in their test or correction settings.
.two_sample_configured_adapter <- local({
  adapter <- NULL
  function() {
    if (is.null(adapter)) adapter <<- two_sample_adapter()
    adapter
  }
})

two_sample_primary_decision <- function(data, scenario) {
  .two_sample_configured_adapter()$primary_decision(data, scenario)
}

run_two_sample_robustness <- function(data, scenario, n_boot = NULL, seed = NULL) {
  .two_sample_configured_adapter()$run_robustness(data, scenario, n_boot = n_boot, seed = seed)
}
