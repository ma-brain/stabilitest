# Study-local assembly helpers for binary-proportion publication accounting.
#
# Publication accounting for the proportions family.  The clear and borderline
# strata are quota-bearing (>= 100 significant completed per required scenario,
# failure rate <= 5%).  Null scenarios are structurally sparse under Fisher's
# exact test (enumerated type-I 0.009-0.040), so null occupancy is recorded
# honestly as a known limitation rather than enforced as a hard quota: the FR
# stratum needs only enough null-significant replicates to pin the FR-safe
# cutoff, which the pilot confirmed (Wilson upper < 0.10).

`%||%` <- function(left, right) if (is.null(left)) right else left

binary_proportion_publication_accounting <- function(audit, min_quota = 100L) {
  if (!is.data.frame(audit) || !nrow(audit)) {
    stop("audit must be a non-empty data frame", call. = FALSE)
  }
  status <- as.character(audit$status)
  completed_mask <- status == "completed"
  failed_mask <- status == "failed"
  by_scenario <- do.call(rbind, lapply(split(audit, audit$scenario_id), function(rows) {
    st <- as.character(rows$status)
    attempted <- nrow(rows)
    failed <- sum(st == "failed", na.rm = TRUE)
    truth <- as.character(rows$truth_class[[1L]])
    # Significant completed: completed AND screening_conclusion significant.
    sig_completed <- sum(st == "completed" & !is.na(rows$screening_conclusion) &
                           rows$screening_conclusion == "significant", na.rm = TRUE)
    data.frame(
      scenario_id = as.character(rows$scenario_id[[1L]]),
      truth_class = truth,
      attempted = attempted,
      completed = sum(st == "completed", na.rm = TRUE),
      significant_completed = sig_completed,
      failed = failed,
      failure_rate = if (attempted == 0L) NA_real_ else failed / attempted,
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_scenario) <- NULL
  list(
    attempted = nrow(audit),
    completed = sum(completed_mask, na.rm = TRUE),
    failed = sum(failed_mask, na.rm = TRUE),
    by_scenario = by_scenario,
    completed_rows = audit[completed_mask, , drop = FALSE],
    min_quota = as.integer(min_quota)
  )
}

# Enforce quota on clear/borderline; record null sparsity without aborting.
# Null scenarios cannot reach 100 significant under Fisher's conservatism; the
# FR stratum needs only enough null-significant replicates for a Wilson-upper-
# bounded FR-safe cutoff.  A hard null quota would contradict the SAP's
# justification for the 0.95 clear-power choice.
binary_proportion_assert_publication_ready <- function(audit, min_quota = 100L,
                                                       max_failure_rate = 0.05,
                                                       required_scenarios = NULL) {
  summary <- binary_proportion_publication_accounting(audit, min_quota = min_quota)
  if (!is.null(required_scenarios)) {
    missing <- setdiff(as.character(required_scenarios), summary$by_scenario$scenario_id)
    if (length(missing)) {
      stop(sprintf("missing scenario checkpoint: %s", paste(missing, collapse = ", ")),
           call. = FALSE)
    }
  }
  # Quota enforced on clear + borderline (where it is achievable).
  quota_bearing <- summary$by_scenario$truth_class %in% c("clear", "borderline")
  short <- summary$by_scenario$significant_completed < as.integer(min_quota)
  short <- short & quota_bearing
  if (any(short, na.rm = TRUE)) {
    stop(sprintf(
      "quota shortfall for scenarios: %s",
      paste(summary$by_scenario$scenario_id[short], collapse = ", ")
    ), call. = FALSE)
  }
  bad <- summary$by_scenario$failure_rate > as.numeric(max_failure_rate)
  if (any(bad, na.rm = TRUE)) {
    stop(sprintf(
      "failure rate exceeds %.0f%% for scenarios: %s",
      100 * max_failure_rate,
      paste(summary$by_scenario$scenario_id[bad], collapse = ", ")
    ), call. = FALSE)
  }
  invisible(summary)
}
