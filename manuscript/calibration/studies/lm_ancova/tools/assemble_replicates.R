# Study-local assembly helpers for ANCOVA publication accounting.

`%||%` <- function(left, right) if (is.null(left)) right else left

lm_ancova_assert_assembly_status <- function(status) {
  status <- as.integer(status)
  if (length(status) != 1L || is.na(status) || status != 0L) {
    stop("nonzero assembly child-process status", call. = FALSE)
  }
  invisible(status)
}

lm_ancova_publication_accounting <- function(audit, min_quota = 100L) {
  if (!is.data.frame(audit) || !nrow(audit)) {
    stop("audit must be a non-empty data frame", call. = FALSE)
  }
  status <- as.character(audit$status)
  completed_mask <- status == "completed"
  failed_mask <- status == "failed"
  excluded_mask <- status == "excluded"
  by_scenario <- do.call(rbind, lapply(split(audit, audit$scenario_id), function(rows) {
    st <- as.character(rows$status)
    attempted <- nrow(rows)
    failed <- sum(st == "failed", na.rm = TRUE)
    data.frame(
      scenario_id = as.character(rows$scenario_id[[1L]]),
      attempted = attempted,
      completed = sum(st == "completed", na.rm = TRUE),
      failed = failed,
      excluded = sum(st == "excluded", na.rm = TRUE),
      failure_rate = if (attempted == 0L) NA_real_ else failed / attempted,
      stringsAsFactors = FALSE
    )
  }))
  rownames(by_scenario) <- NULL
  list(
    attempted = nrow(audit),
    completed = sum(completed_mask, na.rm = TRUE),
    failed = sum(failed_mask, na.rm = TRUE),
    excluded = sum(excluded_mask, na.rm = TRUE),
    by_scenario = by_scenario,
    completed_rows = audit[completed_mask, , drop = FALSE],
    min_quota = as.integer(min_quota)
  )
}

lm_ancova_assert_publication_ready <- function(audit, min_quota = 100L,
                                               max_failure_rate = 0.05,
                                               required_scenarios = NULL) {
  summary <- lm_ancova_publication_accounting(audit, min_quota = min_quota)
  if (!is.null(required_scenarios)) {
    missing <- setdiff(as.character(required_scenarios), summary$by_scenario$scenario_id)
    if (length(missing)) {
      stop(sprintf("missing scenario checkpoint: %s", paste(missing, collapse = ", ")),
           call. = FALSE)
    }
  }
  short <- summary$by_scenario$completed < as.integer(min_quota)
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
