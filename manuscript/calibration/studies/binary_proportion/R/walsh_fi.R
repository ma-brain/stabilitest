# Classic Walsh event-flip fragility index (comparator, never in the score).
#
# Convention (frozen, documented in the SAP):
#   - The smaller-event arm is identified (fewer total events).
#   - To overturn a significant conclusion, non-events in that arm are flipped
#     0 -> 1 one at a time (adding events, reducing the between-arm disparity)
#     until the Fisher exact p-value reaches >= alpha.
#   - The index is the count of flips required; 0 if already non-significant.
# This matches the literature Walsh FI event-flip definition for 2x2 tables
# (flip outcomes in the smaller-event arm toward the larger-event arm).  It is
# archived per replicate for comparison with the removal-based fragility
# component; it never enters the score or any gate.

.walsh_fi_table <- function(g1, g2) {
  matrix(c(sum(g1), length(g1) - sum(g1),
           sum(g2), length(g2) - sum(g2)), nrow = 2L)
}

walsh_event_flip_fi <- function(group1, group2, alpha = 0.05,
                                direction = "overturn") {
  group1 <- as.integer(group1)
  group2 <- as.integer(group2)
  if (anyNA(group1) || anyNA(group2)) {
    stop("walsh_event_flip_fi does not allow missing outcomes", call. = FALSE)
  }
  if (!all(group1 %in% c(0L, 1L)) || !all(group2 %in% c(0L, 1L))) {
    stop("walsh_event_flip_fi requires 0/1 vectors", call. = FALSE)
  }
  events1 <- sum(group1)
  events2 <- sum(group2)
  n1 <- length(group1)
  n2 <- length(group2)
  tab <- .walsh_fi_table(group1, group2)
  significant0 <- stats::fisher.test(tab)$p.value < alpha

  # If already non-significant, no flips are needed to "overturn".
  flip_arm <- if (events1 <= events2) "group1" else "group2"
  if (!significant0) {
    out <- 0L
    attr(out, "flip_arm") <- flip_arm
    return(out)
  }

  # Flip non-events (0 -> 1) in the smaller-event arm, adding events there to
  # shrink the between-arm disparity until significance is overturned.
  if (identical(flip_arm, "group1")) {
    non_events_flip <- n1 - events1
  } else {
    non_events_flip <- n2 - events2
  }

  fi <- NA_integer_
  for (k in seq_len(non_events_flip)) {
    if (identical(flip_arm, "group1")) {
      tab_k <- matrix(c(events1 + k, n1 - events1 - k, events2, n2 - events2),
                      nrow = 2L)
    } else {
      tab_k <- matrix(c(events1, n1 - events1, events2 + k, n2 - events2 - k),
                      nrow = 2L)
    }
    p_k <- stats::fisher.test(tab_k)$p.value
    if (p_k >= alpha) {
      fi <- k
      break
    }
  }
  if (is.na(fi)) fi <- non_events_flip
  attr(fi, "flip_arm") <- flip_arm
  fi
}
