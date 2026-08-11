stale_welch_reference_pattern <- paste0(
  "Welch.{0,120}55.{0,30}70.{0,60}",
  "(validated|(?<!no longer )active|reference range)"
)

stale_bootstrap_replication_pattern <- paste0(
  "bootstrap reproducibility probability|",
  "estimat(e|es|ing).{0,40}(the )?reproducibility probability|",
  "(?<!not the )probability.{0,30}(a )?(repeat|replicate|future|new)",
  ".{0,30}(trial|study|sample).{0,30}(agree|replicate|confirm)|",
  "would a (repeat|replicate|future|new).{0,30}(trial|study|sample)",
  ".{0,30}(agree|replicate|confirm|reach)|",
  "simulated reruns.{0,40}(confirm|confirmed)"
)
