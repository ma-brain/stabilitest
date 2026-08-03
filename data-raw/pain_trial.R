# Source of the fixed case-study dataset (R/data_pain.R).
# Values were drawn from N(-18.5, 12.3^2) / N(-9.2, 11.8^2), standardized to
# the target sample moments, injected with one extreme responder (-52.0,
# treatment #14) and one minimal responder (+3.0, placebo #22), then rounded
# to one decimal. The vectors are shipped as exported constants so that all
# manuscript numbers are exactly reproducible; convert to data/*.rda with
# usethis::use_data(pain_treatment, pain_placebo) if preferred.
