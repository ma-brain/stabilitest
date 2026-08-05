# Task 15 publication archive

This directory is the immutable publication freeze from Task 15.  It records
the broad-family experiment, including its historical `two_sample` calibration
family, manifests, summaries, and output hashes.  These files are historical
evidence only and are **not active** calibration inputs.

The only active runtime calibration registry is
`inst/extdata/calibration-registry.csv`.  That registry uses method-specific
calibration units (for example, `welch_unpaired` and `paired_t`) and is loaded
by the package at runtime.  The broad-family registry and results in this
directory must not be renamed, regenerated, or used as fresh confirmation of
any method-specific calibration claim.

In particular, the Task 15 validation rows were inspected during the original
experiment.  Reusing them as held-out confirmation would invalidate the new
method-specific calibration design; any reanalysis is exploratory and must be
identified as such.
