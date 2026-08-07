test_that("the active registry has no generic two_sample calibration key", {
  root <- normalizePath(testthat::test_path("..", ".."))
  source_registry <- file.path(root, "inst", "extdata",
                               "calibration-registry.csv")
  registry_path <- if (file.exists(source_registry)) {
    source_registry
  } else {
    system.file("extdata", "calibration-registry.csv", package = "stabilitest")
  }
  expect_true(nzchar(registry_path) && file.exists(registry_path),
              info = "installed calibration registry is missing")
  registry <- utils::read.csv(registry_path,
                              stringsAsFactors = FALSE)
  expect_false(any(registry$calibration_unit == "two_sample"))
  expect_true(any(registry$calibration_unit == "welch_unpaired"))
})

test_that("Gate A ANCOVA documentation audit passes in the source tree", {
  root <- normalizePath(testthat::test_path("..", ".."))
  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  skip_if_not(file.exists(audit), "documentation audit tool is not shipped")
  sap <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova", "CALIBRATION_SAP.md"
  )
  skip_if_not(file.exists(sap), "ANCOVA SAP is not shipped in the package tarball")
  status <- system2("Rscript", audit, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(status, "status"), NULL)
  expect_true(any(grepl("audit passed", status, ignore.case = TRUE)))
})

test_that("Gate B uncalibrated ANCOVA decision is published and documented", {
  root <- normalizePath(testthat::test_path("..", ".."))
  published <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova", "published"
  )
  skip_if_not(dir.exists(published), "ANCOVA published/ directory missing")

  required <- c(
    "candidate.rds",
    "candidate-diagnostics.json",
    "training-occupancy.csv",
    "training-failures.csv",
    "power-verification.csv",
    "training-manifest.rds",
    "registry.csv",
    "registry.rds",
    "hash_ledger.rds"
  )
  for (name in required) {
    expect_true(file.exists(file.path(published, name)), info = name)
  }

  candidate <- readRDS(file.path(published, "candidate.rds"))
  expect_identical(candidate$status, "uncalibrated")
  expect_identical(candidate$reason, "no_feasible_thresholds")
  expect_true(all(is.na(candidate$cutoffs)))
  expect_false(isTRUE(candidate$held_out_opened))

  registry <- utils::read.csv(file.path(published, "registry.csv"),
                              stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_true(any(registry$calibration_unit == "lm_ancova"))
  ancova <- registry[registry$calibration_unit == "lm_ancova", , drop = FALSE]
  expect_identical(ancova$status, "uncalibrated")
  expect_true(all(is.na(ancova$cutoff_fragile)))
  expect_true(all(is.na(ancova$cutoff_robust)))
  expect_match(paste(ancova$supported_conditions, collapse = " "),
               "no_feasible_thresholds", fixed = TRUE)

  policy_files <- file.path(root, c(
    "README.md", "NEWS.md",
    "manuscript/calibration/README.md",
    "manuscript/calibration/studies/lm_ancova/CALIBRATION_SAP.md"
  ))
  text <- paste(unlist(lapply(policy_files, readLines, warn = FALSE)),
                collapse = "\n")
  expect_match(text, "no_feasible_thresholds", fixed = TRUE)
  expect_match(text, "held-out not opened|held.out not opened|validation.*not opened",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "Gate B.{0,80}(fail-closed|uncalibrated)|fail-closed.{0,80}Gate B",
               perl = TRUE, ignore.case = TRUE)
})

test_that("Gate B uncalibrated ANCOVA v2 decision is published and documented", {
  root <- normalizePath(testthat::test_path("..", ".."))
  published <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova_v2", "published"
  )
  skip_if_not(dir.exists(published), "ANCOVA v2 published/ directory missing")

  required <- c(
    "candidate.rds",
    "candidate-diagnostics.json",
    "training-occupancy.csv",
    "training-failures.csv",
    "power-verification.csv",
    "training-manifest.rds",
    "registry.csv",
    "registry.rds",
    "hash_ledger.rds"
  )
  for (name in required) {
    expect_true(file.exists(file.path(published, name)), info = name)
  }

  candidate <- readRDS(file.path(published, "candidate.rds"))
  expect_identical(candidate$status, "uncalibrated")
  expect_identical(candidate$reason, "no_feasible_thresholds")
  expect_true(is.na(candidate$cutoff) ||
                (is.numeric(candidate$cutoff) && !is.finite(candidate$cutoff)))
  expect_false(isTRUE(candidate$held_out_opened))
  expect_identical(candidate$candidate_hash, "3dc2a1f840b3eb725bea629dc130f070")

  registry <- utils::read.csv(file.path(published, "registry.csv"),
                              stringsAsFactors = FALSE, na.strings = c("", "NA"))
  expect_true(any(registry$calibration_unit == "lm_ancova_v2"))
  ancova_v2 <- registry[registry$calibration_unit == "lm_ancova_v2", , drop = FALSE]
  expect_identical(ancova_v2$status, "uncalibrated")
  expect_identical(ancova_v2$version, "lm-ancova-v2-2026-1")
  expect_true(all(is.na(ancova_v2$cutoff_fragile)))
  expect_true(all(is.na(ancova_v2$cutoff_robust)))
  expect_match(paste(ancova_v2$supported_conditions, collapse = " "),
               "no_feasible_thresholds", fixed = TRUE)
  expect_match(paste(ancova_v2$supported_conditions, collapse = " "),
               "jackknife-light|fragility=0.5", perl = TRUE)

  policy_files <- file.path(root, c(
    "README.md", "NEWS.md",
    "manuscript/calibration/README.md",
    "manuscript/calibration/studies/lm_ancova_v2/CALIBRATION_SAP.md",
    "manuscript/calibration/studies/lm_ancova_v2/README.md"
  ))
  text <- paste(unlist(lapply(policy_files, readLines, warn = FALSE)),
                collapse = "\n")
  expect_match(text, "lm_ancova_v2", fixed = TRUE)
  expect_match(text, "no_feasible_thresholds", fixed = TRUE)
  expect_match(text, "held-out not opened|held.out not opened|validation.*not opened",
               perl = TRUE, ignore.case = TRUE)
  expect_match(
    text,
    "Gate B.{0,120}(fail-closed|uncalibrated)|(fail-closed|uncalibrated).{0,120}Gate B",
    perl = TRUE,
    ignore.case = TRUE
  )
  expect_match(text, "3dc2a1f840b3eb725bea629dc130f070", fixed = TRUE)
})

test_that("docs describe v2 as executed fail-closed with empirical pilot metrics", {
  root <- normalizePath(testthat::test_path("..", ".."))
  policy_files <- file.path(root, c(
    "NEWS.md",
    "manuscript/calibration/README.md"
  ))
  text <- paste(unlist(lapply(policy_files, readLines, warn = FALSE)),
                collapse = "\n")
  expect_match(text, "executed fail-closed|fail-closed after.{0,40}pilot",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "24\\.4", perl = TRUE)
  expect_match(text, "0\\.034", perl = TRUE)
  expect_match(text, "0\\.892", perl = TRUE)
  expect_match(text, "0\\.554", perl = TRUE)
  expect_match(text, "0\\.70", perl = TRUE)
  expect_match(
    text,
    "Finding 4|2026-08-06-lm-ancova-v3-design",
    perl = TRUE,
    ignore.case = TRUE
  )
  expect_match(text, "false.?GO|pilot GO|location metrics",
               perl = TRUE, ignore.case = TRUE)
})

test_that("Gate A ANCOVA v2 Track A SAP freezes the two-band protocol", {
  root <- normalizePath(testthat::test_path("..", ".."))
  sap <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova_v2",
    "CALIBRATION_SAP.md"
  )
  expect_true(file.exists(sap), info = "missing lm_ancova_v2 CALIBRATION_SAP.md")
  if (!file.exists(sap)) {
    return(invisible())
  }

  text <- paste(readLines(sap, warn = FALSE), collapse = "\n")
  expect_match(text, "lm_ancova_v2", fixed = TRUE)
  expect_match(text, "fragility\\s*=\\s*0\\.5", perl = TRUE)
  expect_match(text, "bootstrap\\s*=\\s*0\\.5", perl = TRUE)
  expect_match(text, "jackknife\\s*=\\s*0", perl = TRUE)
  expect_match(text, "Fragile if score\\s*[≤<=]\\s*L|score\\s*[≤<=]\\s*L.{0,40}Fragile",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "Not fragile", ignore.case = TRUE)
  expect_match(text, "null.{0,40}clear|fitting strata.{0,40}null",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "borderline.{0,40}diagnostic|diagnostic.?only",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "median\\(clear\\).{0,20}median\\(null\\)",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "P\\(null\\s*>\\s*median\\(clear\\)\\)",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "[≥>=]\\s*20", perl = TRUE)
  expect_match(text, "15\\s*[≤<=]\\s*Δ\\s*<\\s*20|15\\s*[≤<=].{0,10}<\\s*20",
               perl = TRUE)
  expect_match(text, "Δ\\s*<\\s*15|<\\s*15", perl = TRUE)
  expect_match(text, "[≤<=]\\s*0\\.10", perl = TRUE)
  expect_match(text, "0\\.10\\s*<\\s*O\\s*[≤<=]\\s*0\\.20|0\\.10\\s*<.{0,10}[≤<=]\\s*0\\.20",
               perl = TRUE)
  expect_match(text, "O\\s*>\\s*0\\.20|>\\s*0\\.20", perl = TRUE)
  expect_match(text, "[≥>=]\\s*0\\.75", perl = TRUE)
  expect_match(text, "0\\.70\\s*[≤<=]\\s*AUC\\s*<\\s*0\\.75|0\\.70\\s*[≤<=].{0,10}<\\s*0\\.75",
               perl = TRUE)
  expect_match(text, "AUC\\s*<\\s*0\\.70|<\\s*0\\.70", perl = TRUE)
  expect_match(text, "0.90", fixed = TRUE)
  expect_match(text, "0.95", fixed = TRUE)
  expect_match(text, "workers.{0,20}4|Workers.+`4`",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "n_boot.{0,20}1000|`1000`", perl = TRUE, ignore.case = TRUE)
  expect_match(text, "max_screen_draws.{0,20}10000|`10000`",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "100.{0,40}significant|significant.{0,40}100",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "false reassurance.{0,40}0\\.05|FR.{0,40}0\\.05",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "Wilson upper\\s*[≤<=]\\s*0\\.10",
               perl = TRUE, ignore.case = TRUE)
  expect_match(
    text,
    "Not.?fragile identification.{0,40}0\\.70|clear.{0,40}0\\.70",
    perl = TRUE,
    ignore.case = TRUE
  )
  expect_match(text, "Wilson lower\\s*[≥>=]\\s*0\\.60",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "no_feasible_thresholds", fixed = TRUE)
  expect_false(grepl("no_feasible_threshold(?!s)", text, perl = TRUE))
  expect_false(grepl(
    "no_feasible_thresholds.{0,60}or equivalent|or equivalent.{0,60}no_feasible",
    text, perl = TRUE, ignore.case = TRUE
  ))
  expect_match(
    text,
    "v1 validation.{0,80}(absent|not|never)|absent from v2|no v1 validation",
    perl = TRUE,
    ignore.case = TRUE
  )

  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  skip_if_not(file.exists(audit), "documentation audit tool is not shipped")
  status <- system2("Rscript", audit, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(status, "status"), NULL)
  expect_true(any(grepl("audit passed", status, ignore.case = TRUE)))
})

test_that("Phase 1 ANCOVA v3 Track E SAP freezes the violation-detection protocol", {
  root <- normalizePath(testthat::test_path("..", ".."))
  sap <- file.path(
    root, "manuscript", "calibration", "studies", "lm_ancova_v3",
    "CALIBRATION_SAP.md"
  )
  expect_true(file.exists(sap), info = "missing lm_ancova_v3 CALIBRATION_SAP.md")
  if (!file.exists(sap)) {
    return(invisible())
  }

  text <- paste(readLines(sap, warn = FALSE), collapse = "\n")
  expect_match(text, "lm_ancova_v3", fixed = TRUE)
  expect_match(text, "Track E|violation", perl = TRUE, ignore.case = TRUE)
  expect_match(text, "fragility\\s*=\\s*0\\.5", perl = TRUE)
  expect_match(text, "bootstrap\\s*=\\s*0\\.5", perl = TRUE)
  expect_match(text, "jackknife\\s*=\\s*0", perl = TRUE)
  expect_match(text, "54001", fixed = TRUE)
  expect_match(text, "20260807", fixed = TRUE)
  expect_match(text, "Track D.{0,80}parked|parked.{0,80}Track D",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "51001|52001|53001", perl = TRUE)
  expect_match(text, "ΔAUC|AUC_score", perl = TRUE)
  expect_match(text, "clean\\s*>\\s*violated", perl = TRUE, ignore.case = TRUE)
  expect_match(text, "0\\.10", perl = TRUE)
  expect_match(text, "lower bound\\s*>\\s*0|CI lower.{0,20}>\\s*0",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "cluster.?bootstrap|scenario.?cluster",
               perl = TRUE, ignore.case = TRUE)
  expect_match(text, "no gate|not gated", perl = TRUE, ignore.case = TRUE)
  expect_match(
    text,
    "v1.{0,40}validation|validation.{0,40}(seeds|IDs).{0,40}(absent|no v3)",
    perl = TRUE,
    ignore.case = TRUE
  )

  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  skip_if_not(file.exists(audit), "documentation audit tool is not shipped")
  status <- system2("Rscript", audit, stdout = TRUE, stderr = TRUE)
  expect_identical(attr(status, "status"), NULL)
  expect_true(any(grepl("audit passed", status, ignore.case = TRUE)))
})

test_that("the calibration documentation audit passes", {
  root <- normalizePath(testthat::test_path("..", ".."))
  audit <- file.path(root, "tools", "check-calibration-documentation.R")
  skip_if_not(file.exists(audit),
              "audit script is not shipped in the package tarball")
  status <- system2(
    file.path(R.home("bin"), "Rscript"), audit,
    stdout = TRUE, stderr = TRUE
  )
  attr(status, "status") <- attr(status, "status")
  if (is.null(attr(status, "status"))) attr(status, "status") <- 0L
  expect_equal(attr(status, "status"), 0L,
               info = paste(status, collapse = "\n"))
})

test_that("the proportions Phase 1 SAP freezes the key constants", {
  root <- normalizePath(testthat::test_path("..", ".."))
  sap <- file.path(root, "manuscript", "calibration", "studies",
                   "binary_proportion", "CALIBRATION_SAP.md")
  skip_if_not(file.exists(sap),
              "proportions SAP is not shipped in the package tarball")
  text <- paste(readLines(sap, warn = FALSE), collapse = "\n")
  expect_true(grepl("fragility = 0.5.*bootstrap = 0.5.*jackknife = 0", text))
  expect_true(grepl("clear exact power 0.95", text))
  expect_true(grepl("projected RI >= 0.72", text))
  expect_true(grepl("20260809L", text))
  expect_true(grepl("appear in no[[:space:]]+calibration ledger", text))
})
