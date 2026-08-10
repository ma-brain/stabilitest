---
output: pdf_document
fontsize: 11pt
geometry: margin=0.8in
date: 2026-08-10
---

\thispagestyle{empty}
\today

Editor  
The R Journal  

Dear Editor,

Please consider our article titled "Stabilitest: Robustness and Fragility
Analysis of Statistical Test Conclusions in R" for publication in the R
Journal.

The manuscript introduces `stabilitest`, a package that asks how easily a
statistical conclusion could be overturned. It combines three complementary
sensitivity views of one pre-specified analysis --- jackknife leave-one-out
influence, greedy worst-case observation removal in the spirit of the maximum
influence perturbation of Broderick, Giordano and Meager, and bootstrap
same-decision rates --- across two-sample location and proportion
tests, linear model and ANCOVA terms, GLM and Cox terms, and TOST equivalence
and non-inferiority endpoints.

We believe readers of the R Journal will find the article useful for two
reasons beyond the software itself. First, the observed-sample perturbation
analysis complements the broader emphasis on sensitivity analysis in ICH
E9(R1). The article also positions the package carefully against the existing R
ecosystem --- `fragility`, `sensemakr`, `konfound`, `EValue`, `influence.ME`,
`car`, and `zaminfluence` --- distinguishing tools that reason about unmeasured
threats from those, like this one, that interrogate the observed sample.

Second, the article takes an unusual position on interpretation thresholds. A
categorical robustness verdict is treated as a claim requiring evidence: the
package emits one only where an independent, pre-registered calibration study
has validated thresholds for that exact analysis configuration, and otherwise
suppresses the label while retaining all numeric output. We report a successful
Fisher exact-test study, a prospective Welch study whose frozen training phase
found no candidate satisfying the pre-specified gates after 4,500 completed
analyses, and three negative ANCOVA studies. Because the Welch study failed
closed at training, its held-out validation data were not opened. These results
establish empirical infeasibility under their frozen designs without claiming
that all possible future designs must fail. We consider the negative results
contributions rather than omissions and welcome reviewer scrutiny of that
framing.

All results in the article are computed from code or read from committed
artifacts with committed generation scripts; nothing is transcribed. Heavy
calibration is exposed through those scripts and frozen artifacts, while the
article itself knits to HTML and PDF well inside the journal's ten-minute
budget. Full regeneration and a clean-checkout rehearsal are documented in
`REPRODUCE.md` and the accompanying reproduction report.

The article targets the published CRAN package version 0.6.0 at
<https://CRAN.R-project.org/package=stabilitest>. Final package and article
checks are recorded in the accompanying reproduction report.

\bigskip

Regards,

Marius Ardelean  
Independent researcher  
marally@gmail.com
