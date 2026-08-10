---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor  
The R Journal  
\bigskip

Dear Editor,
\bigskip

Please consider our article titled "stabilitest: Robustness and Fragility
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
reasons beyond the software itself. First, it addresses a question that
regulatory guidance (ICH E9(R1)) explicitly asks analysts to answer but
supplies no machinery for, and it positions the package carefully against the
existing R ecosystem for sensitivity analysis --- `fragility`, `sensemakr`,
`konfound`, `EValue`, `influence.ME`, `car`, and the non-CRAN `zaminfluence`
--- distinguishing packages that reason about unmeasured threats from those,
like this one, that interrogate the observed sample.

Second, the article takes an unusual position on interpretation thresholds. A
categorical robustness verdict is treated as a claim requiring evidence: the
package emits one only where an independent, pre-registered calibration study
has validated thresholds for that exact analysis configuration, and otherwise
suppresses the label while retaining all numeric output. We report a successful
Fisher exact-test study, a prospective Welch study that found no feasible
thresholds after 4,500 completed training analyses, and three negative ANCOVA
studies. The latter establish empirical infeasibility under their frozen
designs without claiming that all possible future designs must fail. We
consider these negative results contributions rather than omissions, and we
would welcome reviewer scrutiny of that framing in particular.

All results in the article are computed from code or read from committed
artifacts with committed generation scripts; nothing is transcribed. The
article knits to HTML and PDF well inside the journal's ten-minute budget,
with heavy simulation evidence loaded from artifacts whose full regeneration
is documented separately in `REPRODUCE.md`.

The article targets the published package version 0.6.0 at
<https://CRAN.R-project.org/package=stabilitest>. Final package and article
checks are recorded in the accompanying reproduction report.

\bigskip
\bigskip

Regards,

\
\
\

Marius Ardelean  
Independent researcher  
marally@gmail.com
