# paretoinfer 0.2.0

## Resubmission

Version 0.1.0 was submitted on 2026-10-15 and withdrawn at our request before
review was completed, because we found a flaw in its certificate logic (an
alternative could be discarded through a chain of tolerance-based
certificates, which in rare configurations removed every Pareto-optimal
alternative). Version 0.2.0 corrects it; see NEWS.md.

The words flagged as possibly misspelled in the Description are correct:
"Waudby" and "Ramdas" are the surnames of the authors of the cited paper
(Waudby-Smith and Ramdas, 2024), and "auditable" is a standard English word.

## Test environments

* local: macOS 26 (Apple Silicon), R 4.6, `R CMD check --as-cran`
* CI (GitHub Actions): macOS-latest (R release), windows-latest (R release),
  ubuntu-latest (R devel, release, oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

* "New submission" (CRAN incoming feasibility). This is the first release.
  The same note lists the surnames Waudby and Ramdas and the word "auditable"
  as possibly misspelled; see above.

## Notes for the reviewers

* The package builds on the confidence-sequence kernels exported by `seqbench`
  (CRAN, same authors) and cites Waudby-Smith and Ramdas (2024) with its DOI in
  the Description. The Monte Carlo test of the guarantee is skipped on CRAN
  (`skip_on_cran()`); the deterministic tests of the certificate logic run
  everywhere.
* Examples run in under two seconds each; no example is wrapped in `\dontrun{}`.
  The four vignettes together take about two minutes to build (Monte Carlo
  illustrations with small replication counts).
