# CLAUDE.md (paper/)

Guidance for Claude Code when working on the manuscript in `paper/`. The package
itself is described in the root `CLAUDE.md` (local only). Resume notes for the
next session are in `paper/RETOMADA.md`; status and run order in `paper/README.md`.

## The manuscript

- Title: "Anytime-Valid Identification of Pareto-Optimal Alternatives from Noisy
  Simulations". Authors, in this order: Raydonal Ospina, André Leite, Cristiano
  Ferraz (CAST Lab, Department of Statistics, UFPE).
- Target: Technometrics (ASA template, double-blind, Q1, no APC on the
  subscription route). Fallbacks without APC: CSDA, EJOR, INFORMS JoC.
- Limits: main text at most 25 pages (currently exactly 25), abstract under 250
  words; supplementary material is unrestricted.
- Describes package version 0.2.0 (exact-dominance discarding, epsilon only for
  acceptance, certified epsilon-cover, open-pair allocation, optional cover phase).

## Layout and build

- `manuscript/main.tex` inputs `sections/{intro,problem,failure,method,simulation,applications,discussion}.tex`;
  `manuscript/supplement.tex` inputs `sections/supp_proofs.tex` and `sections/supp_results.tex`.
- Cross-references between the two use `xr-hyper`: in the main text `S-` prefixes
  supplement labels, in the supplement `M-` prefixes main labels. Compile
  supplement, main, supplement:
  `cd paper/manuscript && latexmk -pdf supplement.tex && latexmk -pdf main.tex && latexmk -pdf supplement.tex`.
- `\newcommand{\blind}{0}` in `main.tex`: set to 1 for the anonymous version.
- Floats and the bibliography use `\spacingset{1}`; body text uses 1.8.
- Notation macros are defined in the `main.tex` preamble (`\mub`, `\epsb`, `\Pset`,
  `\Peps`, `\Sset`, `\Aset`, `\Cset`, `\Dset`, `\Rset`, `\Gev`, `\bdom`); use them.

## Generated content: never edit by hand

| Output | Script |
|---|---|
| `figures/fig1_failure.pdf` | `code/fig1_failure.R` |
| `figures/fig2_efficiency.pdf`, `tables/tab1_validity.tex`, `results/study2_summary.rds` | `code/fig_studies.R` |
| `figures/fig3_applications.pdf` | `code/fig_apps.R` |
| `tables/tabS1_horizon.tex` … `tabS4_inventory.tex` | `code/supp_tables.R` |

Figures share `code/fig_theme.R` (validated palette: blue #2a78d6, aqua #1baf7a,
orange #eb6834, grey #9a9893, yellow #eda100). Scripts run from the package root
and load the package from `paper/.lib/current` (installed by `code/00_install.R`).
`code/common.R` holds the `run_one()` harness and `compute_sets_v010()`, the
0.1.0 rule kept for comparison. `PI_R` sets the number of replications.

## Writing rules (from the user's style guides)

- Author voice; no AI-sounding or self-describing text; abstract is not a press
  release and contains no pre-emptive rebuttals.
- No em-dashes; no rhetoric words (clearly, novel, remarkably, importantly,
  notably, indeed, …) or hedges (very, quite, …); no "note that"; neutral rather
  than absolute claims.
- Unified notation; "Section~", "Figure~", "Table~" spelled out and every figure
  and table cited, in order.
- IMRaD with hypotheses H1 to H3 stated in the introduction and assessed in the
  discussion, including the ones not supported.
- Every number in the text must come from `results/`; after rerunning a script,
  recheck the sentences that quote it (see the findings list in `README.md`).
