# Replication package: Probabilistic Child Care Deserts

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![R >= 4.3](https://img.shields.io/badge/R-%3E%3D%204.3-1f65b7.svg)](https://www.r-project.org/)
[![Replication guide](https://img.shields.io/badge/guide-online-9E1B32.svg)](https://joonho112.github.io/probabilistic-childcare-deserts-replication/)

This repository is the replication package for

> **Probabilistic child care deserts: Bayesian accessibility estimation with false discovery control in Alabama.**
> JoonHo Lee (2026).
> arXiv preprint [identifier forthcoming].
> <!-- TODO(author): add the arXiv identifier and DOI once posted; confirm co-authors and order. -->

It contains the code that turns a deterministic child care **desert map** of
Alabama into tract-level posterior desert *probabilities* and error-controlled
policy declarations, the disclosure-safe aggregate results that back every figure
and table, and a step-by-step guide to reproducing them. The confidential inputs
are **not** distributed (see [Data availability](#data-availability)); the package
ships code and results, and a reader who obtains the data can rerun the whole
pipeline.

A rendered walkthrough is the
[replication guide](https://joonho112.github.io/probabilistic-childcare-deserts-replication/)
(source in `book/`, built with `quarto render book`). Read the guide to
reproduce; read the paper to understand the science.

---

## Overview

A child care desert is a census tract where accessible child care supply is
inadequate for the number of young children — here, fewer than $A = 0.33$
accessible slots per child under an enhanced two-step floating catchment area
(E2SFCA) access score. That binary label steers public investment, but it is
computed from uncertain inputs and says nothing about how sure any single
designation is.

This study keeps the access model fixed and asks a sharper question. It
propagates the two dominant input uncertainties — American Community Survey (ACS)
under-five **demand** error and survey-calibrated **capacity** error — through a
Besag–York–Mollié (BYM2) Bayesian spatial coverage model with a Tweedie
likelihood, draws 2,000 joint posterior samples for 1,409 Alabama tracts, and
gives each tract a posterior probability of being a desert. Those probabilities
then feed declaration procedures that control the false discovery rate (FDR) and
the false discovery exceedance (FDX) at preregistered levels, with county
decisions weighted by children under five. The output is a tiered policy product:
a defensible core, an extended priority list, and an explicit agenda for where
better data would matter most.

## What is reproduced

- **The posterior desert-probability map** for 1,409 tracts. The median tract
  desert probability is **0.611**; 816 tracts exceed a probability of 0.50, but
  517 (36.7%) fall in an ambiguous 0.25–0.75 band, and posterior intervals in the
  high-uncertainty demand stratum are more than twice as wide as elsewhere.
- **The error-controlled declarations**: **512** tracts at FDR $q=0.10$ (achieved
  mean LIS 0.099991), a stricter **412-tract** FDX core, and **13 of 67** counties
  under the child-weighted rule.
- **The reclassification** of the deterministic map: of its **690** point deserts,
  **500** survive (72.5%), **190** lose declarable status without being exonerated
  (median desert probability 0.689), and **12** enter.
- **The preregistered sensitivity analyses** ($\gamma$: 512/431/377; $A$:
  296/512/885; county weight: 13/9/19) and the triangulation against a 519-tract
  stability anchor (441, or 85.0%, FDR-declared).

Every headline value traces to `outputs/key_numbers.csv` (63 keys); the figures
and tables read that registry, and `manifest/verify_outputs.R` checks the shipped
artifacts against it.

## Reproduction tracks

| | Track C — exhibits *(default)* | Track A — full rerun |
|---|---|---|
| **Starts from** | the shipped aggregate layer `data/derived/` | the restricted inputs + a built companion P01 codebase |
| **Reproduces** | every figure and the headline numbers | every number, from raw sources |
| **Requires** | R and CRAN packages only | R, INLA, and the confidential data |
| **Time** | about a minute on a laptop | tens of minutes, plus data-access lead time |

Track C is the default entry point and needs no INLA and no restricted data.

## Quick start (Track C)

From the package root, or after opening `probabilistic-childcare-deserts-replication.Rproj`:

```r
source("scripts/00_setup.R")            # environment check
```
```sh
Rscript scripts/reproduce_exhibits.R    # rebuild F1–F6 into results/figures/
Rscript manifest/verify_outputs.R       # check the shipped outputs against the registry
```

`reproduce_exhibits.R` reads only the disclosure-safe `data/derived/` layer and
writes the six figures to `results/figures/`; compare them against the canonical
`outputs/figures/`. Chapter 2 of the guide covers setup, and Chapter 7 walks
through the exhibits one at a time.

## Requirements

- **R ≥ 4.3.** The authors ran R 4.6.0 (`sessioninfo.txt`).
- **Track C: CRAN only** — `sf`, `dplyr`, `tidyr`, `readr`, `ggplot2`,
  `patchwork`, `scales`, `gt`, `leaflet`, `htmlwidgets`, `digest`.
- **Track A: add the modeling stack** — `spdep`, `Matrix`, `truncnorm`,
  `readxl`, `furrr`, `future`, and **INLA** (not on CRAN):

  ```r
  install.packages("INLA",
    repos = c(INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)
  ```

  The authors used INLA 26.6.8.

## Repository structure

```
probabilistic-childcare-deserts-replication/
├── README.md                   this file
├── LICENSE                     MIT (code and documentation only)
├── CITATION.cff                paper + software metadata
├── _run_order.md               the canonical run order (both tracks)
├── sessioninfo.txt             the recorded R environment
├── probabilistic-childcare-deserts-replication.Rproj
├── .github/workflows/          renders the guide to GitHub Pages
├── scripts/
│   ├── 00_setup.R              environment check
│   ├── 00-2 … 04-1             the eleven numbered analysis steps
│   ├── build_derived.R         custodian: protected objects → data/derived/
│   ├── reproduce_exhibits.R    Track C: rebuild figures from data/derived/
│   └── 99_reproduce_all.R      Track A: run the whole pipeline
├── R/                          fct_io, fct_uncertainty, fct_bym2, fct_fdr,
│                               fct_viz, fct_leaflet
├── data/
│   ├── DATA_ACCESS.md          how to obtain every input
│   └── derived/                the shipped disclosure-safe aggregate layer
├── restricted-data/            where restricted inputs go locally (git-ignored)
├── outputs/                    the authors' canonical exhibits + key_numbers.csv
├── results/                    Track C regeneration target
├── provenance/                 source integrity + calibration metadata
├── manifest/                   SHA-256 ledger + verify_outputs.R
├── verification/               reproduction map + expected headline values
└── book/                       the replication guide (Quarto → GitHub Pages)
```

## Data availability

- **The confidential inputs are not distributed and cannot be redistributed.**
  The analysis used Alabama Department of Human Resources (DHR) administrative
  child care records — shared under a one-way data-use agreement (A24-0563) and
  confidential under Ala. Code § 38-2-6 — and commercially licensed provider
  geocoding. Neither may be shared here.
- **You do not need them to reproduce the exhibits.** The disclosure-safe
  aggregate layer in `data/derived/` ships with the repository, and Track C
  rebuilds every figure from it. Tract-level results are blanked for the 46
  display-suppressed tracts (zero demand or fewer than 10 children); no output
  contains a provider coordinate.
- **Public inputs are re-downloadable.** The ACS 2023 5-year estimates and the
  2020 Census/TIGER tract geometry are public; the only geometry the exhibits
  need already ships, simplified, under `data/derived/`.

The full data-governance statement — what each source is, why it is restricted,
and the process to request it — is in
[`data/DATA_ACCESS.md`](data/DATA_ACCESS.md).

## Reproducibility

- **Seeds are fixed throughout.** Preparation uses `20260715`; the joint
  posterior draws from three independent streams (latent `20260716`, demand
  `20260717`, supply `20260718`). INLA is pinned single-threaded for
  bit-reproducibility.
- **What reproduces exactly.** Track C does arithmetic on frozen inputs and
  reproduces every figure and number exactly. A full Track-A refit reproduces
  them up to the negligible drift of a different linear-algebra backend.
- **Verify it.** `Rscript manifest/verify_outputs.R` checks the shipped outputs
  against `outputs/key_numbers.csv` and the expected headline values, and exits
  non-zero on any mismatch. `manifest/pipeline_manifest.csv` is a SHA-256 ledger
  of every shipped artifact.

## Citation

Please cite the paper:

```bibtex
@article{lee2026probabilistic,
  author  = {Lee, JoonHo},
  title   = {Probabilistic Child Care Deserts: {Bayesian} Accessibility
             Estimation with False Discovery Control in {Alabama}},
  year    = {2026},
  journal = {arXiv preprint},
  note    = {arXiv preprint [identifier forthcoming]}
}
% TODO(author): add the arXiv identifier / DOI once posted; confirm co-authors.
```

Machine-readable metadata is in [`CITATION.cff`](CITATION.cff).

## Author

**JoonHo Lee**, The University of Alabama — corresponding author and package
maintainer ([jlee296@ua.edu](mailto:jlee296@ua.edu), GitHub
[@joonho112](https://github.com/joonho112), ORCID
[0009-0006-4019-8703](https://orcid.org/0009-0006-4019-8703)).

## License

Released under the MIT License. Copyright (c) 2026 JoonHo Lee. The license covers
the code and documentation only; the underlying data are not distributed and are
governed by separate agreements (see [`LICENSE`](LICENSE) and
[`data/DATA_ACCESS.md`](data/DATA_ACCESS.md)).
