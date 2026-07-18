# Canonical run order

There are two ways to reproduce this study. Pick the one that matches what you
have. Full detail is in the [replication guide](https://joonho112.github.io/probabilistic-childcare-deserts-replication/);
the data you would need for Track A is described in
[`data/DATA_ACCESS.md`](data/DATA_ACCESS.md).

Run every command from the package root in a clean R session.

---

## Track C — rebuild the exhibits from the shipped aggregate layer (no restricted data)

This is the default. It needs only a clean clone and a CRAN-only R stack.

```r
source("scripts/00_setup.R")            # environment check
```
```sh
Rscript scripts/reproduce_exhibits.R    # rebuild F1–F6 into results/figures/
Rscript manifest/verify_outputs.R       # check shipped outputs against key_numbers.csv
```

`scripts/reproduce_exhibits.R` reads only `data/derived/` (the disclosure-safe
tract/county tables and the public simplified tract geometry) and writes the six
figures to `results/figures/`. Compare them against the canonical
`outputs/figures/` shipped by the authors.

---

## Track A — rerun the whole pipeline from the restricted inputs

This reproduces every number from scratch. It requires the restricted inputs (a
built companion **P01** codebase and the four DHR quarterly survey workbooks;
see `data/DATA_ACCESS.md`) and the INLA toolchain. Point the environment
variables at your inputs, then run the pipeline in order. Each script validates
its upstream contract and stops on a failed gate.

| Order | Step | Command | Canonical outputs |
|---:|---|---|---|
| 1 | 0.2 | `Rscript scripts/00-2_audit_sources.R` | provenance + environment/source audit |
| 2 | 1.1 | `Rscript scripts/01-1_demand_uncertainty.R` | ACS demand uncertainty object |
| 3 | 1.2 | `Rscript scripts/01-2_supply_uncertainty.R` | capacity-error + fixed E2SFCA operator |
| 4 | 1.3 | `Rscript scripts/01-3_preregister_decisions.R` | frozen, hashed primary specification |
| 5 | 2.1 | `Rscript scripts/02-1_fit_bym2.R` | fitted Tweedie BYM2 model + graph audit |
| 6 | 2.2 | `Rscript scripts/02-2_posterior_draws.R` | 2,000 joint posterior coverage draws |
| 7 | 2.3 | `Rscript scripts/02-3_desert_probability.R` | tract desert probability and LIS |
| 8 | 3.1 | `Rscript scripts/03-1_sun_fdr.R` | tract FDR/FDX + county declarations |
| 9 | 3.2 | `Rscript scripts/03-2_compare_p01.R` | 2×2 vs the deterministic P01 map |
| 10 | 3.3 | `Rscript scripts/03-3_sensitivity.R` | preregistered sensitivity + triangulation |
| 11 | 4.1 | `Rscript scripts/04-1_visualize.R` | figures, tables, aggregate leaflet |

Or run all eleven steps in fresh subprocesses with the orchestrator:

```sh
Rscript scripts/99_reproduce_all.R
```

After a full rerun, refresh the shipped aggregate layer and verify:

```sh
Rscript scripts/build_derived.R         # regenerate data/derived/ (custodian step)
Rscript manifest/verify_outputs.R       # check outputs against key_numbers.csv
```

**Determinism.** Fixed seeds run throughout: `20260715` for deterministic
preparation and `20260716` for the uncertainty preflight, with independent joint
posterior streams `20260716` (latent), `20260717` (demand), and `20260718`
(supply). The INLA fit and sampling force serial outer and BLAS/OpenMP execution,
and each latent batch resets both the R and INLA RNG state. Track A arithmetic on
frozen inputs reproduces the published numbers exactly; a full INLA refit
reproduces them up to the negligible drift of a different linear-algebra backend.

**Single source of truth.** Every headline value lives once in
`outputs/key_numbers.csv` (63 keys). Figures and tables read that registry rather
than recomputing, and `manifest/verify_outputs.R` checks the shipped artifacts
against it.
