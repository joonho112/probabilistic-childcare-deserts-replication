# `data/derived/` — the disclosure-safe aggregate layer

Everything in this directory is a **disclosure-safe aggregate** produced by
[`../../scripts/build_derived.R`](../../scripts/build_derived.R) from the
protected analytic objects. **No file here contains a provider record or
coordinate.** Tract-level result columns are blanked for the 46 display-
suppressed tracts (zero demand or fewer than 10 children under five), exactly as
the paper's maps suppress them; the public ACS demand columns are kept. The
aggregate counts reported in the paper (e.g. 512 FDR / 412 FDX / 13 counties)
include those tracts — only their per-tract display is withheld. See
[`../DATA_ACCESS.md`](../DATA_ACCESS.md) for the full data-governance statement.

This layer is what the replication guide and `scripts/reproduce_exhibits.R`
(Track C) read. You do not need any restricted data to use it.

## Contents

| File | Rows | What it is |
|---|---:|---|
| `tract_results.csv` | 1,436 | One row per 2020 tract: public ACS demand, the display flag, and (for displayable tracts) the posterior desert probability, LIS, coverage summary, and FDR/FDX/comparison flags. See `tract_results_dictionary.csv` for every column. |
| `tract_results_dictionary.csv` | 23 | Column-by-column definitions for `tract_results.csv`. |
| `county_results.csv` | 67 | One row per county: child-weighted LIS, declaration flag, and FCR-style interval. |
| `fdr_path.csv` | 1,409 | The LIS step-up selection path (rank, LIS, running mean LIS) behind the FDR declaration (Figure F2; the paper's Figure 5a). |
| `fdx_path.csv` | 1,409 | The FDX search path (rank, posterior exceedance probability) behind the FDX core (Figure F3; the paper's Figure 5b). Anonymous ranks; no GEOID. |
| `sensitivity_gamma.csv` | 3 | FDR declarations under the preregistered buffer grid (γ = 0 / 0.03 / 0.05). |
| `sensitivity_A.csv` | 3 | FDR declarations under the adequacy-standard grid (A = 0.25 / 0.33 / 0.50). |
| `sensitivity_county_weight.csv` | 3 | County declarations under the weight grid (under-five / equal / area). |
| `triangulation.csv` | 1 | Overlap of the companion 519-tract always-desert anchor with the FDR and FDX sets. |
| `al_tracts_2020_simplified.geojson` | 1,436 | Public, simplified 2020 Census/TIGER tract polygons (EPSG:4326) carrying GEOID, county, the ACS under-five estimate, and the display flag — the only geometry the exhibits need. |
| `key_numbers.csv` | 63 | A copy of the scalar registry (the single source of truth for every headline value in the paper). |

## Regeneration (custodian only)

```sh
Rscript scripts/build_derived.R
```

This requires the protected analytic objects (a completed Track-A run); only the
data custodian can run it. Public readers consume its output — what ships here.
