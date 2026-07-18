# `data/`

This directory holds the study's data layers. Read
[`DATA_ACCESS.md`](DATA_ACCESS.md) first — it is the full data-governance
statement.

| Path | Ships? | What it is |
|---|---|---|
| [`DATA_ACCESS.md`](DATA_ACCESS.md) | yes | How to obtain every input; what is and is not distributed. |
| [`derived/`](derived/) | **yes** | The disclosure-safe aggregate layer: tract/county result tables, selection paths, sensitivity grids, the public simplified tract geometry, and the scalar registry. This is what the guide and Track C read. |
| `analytic/` | no (git-ignored) | The protected analytic objects a full pipeline run writes (the fitted BYM2 model, the coverage posterior, the declaration objects). Rebuilt by a Track A run; never distributed. |
| `interim/` | no (git-ignored) | Scratch objects, including the raw 1,409 × 2,000 posterior draw matrix. Never distributed. |

The `.gitignore` blocks the entire `data/` tree except `derived/` and the
Markdown files, and blocks every serialized-object extension (`.rds`, `.parquet`,
…) as defense in depth. If you rerun the pipeline (Track A), `analytic/` and
`interim/` are recreated locally and stay local.
