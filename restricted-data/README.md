# `restricted-data/`

This directory is **empty in the public repository and is git-ignored** (except
this file). It is where you place the restricted and public source inputs once
you have obtained them (see [`../data/DATA_ACCESS.md`](../data/DATA_ACCESS.md)).
Nothing here is ever distributed.

You only need to populate this directory to run **Track A** (the full pipeline
from raw sources). To reproduce the figures and tables without restricted data,
use Track C — nothing goes here.

## Expected layout

```
restricted-data/
└── dhr_surveys/                        (RESTRICTED — obtain from Alabama DHR)
    ├── quarterly_survey_2024-07.xlsx   each workbook has a "Centers" and a "Homes" sheet
    ├── quarterly_survey_2024-10.xlsx
    ├── quarterly_survey_2025-04.xlsx
    └── quarterly_survey_2025-07.xlsx
```

The companion **P01** codebase (which holds the restricted provider/geocoding
artifacts P07 reads) lives OUTSIDE this package. Point the environment variables
at your inputs before running Track A:

| Variable | Default | Points at |
|---|---|---|
| `P01_CODEBASE_ROOT` | `../codebase-P01` | your built companion P01 codebase (read-only) |
| `P07_DHR_SURVEY_DIR` | `restricted-data/dhr_surveys` | the four quarterly survey workbooks above |
| `CENSUS_API_KEY` | — | a free Census API key, if you re-pull ACS demand |

The `.gitignore` rule for this directory is `/restricted-data/*` with a single
exception for this `README.md`, so any data file you place here is automatically
excluded from version control.
