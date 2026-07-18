# Data access

**This package ships code and results, not data.** No provider records, no
geocoded coordinates, and no analytic data objects are included. That is a
deliberate constraint of a confidential-data study, not an oversight. This file
explains exactly what the study used, what is and is not shipped here, and how a
researcher could obtain the inputs to rerun the pipeline from scratch.

---

## Who needs this, and who does not

- **You do NOT need any restricted data to reproduce the figures, tables, and
  headline numbers.** The disclosure-safe aggregate layer in
  [`derived/`](derived/) ships with the repository. `scripts/reproduce_exhibits.R`
  ("Track C") rebuilds every figure from it, and `manifest/verify_outputs.R`
  checks every headline value. This is the default path; start there.

- **You DO need the restricted inputs only to rerun the full pipeline from raw
  sources** ("Track A"): the Bayesian model fit, the posterior draws, and the
  false-discovery declarations. That requires the two restricted sources below,
  a built companion **P01** codebase, and the INLA toolchain.

---

## The restricted inputs

### 1. Alabama DHR administrative child care data

**What it is.** The analysis rests on the companion study's fixed universe of
2,164 licensed child care providers and 117,062 licensed daytime slots, plus
four statewide quarterly provider surveys (July 2024, October 2024, April 2025,
July 2025) reported as sector aggregates. These derive from the Alabama
Department of Human Resources (DHR) administrative child care **licensing**
records.

**Why it is restricted.** The data were shared with the University of Alabama
under a Federal Demonstration Partnership one-way Data Transfer and Use Agreement
(Agreement **A24-0563**) and are confidential under Alabama law
(**Ala. Code § 38-2-6**). The agreement bars redistribution: the recipient
"shall not disclose, release, sell, rent, lease, loan, or otherwise grant access
to the Data to any third party" without DHR's prior written consent, and
personally identifiable information "shall not be disclosed or released for any
purpose." No third-party collaborators are permitted on the data, and the source
must be credited in any public disclosure. DHR is the source of the data; its
provision does not imply endorsement of any analysis or conclusion.

**How to request it.** Redistribution of the restricted inputs is governed by the
DHR agreement, not by this package's MIT code license. A researcher who wants to
rerun the pipeline from raw sources would:

1. contact the Alabama DHR Child Care Services Division (the CCDF Lead Agency
   data office), routed through the authors ([jlee296@ua.edu](mailto:jlee296@ua.edu));
2. execute their own institutional Data Use / Transfer Agreement with DHR, naming
   an institutional principal investigator, for a single project with no
   redistribution and secure transfer;
3. obtain their institutional IRB approval or exemption; and
4. receive the data by secure transfer.

Once obtained, the four survey workbooks go under `restricted-data/dhr_surveys/`,
named `quarterly_survey_2024-07.xlsx`, `quarterly_survey_2024-10.xlsx`,
`quarterly_survey_2025-04.xlsx`, `quarterly_survey_2025-07.xlsx` (each with a
`Centers` and a `Homes` sheet), or point `P07_DHR_SURVEY_DIR` at their location.

### 2. Commercial address geocoding (Melissa)

**What it is.** The companion P01 study geocoded provider street addresses to
rooftop coordinates using Melissa (www.melissa.com), which drives the travel-time
catchments behind the fixed E2SFCA operator. **P07 never touches the geocoding
directly** — it consumes P01's already-geocoded supply file as a read-only
upstream artifact — so the restriction propagates transitively.

**Why it is restricted.** The Melissa data were obtained under a commercial,
single-use license: the delivered list "is for one-time rental use only" and "may
not be used for building or integrating into any other databases." That term
forbids redistributing the coordinates, so they are excluded from this package
entirely.

**How to obtain an equivalent.** Either license your own Melissa product, or
substitute a free geocoder — the US Census Geocoder
(`https://geocoding.geo.census.gov`) or OpenStreetMap/Nominatim — accepting a
lower match rate and precision (the authors moved to a commercial geocoder
because an open-geocoder pass left roughly 15% of providers unmatched or
low-confidence).

### The companion P01 codebase

P07 is a downstream, read-only consumer of a **built companion P01 codebase**
(the deterministic child care desert study). It reads P01's canonical tract- and
provider-level artifacts in place and never modifies them. To run Track A you
need a completed P01 build — which itself requires the two restricted sources
above — and you point `P01_CODEBASE_ROOT` at it (default `../codebase-P01`).

---

## The public inputs (redistributable)

Everything below is a public product you can re-download. The only geometry the
exhibits need already ships, simplified, as
[`derived/al_tracts_2020_simplified.geojson`](derived/al_tracts_2020_simplified.geojson).

| Source | Where | Version / geography | Used for |
|---|---|---|---|
| ACS 5-year estimates | Census API `https://api.census.gov/data/2023/acs/acs5` (e.g. via `tidycensus`) | 2023 5-year; Alabama census tracts | Under-five demand (`B01001_003` + `B01001_027`) and its 90% MOE |
| 2020 cartographic census tracts | `https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_01_tract_500k.zip` | 2020, Alabama | The 1,436-tract spatial universe |

A free Census API key (`https://api.census.gov/data/key_signup.html`) is supplied
via the `CENSUS_API_KEY` environment variable and must never be committed.

---

## What is, and is not, shipped

**Shipped (disclosure-safe):**

- all code (`scripts/`, `R/`);
- the aggregate results in `data/derived/` (tract- and county-level tables, the
  selection paths, the sensitivity grids, the public simplified geometry, and the
  scalar registry `key_numbers.csv`);
- the rendered exhibits in `outputs/` (figures, tables, the aggregate leaflet);
- provenance and integrity metadata (`provenance/`, `manifest/`).

Every tract-level result column is blanked for the 46 display-suppressed tracts
(zero demand or fewer than 10 children), matching the paper's maps. No output
contains a provider identifier or coordinate.

**Never shipped, and never to be committed:**

- provider records, addresses, or geocoded coordinates;
- the four DHR quarterly survey workbooks;
- the analytic and interim `.rds` objects (the fitted model and the raw
  1,409 × 2,000 posterior draw matrix), which the `.gitignore` blocks by
  extension as defense in depth;
- any Census API key.

Redistribution of the restricted inputs is governed by the DHR agreement and the
Melissa license, not by this package's MIT code license.

---

## A note for authors before public release

This repository is signed (it carries the author's name, ORCID, and email) and
names the funding award and the data-sharing agreement. If the manuscript is
still under **double-anonymized review**, do not link this repository from the
submission — doing so would de-anonymize the authors. Publish or link it only
once review permits, and after the partner-agency review of the manuscript
required by the data-sharing agreement is complete.

Questions: JoonHo Lee, [jlee296@ua.edu](mailto:jlee296@ua.edu).
