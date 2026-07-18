#!/usr/bin/env Rscript

# ============================================================================
# scripts/00-2_audit_sources.R -- P07 Step 0.2: source and environment audit.
# ----------------------------------------------------------------------------
# Purpose
#   The provenance gate for the whole pipeline. Before any modeling it (1)
#   confirms every required R package is installed, (2) SHA-256 hashes and
#   independently re-checks the read-only P01 analytic artifacts against P01's
#   own key numbers, (3) audits the Queen contiguity graph (islands and
#   connected components) for both the full and the nonzero-demand tract sets,
#   (4) aggregates the four restricted DHR quarterly licensing survey workbooks
#   into per-sector calibration, and (5) runs a real INLA BYM2 smoke fit so a
#   broken INLA install fails here, loudly, rather than mid-run.
#
# Method
#   Cross-checks are assertion-based: every count (1,436 tracts; 1,409 eligible;
#   690 E2SFCA deserts; 117,062 licensed slots; graph islands/components) must
#   match P01 exactly or the script stops. The survey workbooks are aggregate
#   state/sector tables with no provider key, so they calibrate sector means but
#   cannot identify a provider-level residual variance.
#
# Reads   P01 analytic .rds (demand, supply, e2sfca, deserts, covariates,
#         weights, stability) + P01 outputs/key_numbers.csv; four restricted DHR
#         survey workbooks (quarterly_survey_YYYY-MM.xlsx; see data/DATA_ACCESS.md).
# Writes  provenance/{provenance,package_audit,survey_calibration,
#         source_audit_summary}.csv; data/interim/00-2_source_audit.rds; SSOT
#         rows in outputs/key_numbers.csv; sessioninfo.txt.
# ============================================================================

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))

# Every package the full P07 pipeline needs; the audit below refuses to proceed
# unless all are installed (fail fast at Step 0.2, not mid-model).
required_packages <- c(
  "sf", "spdep", "Matrix", "dplyr", "tidyr", "INLA", "inlabru",
  "ggplot2", "gt", "leaflet", "furrr", "readxl", "digest"
)
# Record availability and version of each dependency for the provenance trail.
package_audit <- data.frame(
  package = required_packages,
  available = vapply(required_packages, requireNamespace, logical(1), quietly = TRUE),
  version = vapply(
    required_packages,
    function(x) if (requireNamespace(x, quietly = TRUE)) as.character(utils::packageVersion(x)) else NA_character_,
    character(1)
  ),
  stringsAsFactors = FALSE
)
assert_that(all(package_audit$available), paste("Missing required packages:", paste(package_audit$package[!package_audit$available], collapse = ", ")))

# The seven read-only P01 analytic artifacts P07 consumes.
asset_names <- c(
  "01_demand_tracts.rds", "01_supply_providers.rds", "02_e2sfca.rds",
  "02_deserts_classified.rds", "01_covariates_tracts.rds",
  "05_spatial_weights.rds", "04_stability.rds"
)
asset_paths <- file.path(P01_ANALYTIC, asset_names)
assert_that(all(file.exists(asset_paths)), "One or more required P01 analytic assets are missing.")

# Load each P01 artifact through the traversal-guarded reader.
demand <- read_p01("01_demand_tracts.rds")
supply <- read_p01("01_supply_providers.rds")
e2sfca <- read_p01("02_e2sfca.rds")
deserts <- read_p01("02_deserts_classified.rds")
covariates <- read_p01("01_covariates_tracts.rds")
weights <- read_p01("05_spatial_weights.rds")
stability <- read_p01("04_stability.rds")
p01_ssot <- read_p01("key_numbers.csv", "outputs")

# Independently re-derive P01's headline counts from the raw artifacts and stop
# on any disagreement -- P07 must not silently inherit a changed upstream number.
assert_that(nrow(demand) == 1436L && !anyDuplicated(demand$GEOID), "Demand tract universe failed.")
assert_that(identical(demand$GEOID, e2sfca$GEOID), "Demand/E2SFCA order mismatch.")
assert_that(identical(demand$GEOID, deserts$GEOID), "Demand/classification order mismatch.")
assert_that(setequal(demand$GEOID, covariates$GEOID), "Covariate GEOID set mismatch.")
assert_that(sum(demand$total_under5E) == 294417, "Under-five total differs from P01 SSOT.")
assert_that(dplyr::n_distinct(demand$total_under5M) == 1246L, "Distinct demand MOE count differs from P01.")
assert_that(sum(demand$is_zero_demand) == 27L, "Zero-demand count differs from P01.")
assert_that(sum(demand$high_cv_flag, na.rm = TRUE) == 605L, "High-CV demand count differs from P01.")
assert_that(nrow(supply) == 2164L && !anyDuplicated(supply$facility_id), "Supply provider universe failed.")
assert_that(sum(supply$day_capacity) == 117062, "Licensed day-capacity total differs from P01.")
assert_that(sum(deserts$is_desert_e2sfca, na.rm = TRUE) == 690L, "P01 E2SFCA desert total differs from SSOT.")
assert_that(sum(deserts$desert_reporting_eligible) == 1409L, "Eligible tract denominator differs from P01.")
assert_that(all.equal(e2sfca$e2sfca, deserts$access_e2sfca, tolerance = 0) == TRUE, "E2SFCA copies disagree.")

# The 12 SES covariate families must each carry estimate/MOE/CV/high-CV columns.
ses_stems <- c(
  "poverty_rate", "snap_rate", "children_assistance_rate", "unemployment_rate",
  "female_lfp_rate", "minority_rate", "hispanic_rate", "median_hh_income",
  "no_vehicle_rate", "commute_60plus_rate", "no_internet_rate",
  "rent_burden_30plus_rate"
)
ses_columns <- unlist(lapply(ses_stems, function(x) paste0(x, c("E", "M", "_cv", "_high_cv"))))
assert_that(all(ses_columns %in% names(covariates)), "The 12 SES E/M/CV/high-CV families are incomplete.")

# Pull P01's always-desert stability anchor, falling back to the fresh-result
# metadata if the summary table does not expose it.
always_desert <- stability$primary_summary$n_tracts[
  as.character(stability$primary_summary$stability_class) == "always_desert"
]
if (length(always_desert) == 0L) {
  always_desert <- stability$metadata$canonical_fresh_result$always_desert_tracts
}
assert_that(length(always_desert) == 1L && always_desert == 519L, "P01 always-desert total differs from SSOT.")

# Audit the Queen contiguity graph. The full graph spans all 1,436 tracts; the
# model graph drops the 27 zero-demand tracts, and their island/component counts
# must match the values reported in the paper.
full_nb <- weights$full_nb
assert_that(identical(attr(full_nb, "region.id"), demand$GEOID), "Queen graph region IDs do not match demand order.")
full_islands <- sum(spdep::card(full_nb) == 0L)
full_components <- spdep::n.comp.nb(full_nb)$nc
eligible <- !demand$is_zero_demand
model_nb <- spdep::subset.nb(full_nb, subset = eligible)
model_islands <- sum(spdep::card(model_nb) == 0L)
model_components <- spdep::n.comp.nb(model_nb)$nc
assert_that(length(full_nb) == 1436L && full_islands == 2L && full_components == 3L, "Full Queen graph audit failed.")
assert_that(length(model_nb) == 1409L && model_islands == 1L && model_components == 2L, "Model Queen graph audit failed.")
assert_that(identical(attr(model_nb, "region.id"), demand$GEOID[eligible]), "Subset Queen graph order mismatch.")

# Quarterly licensing surveys are aggregate state/sector workbooks. They can
# calibrate sector means but have no provider key and cannot identify a
# provider-level residual variance.
#
# These four workbooks are RESTRICTED DHR inputs and are NOT distributed with
# this package (see data/DATA_ACCESS.md). To run this step, obtain your own
# copies, name each with the convention quarterly_survey_YYYY-MM.xlsx (one
# workbook per wave, each carrying a "Centers" and a "Homes" sheet), and either
# place them in restricted-data/dhr_surveys/ or point the P07_DHR_SURVEY_DIR
# environment variable at the directory that holds them. The public code reads
# one workbook per wave under the stable generic names built below.
survey_root <- Sys.getenv(
  "P07_DHR_SURVEY_DIR",
  unset = file.path(P07_CODEBASE_ROOT, "restricted-data", "dhr_surveys")
)
survey_waves <- c("2024-07", "2024-10", "2025-04", "2025-07")
survey_files <- paste0("quarterly_survey_", survey_waves, ".xlsx")
survey_paths <- file.path(survey_root, survey_files)
assert_that(all(file.exists(survey_paths)), "Quarterly survey workbooks are missing; obtain them (see data/DATA_ACCESS.md) and set P07_DHR_SURVEY_DIR.")

# Each survey header cell states "contacted of eligible"; pull the two integers.
parse_contact <- function(text) {
  nums <- as.numeric(unlist(regmatches(text, gregexpr("[0-9]+", text))))
  assert_that(length(nums) >= 2L, "Could not parse survey contact numerator/denominator.")
  c(contacted = nums[1L], eligible = nums[2L])
}

survey_rows <- list()
row_index <- 1L
# Read one Centers sheet and one Homes sheet per wave: contact counts from a
# header cell and the licensed/ability capacities from two fixed cells.
for (i in seq_along(survey_paths)) {
  for (sector in c("Centers", "Homes")) {
    contact_cell <- readxl::read_excel(
      survey_paths[i], sheet = sector, range = "A1:A1", col_names = FALSE,
      col_types = "text", .name_repair = "minimal", progress = FALSE
    )
    capacity_cells <- readxl::read_excel(
      survey_paths[i], sheet = sector, range = "B2:B3", col_names = FALSE,
      col_types = "numeric", .name_repair = "minimal", progress = FALSE
    )
    contact <- parse_contact(as.character(contact_cell[[1L]][1L]))
    licensed <- as.numeric(capacity_cells[[1L]][1L])
    ability <- as.numeric(capacity_cells[[1L]][2L])
    assert_that(is.finite(licensed) && licensed > 0 && is.finite(ability) && ability > 0, "Survey capacity cells are invalid.")
    survey_rows[[row_index]] <- data.frame(
      wave = survey_waves[i], sector = sector,
      contacted = unname(contact["contacted"]), eligible_providers = unname(contact["eligible"]),
      response_rate = unname(contact["contacted"] / contact["eligible"]),
      licensed_capacity = licensed, ability_capacity = ability,
      ability_to_licensed = ability / licensed,
      source_file = survey_files[i], stringsAsFactors = FALSE
    )
    row_index <- row_index + 1L
  }
}
# Stack the eight (four waves x two sectors) rows into one calibration table.
survey_calibration <- do.call(rbind, survey_rows)
assert_that(nrow(survey_calibration) == 8L, "Expected four waves by two sectors.")
assert_that(all(survey_calibration$ability_to_licensed > 0 & survey_calibration$ability_to_licensed <= 1), "Survey calibration ratios are outside (0,1].")
utils::write.csv(survey_calibration, p07_path("provenance", "survey_calibration.csv"), row.names = FALSE, na = "")

# An actual BYM2 smoke fit checks compiled INLA execution and posterior sampling.
# Five-node ring graph used only to exercise the compiled INLA BYM2 path.
toy_adj <- Matrix::sparseMatrix(
  i = c(1, 2, 2, 3, 3, 4, 4, 5, 5, 1),
  j = c(2, 1, 3, 2, 4, 3, 5, 4, 1, 5),
  x = 1, dims = c(5, 5), symmetric = FALSE
)
toy_graph <- INLA::inla.read.graph(toy_adj)
toy <- data.frame(y = c(4, 6, 5, 8, 7), E = rep(10, 5), idx = seq_len(5))
toy_fit <- INLA::inla(
  y ~ 1 + f(
    idx, model = "bym2", graph = toy_graph, scale.model = TRUE,
    adjust.for.con.comp = TRUE,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 0.5))
    )
  ),
  family = "poisson", data = toy, E = toy$E,
  control.predictor = list(compute = TRUE),
  control.compute = list(config = TRUE), num.threads = "1:1", verbose = FALSE
)
toy_sample <- INLA::inla.posterior.sample(1L, toy_fit)
assert_that(length(toy_sample) == 1L && all(is.finite(toy_fit$summary.linear.predictor$mean)), "INLA BYM2 smoke fit failed.")

# Assemble the provenance ledger: hash, size, and modified time of every input,
# tagged with release level and PII status.
all_paths <- c(asset_paths, file.path(P01_OUTPUTS, "key_numbers.csv"), survey_paths)
provenance <- data.frame(
  asset = c(asset_names, "P01 key_numbers.csv", paste0("Quarterly survey ", survey_waves)),
  path = all_paths,
  sha256 = vapply(all_paths, file_sha256, character(1)),
  bytes = as.numeric(file.info(all_paths)$size),
  modified_utc = format(file.info(all_paths)$mtime, tz = "UTC", usetz = TRUE),
  read_only_upstream = TRUE,
  contains_pii = FALSE,
  release_level = c(
    "tract aggregate", "provider-level restricted model input", rep("tract aggregate", 5),
    "aggregate registry", rep("state/county aggregate", 4)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(provenance, p07_path("provenance", "provenance.csv"), row.names = FALSE, na = "")
utils::write.csv(package_audit, p07_path("provenance", "package_audit.csv"), row.names = FALSE, na = "")

# Machine-readable observed-vs-expected audit summary (all checks must pass).
source_summary <- data.frame(
  check = c(
    "tracts", "nonzero_demand_tracts", "zero_demand_tracts", "children_under5",
    "distinct_under5_moe", "high_cv_tracts", "providers", "day_capacity",
    "p01_e2sfca_deserts", "p01_always_desert", "full_graph_islands",
    "full_graph_components", "model_graph_islands", "model_graph_components",
    "quarterly_survey_waves", "survey_provider_linkage", "inla_bym2_smoke"
  ),
  observed = c(
    1436, 1409, 27, 294417, 1246, 605, 2164, 117062, 690, 519,
    full_islands, full_components, model_islands, model_components, 4, "none", "pass"
  ),
  expected = c(
    1436, 1409, 27, 294417, 1246, 605, 2164, 117062, 690, 519,
    2, 3, 1, 2, ">=1 if available", "none", "pass"
  ),
  passed = TRUE,
  stringsAsFactors = FALSE
)
utils::write.csv(source_summary, p07_path("provenance", "source_audit_summary.csv"), row.names = FALSE, na = "")

# Persist the interim audit object for later steps and the manuscript.
save_analytic(
  list(
    source_summary = source_summary,
    package_audit = package_audit,
    survey_calibration = survey_calibration,
    graph = list(
      full_n = length(full_nb), full_islands = full_islands, full_components = full_components,
      model_n = length(model_nb), model_islands = model_islands, model_components = model_components
    ),
    completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
  ),
  "00-2_source_audit.rds", "interim"
)

# Register the audited counts as SSOT key numbers.
append_key_numbers(data.frame(
  key = c(
    "n_p01_desert_e2sfca", "p01_always_desert", "n_graph_islands_full",
    "n_graph_components_full", "n_graph_islands_model", "n_graph_components_model",
    "quarterly_survey_available", "survey_provider_linkage", "inla_version"
  ),
  value = c(690, 519, full_islands, full_components, model_islands, model_components, "yes", "none", as.character(utils::packageVersion("INLA"))),
  unit = c("tracts", "tracts", "islands", "components", "islands", "components", "yes/no", "linkage", "version"),
  source_script = "scripts/00-2_audit_sources.R",
  note = c(
    "Read from and independently checked against P01 SSOT.",
    "Fresh P01 stability always-desert anchor.",
    "Full 1,436-node Queen graph.", "Full 1,436-node Queen graph.",
    "Nonzero-demand 1,409-node model graph; one full-graph island is excluded with zero demand.",
    "Nonzero-demand 1,409-node model graph.",
    "Four canonical Fourth-share aggregate workbooks were found.",
    "Workbooks are state/county by sector aggregates and contain no registry ID.",
    "Local BYM2 smoke fit and posterior sampling passed."
  ), stringsAsFactors = FALSE
))

# Freeze the exact package/session state used for this run.
writeLines(capture.output(sessionInfo()), p07_path("sessioninfo.txt"))
cat("Step 0.2 source/environment audit: PASS\n")
