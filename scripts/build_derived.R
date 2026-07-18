#!/usr/bin/env Rscript
# =============================================================================
# build_derived.R : produce the disclosure-safe aggregate layer (data/derived/)
# =============================================================================
#
# Purpose : Regenerate the SHIPPED aggregate layer that the "no restricted data"
#           reproduction track (Track C) and the replication guide read. It
#           reads the protected analytic objects produced by the full pipeline
#           (data/analytic/*.rds -- tract- and county-level posterior results)
#           and writes ONLY disclosure-safe aggregates to data/derived/:
#             * tract_results.csv  -- one row per 2020 Census tract, with the
#                 posterior desert probability, LIS, coverage summary, and the
#                 FDR/FDX/county-comparison flags. Tract-level RESULT columns
#                 are blanked (NA) for the 46 display-suppressed tracts (zero
#                 demand or fewer than 10 children), exactly as the paper's
#                 maps suppress them. The public ACS demand columns are kept.
#             * county_results.csv -- one row per county (67), the child-
#                 weighted LIS, declaration flag, and FCR-style interval.
#             * fdr_path.csv / fdx_path.csv -- the anonymous ranked selection
#                 paths behind Figure 5 (rank vs running mean LIS / exceedance).
#             * sensitivity_*.csv, triangulation.csv -- the preregistered grids.
#             * al_tracts_2020_simplified.geojson -- public simplified 2020
#                 Census/TIGER tract polygons (EPSG:4326); the only geometry
#                 the exhibits need. It carries GEOID + county + the public ACS
#                 under-5 estimate + the display-suppression flag, nothing else.
#             * key_numbers.csv    -- a copy of the scalar registry (SSOT).
#
#           This script is the audit trail for HOW every shipped number was
#           reduced to a safe aggregate. Only someone who already holds the
#           protected analytic objects (the data custodian) can run it. Public
#           readers never run it -- they consume its output, which is what ships
#           in data/derived/.
#
# Privacy : Every table is checked with assert_no_pii_columns() before it is
#           written; no provider identifier or coordinate ever appears in the
#           analytic objects this reads, and none can appear in the output.
#           Tract results for the 46 display-suppressed tracts are set to NA so
#           no result is published for a tract with fewer than 10 children.
#           Aggregate counts (e.g. the 512 FDR / 412 FDX / 13 counties) live in
#           key_numbers.csv and the summary tables and DO include those tracts;
#           only their per-tract display is suppressed, matching the paper.
#
# Reads   : $P07_ANALYTIC_DIR   (default: ../codebase-P07/data/analytic)
#           $P07_KEY_NUMBERS    (default: outputs/key_numbers.csv)
#           $P07_P01_DEMAND_RDS (default: ../codebase-P01/data/analytic/01_demand_tracts.rds)
# Writes  : data/derived/{tract_results.csv, county_results.csv, fdr_path.csv,
#           fdx_path.csv, sensitivity_gamma.csv, sensitivity_A.csv,
#           sensitivity_county_weight.csv, triangulation.csv,
#           al_tracts_2020_simplified.geojson, key_numbers.csv,
#           tract_results_dictionary.csv}
# Run     : Rscript scripts/build_derived.R    (from the package root)
# Author  : JoonHo Lee (jlee296@ua.edu)
# License : MIT (code); see LICENSE and data/DATA_ACCESS.md
# =============================================================================

options(stringsAsFactors = FALSE)
suppressWarnings(suppressMessages({
  library(sf)
  library(dplyr)
}))

# ---- 0. Resolve inputs and outputs -----------------------------------------
# The protected analytic objects are NOT in this repository. Point the two
# environment variables below at your local copies (a completed run of the
# analysis pipeline, and the companion P01 geometry).
analytic_dir <- Sys.getenv("P07_ANALYTIC_DIR",  unset = "../codebase-P07/data/analytic")
keynum_path  <- Sys.getenv("P07_KEY_NUMBERS",   unset = "outputs/key_numbers.csv")
p01_demand   <- Sys.getenv("P07_P01_DEMAND_RDS",
                           unset = "../codebase-P01/data/analytic/01_demand_tracts.rds")
out_dir      <- "data/derived"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(
  "analytic dir not found -- set P07_ANALYTIC_DIR" = dir.exists(analytic_dir),
  "key_numbers.csv not found -- set P07_KEY_NUMBERS" = file.exists(keynum_path),
  "P01 demand geometry not found -- set P07_P01_DEMAND_RDS" = file.exists(p01_demand)
)
rd <- function(f) readRDS(file.path(analytic_dir, f))

# ---- 1. Privacy guard -------------------------------------------------------
# A belt-and-suspenders check: refuse to write any table that carries a column
# whose name looks like a provider identifier or a coordinate. The analytic
# objects are already aggregate (no such columns exist), so this should never
# fire -- but it makes the guarantee explicit and machine-enforced.
BANNED <- c("facility_id", "provider_id", "license_id", "address", "street",
            "latitude", "longitude", "lat", "lon", "lng", "mdlat", "mdlong",
            "x", "y", "geocode")
assert_no_pii_columns <- function(df, label) {
  hit <- intersect(tolower(names(df)), BANNED)
  if (length(hit)) {
    stop(sprintf("Refusing to write %s: banned column(s) present: %s",
                 label, paste(hit, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}
write_safe <- function(df, filename) {
  assert_no_pii_columns(df, filename)
  utils::write.csv(df, file.path(out_dir, filename), row.names = FALSE, na = "")
  cat(sprintf("  wrote %-34s (%d rows)\n", filename, nrow(df)))
}

# ---- 2. Read the protected analytic objects --------------------------------
comp <- rd("03_p01_comparison.rds")          # per-tract results + P01 comparison
decl <- rd("03_fdr_declarations.rds")        # county results + selection paths
sens <- rd("03_triangulation_sensitivity.rds")
dem  <- rd("01_demand_uncertainty.rds")      # public ACS demand frame (1,436)
geo  <- readRDS(p01_demand)                  # sf: public 2020 tract geometry

# Sanity checks against the published universes before we reduce anything.
stopifnot(
  "comparison tracts != 1,409" = nrow(comp$tracts) == 1409L,
  "demand frame != 1,436"      = nrow(dem$tracts)  == 1436L,
  "geometry != 1,436"          = nrow(geo)         == 1436L,
  "counties != 67"             = nrow(decl$counties) == 67L,
  "FDR total != 512"           = sum(comp$tracts$fdr_desert) == 512L,
  "FDX total != 412"           = sum(comp$tracts$fdx_desert) == 412L,
  "counties declared != 13"    = sum(decl$counties$county_fdr_desert) == 13L
)

# ---- 3. Tract results (1,436 rows; results blanked for suppressed tracts) ---
# Public demand frame: GEOID, county, the ACS under-5 estimate and its MOE, the
# ACS coefficient of variation, and the eligibility / zero-demand flags. All of
# these are public Census products and are never suppressed.
frame <- dem$tracts[, c("GEOID", "county_fips", "county_name",
                        "demand_mean", "demand_moe90", "cv_under5",
                        "high_cv_flag", "is_zero_demand", "eligible")]

# The paper's display rule: suppress a tract if it has zero demand or fewer than
# 10 children under five. This yields the 46 suppressed tracts (27 zero-demand +
# 19 below the 10-child floor) referenced throughout the manuscript.
frame$display_allowed <- !frame$is_zero_demand & frame$demand_mean >= 10
stopifnot(
  "display-suppressed tracts != 46" = sum(!frame$display_allowed) == 46L,
  "displayable tracts != 1,390"     = sum(frame$display_allowed)  == 1390L
)

# Model results (defined only on the 1,409 nonzero-demand tracts).
model <- comp$tracts[, c("GEOID", "input_rate",
                         "coverage_median", "coverage_q025", "coverage_q975",
                         "width95", "relative_width95",
                         "desert_probability", "LIS",
                         "point_desert_e2sfca", "fdr_desert", "fdx_desert",
                         "fdr_rank", "comparison_class")]
tract <- dplyr::left_join(frame, model, by = "GEOID")

# Blank every result column for the 46 display-suppressed tracts. The public
# demand columns and the flags (is_zero_demand, eligible, display_allowed) are
# retained so the row is still accounted for; only the result is withheld.
result_cols <- c("input_rate", "coverage_median", "coverage_q025",
                 "coverage_q975", "width95", "relative_width95",
                 "desert_probability", "LIS", "point_desert_e2sfca",
                 "fdr_desert", "fdx_desert", "fdr_rank", "comparison_class")
tract[!tract$display_allowed, result_cols] <- NA
tract <- tract[order(tract$GEOID), ]
write_safe(tract, "tract_results.csv")

# ---- 4. County results (aggregate; safe to ship in full) -------------------
county <- decl$counties[, c("county_fips", "county_name", "n_tracts",
                            "total_weight", "county_lis",
                            "county_desert_probability",
                            "county_aggregate_desert_probability",
                            "county_fdr_desert", "coverage_median",
                            "fcr_miscoverage", "fcr_lower", "fcr_upper")]
county <- county[order(county$county_fips), ]
write_safe(county, "county_results.csv")

# ---- 5. Selection paths (anonymous ranked sequences; Figure 5) -------------
write_safe(decl$fdr_path, "fdr_path.csv")    # rank, LIS, cumulative_mean_LIS
write_safe(decl$fdx_path, "fdx_path.csv")    # rank, posterior_exceedance_probability

# ---- 6. Preregistered sensitivity grids and triangulation ------------------
write_safe(sens$gamma_sensitivity,         "sensitivity_gamma.csv")
write_safe(sens$A_sensitivity,             "sensitivity_A.csv")
write_safe(sens$county_weight_sensitivity, "sensitivity_county_weight.csv")
write_safe(sens$triangulation,             "triangulation.csv")

# ---- 7. Public simplified tract geometry (GeoJSON, EPSG:4326) --------------
# Ship a light, public polygon layer the maps can draw. It carries only public
# ACS fields and the display flag -- no result columns (those join in from
# tract_results.csv on GEOID).
geo_attr <- frame[, c("GEOID", "county_fips", "county_name",
                      "demand_mean", "is_zero_demand", "display_allowed")]
names(geo_attr)[names(geo_attr) == "demand_mean"] <- "under5_acs_estimate"
geom <- geo[, "GEOID"] |>
  dplyr::left_join(geo_attr, by = "GEOID") |>
  sf::st_transform(4326) |>
  sf::st_simplify(dTolerance = 0.001, preserveTopology = TRUE)
assert_no_pii_columns(sf::st_drop_geometry(geom), "al_tracts_2020_simplified.geojson")
geojson_path <- file.path(out_dir, "al_tracts_2020_simplified.geojson")
if (file.exists(geojson_path)) unlink(geojson_path)
sf::st_write(geom, geojson_path, quiet = TRUE)
cat(sprintf("  wrote %-34s (%d polygons, %.2f MB)\n",
            "al_tracts_2020_simplified.geojson", nrow(geom),
            file.info(geojson_path)$size / 1024^2))

# ---- 8. Copy the scalar registry (SSOT) ------------------------------------
file.copy(keynum_path, file.path(out_dir, "key_numbers.csv"), overwrite = TRUE)
cat("  copied key_numbers.csv (scalar registry)\n")

# ---- 9. Column dictionary for tract_results.csv ----------------------------
dictionary <- data.frame(
  column = c("GEOID", "county_fips", "county_name", "demand_mean",
             "demand_moe90", "cv_under5", "high_cv_flag", "is_zero_demand",
             "eligible", "display_allowed", "input_rate", "coverage_median",
             "coverage_q025", "coverage_q975", "width95", "relative_width95",
             "desert_probability", "LIS", "point_desert_e2sfca", "fdr_desert",
             "fdx_desert", "fdr_rank", "comparison_class"),
  description = c(
    "11-digit 2020 Census tract GEOID.",
    "5-digit county FIPS code.",
    "County name.",
    "ACS 2023 5-year estimate of children under five (public).",
    "ACS 2023 5-year 90% margin of error for the under-five count (public).",
    "ACS coefficient of variation for the under-five count (public).",
    "TRUE if the demand coefficient of variation exceeds 0.40.",
    "TRUE if the tract has a zero ACS under-five estimate (excluded from the model).",
    "TRUE if the tract enters the 1,409-tract analysis universe.",
    "TRUE if the tract may be shown (>=10 children and nonzero demand); result columns are NA when FALSE.",
    "Fixed E2SFCA accessible slots per child (survey-calibrated expectation).",
    "Posterior median coverage (slots per child).",
    "Posterior 2.5% quantile of coverage.",
    "Posterior 97.5% quantile of coverage.",
    "Width of the 95% posterior coverage interval.",
    "95% interval width divided by the posterior median coverage.",
    "Posterior probability that coverage < 0.33 (the desert probability).",
    "Local index of significance, 1 - desert_probability.",
    "TRUE if the tract is a deterministic (P01) E2SFCA desert.",
    "TRUE if the tract is declared under the FDR q=0.10 step-up.",
    "TRUE if the tract is in the FDX c=0.10, alpha=0.05 headline core.",
    "Rank of the tract in the FDR selection path (1 = strongest evidence).",
    "Cross-classification vs the deterministic map: both_point_and_fdr / point_only / fdr_only / neither."
  ),
  stringsAsFactors = FALSE
)
write_safe(dictionary, "tract_results_dictionary.csv")

cat("\nbuild_derived.R: data/derived/ is complete and disclosure-safe.\n")
