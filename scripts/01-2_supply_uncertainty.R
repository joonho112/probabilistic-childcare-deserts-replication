#!/usr/bin/env Rscript

# ============================================================================
# scripts/01-2_supply_uncertainty.R -- P07 Step 1.2: capacity uncertainty.
# ----------------------------------------------------------------------------
# Purpose
#   Build the second propagated input uncertainty -- measurement error in usable
#   licensed capacity -- and rebuild the fixed E2SFCA allocation operator so the
#   rest of the pipeline treats the access geometry as constant.
#
# Method
#   The enhanced two-step floating catchment area is reconstructed as a sparse
#   linear operator from the P01 origin-destination matrix and verified to
#   reproduce P01's E2SFCA scores to < 1e-12. The four aggregate quarterly
#   surveys identify only sector-level quantities, so capacity is modeled
#   hierarchically: licensed x sector-mean-multiplier x shared-sector-temporal
#   lognormal x provider-residual lognormal (mean-one, preregistered CV = 0.15).
#   Pushing 2,000 capacity draws through the fixed operator yields tract coverage
#   rates that carry the capacity uncertainty.
#
# Reads   the Step 1.1 demand object; P01 01_supply_providers.rds,
#         01_od_matrix.rds, 02_e2sfca.rds (all restricted / read-only);
#         provenance/survey_calibration.csv.
# Writes  data/analytic/01_supply_uncertainty.rds;
#         outputs/tables/01_supply_uncertainty_summary.csv; SSOT rows.
# ============================================================================

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_uncertainty.R"))

set.seed(20260715)
demand_bundle <- readRDS(p07_path("data", "analytic", "01_demand_uncertainty.rds"))
demand <- demand_bundle$tracts
supply_sf <- read_p01("01_supply_providers.rds")
supply <- sf::st_drop_geometry(supply_sf)
od <- read_p01("01_od_matrix.rds")
e2sfca <- read_p01("02_e2sfca.rds")
survey <- utils::read.csv(p07_path("provenance", "survey_calibration.csv"), stringsAsFactors = FALSE)

assert_that(sum(supply$day_capacity) == 117062, "Licensed day-capacity center failed.")
assert_that(all(c("facility_id", "facility_type", "day_capacity") %in% names(supply)), "Supply fields are incomplete.")
assert_that(nrow(survey) == 8L && all(c("Centers", "Homes") %in% survey$sector), "Survey calibration is incomplete.")

fixed <- build_fixed_e2sfca_operator(
  od = od,
  demand_geoid = demand$GEOID,
  demand_count = demand$demand_mean,
  provider_id = supply$facility_id
)
licensed_reproduction <- as.numeric(fixed$operator %*% supply$day_capacity)
assert_that(max(abs(licensed_reproduction - e2sfca$e2sfca)) < 1e-12, "Fixed allocation operator does not reproduce P01 E2SFCA.")

survey_summary <- survey |>
  dplyr::group_by(.data$sector) |>
  dplyr::summarise(
    mean_multiplier = mean(.data$ability_to_licensed),
    sd_multiplier = stats::sd(.data$ability_to_licensed),
    temporal_cv = .data$sd_multiplier / .data$mean_multiplier,
    mean_response_rate = mean(.data$response_rate),
    n_waves = dplyr::n(),
    .groups = "drop"
  )

# Survey grouping: family/group homes map to Homes; centers, faith-based, and
# excepted university/other programs map to Centers. This mapping is explicit
# and can be varied only in sensitivity work.
supply$survey_sector <- ifelse(
  supply$facility_type %in% c("Family Home", "Group Home"), "Homes", "Centers"
)
supply$sector_mean_multiplier <- survey_summary$mean_multiplier[match(supply$survey_sector, survey_summary$sector)]
supply$sector_temporal_cv <- survey_summary$temporal_cv[match(supply$survey_sector, survey_summary$sector)]
supply$provider_residual_cv <- 0.15

provider_parameters <- supply[c(
  "facility_id", "facility_type", "survey_sector", "day_capacity",
  "sector_mean_multiplier", "sector_temporal_cv", "provider_residual_cv"
)]
names(provider_parameters)[names(provider_parameters) == "day_capacity"] <- "licensed_capacity"

supply_object <- list(
  providers = provider_parameters,
  operator = fixed$operator,
  fixed_weighted_demand = fixed$weighted_demand,
  tract_geoid = demand$GEOID,
  licensed_rate = licensed_reproduction,
  metadata = list(
    allocation = "P01 fixed 0-5/5-10/10-15 E2SFCA weights; catchment and competition not randomized",
    survey_role = "four-wave statewide sector mean and temporal uncertainty",
    provider_residual_prior = "mean-one lognormal, CV=0.15",
    survey_mapping = "Family Home + Group Home => Homes; all other facility types => Centers",
    seed = 20260716L,
    planned_draws = 2000L,
    n_fixed_pairs = fixed$n_pairs
  )
)

# Draw 2,000 provider capacity realizations and push each through the fixed
# operator to get the tract coverage rates that carry the capacity uncertainty.
rate_draws <- draw_capacity_rates(supply_object, n_draws = 2000L, seed = 20260716L)
tract_summary <- data.frame(
  GEOID = rownames(rate_draws),
  licensed_rate = licensed_reproduction,
  capacity_rate_mean = rowMeans(rate_draws),
  capacity_rate_sd = apply(rate_draws, 1, stats::sd),
  capacity_rate_q05 = apply(rate_draws, 1, stats::quantile, probs = 0.05, names = FALSE),
  capacity_rate_q50 = apply(rate_draws, 1, stats::quantile, probs = 0.50, names = FALSE),
  capacity_rate_q95 = apply(rate_draws, 1, stats::quantile, probs = 0.95, names = FALSE),
  stringsAsFactors = FALSE
)
assert_that(nrow(tract_summary) == 1436L && !anyDuplicated(tract_summary$GEOID), "Tract supply summary universe failed.")
assert_that(all(is.finite(rate_draws)) && all(rate_draws >= 0), "Capacity rate draws are invalid.")
assert_that(all(tract_summary$capacity_rate_q05 <= tract_summary$capacity_rate_q50 & tract_summary$capacity_rate_q50 <= tract_summary$capacity_rate_q95), "Capacity intervals are unordered.")

supply_object$tract_summary <- tract_summary
supply_object$survey_summary <- survey_summary
supply_object$metadata$licensed_total <- sum(provider_parameters$licensed_capacity)
supply_object$metadata$survey_calibrated_expected_total <- sum(
  provider_parameters$licensed_capacity * provider_parameters$sector_mean_multiplier
)
save_analytic(supply_object, "01_supply_uncertainty.rds")

sector_table <- provider_parameters |>
  dplyr::group_by(.data$survey_sector) |>
  dplyr::summarise(
    providers = dplyr::n(),
    licensed_slots = sum(.data$licensed_capacity),
    survey_mean_multiplier = dplyr::first(.data$sector_mean_multiplier),
    survey_temporal_cv = dplyr::first(.data$sector_temporal_cv),
    provider_residual_cv = dplyr::first(.data$provider_residual_cv),
    expected_effective_slots = sum(.data$licensed_capacity * .data$sector_mean_multiplier),
    .groups = "drop"
  )
utils::write.csv(sector_table, p07_path("outputs", "tables", "01_supply_uncertainty_summary.csv"), row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c(
    "total_day_slots", "capacity_moe_model", "capacity_cv_assumed",
    "capacity_survey_center_multiplier", "capacity_survey_home_multiplier",
    "expected_effective_day_slots"
  ),
  value = c(
    117062, "survey-sector mean + shared temporal + provider lognormal",
    0.15,
    survey_summary$mean_multiplier[survey_summary$sector == "Centers"],
    survey_summary$mean_multiplier[survey_summary$sector == "Homes"],
    round(supply_object$metadata$survey_calibrated_expected_total, 6)
  ),
  unit = c("licensed day slots", "model", "provider residual CV", "multiplier", "multiplier", "expected effective day slots"),
  source_script = "scripts/01-2_supply_uncertainty.R",
  note = c(
    "Canonical P01 licensed baseline; exact fixed-operator center reproduced.",
    "Four aggregate survey waves inform sector means and shared temporal variation; provider IDs are unavailable.",
    "Preregistered residual measurement-error prior, held fixed after Step 1.2.",
    "Mean of four statewide ability/licensed ratios.",
    "Mean of four statewide ability/licensed ratios.",
    "Survey-calibrated expectation; not a replacement for the 117,062 licensed baseline."
  ), stringsAsFactors = FALSE
))

cat("Step 1.2 supply uncertainty: PASS\n")
