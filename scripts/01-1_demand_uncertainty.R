#!/usr/bin/env Rscript

# ============================================================================
# scripts/01-1_demand_uncertainty.R -- P07 Step 1.1: ACS demand uncertainty.
# ----------------------------------------------------------------------------
# Purpose
#   Build the first of the two propagated input uncertainties: sampling error in
#   the ACS 2023 under-five child count. Each tract's count is an estimate with a
#   published margin of error; this step turns that MOE into the sampling
#   distribution the joint posterior later draws from (Step 2.2).
#
# Method
#   The 90% margin of error is converted to a standard error, SE = MOE / 1.645,
#   and each eligible tract is given a Normal sampling distribution truncated
#   below at one child. The truncation keeps the inverse-demand multiplier
#   D_hat / D_draw finite downstream. The 27 zero-demand tracts are retained for
#   map topology but excluded from the model and the desert denominator. A small
#   fixed-seed draw audit confirms high-CV tracts get wider relative draws.
#
# Reads   P01 01_demand_tracts.rds (via the traversal-guarded reader).
# Writes  data/analytic/01_demand_uncertainty.rds;
#         outputs/tables/01_demand_uncertainty_summary.csv; SSOT rows.
# ============================================================================

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_uncertainty.R"))

set.seed(20260715)
demand <- read_p01("01_demand_tracts.rds")
required <- c(
  "GEOID", "county_fips", "county_name", "total_under5E", "total_under5M",
  "cv_under5", "high_cv_flag", "is_zero_demand"
)
assert_that(all(required %in% names(demand)), "P01 demand object lacks required columns.")
assert_that(nrow(demand) == 1436L && !anyDuplicated(demand$GEOID), "Demand universe is not canonical.")

out <- sf::st_drop_geometry(demand)[required]
names(out)[names(out) == "total_under5E"] <- "demand_mean"
names(out)[names(out) == "total_under5M"] <- "demand_moe90"
# Convert the published 90% margin of error to a standard error (SE = MOE / 1.645).
out$demand_se <- demand_se_from_moe(out$demand_moe90)
out$eligible <- !out$is_zero_demand
out$reliability_tier <- ifelse(
  out$is_zero_demand, "zero_demand_excluded",
  ifelse(out$high_cv_flag, "high_cv_gt_0.40", "cv_le_0.40")
)
out$sampling_distribution <- ifelse(
  out$eligible,
  "TruncNormal(mean=demand_mean, sd=demand_se, lower=1 child)",
  "Point mass at 0; excluded from coverage model"
)
out$moe_z <- 1.645
out$primary_propagation_candidate <- "modular_joint_posterior_MC"
out$alternate_propagation_candidate <- "uncertain_offset_in_model"

assert_that(sum(out$demand_mean) == 294417, "Demand mean total failed.")
assert_that(dplyr::n_distinct(out$demand_moe90) == 1246L, "Demand MOE distinct count failed.")
assert_that(dplyr::n_distinct(out$demand_se) == 1246L, "Demand SE distinct count failed.")
assert_that(sum(out$eligible) == 1409L && sum(out$is_zero_demand) == 27L, "Demand eligibility failed.")
assert_that(sum(out$high_cv_flag, na.rm = TRUE) == 605L, "High-CV demand count failed.")
assert_that(all(out$demand_se >= 0) && all(is.finite(out$demand_se)), "Demand SE integrity failed.")

# A compact fixed-seed draw audit confirms the generator and expected width
# ordering without persisting row-by-draw values at this step.
eligible_out <- out[out$eligible, ]
means <- stats::setNames(eligible_out$demand_mean, eligible_out$GEOID)
ses <- stats::setNames(eligible_out$demand_se, eligible_out$GEOID)
audit_draws <- draw_truncated_demand(means, ses, n_draws = 200L, seed = 20260716L, lower = 1)
audit_sd <- apply(audit_draws, 2, stats::sd)
assert_that(all(is.finite(audit_draws)) && all(audit_draws >= 0), "Truncated demand draw audit failed.")
width_by_tier <- data.frame(
  tier = eligible_out$reliability_tier,
  simulated_sd = as.numeric(audit_sd),
  simulated_cv = as.numeric(audit_sd) / eligible_out$demand_mean,
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(.data$tier) |>
  dplyr::summarise(
    n_tracts = dplyr::n(),
    median_simulated_sd = stats::median(.data$simulated_sd),
    median_simulated_cv = stats::median(.data$simulated_cv),
    .groups = "drop"
  )
assert_that(
  width_by_tier$median_simulated_cv[width_by_tier$tier == "high_cv_gt_0.40"] >
    width_by_tier$median_simulated_cv[width_by_tier$tier == "cv_le_0.40"],
  "High-CV tracts do not have wider relative demand draws."
)

metadata <- list(
  source = file.path(P01_ANALYTIC, "01_demand_tracts.rds"),
  source_sha256 = file_sha256(file.path(P01_ANALYTIC, "01_demand_tracts.rds")),
  distribution = "eligible tract-specific Normal truncated below at 1 child; zero-demand point mass at 0",
  moe_confidence = 0.90,
  moe_z = 1.645,
  zero_demand_rule = "27 zero-demand tracts retained in object and excluded from desert denominator/model",
  propagation_paths = c("modular_joint_posterior_MC", "uncertain_offset_in_model"),
  draw_seed = 20260716L,
  planned_posterior_draws = 2000L
)
save_analytic(list(tracts = out, metadata = metadata, draw_audit = width_by_tier), "01_demand_uncertainty.rds")

summary_table <- data.frame(
  group = c("all", "eligible", "high_cv", "cv_le_0.40", "zero_demand"),
  n_tracts = c(1436, 1409, 605, 804, 27),
  children = c(
    sum(out$demand_mean), sum(out$demand_mean[out$eligible]),
    sum(out$demand_mean[out$high_cv_flag %in% TRUE]),
    sum(out$demand_mean[out$high_cv_flag %in% FALSE]),
    sum(out$demand_mean[out$is_zero_demand])
  ),
  rule = c("canonical", "nonzero demand", "CV > 0.40", "CV <= 0.40", "excluded from classifications"),
  stringsAsFactors = FALSE
)
utils::write.csv(summary_table, p07_path("outputs", "tables", "01_demand_uncertainty_summary.csv"), row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c("n_high_cv_demand", "total_under5", "n_zero_demand", "demand_uncertainty_distribution"),
  value = c(605, 294417, 27, "eligible lower-truncated Normal at 1 child"),
  unit = c("tracts", "children under 5", "tracts", "distribution"),
  source_script = "scripts/01-1_demand_uncertainty.R",
  note = c(
    "P01 CV > 0.40 flag among nonzero-demand tracts.",
    "Canonical ACS 2023 under-five estimate.",
    "Retained for map topology but excluded from model and desert denominator.",
    "SE equals 90% MOE / 1.645; eligible positive-demand counts are truncated at one child to keep inverse coverage moments finite."
  ), stringsAsFactors = FALSE
))

cat("Step 1.1 demand uncertainty: PASS\n")
