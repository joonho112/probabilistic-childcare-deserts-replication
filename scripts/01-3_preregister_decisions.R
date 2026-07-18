#!/usr/bin/env Rscript

# ============================================================================
# scripts/01-3_preregister_decisions.R -- P07 Step 1.3: preregistration.
# ----------------------------------------------------------------------------
# Purpose
#   Freeze the primary analytic specification BEFORE any model result is seen,
#   and record the labeled sensitivity grids that may probe robustness but never
#   replace the primary. This is what makes the desert declarations confirmatory
#   rather than chosen after seeing the answer.
#
# Method
#   The primary choices -- adequacy standard A = 0.33, desert buffer gamma = 0,
#   FDR target q = 0.10, FDX c = 0.10 and alpha = 0.05, county weight = under-five
#   population, 2,000 draws, and the desert rule "desert iff rho < A - gamma" --
#   are written to a choice table that is then SHA-256 hashed. Every downstream
#   script re-checks that hash, so the specification cannot drift. Two coverage
#   anchors (licensed slots per child ~ 0.398; child-weighted fixed E2SFCA
#   ~ 0.394) are re-verified here as a guard on the fixed access geometry.
#
# Reads   the Step 1.1 demand and Step 1.2 supply objects; P01 02_e2sfca.rds.
# Writes  outputs/tables/PREREG_choices.csv (then hashes it);
#         data/analytic/01_prereg_region.rds; SSOT rows.
# ============================================================================

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))

demand <- readRDS(p07_path("data", "analytic", "01_demand_uncertainty.rds"))
supply <- readRDS(p07_path("data", "analytic", "01_supply_uncertainty.rds"))
e2sfca <- read_p01("02_e2sfca.rds")

primary <- list(
  coverage_estimand = "design-based areal accessible slots per under-five child",
  threshold_A = 0.33,
  gamma = 0,
  fdr_q = 0.10,
  fdx_c = 0.10,
  fdx_alpha = 0.05,
  county_weight = "under5",
  posterior_draws = 2000L,
  demand_distribution = "eligible Normal using MOE/1.645 truncated below at 1 child; zero-demand point mass at 0",
  capacity_distribution = "survey-sector mean + shared temporal lognormal + provider residual lognormal CV=0.15",
  allocation_design = "fixed P01 E2SFCA 0-5/5-10/10-15 weights 1/0.68/0.22"
)
sensitivity <- list(
  gamma_grid = c(0, 0.03, 0.05),
  threshold_A_grid = c(0.25, 0.33, 0.50),
  county_weight_grid = c("under5", "equal", "area")
)

assert_that(identical(primary$threshold_A, 0.33), "Primary A must equal the P01 rule.")
assert_that(identical(primary$gamma, 0), "Primary gamma must be zero.")
assert_that(identical(primary$fdr_q, 0.10), "Primary FDR q must be 0.10.")
assert_that(identical(primary$county_weight, "under5"), "Primary county weight must be under-five population.")
assert_that(primary$gamma %in% sensitivity$gamma_grid, "Primary gamma is absent from its sensitivity grid.")
assert_that(primary$threshold_A %in% sensitivity$threshold_A_grid, "Primary A is absent from its sensitivity grid.")
assert_that(primary$county_weight %in% sensitivity$county_weight_grid, "Primary county weight is absent from its sensitivity grid.")

tracts <- demand$tracts
licensed_state_ratio <- supply$metadata$licensed_total / sum(tracts$demand_mean)
fixed_accessible_slots <- sum(e2sfca$e2sfca * tracts$demand_mean)
fixed_accessible_ratio <- fixed_accessible_slots / sum(tracts$demand_mean)
allocation_gap_slots <- supply$metadata$licensed_total - fixed_accessible_slots
assert_that(abs(licensed_state_ratio - 0.397606) < 1e-5, "Licensed statewide slots/child anchor failed.")
assert_that(abs(fixed_accessible_ratio - 0.394193) < 1e-5, "Child-weighted fixed E2SFCA anchor failed.")

choices <- data.frame(
  choice_id = c(
    "coverage_estimand", "threshold_A", "gamma_primary", "fdr_q_primary",
    "fdx_c_primary", "fdx_alpha_primary", "county_weight_primary",
    "posterior_draws", "demand_distribution", "capacity_distribution",
    "allocation_design", "gamma_sensitivity", "A_sensitivity",
    "county_weight_sensitivity"
  ),
  value = c(
    primary$coverage_estimand, primary$threshold_A, primary$gamma,
    primary$fdr_q, primary$fdx_c, primary$fdx_alpha,
    primary$county_weight, primary$posterior_draws,
    primary$demand_distribution, primary$capacity_distribution,
    primary$allocation_design,
    paste(sensitivity$gamma_grid, collapse = ";"),
    paste(sensitivity$threshold_A_grid, collapse = ";"),
    paste(sensitivity$county_weight_grid, collapse = ";")
  ),
  unit = c(
    "definition", "slots per child", "slots per child", "posterior expected FDR",
    "FDP exceedance threshold", "tail probability", "weight", "joint draws",
    "distribution", "distribution", "fixed design", "slots per child",
    "slots per child", "weights"
  ),
  role = c(rep("primary_locked", 11), rep("sensitivity_only", 3)),
  rationale = c(
    "Design-based areal coverage; FCA functional-form uncertainty is P13.",
    "Anchored to P01's exact score < 0.33 desert rule.",
    "Exact P01 rule reproduction; alternatives are sensitivity only.",
    "Approved master-plan target.",
    "Approved conservative FDX headline threshold.",
    "Approved FDX tail probability.",
    "County declarations represent child exposure.",
    "Stable Monte Carlo resolution within local compute budget.",
    "Directly preserves ACS 90% MOE semantics.",
    "Available surveys inform sector mean/temporal variation; residual CV is preregistered.",
    "Prevents drift into P13 FCA-form uncertainty.",
    "Robustness only; cannot replace gamma=0 primary.",
    "Robustness only; cannot replace A=0.33 primary.",
    "Robustness only; cannot replace under-five weighting primary."
  ),
  locked_step = "1.3",
  stringsAsFactors = FALSE
)
prereg_path <- p07_path("outputs", "tables", "PREREG_choices.csv")
utils::write.csv(choices, prereg_path, row.names = FALSE, na = "")
# Hash the frozen choice table; every downstream script re-checks this exact
# digest, so the primary specification cannot be edited after the fact.
prereg_sha256 <- file_sha256(prereg_path)

save_analytic(
  list(
    primary = primary,
    sensitivity = sensitivity,
    coverage_anchors = list(
      licensed_slots_per_child = licensed_state_ratio,
      fixed_e2sfca_child_weighted = fixed_accessible_ratio,
      fixed_accessible_slots = fixed_accessible_slots,
      licensed_slots_not_allocated_within_fixed_design = allocation_gap_slots
    ),
    choices_sha256 = prereg_sha256,
    locked_step = "1.3",
    rule = "desert iff rho < A - gamma; equality is non-desert"
  ),
  "01_prereg_region.rds"
)

append_key_numbers(data.frame(
  key = c(
    "region_A_threshold", "gamma_primary", "fdr_q_primary", "fdx_c_primary",
    "fdx_alpha_primary", "county_weight_primary", "posterior_draws_primary",
    "licensed_slots_per_child", "fixed_e2sfca_child_weighted",
    "prereg_choices_sha256"
  ),
  value = c(
    primary$threshold_A, primary$gamma, primary$fdr_q, primary$fdx_c,
    primary$fdx_alpha, primary$county_weight, primary$posterior_draws,
    licensed_state_ratio, fixed_accessible_ratio, prereg_sha256
  ),
  unit = c(
    "slots per child", "slots per child", "posterior expected FDR",
    "FDP threshold", "tail probability", "weight", "draws",
    "licensed slots per child", "child-weighted fixed E2SFCA", "sha256"
  ),
  source_script = "scripts/01-3_preregister_decisions.R",
  note = c(
    "Locked to P01's 0.33 adequate-access boundary.",
    "Primary exactly reproduces P01 threshold; 0.03/0.05 are sensitivity only.",
    "Locked before posterior results.", "Locked before posterior results.",
    "Locked before posterior results.", "Under-five population; equal/area are sensitivity only.",
    "Joint latent/input uncertainty draws.",
    "117,062 licensed day slots divided by 294,417 children.",
    "Fixed E2SFCA allocates 116,057 accessible-slot equivalents; 1,005 licensed slots are outside the fixed reachable allocation.",
    "Hash of immutable preregistration choice table."
  ), stringsAsFactors = FALSE
))

cat("Step 1.3 preregistration: PASS\n")
