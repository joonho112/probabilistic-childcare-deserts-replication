#!/usr/bin/env Rscript

# P07 Step 3.3 -- Preregistered sensitivity grids and P01 always-desert triangulation.
#
# Purpose:
#   Stress-tests the headline declarations against the analytic choices a
#   skeptical reader would question -- the buffer gamma, the coverage threshold
#   A, and the county weighting scheme -- and triangulates against an external
#   anchor. Answers the "arbitrary threshold" kill-risk without letting any
#   alternative displace the locked primary specification.
#
# Method:
#   Re-runs the LIS step-up over three preregistered grids WITHOUT changing the
#   primary: gamma in {0, 0.03, 0.05} -> 512/431/377 FDR tracts; A in
#   {0.25, 0.33, 0.50} -> 296/512/885 FDR tracts; county weights
#   {under5, equal, area} -> 13/9/19 counties. Each grid re-asserts that its
#   primary cell reproduces the Step 3.1 result. Triangulation against the P01
#   519-tract always-desert (E2SFCA < 0.20) anchor: 441 (85.0%) fall in the FDR
#   set and 377 (72.6%) in the FDX core -- an INTERSECTION, not a nesting (the
#   anchor is not a subset of either declared set). Also emits a prose kill-risk
#   paragraph with every number interpolated live from the objects.
#
# Reads:
#   data/analytic/03_fdr_declarations.rds          (primary declarations)
#   data/analytic/03_p01_comparison.rds            (P01 comparison object)
#   data/interim/02_coverage_posterior_draws.rds   (draws for grid recomputation)
#   data/analytic/01_prereg_region.rds             (locked primary + sensitivity grids)
#   <P01>/04_stability.rds                          (always-desert stability class)
#   <P01>/01_demand_tracts.rds                      (tract land area for area weights)
#   <P01>/outputs/key_numbers.csv                   (anchor: 519 always-desert tracts)
#
# Writes:
#   data/analytic/03_triangulation_sensitivity.rds   (triangulation + all three grids)
#   outputs/tables/03_triangulation.csv
#   outputs/tables/03_gamma_sensitivity.csv
#   outputs/tables/03_A_sensitivity.csv
#   outputs/tables/03_county_weight_sensitivity.csv
#   manuscript/03_kill_risk_response.md              (prose robustness paragraph)
#   outputs/key_numbers.csv                          (appended single-source-of-truth numbers)

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_fdr.R"))

declarations <- readRDS(p07_path("data", "analytic", "03_fdr_declarations.rds"))
comparison <- readRDS(p07_path("data", "analytic", "03_p01_comparison.rds"))
draws <- readRDS(p07_path("data", "interim", "02_coverage_posterior_draws.rds"))
prereg <- readRDS(p07_path("data", "analytic", "01_prereg_region.rds"))
stability <- read_p01("04_stability.rds")
demand_sf <- read_p01("01_demand_tracts.rds")
p01_ssot <- read_p01("key_numbers.csv", "outputs")

tracts <- declarations$tracts
assert_that(identical(tracts$GEOID, draws$GEOID), "Sensitivity draw/tract order mismatch.")
assert_that(identical(prereg$choices_sha256, file_sha256(p07_path("outputs", "tables", "PREREG_choices.csv"))), "Preregistration hash changed.")

# Anchor triangulation to P01's published 519-tract always-desert count.
p01_always <- as.numeric(p01_ssot$value[p01_ssot$key == "stability_always_desert_tracts"])
assert_that(length(p01_always) == 1L && p01_always == 519L, "P01 always-desert SSOT anchor failed.")
stability_table <- stability$tract_table
# The tract IDs classified always-desert across P01's stability grid (E2SFCA < 0.20).
always_ids <- stability_table$GEOID[stability_table$stability_class == "always_desert"]
assert_that(length(always_ids) == p01_always && !anyDuplicated(always_ids), "Always-desert ID set failed.")

tracts$always_desert_p01 <- tracts$GEOID %in% always_ids
# Overlap of the 519 anchor with the P07 FDR and FDX sets (an intersection, not a subset).
n_always_fdr <- sum(tracts$always_desert_p01 & tracts$fdr_desert)
n_always_fdx <- sum(tracts$always_desert_p01 & tracts$fdx_desert)
triangulation <- data.frame(
  anchor = "P01 always-desert (E2SFCA <0.20)",
  n_anchor = p01_always,
  n_in_fdr = n_always_fdr,
  pct_in_fdr = 100 * n_always_fdr / p01_always,
  n_in_fdx = n_always_fdx,
  pct_in_fdx = 100 * n_always_fdx / p01_always,
  # Declared tracts OUTSIDE the anchor -- direct evidence the sets are not nested.
  n_fdr_outside_anchor = sum(tracts$fdr_desert & !tracts$always_desert_p01),
  n_fdx_outside_anchor = sum(tracts$fdx_desert & !tracts$always_desert_p01),
  stringsAsFactors = FALSE
)

q <- prereg$primary$fdr_q
gamma_grid <- prereg$sensitivity$gamma_grid
A_primary <- prereg$primary$threshold_A
# gamma grid: for each buffer recompute desert probabilities at the shifted
# boundary A - gamma, re-run the LIS step-up, and tag whether it is the primary.
gamma_rows <- lapply(gamma_grid, function(gamma) {
  p <- rowMeans(draws$rho_draws < (A_primary - gamma))
  step <- lis_step_up(1 - p, q)
  data.frame(
    scenario = paste0("gamma_", format(gamma, trim = TRUE)),
    A = A_primary, gamma = gamma, threshold = A_primary - gamma,
    n_probability_gt_0.5 = sum(p > 0.5),
    n_fdr = step$k,
    achieved_mean_lis = step$achieved_mean_lis,
    role = ifelse(gamma == prereg$primary$gamma, "primary_locked", "sensitivity_only"),
    stringsAsFactors = FALSE
  )
})
gamma_sensitivity <- do.call(rbind, gamma_rows)
row.names(gamma_sensitivity) <- NULL
# The gamma = 0 cell must reproduce the Step 3.1 primary FDR count exactly.
assert_that(gamma_sensitivity$n_fdr[gamma_sensitivity$gamma == 0] == sum(tracts$fdr_desert), "Gamma primary does not reproduce FDR result.")

A_grid <- prereg$sensitivity$threshold_A_grid
# A grid: the same recomputation sweeping the coverage threshold with gamma fixed at 0.
A_rows <- lapply(A_grid, function(A_value) {
  p <- rowMeans(draws$rho_draws < A_value)
  step <- lis_step_up(1 - p, q)
  data.frame(
    scenario = paste0("A_", format(A_value, trim = TRUE)),
    A = A_value, gamma = 0, threshold = A_value,
    n_probability_gt_0.5 = sum(p > 0.5),
    n_fdr = step$k,
    achieved_mean_lis = step$achieved_mean_lis,
    role = ifelse(A_value == A_primary, "primary_locked", "sensitivity_only"),
    stringsAsFactors = FALSE
  )
})
A_sensitivity <- do.call(rbind, A_rows)
row.names(A_sensitivity) <- NULL
assert_that(A_sensitivity$n_fdr[A_sensitivity$A == A_primary] == sum(tracts$fdr_desert), "A primary does not reproduce FDR result.")

demand <- sf::st_drop_geometry(demand_sf)
demand_match <- match(tracts$GEOID, demand$GEOID)
assert_that(!anyNA(demand_match), "Demand area join failed.")
# Three county weighting schemes: under-five population (primary), equal per tract,
# and land area -- the estimand choice the county declaration is most sensitive to.
weights_list <- list(
  under5 = tracts$demand_mean,
  equal = rep(1, nrow(tracts)),
  area = demand$area_sqkm[demand_match]
)
county_sets <- list()
county_rows <- list()
primary_county_set <- declarations$counties$county_fips[declarations$counties$county_fdr_desert]
# For each scheme, re-aggregate county LIS, re-select, and compare to the primary
# county set (overlap, additions, removals, Jaccard).
for (weight_name in names(weights_list)) {
  county_table <- aggregate_county_lis(tracts, weight = weights_list[[weight_name]])
  county_step <- lis_step_up(county_table$county_lis, q)
  county_table$declared <- county_step$selected
  county_table$weight_scheme <- weight_name
  selected <- county_table$county_fips[county_table$declared]
  county_sets[[weight_name]] <- selected
  county_rows[[weight_name]] <- data.frame(
    weight_scheme = weight_name,
    n_declared = length(selected),
    overlap_with_primary = length(intersect(selected, primary_county_set)),
    additions_vs_primary = length(setdiff(selected, primary_county_set)),
    removals_vs_primary = length(setdiff(primary_county_set, selected)),
    jaccard_vs_primary = length(intersect(selected, primary_county_set)) / length(union(selected, primary_county_set)),
    achieved_mean_lis = county_step$achieved_mean_lis,
    role = ifelse(weight_name == "under5", "primary_locked", "sensitivity_only"),
    stringsAsFactors = FALSE
  )
}
county_weight_sensitivity <- do.call(rbind, county_rows)
row.names(county_weight_sensitivity) <- NULL
# The under-five scheme must reproduce the locked primary county selection.
assert_that(setequal(county_sets$under5, primary_county_set), "Under-five county sensitivity does not reproduce primary.")

save_analytic(
  list(
    tracts = tracts[c("GEOID", "always_desert_p01", "fdr_desert", "fdx_desert", "desert_probability")],
    triangulation = triangulation,
    gamma_sensitivity = gamma_sensitivity,
    A_sensitivity = A_sensitivity,
    county_weight_sensitivity = county_weight_sensitivity,
    county_sets = county_sets,
    metadata = list(
      primary_unchanged = TRUE,
      primary = list(A = A_primary, gamma = prereg$primary$gamma, q = q, county_weight = prereg$primary$county_weight),
      prereg_sha256 = prereg$choices_sha256,
      interpretation = "sensitivity results are adjacent robustness checks and cannot replace primary"
    )
  ),
  "03_triangulation_sensitivity.rds"
)
utils::write.csv(triangulation, p07_path("outputs", "tables", "03_triangulation.csv"), row.names = FALSE, na = "")
utils::write.csv(gamma_sensitivity, p07_path("outputs", "tables", "03_gamma_sensitivity.csv"), row.names = FALSE, na = "")
utils::write.csv(A_sensitivity, p07_path("outputs", "tables", "03_A_sensitivity.csv"), row.names = FALSE, na = "")
utils::write.csv(county_weight_sensitivity, p07_path("outputs", "tables", "03_county_weight_sensitivity.csv"), row.names = FALSE, na = "")

# Assemble the manuscript robustness paragraph, interpolating every number live from
# the objects above so the prose can never drift from the computed results.
kill_risk_text <- c(
  "# Kill-risk response: arbitrariness and robustness",
  "",
  sprintf(
    paste0(
      "We anchored adequate access at 0.33 slots per child, matching the prior P01 decision rule, and fixed the primary buffer (`gamma = 0`), tract FDR target (`q = 0.10`), FDX target (`c = 0.10`, `alpha = 0.05`), and county weights (under-five population) before calculating posterior desert probabilities. ",
      "The primary LIS step-up declared %d tracts. Treating stricter buffers as sensitivity analyses reduced the declarations to %d at `gamma = 0.03` and %d at `gamma = 0.05`; changing A produced %d declarations at A=0.25 and %d at A=0.50. ",
      "County declarations were more sensitive to the estimand: %d under child weights, %d under equal tract weights, and %d under area weights. We therefore retain child weighting as the policy-relevant exposure estimand rather than selecting the weighting scheme that yields a preferred count. ",
      "As a separate triangulation, %d (%.1f%%) of P01's 519 always-desert tracts were in the FDR set and %d (%.1f%%) were in the more conservative FDX set. These checks do not eliminate judgment in the 0.33 policy boundary, but they make that judgment explicit, prevent result-driven threshold substitution, and show how conclusions move under the alternatives specified in advance."
    ),
    sum(tracts$fdr_desert),
    gamma_sensitivity$n_fdr[gamma_sensitivity$gamma == 0.03],
    gamma_sensitivity$n_fdr[gamma_sensitivity$gamma == 0.05],
    A_sensitivity$n_fdr[A_sensitivity$A == 0.25],
    A_sensitivity$n_fdr[A_sensitivity$A == 0.50],
    length(county_sets$under5), length(county_sets$equal), length(county_sets$area),
    n_always_fdr, triangulation$pct_in_fdr,
    n_always_fdx, triangulation$pct_in_fdx
  )
)
writeLines(kill_risk_text, p07_path("manuscript", "03_kill_risk_response.md"))

append_key_numbers(data.frame(
  key = c(
    "n_certain519_in_fdr", "pct_certain519_in_fdr", "n_certain519_in_fdx", "pct_certain519_in_fdx",
    "fdr_count_gamma03", "fdr_count_gamma05", "fdr_count_A025", "fdr_count_A050",
    "county_count_equal", "county_count_area"
  ),
  value = c(
    n_always_fdr, triangulation$pct_in_fdr, n_always_fdx, triangulation$pct_in_fdx,
    gamma_sensitivity$n_fdr[gamma_sensitivity$gamma == 0.03],
    gamma_sensitivity$n_fdr[gamma_sensitivity$gamma == 0.05],
    A_sensitivity$n_fdr[A_sensitivity$A == 0.25],
    A_sensitivity$n_fdr[A_sensitivity$A == 0.50],
    county_weight_sensitivity$n_declared[county_weight_sensitivity$weight_scheme == "equal"],
    county_weight_sensitivity$n_declared[county_weight_sensitivity$weight_scheme == "area"]
  ),
  unit = c("tracts", "percent", "tracts", "percent", "tracts", "tracts", "tracts", "tracts", "counties", "counties"),
  source_script = "scripts/03-3_sensitivity.R",
  note = c(
    "Intersection of P01 always-desert 519 and P07 FDR set.",
    "Percent of the P01 always-desert 519 in the P07 FDR set.",
    "Intersection of P01 always-desert 519 and P07 FDX headline set.",
    "Percent of the P01 always-desert 519 in the P07 FDX headline set.",
    "Sensitivity only at strict threshold 0.30; primary gamma remains 0.",
    "Sensitivity only at strict threshold 0.28; primary gamma remains 0.",
    "Sensitivity-only FDR declarations at A=0.25.",
    "Sensitivity-only FDR declarations at A=0.50.",
    "Sensitivity-only county declarations with equal tract weights.",
    "Sensitivity-only county declarations with land-area weights."
  ), stringsAsFactors = FALSE
))

cat("Step 3.3 triangulation and sensitivity: PASS\n")
