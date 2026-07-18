#!/usr/bin/env Rscript

# P07 Step 3.1 -- Desert DECLARATIONS: FDR, FDX, and child-weighted county selection.
#
# Purpose:
#   Turns per-tract desert probabilities into formal declarations that control
#   error at pre-set rates. This is where P07 moves from "how probable" to
#   "which tracts and counties we call deserts" -- the headline policy output.
#
# Method:
#   Three preregistered procedures over the LIS values (LIS = 1 - p):
#   1. LIS step-up FDR (Sun & Cai 2015): sort LIS ascending, declare the longest
#      prefix whose running mean LIS <= q. Controls the posterior expected false
#      discovery rate at q = 0.10 -> 512 tracts (achieved mean LIS 0.099991).
#   2. Posterior FDX (Genovese & Wasserman 2004; Lehmann & Romano 2005): within
#      the FDR prefix, the longest prefix whose joint-posterior empirical
#      P(FDP > c) <= alpha. Controls false-discovery EXCEEDANCE at c = 0.10,
#      alpha = 0.05 -> a 412-tract conservative core (tail 0.0495).
#   3. County selection: aggregate tract LIS with under-five population weights,
#      apply the same q step-up across 67 counties -> 13 counties, and attach
#      FCR-style posterior coverage intervals (Benjamini & Yekutieli 2005) at
#      miscoverage q*R/m on the child-weighted county coverage draws.
#   The nesting FDX subset FDR subset {p > 0.5} is asserted.
#
# Reads:
#   data/analytic/02_desert_probability.rds        (tract LIS + probabilities)
#   data/interim/02_coverage_posterior_draws.rds   (full draw matrix for FDX + intervals)
#   data/analytic/01_prereg_region.rds             (locked q, c, alpha, A, gamma)
#
# Writes:
#   data/analytic/03_fdr_declarations.rds        (tracts + counties + FDR/FDX paths)
#   outputs/tables/03_fdr_declaration_summary.csv
#   outputs/tables/03_county_declarations.csv
#   outputs/key_numbers.csv                      (appended single-source-of-truth numbers)
#
# References:
#   Sun & Cai (2015); Genovese & Wasserman (2004); Lehmann & Romano (2005);
#   Benjamini & Yekutieli (2005, false coverage-statement rate).

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_fdr.R"))

probability <- readRDS(p07_path("data", "analytic", "02_desert_probability.rds"))
draws <- readRDS(p07_path("data", "interim", "02_coverage_posterior_draws.rds"))
prereg <- readRDS(p07_path("data", "analytic", "01_prereg_region.rds"))
tracts <- probability$tracts

assert_that(identical(tracts$GEOID, draws$GEOID), "Probability/draw tract order mismatch.")
assert_that(identical(prereg$choices_sha256, file_sha256(p07_path("outputs", "tables", "PREREG_choices.csv"))), "Preregistration hash changed.")
q <- prereg$primary$fdr_q
c_fdx <- prereg$primary$fdx_c
alpha_fdx <- prereg$primary$fdx_alpha
A <- prereg$primary$threshold_A
gamma <- prereg$primary$gamma
# Rebuild the per-draw desert indicator (tract-by-draw) that the FDX exceedance
# calculation needs; the point probabilities alone cannot express P(FDP > c).
desert_indicator <- draws$rho_draws < (A - gamma)

# Procedure 1 -- LIS step-up FDR: declare the longest ascending-LIS prefix whose
# running mean LIS <= q, controlling posterior expected FDR at q = 0.10.
fdr <- lis_step_up(tracts$LIS, q = q)
# Procedure 2 -- posterior FDX: within the FDR prefix (max_k = fdr$k), the longest
# prefix whose joint-posterior empirical P(FDP > c) <= alpha (exceedance control).
fdx <- posterior_fdx_step_up(
  tracts$LIS, desert_indicator,
  c = c_fdx, alpha = alpha_fdx, max_k = fdr$k
)
tracts$fdr_desert <- fdr$selected
tracts$fdx_desert <- fdx$selected
tracts$fdr_rank <- rank(tracts$LIS, ties.method = "first")

assert_that(fdr$k > 0L && fdr$achieved_mean_lis <= q + 1e-12, "FDR step-up boundary failed.")
if (fdr$k < nrow(tracts)) assert_that(fdr$next_mean_lis > q, "FDR did not select the maximal valid prefix.")
# Enforce the nesting FDX subset of FDR.
assert_that(fdx$k <= fdr$k && all(!tracts$fdx_desert | tracts$fdr_desert), "FDX is not nested in FDR.")
assert_that(fdx$achieved_tail_probability <= alpha_fdx + 1 / ncol(desert_indicator), "FDX posterior exceedance target failed.")
# ...and FDR subset of the p > 0.5 probability screen.
assert_that(all(!tracts$fdr_desert | tracts$desert_probability > 0.5), "FDR set is not nested in p>0.5 screen.")

# Procedure 3: aggregate tract LIS with the preregistered under-five exposure
# weights, then apply the same q step-up across 67 counties.
county <- aggregate_county_lis(tracts, weight = tracts$demand_mean)
county$county_desert_probability <- 1 - county$county_lis
county_fdr <- lis_step_up(county$county_lis, q = q)
county$county_fdr_desert <- county_fdr$selected

# Child-weighted county coverage draws provide the interval estimand. The
# declaration itself follows the preregistered child-weighted LIS aggregation.
codes <- county$county_fips
group <- factor(tracts$county_fips, levels = codes)
# Child-weighted county coverage draws: sum tract draws weighted by under-five demand
# within each county, then divide by the county child total (next lines).
weighted_draws <- rowsum(draws$rho_draws * tracts$demand_mean, group = group, reorder = FALSE)
weight_totals <- as.numeric(rowsum(tracts$demand_mean, group = group, reorder = FALSE))
county_coverage_draws <- weighted_draws / weight_totals
assert_that(nrow(county_coverage_draws) == 67L && all(is.finite(county_coverage_draws)), "County coverage draws failed.")
county$county_aggregate_desert_probability <- rowMeans(county_coverage_draws < (A - gamma))
county$coverage_median <- apply(county_coverage_draws, 1L, stats::median)

m <- nrow(county)
R <- sum(county$county_fdr_desert)
# FCR-adjusted miscoverage level q*R/m for the R selected counties (Benjamini & Yekutieli 2005).
fcr_miscoverage <- if (R > 0L) min(1, q * R / m) else NA_real_
county$fcr_miscoverage <- ifelse(county$county_fdr_desert, fcr_miscoverage, NA_real_)
county$fcr_lower <- NA_real_
county$fcr_upper <- NA_real_
# Two-sided FCR posterior intervals, computed only for declared counties at the
# adjusted miscoverage, from the child-weighted county coverage draws.
if (R > 0L) {
  selected <- which(county$county_fdr_desert)
  county$fcr_lower[selected] <- apply(
    county_coverage_draws[selected, , drop = FALSE], 1L,
    stats::quantile, probs = fcr_miscoverage / 2, names = FALSE, type = 8
  )
  county$fcr_upper[selected] <- apply(
    county_coverage_draws[selected, , drop = FALSE], 1L,
    stats::quantile, probs = 1 - fcr_miscoverage / 2, names = FALSE, type = 8
  )
}
assert_that(all(!county$county_fdr_desert | county$fcr_lower <= county$coverage_median), "County FCR lower interval failed.")
assert_that(all(!county$county_fdr_desert | county$coverage_median <= county$fcr_upper), "County FCR upper interval failed.")

metadata <- list(
  procedure1 = list(q = q, k = fdr$k, achieved_mean_lis = fdr$achieved_mean_lis, cutoff_lis = fdr$cutoff_lis),
  procedure2 = list(
    c = c_fdx, alpha = alpha_fdx, k = fdx$k,
    achieved_tail_probability = fdx$achieved_tail_probability,
    implementation = "joint posterior empirical P(FDP>c), restricted to FDR prefix"
  ),
  procedure3 = list(
    q = q, k = county_fdr$k, achieved_mean_lis = county_fdr$achieved_mean_lis,
    weight = "under-five population",
    interval = "selected-county posterior interval at miscoverage q*R/m (FCR-style)"
  ),
  nesting = "FDX subset FDR subset p>0.5",
  n_draws = ncol(draws$rho_draws),
  prereg_sha256 = prereg$choices_sha256
)
save_analytic(
  list(
    tracts = tracts,
    counties = county,
    metadata = metadata,
    fdr_path = data.frame(rank = seq_along(fdr$sorted_lis), LIS = fdr$sorted_lis, cumulative_mean_LIS = fdr$cumulative_mean_lis),
    fdx_path = data.frame(rank = seq_along(fdx$tail_probability), posterior_exceedance_probability = fdx$tail_probability)
  ),
  "03_fdr_declarations.rds"
)

summary <- data.frame(
  procedure = c("probability_gt_0.5", "FDR_q10", "FDX_c10_alpha05", "county_child_weighted_FDR_q10"),
  declarations = c(sum(tracts$desert_probability > 0.5), fdr$k, fdx$k, county_fdr$k),
  target = c(NA, q, alpha_fdx, q),
  achieved = c(NA, fdr$achieved_mean_lis, fdx$achieved_tail_probability, county_fdr$achieved_mean_lis),
  unit = c("tracts", "tracts", "tracts", "counties"),
  stringsAsFactors = FALSE
)
utils::write.csv(summary, p07_path("outputs", "tables", "03_fdr_declaration_summary.csv"), row.names = FALSE, na = "")
utils::write.csv(
  county[c("county_fips", "county_name", "n_tracts", "total_weight", "county_lis", "county_desert_probability", "county_aggregate_desert_probability", "county_fdr_desert", "coverage_median", "fcr_miscoverage", "fcr_lower", "fcr_upper")],
  p07_path("outputs", "tables", "03_county_declarations.csv"), row.names = FALSE, na = ""
)

append_key_numbers(data.frame(
  key = c(
    "n_desert_fdr_q10", "n_desert_fdx", "n_desert_counties_weighted",
    "fdr_achieved_mean_lis", "fdx_achieved_tail_probability", "county_fdr_achieved_mean_lis"
  ),
  value = c(
    fdr$k, fdx$k, county_fdr$k,
    fdr$achieved_mean_lis, fdx$achieved_tail_probability, county_fdr$achieved_mean_lis
  ),
  unit = c("tracts", "tracts", "counties", "posterior probability", "posterior probability", "posterior probability"),
  source_script = "scripts/03-1_sun_fdr.R",
  note = c(
    paste0("LIS step-up achieved mean LIS=", signif(fdr$achieved_mean_lis, 6), "."),
    paste0("Joint posterior empirical P(FDP>0.10)=", signif(fdx$achieved_tail_probability, 6), "; restricted to FDR prefix."),
    "Tract LIS aggregated with under-five weights; selected-county coverage intervals use FCR-style q*R/m miscoverage.",
    "Mean LIS among the primary FDR prefix.",
    "Joint posterior empirical probability that FDP exceeds 0.10 for the primary FDX prefix.",
    "Mean county LIS among the under-five-weighted county prefix."
  ), stringsAsFactors = FALSE
))

cat("Step 3.1 FDR/FDX/county declarations: PASS\n")
