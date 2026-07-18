#!/usr/bin/env Rscript

# P07 Step 2.3 -- Tract desert posterior probability and local index of significance.
#
# Purpose:
#   Converts the 2,000 joint coverage draws into one interpretable number per
#   tract: the posterior probability that the tract is a child care desert.
#   These probabilities (and their complements, the LIS values) are the inputs
#   to the Step 3.1 false-discovery declarations.
#
# Method:
#   A tract is a "desert" in a draw when its coverage falls below the strict
#   boundary A - gamma (primary: A = 0.33 slots/child, gamma = 0). The desert
#   posterior probability is the Monte Carlo average
#       p = mean(rho_draw < A - gamma),
#   estimated at resolution 1/2000 = 0.0005. The local index of significance is
#   the complement LIS = 1 - p = P(coverage >= A - gamma | data): the posterior
#   probability of being a NON-desert, used as the local false-discovery weight
#   in the Sun & Cai (2015) sense. Reported: median p = 0.611; p > 0.50 in 816
#   tracts, p > 0.90 in 280, p > 0.95 in 152. The p + LIS = 1 identity is
#   asserted to < sqrt(machine epsilon).
#
# Reads:
#   data/analytic/02_coverage_posterior.rds        (per-tract summary)
#   data/interim/02_coverage_posterior_draws.rds   (full draw matrix)
#   data/analytic/01_prereg_region.rds             (locked A, gamma, draw count)
#   data/analytic/01_demand_uncertainty.rds        (county identifiers)
#   <P01>/02_deserts_classified.rds                (deterministic point desert flags)
#
# Writes:
#   data/analytic/02_desert_probability.rds            (tracts + summary counts + ambiguity)
#   outputs/tables/02_desert_probability_summary.csv   (count/child totals by criterion)
#   outputs/tables/02_probability_ambiguity_by_cv.csv  (ambiguous-tract shares by CV tier)
#   outputs/key_numbers.csv                            (appended single-source-of-truth numbers)
#
# Reference:
#   Sun & Cai (2015), oracle local-index-of-significance FDR.

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))

posterior <- readRDS(p07_path("data", "analytic", "02_coverage_posterior.rds"))
draws <- readRDS(p07_path("data", "interim", "02_coverage_posterior_draws.rds"))
prereg <- readRDS(p07_path("data", "analytic", "01_prereg_region.rds"))
demand <- readRDS(p07_path("data", "analytic", "01_demand_uncertainty.rds"))$tracts
point <- read_p01("02_deserts_classified.rds")

assert_that(identical(prereg$choices_sha256, file_sha256(p07_path("outputs", "tables", "PREREG_choices.csv"))), "Preregistration hash changed.")
A <- prereg$primary$threshold_A
gamma <- prereg$primary$gamma
# Strict desert boundary; with the primary gamma = 0 this is exactly A = 0.33.
threshold <- A - gamma
assert_that(identical(A, 0.33) && identical(gamma, 0), "Primary desert boundary changed.")
assert_that(identical(draws$GEOID, posterior$tracts$GEOID), "Posterior draw/summary order mismatch.")
assert_that(ncol(draws$rho_draws) == prereg$primary$posterior_draws, "Posterior draw count mismatch.")

# Boolean tract-by-draw matrix: TRUE wherever a draw's coverage is below the boundary.
desert_indicator <- draws$rho_draws < threshold
# Desert posterior probability p = fraction of draws below the boundary (resolution 1/2000).
desert_probability <- rowMeans(desert_indicator)
# LIS = 1 - p = P(coverage >= boundary | data): the local NON-desert probability that
# the Step 3.1 Sun & Cai step-up treats as the local false-discovery weight.
lis <- 1 - desert_probability

result <- posterior$tracts
result$desert_probability <- desert_probability
result$LIS <- lis
# Finest resolvable probability step equals 1 / number of draws.
result$probability_resolution <- 1 / ncol(draws$rho_draws)
# Attach the deterministic P01 E2SFCA point-desert flag for the Step 3.2 comparison.
result$point_desert_e2sfca <- point$is_desert_e2sfca[
  match(result$GEOID, point$GEOID)
]
result$county_fips <- demand$county_fips[match(result$GEOID, demand$GEOID)]
result$county_name <- demand$county_name[match(result$GEOID, demand$GEOID)]

assert_that(nrow(result) == 1409L && !anyDuplicated(result$GEOID), "Desert probability universe failed.")
assert_that(all(result$desert_probability >= 0 & result$desert_probability <= 1), "Desert probabilities are outside [0,1].")
# Guard the p + LIS = 1 identity numerically (to within sqrt machine epsilon).
assert_that(max(abs(result$LIS - (1 - result$desert_probability))) < .Machine$double.eps^0.5, "LIS identity failed.")
assert_that(!anyNA(result$point_desert_e2sfca), "P01 point flags did not join completely.")

counts <- data.frame(
  criterion = c(
    "p > 0.50", "p > 0.90", "p > 0.95", "0.25 <= p <= 0.75",
    "p = 0", "p = 1", "P01 point desert"
  ),
  n_tracts = c(
    sum(result$desert_probability > 0.50),
    sum(result$desert_probability > 0.90),
    sum(result$desert_probability > 0.95),
    sum(result$desert_probability >= 0.25 & result$desert_probability <= 0.75),
    sum(result$desert_probability == 0),
    sum(result$desert_probability == 1),
    sum(result$point_desert_e2sfca)
  ),
  children = c(
    sum(result$demand_mean[result$desert_probability > 0.50]),
    sum(result$demand_mean[result$desert_probability > 0.90]),
    sum(result$demand_mean[result$desert_probability > 0.95]),
    sum(result$demand_mean[result$desert_probability >= 0.25 & result$desert_probability <= 0.75]),
    sum(result$demand_mean[result$desert_probability == 0]),
    sum(result$demand_mean[result$desert_probability == 1]),
    sum(result$demand_mean[result$point_desert_e2sfca])
  ),
  stringsAsFactors = FALSE
)

# Summarize how many tracts sit in the ambiguous [0.25, 0.75] probability band,
# split by CV tier -- where uncertainty makes the desert call least decisive.
ambiguity <- result |>
  dplyr::mutate(
    cv_tier = ifelse(.data$high_cv_flag, "high_cv_gt_0.40", "cv_le_0.40"),
    ambiguous = .data$desert_probability >= 0.25 & .data$desert_probability <= 0.75
  ) |>
  dplyr::group_by(.data$cv_tier) |>
  dplyr::summarise(
    n_tracts = dplyr::n(),
    ambiguous_tracts = sum(.data$ambiguous),
    ambiguous_pct = 100 * mean(.data$ambiguous),
    median_desert_probability = stats::median(.data$desert_probability),
    .groups = "drop"
  )

metadata <- list(
  threshold_A = A,
  gamma = gamma,
  strict_threshold = threshold,
  probability_definition = "mean(rho_draw < A - gamma)",
  lis_definition = "1 - desert_probability = P(rho >= A - gamma | data)",
  n_draws = ncol(draws$rho_draws),
  probability_resolution = 1 / ncol(draws$rho_draws),
  prereg_sha256 = prereg$choices_sha256
)
save_analytic(list(tracts = result, summary_counts = counts, ambiguity = ambiguity, metadata = metadata), "02_desert_probability.rds")
utils::write.csv(counts, p07_path("outputs", "tables", "02_desert_probability_summary.csv"), row.names = FALSE, na = "")
utils::write.csv(ambiguity, p07_path("outputs", "tables", "02_probability_ambiguity_by_cv.csv"), row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c("n_prob_gt_50", "n_prob_gt_90", "n_prob_gt_95", "median_desert_prob"),
  value = c(
    sum(result$desert_probability > 0.50),
    sum(result$desert_probability > 0.90),
    sum(result$desert_probability > 0.95),
    stats::median(result$desert_probability)
  ),
  unit = c("tracts", "tracts", "tracts", "posterior probability"),
  source_script = "scripts/02-3_desert_probability.R",
  note = c(
    "Strictly greater than 0.50 among 1,409 nonzero-demand tracts.",
    "High-confidence probability screen; not the FDR declaration.",
    "Very-high-confidence probability screen; not the FDR declaration.",
    "Median across tract-specific desert posterior probabilities."
  ), stringsAsFactors = FALSE
))

cat("Step 2.3 desert posterior probability: PASS\n")
