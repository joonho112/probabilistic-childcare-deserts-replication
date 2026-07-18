# ============================================================================
# fct_uncertainty.R -- Input-uncertainty helpers and the FIXED E2SFCA operator
#                       for P07.
# ----------------------------------------------------------------------------
# Purpose
#   Turns the two measured inputs -- ACS under-five demand and licensed provider
#   capacity -- into sampling distributions, and encodes the enhanced two-step
#   floating catchment (E2SFCA) allocation as a single sparse linear operator so
#   that a Monte Carlo capacity draw becomes one matrix multiply.
#
# Demand uncertainty
#   ACS publishes a 90% margin of error (MOE). demand_se_from_moe() converts it
#   to a standard error (SE = MOE / 1.645, the 90% normal quantile).
#   draw_truncated_demand() then samples each tract's demand from a Normal
#   truncated below at 1 child, which keeps inverse-coverage moments finite.
#
# Capacity uncertainty (mean-one lognormal multipliers)
#   lognormal_sigma_from_cv() maps a coefficient of variation to the lognormal
#   log-scale sd: sigma = sqrt(log(1 + CV^2)). draw_lognormal_multiplier() draws
#   multipliers with mean exactly one by setting meanlog = log(mean) - sigma^2/2,
#   so multiplying a baseline by them adds spread without shifting the mean.
#
# E2SFCA allocation (Luo & Qi 2009)
#   e2sfca_decay_weight() is the stepped distance decay: weight 1 within 5 min,
#   0.68 within 10, 0.22 within 15, and 0 beyond. build_fixed_e2sfca_operator()
#   assembles the two-step floating catchment as a sparse tract x provider matrix
#   over exactly 47,670 <=15-minute pairs: step 1 normalizes each provider by its
#   decay-weighted demand (competition); step 2 sums provider ratios back to
#   tracts. Coverage is then A %*% capacity. draw_capacity_rates() builds provider
#   capacities as
#     licensed x sector-mean-multiplier x shared sector temporal lognormal
#              x provider residual lognormal
#   and pushes every draw through the FIXED operator, so only capacity -- never
#   the catchment geometry -- is randomized.
#
# Key functions
#   demand_se_from_moe()          90% MOE -> SE
#   draw_truncated_demand()       per-tract truncated-Normal demand draws
#   lognormal_sigma_from_cv()     CV -> lognormal log-scale sigma
#   draw_lognormal_multiplier()   mean-one lognormal multipliers
#   e2sfca_decay_weight()         stepped 1 / 0.68 / 0.22 catchment decay
#   build_fixed_e2sfca_operator() sparse E2SFCA allocation operator
#   draw_capacity_rates()         Monte Carlo tract coverage-rate draws
#
# Reference
#   Luo, W., & Qi, Y. (2009). An enhanced two-step floating catchment area
#   (E2SFCA) method for measuring spatial accessibility to primary care
#   physicians. Health & Place, 15(4), 1100-1107.
# ============================================================================

demand_se_from_moe <- function(moe90, z = 1.645) {
  assert_that(is.numeric(moe90) && all(is.finite(moe90)) && all(moe90 >= 0), "Demand MOE must be finite and nonnegative.")
  # SE from a 90% margin of error: MOE = z * SE with z = 1.645 (the 90% Normal
  # quantile), so SE = MOE / z.
  moe90 / z
}

draw_truncated_demand <- function(mean, se, n_draws, seed = 20260716L, lower = 0) {
  assert_that(requireNamespace("truncnorm", quietly = TRUE), "Package 'truncnorm' is required.")
  assert_that(length(mean) == length(se) && all(is.finite(mean)) && all(mean >= 0), "Demand parameters are invalid.")
  assert_that(all(is.finite(se)) && all(se >= 0), "Demand standard errors are invalid.")
  assert_that(length(lower) %in% c(1L, length(mean)) && all(is.finite(lower)) && all(lower >= 0), "Demand lower bound is invalid.")
  assert_that(n_draws >= 1L && n_draws == as.integer(n_draws), "n_draws must be a positive integer.")
  # Fixed seed makes the sampling distribution reproducible across runs.
  set.seed(seed)
  n <- length(mean)
  lower <- rep(lower, length.out = n)
  assert_that(all(mean >= lower), "Demand mean must not be below its lower bound.")
  draws <- matrix(0, nrow = n_draws, ncol = n)
  # Tracts with positive SE are sampled; SE == 0 becomes a deterministic point
  # mass handled below, so the truncated sampler is never called with zero spread.
  stochastic <- se > 0
  # Vectorized truncated-Normal draw for all stochastic tracts at once: lower
  # bound a = tract lower bound, upper b = Inf, filling an n_draws x n matrix.
  if (any(stochastic)) {
    draws[, stochastic] <- matrix(
      truncnorm::rtruncnorm(
        n_draws * sum(stochastic), a = rep(lower[stochastic], each = n_draws), b = Inf,
        mean = rep(mean[stochastic], each = n_draws),
        sd = rep(se[stochastic], each = n_draws)
      ),
      nrow = n_draws, ncol = sum(stochastic)
    )
  }
  # Deterministic (SE == 0) tracts: every draw equals the mean, floored at the lower bound.
  if (any(!stochastic)) draws[, !stochastic] <- rep(pmax(mean[!stochastic], lower[!stochastic]), each = n_draws)
  colnames(draws) <- names(mean)
  draws
}

lognormal_sigma_from_cv <- function(cv) {
  assert_that(is.numeric(cv) && all(is.finite(cv)) && all(cv >= 0), "CV must be finite and nonnegative.")
  # Exact lognormal log-scale sd for a target CV: sigma^2 = log(1 + CV^2);
  # log1p keeps this accurate for small CV.
  sqrt(log1p(cv^2))
}

draw_lognormal_multiplier <- function(n, cv, mean_multiplier = 1, seed = NULL) {
  assert_that(n >= 1L && n == as.integer(n), "n must be a positive integer.")
  assert_that(length(cv) %in% c(1L, n) && all(cv >= 0), "cv length/value is invalid.")
  assert_that(length(mean_multiplier) %in% c(1L, n) && all(mean_multiplier > 0), "mean multiplier is invalid.")
  if (!is.null(seed)) set.seed(seed)
  cv <- rep(cv, length.out = n)
  mean_multiplier <- rep(mean_multiplier, length.out = n)
  sigma <- lognormal_sigma_from_cv(cv)
  # meanlog = log(mean) - sigma^2/2 makes E[multiplier] = mean_multiplier exactly,
  # so the draws add spread without shifting the mean.
  stats::rlnorm(n, meanlog = log(mean_multiplier) - 0.5 * sigma^2, sdlog = sigma)
}

# Stepped E2SFCA distance decay (Luo & Qi 2009): full weight within 5 minutes,
# 0.68 within 10, 0.22 within 15, and unreachable (0) beyond 15.
e2sfca_decay_weight <- function(minutes) {
  ifelse(minutes <= 5, 1, ifelse(minutes <= 10, 0.68, ifelse(minutes <= 15, 0.22, 0)))
}

# Reconstruct the P01 E2SFCA allocation as a fixed sparse tract x provider
# operator A, so that (tract coverage rates) = A %*% (provider capacities). The
# geometry is frozen here; only capacity is randomized downstream.
build_fixed_e2sfca_operator <- function(od, demand_geoid, demand_count, provider_id) {
  required <- c("tract_geoid", "provider_id", "minutes", "routing_status")
  assert_that(all(required %in% names(od)), "OD matrix lacks required fields.")
  assert_that(length(demand_geoid) == length(demand_count) && !anyDuplicated(demand_geoid), "Demand IDs/counts are invalid.")
  assert_that(!anyDuplicated(provider_id), "Provider IDs are invalid.")
  # Keep only successfully routed origin-destination pairs within the 15-minute
  # catchment; there must be exactly 47,670 of them (asserted below).
  keep <- od$routing_status == "ok" & is.finite(od$minutes) & od$minutes >= 0 & od$minutes <= 15
  pairs <- od[keep, required, drop = FALSE]
  assert_that(nrow(pairs) == 47670L, "Expected 47,670 fixed <=15-minute OD pairs.")
  # Map each pair's tract and provider IDs to row/column indices in the canonical
  # demand and provider universes.
  from <- match(as.character(pairs$tract_geoid), as.character(demand_geoid))
  to <- match(as.character(pairs$provider_id), as.character(provider_id))
  assert_that(!anyNA(from) && !anyNA(to), "OD IDs are outside the canonical universes.")
  # Decay weight for every kept pair.
  w <- e2sfca_decay_weight(pairs$minutes)
  weighted_demand <- numeric(length(provider_id))
  # Step 1 (competition): each provider's denominator is the decay-weighted demand
  # summed over the tracts that can reach it (rowsum groups by provider index).
  sums <- rowsum(demand_count[from] * w, group = to, reorder = FALSE)
  weighted_demand[as.integer(rownames(sums))] <- sums[, 1L]
  # Step 2: the provider-to-tract allocation coefficient is its decay weight
  # divided by that weighted demand (0 when the provider has no reachable demand).
  coefficient <- ifelse(weighted_demand[to] > 0, w / weighted_demand[to], 0)
  # Assemble A from (tract row, provider column, coefficient) triplets; summing
  # over a tract's providers gives its accessible-slots-per-child coverage rate.
  operator <- Matrix::sparseMatrix(
    i = from, j = to, x = coefficient,
    dims = c(length(demand_geoid), length(provider_id)),
    dimnames = list(as.character(demand_geoid), as.character(provider_id))
  )
  list(
    operator = operator,
    weighted_demand = weighted_demand,
    n_pairs = nrow(pairs),
    decay_breaks = c(5, 10, 15),
    decay_weights = c(1, 0.68, 0.22)
  )
}

# Monte Carlo tract coverage rates. Each provider's capacity is licensed capacity
# times a survey sector mean, a sector-shared temporal multiplier, and an
# independent provider residual; all draws are pushed through the fixed operator.
draw_capacity_rates <- function(supply_object, n_draws = 2000L, seed = 20260716L) {
  assert_that(inherits(supply_object$operator, "sparseMatrix"), "Supply object lacks a sparse fixed-allocation operator.")
  providers <- supply_object$providers
  required <- c("facility_id", "licensed_capacity", "survey_sector", "sector_mean_multiplier", "provider_residual_cv", "sector_temporal_cv")
  assert_that(all(required %in% names(providers)), "Supply provider parameters are incomplete.")
  # One reproducible seed for every capacity draw in this call.
  set.seed(seed)
  sectors <- unique(providers$survey_sector)
  # Shared temporal multiplier: one mean-one lognormal series per sector,
  # capturing wave-to-wave variation common to every provider in that sector.
  common <- lapply(sectors, function(sector) {
    cv <- unique(providers$sector_temporal_cv[providers$survey_sector == sector])
    assert_that(length(cv) == 1L, "Sector temporal CV must be unique within sector.")
    draw_lognormal_multiplier(n_draws, cv = cv, mean_multiplier = 1)
  })
  names(common) <- sectors
  n_provider <- nrow(providers)
  # Provider-level residual: an independent mean-one lognormal per provider x draw.
  sigma_residual <- lognormal_sigma_from_cv(providers$provider_residual_cv)
  residual <- matrix(
    stats::rlnorm(
      n_provider * n_draws,
      meanlog = rep(-0.5 * sigma_residual^2, times = n_draws),
      sdlog = rep(sigma_residual, times = n_draws)
    ),
    nrow = n_provider, ncol = n_draws
  )
  # Broadcast each sector's shared temporal series across that sector's providers.
  sector_common <- matrix(1, nrow = n_provider, ncol = n_draws)
  for (sector in sectors) {
    sector_common[providers$survey_sector == sector, ] <- rep(
      common[[sector]], each = sum(providers$survey_sector == sector)
    )
  }
  # Effective capacity per provider per draw = licensed x sector mean x provider
  # residual x sector-shared temporal multiplier.
  capacity <- providers$licensed_capacity * providers$sector_mean_multiplier * residual * sector_common
  # One sparse multiply turns the capacity draw matrix into tract coverage rates.
  rates <- as.matrix(supply_object$operator %*% capacity)
  rownames(rates) <- rownames(supply_object$operator)
  rates
}
