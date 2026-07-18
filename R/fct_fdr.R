# ============================================================================
# fct_fdr.R -- Error-controlled desert-declaration rules for P07.
# ----------------------------------------------------------------------------
# Purpose
#   Turns per-tract posterior desert evidence into a declared set of deserts with
#   a controlled error rate, and rolls tract evidence up to counties.
#
# LIS and the two error criteria
#   The local index of significance (LIS) of a tract is the posterior probability
#   that it is NOT a desert (its local-null probability). Sorting tracts by
#   ascending LIS ranks them from most to least likely to be a desert.
#
#   lis_step_up() -- Sun & Cai (2015) LIS step-up controlling the posterior
#   expected false discovery rate (FDR) at level q: sort LIS ascending and declare
#   the largest prefix whose running mean LIS <= q. That running mean is exactly
#   the posterior expected proportion of false declarations in the prefix.
#
#   posterior_fdx_step_up() -- controls false discovery EXCEEDANCE (FDX): within
#   at most the FDR prefix, keep the largest k for which the empirical posterior
#   probability that the false discovery proportion (FDP) exceeds c is <= alpha.
#   Where FDR controls the mean of the FDP, FDX controls its tail, using the
#   per-draw desert indicators directly.
#
#   aggregate_county_lis() -- child-weighted mean tract LIS within each county,
#   the county-level exposure summary.
#
# Key functions
#   lis_step_up()             Sun-Cai FDR step-up (posterior expected FDR <= q)
#   posterior_fdx_step_up()   FDX step-up (P(FDP > c) <= alpha) within the prefix
#   aggregate_county_lis()    child-weighted county mean LIS
#
# Reference
#   Sun & Cai (2015). Local-index-of-significance (LIS) step-up procedure for
#   posterior (compound-decision) false discovery rate control.
# ============================================================================

# Sun-Cai LIS step-up: declare the largest low-LIS prefix whose average LIS stays
# within the target posterior FDR q.
lis_step_up <- function(lis, q = 0.10) {
  assert_that(is.numeric(lis) && all(is.finite(lis)) && all(lis >= 0 & lis <= 1), "LIS values must be in [0,1].")
  assert_that(length(q) == 1L && q > 0 && q < 1, "q must lie in (0,1).")
  # Rank tracts from most to least likely desert (ascending LIS); ties broken by
  # position for determinism.
  ordering <- order(lis, seq_along(lis))
  sorted <- lis[ordering]
  # Running mean of the sorted LIS = the posterior expected FDP of each prefix.
  cumulative_mean <- cumsum(sorted) / seq_along(sorted)
  # Largest prefix whose expected FDP is still <= q (k below); k = 0 declares none.
  valid <- which(cumulative_mean <= q)
  k <- if (length(valid)) max(valid) else 0L
  selected <- rep(FALSE, length(lis))
  if (k > 0L) selected[ordering[seq_len(k)]] <- TRUE
  list(
    selected = selected,
    k = k,
    order = ordering,
    sorted_lis = sorted,
    cumulative_mean_lis = cumulative_mean,
    achieved_mean_lis = if (k > 0L) cumulative_mean[k] else 0,
    cutoff_lis = if (k > 0L) sorted[k] else NA_real_,
    next_mean_lis = if (k < length(lis)) cumulative_mean[k + 1L] else NA_real_,
    q = q
  )
}

# FDX step-up: among the low-LIS tracts, keep the largest prefix whose posterior
# probability of the FDP exceeding c is at most alpha (a tail, not a mean, bound).
posterior_fdx_step_up <- function(lis, desert_indicator_draws, c = 0.10, alpha = 0.05, max_k = length(lis)) {
  assert_that(nrow(desert_indicator_draws) == length(lis), "FDX draw rows and LIS length differ.")
  assert_that(all(desert_indicator_draws %in% c(TRUE, FALSE)), "FDX desert indicators are invalid.")
  assert_that(c > 0 && c < 1 && alpha > 0 && alpha < 1, "FDX c/alpha must lie in (0,1).")
  # Rank by ascending LIS, then flag, per draw, which ranked tracts are NOT
  # deserts in that draw -- these are the potential false discoveries.
  ordering <- order(lis, seq_along(lis))
  false_draws <- !desert_indicator_draws[ordering, , drop = FALSE]
  false_count <- numeric(ncol(false_draws))
  tail_probability <- numeric(length(lis))
  # Walk down the ranking accumulating false counts; tail_probability[k] is the
  # share of posterior draws in which the top-k prefix's FDP exceeds c.
  for (k in seq_along(lis)) {
    false_count <- false_count + false_draws[k, ]
    tail_probability[k] <- mean((false_count / k) > c)
  }
  max_k <- min(as.integer(max_k), length(lis))
  # Largest prefix (bounded by max_k) whose FDP-exceedance probability is <= alpha.
  valid <- which(seq_along(lis) <= max_k & tail_probability <= alpha)
  k <- if (length(valid)) max(valid) else 0L
  selected <- rep(FALSE, length(lis))
  if (k > 0L) selected[ordering[seq_len(k)]] <- TRUE
  list(
    selected = selected,
    k = k,
    order = ordering,
    tail_probability = tail_probability,
    achieved_tail_probability = if (k > 0L) tail_probability[k] else 0,
    cutoff_lis = if (k > 0L) lis[ordering[k]] else NA_real_,
    c = c,
    alpha = alpha,
    n_draws = ncol(desert_indicator_draws),
    max_k = max_k
  )
}

# Aggregate tract LIS to counties as a child-weighted mean, so each county's
# summary reflects where children actually live.
aggregate_county_lis <- function(tract_table, weight, county_fips = tract_table$county_fips) {
  assert_that(length(weight) == nrow(tract_table) && all(is.finite(weight)) && all(weight >= 0), "County weights are invalid.")
  assert_that(all(c("LIS", "county_name") %in% names(tract_table)), "Tract table lacks county fields/LIS.")
  # Iterate over the distinct county codes.
  codes <- sort(unique(as.character(county_fips)))
  out <- lapply(codes, function(code) {
    take <- as.character(county_fips) == code
    total_weight <- sum(weight[take])
    assert_that(total_weight > 0, paste0("County has zero total weight: ", code))
    data.frame(
      county_fips = code,
      county_name = as.character(tract_table$county_name[which(take)[1L]]),
      n_tracts = sum(take),
      total_weight = total_weight,
      # Child-weighted mean tract LIS within the county.
      county_lis = sum(weight[take] * tract_table$LIS[take]) / total_weight,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

