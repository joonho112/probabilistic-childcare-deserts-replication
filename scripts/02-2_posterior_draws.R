#!/usr/bin/env Rscript

# P07 Step 2.2 -- Modular joint posterior Monte Carlo for coverage uncertainty.
#
# Purpose:
#   Propagates three independent sources of uncertainty into 2,000 joint draws
#   of tract coverage rho: (1) the fitted BYM2 latent field, (2) ACS under-five
#   demand (the denominator), and (3) provider capacity (the numerator). These
#   draws are the raw material for every downstream probability and declaration.
#
# Method:
#   "Modular" (a.k.a. cut) Monte Carlo. Rather than refit the model per draw,
#   each joint coverage draw rescales the fitted latent rate by independent
#   input-error multipliers:
#       rho_draw = latent_rate x (demand_hat / demand_draw) x capacity_ratio.
#   latent_rate comes from INLA posterior predictor samples (20 batches of 100,
#   RNG reset per batch); demand_draw from a truncated-normal input stream;
#   capacity_ratio from a lognormal provider/sector capacity stream. The three
#   streams use separate seeds (latent 20260716, demand 20260717, supply 20260718).
#   Cutting the feedback keeps the computation tractable and auditable but, by
#   construction, understates the interaction between input error and smoothing.
#   Registered deviation: the demand truncation lower bound was moved from 0 to 1
#   child BEFORE any posterior was computed, because a preflight showed the
#   inverse-demand multiplier demand_hat/demand_draw could blow up to 128,027 as
#   demand_draw approached zero.
#
# Reads:
#   data/analytic/02_bym2_fit.rds            (fitted model + model_data)
#   data/analytic/01_demand_uncertainty.rds  (demand mean/SE/CV per tract)
#   data/analytic/01_supply_uncertainty.rds  (providers + allocation operator)
#   data/analytic/01_prereg_region.rds       (locked draw count + hash)
#
# Writes:
#   data/analytic/02_coverage_posterior.rds        (per-tract coverage summary + metadata)
#   data/interim/02_coverage_posterior_draws.rds   (full 1,409 x 2,000 draw matrix; git-ignored)
#   outputs/tables/02_posterior_diagnostics.csv     (draw diagnostics, interval widths by CV)
#   outputs/tables/02_model_deviation_register.csv  (appends the demand lower-bound deviation)
#   outputs/key_numbers.csv                         (appended single-source-of-truth numbers)

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_uncertainty.R"))
source(file.path("R", "fct_bym2.R"))

fit_bundle <- readRDS(p07_path("data", "analytic", "02_bym2_fit.rds"))
demand_bundle <- readRDS(p07_path("data", "analytic", "01_demand_uncertainty.rds"))
supply_bundle <- readRDS(p07_path("data", "analytic", "01_supply_uncertainty.rds"))
prereg <- readRDS(p07_path("data", "analytic", "01_prereg_region.rds"))

assert_that(identical(prereg$choices_sha256, file_sha256(p07_path("outputs", "tables", "PREREG_choices.csv"))), "Preregistration hash changed before posterior propagation.")
n_draws <- as.integer(prereg$primary$posterior_draws)
assert_that(n_draws == 2000L, "Primary posterior draw count must remain 2,000.")

demand <- demand_bundle$tracts
eligible <- demand$eligible
model_data <- fit_bundle$model_data
assert_that(identical(model_data$GEOID, demand$GEOID[eligible]), "Fit/demand tract order mismatch.")

# Independent deterministic RNG streams derived from the registered base date.
seed_latent <- 20260716L
seed_demand <- 20260717L
seed_supply <- 20260718L

# Stream 1 (latent): draw the log expected-slots predictor from the INLA posterior
# in 20 batches of 100, resetting the RNG per batch for exact reproducibility.
log_response_predictor <- sample_predictors_batched(
  fit_bundle$fit, n_predictor = sum(eligible), n_draws = n_draws,
  seed = seed_latent, batch_size = 100L
)
# Convert sampled log expected-slots back to a coverage rate by removing the
# demand offset (exp of predictor minus log exposure).
latent_rate <- exp(log_response_predictor - log(model_data$exposure))
rm(log_response_predictor)

# Stream 2 (demand): independent truncated-normal draws of ACS under-five demand,
# floored at lower = 1 child (the registered lower-bound deviation, see below).
demand_draws <- draw_truncated_demand(
  stats::setNames(demand$demand_mean[eligible], demand$GEOID[eligible]),
  stats::setNames(demand$demand_se[eligible], demand$GEOID[eligible]),
  n_draws = n_draws, seed = seed_demand, lower = 1
)
# Inverse-demand multiplier (point demand / sampled demand), transposed to a
# tract-by-draw matrix; the lower = 1 floor keeps this ratio from exploding.
demand_ratio <- demand$demand_mean[eligible] / t(demand_draws)
rm(demand_draws)

# Stream 3 (supply): independent lognormal draws of provider capacity pushed
# through the fixed allocation operator into tract supply rates.
supply_rate_all <- draw_capacity_rates(
  supply_bundle, n_draws = n_draws, seed = seed_supply
)
supply_rate <- supply_rate_all[match(model_data$GEOID, rownames(supply_rate_all)), , drop = FALSE]
center_rate <- model_data$input_rate
supply_ratio <- matrix(1, nrow = nrow(supply_rate), ncol = ncol(supply_rate))
positive_center <- center_rate > 0
# Capacity multiplier = sampled supply rate / point supply rate. Where the point
# rate is a structural zero the ratio stays 1, so the BYM2 field alone carries the uncertainty.
supply_ratio[positive_center, ] <- supply_rate[positive_center, , drop = FALSE] / center_rate[positive_center]
rm(supply_rate_all, supply_rate)

# The modular product: joint coverage draws rescale the fitted latent field by the
# two independent input-error ratios rather than refitting the model per draw.
rho_draws <- latent_rate * demand_ratio * supply_ratio
assert_that(all(is.finite(rho_draws)) && all(rho_draws > 0), "Integrated coverage draws are invalid.")

latent_mean <- rowMeans(latent_rate)
# Sanity gate: the mean latent draw must track the INLA fitted mean (Spearman > 0.98).
latent_alignment <- stats::cor(latent_mean, model_data$fitted_rate_mean, method = "spearman")
assert_that(is.finite(latent_alignment) && latent_alignment > 0.98, "Joint latent samples do not align with fitted coverage.")

# Per-tract (row-wise) quantiles across draws, using type-8 (median-unbiased) plotting positions.
summarize_row <- function(x, probability) {
  apply(x, 1L, stats::quantile, probs = probability, names = FALSE, type = 8)
}
summary <- data.frame(
  GEOID = model_data$GEOID,
  coverage_mean = rowMeans(rho_draws),
  coverage_median = summarize_row(rho_draws, 0.50),
  coverage_sd = apply(rho_draws, 1L, stats::sd),
  coverage_q10 = summarize_row(rho_draws, 0.10),
  coverage_q90 = summarize_row(rho_draws, 0.90),
  coverage_q025 = summarize_row(rho_draws, 0.025),
  coverage_q975 = summarize_row(rho_draws, 0.975),
  latent_rate_mean = latent_mean,
  input_rate = center_rate,
  demand_mean = demand$demand_mean[eligible],
  demand_se = demand$demand_se[eligible],
  demand_cv = demand$cv_under5[eligible],
  high_cv_flag = demand$high_cv_flag[eligible],
  structural_zero_input = !positive_center,
  stringsAsFactors = FALSE
)
summary$width80 <- summary$coverage_q90 - summary$coverage_q10
summary$width95 <- summary$coverage_q975 - summary$coverage_q025
# Interval width relative to the tract's own median; pmax guards against divide-by-zero.
summary$relative_width95 <- summary$width95 / pmax(summary$coverage_median, .Machine$double.eps)

assert_that(nrow(summary) == 1409L && !anyDuplicated(summary$GEOID), "Coverage summary universe failed.")
assert_that(all(summary$coverage_q025 <= summary$coverage_q10 & summary$coverage_q10 <= summary$coverage_median & summary$coverage_median <= summary$coverage_q90 & summary$coverage_q90 <= summary$coverage_q975), "Coverage intervals are unordered.")

# Internal validity check: bin tracts by ACS demand CV and confirm high-CV tracts
# carry wider RELATIVE 95% intervals -- evidence that input error is flowing through.
width_by_cv <- summary |>
  dplyr::mutate(cv_tier = ifelse(.data$high_cv_flag, "high_cv_gt_0.40", "cv_le_0.40")) |>
  dplyr::group_by(.data$cv_tier) |>
  dplyr::summarise(
    n_tracts = dplyr::n(),
    median_width95 = stats::median(.data$width95),
    median_relative_width95 = stats::median(.data$relative_width95),
    .groups = "drop"
  )
assert_that(
  width_by_cv$median_relative_width95[width_by_cv$cv_tier == "high_cv_gt_0.40"] >
    width_by_cv$median_relative_width95[width_by_cv$cv_tier == "cv_le_0.40"],
  "High-CV tracts do not show wider relative integrated intervals."
)

draw_metadata <- list(
  method = "modular joint posterior Monte Carlo",
  composition = "joint INLA Tweedie-BYM2 predictor x demand denominator ratio x fixed-allocation capacity ratio",
  n_draws = n_draws,
  seeds = c(latent = seed_latent, demand = seed_demand, supply = seed_supply),
  latent_rng = "R set.seed(batch_seed) plus INLA seed=batch_seed; num.threads=1:1; parallel.configs=FALSE",
  demand_lower_bound = 1,
  zero_supply_rule = "where fixed expected supply rate is zero, capacity ratio is 1 and BYM2 supplies spatial posterior uncertainty",
  raw_draw_release = "ignored data/interim only",
  prereg_sha256 = prereg$choices_sha256,
  source_sha256 = list(
    # The serialized INLA object embeds non-semantic runtime state, so its
    # whole-file hash changes across otherwise identical serial fits. Hash the
    # deterministic fitted hyperparameter summary instead.
    fit_hyperparameters = file_sha256(p07_path("outputs", "tables", "02_bym2_hyperparameters.csv")),
    demand = file_sha256(p07_path("data", "analytic", "01_demand_uncertainty.rds")),
    supply = file_sha256(p07_path("data", "analytic", "01_supply_uncertainty.rds"))
  )
)
save_analytic(list(tracts = summary, metadata = draw_metadata, width_by_cv = width_by_cv), "02_coverage_posterior.rds")
save_analytic(
  list(GEOID = model_data$GEOID, rho_draws = rho_draws, metadata = draw_metadata),
  "02_coverage_posterior_draws.rds", "interim"
)

diagnostics <- data.frame(
  metric = c(
    "n_draws", "latent_fitted_spearman", "median_tract_posterior_coverage",
    "median_95_width_low_cv", "median_95_width_high_cv",
    "median_relative_95_width_low_cv", "median_relative_95_width_high_cv",
    "structural_zero_input_tracts", "maximum_draw"
  ),
  value = c(
    n_draws, latent_alignment, stats::median(summary$coverage_median),
    width_by_cv$median_width95[width_by_cv$cv_tier == "cv_le_0.40"],
    width_by_cv$median_width95[width_by_cv$cv_tier == "high_cv_gt_0.40"],
    width_by_cv$median_relative_width95[width_by_cv$cv_tier == "cv_le_0.40"],
    width_by_cv$median_relative_width95[width_by_cv$cv_tier == "high_cv_gt_0.40"],
    sum(summary$structural_zero_input), max(rho_draws)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(diagnostics, p07_path("outputs", "tables", "02_posterior_diagnostics.csv"), row.names = FALSE, na = "")

# Register the mathematically necessary positive-count domain correction next
# to the likelihood deviation, before any desert probabilities are computed.
deviations_path <- p07_path("outputs", "tables", "02_model_deviation_register.csv")
deviations <- utils::read.csv(deviations_path, stringsAsFactors = FALSE)
domain_row <- data.frame(
  plan_item = "eligible demand lower bound",
  planned = "Normal truncated at zero",
  implemented = "Normal truncated at one child for eligible tracts; zero-demand remains point mass zero",
  trigger = "with lower zero, 5,221 of 2,818,000 preflight draws were below one and the inverse-demand multiplier reached 128,027",
  rejected_workaround = "silently cap posterior coverage after constructing it",
  scientific_reason = "positive-demand count support begins at one and this makes inverse coverage moments finite",
  stringsAsFactors = FALSE
)
deviations <- rbind(deviations[deviations$plan_item != domain_row$plan_item, , drop = FALSE], domain_row)
utils::write.csv(deviations, deviations_path, row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c(
    "posterior_method", "n_mc_draws", "median_coverage_post", "demand_draw_lower_bound",
    "median_relative_95_width_low_cv", "median_relative_95_width_high_cv"
  ),
  value = c(
    draw_metadata$method, n_draws, stats::median(summary$coverage_median), 1,
    width_by_cv$median_relative_width95[width_by_cv$cv_tier == "cv_le_0.40"],
    width_by_cv$median_relative_width95[width_by_cv$cv_tier == "high_cv_gt_0.40"]
  ),
  unit = c("method", "joint draws", "slots per child", "child", "relative width", "relative width"),
  source_script = "scripts/02-2_posterior_draws.R",
  note = c(
    "One BYM2 fit plus joint latent posterior samples and independent demand/supply input streams.",
    "Raw draws remain under ignored data/interim.",
    "Median across tract-specific posterior medians.",
    "Eligible positive-demand support; zero-demand tracts remain excluded.",
    "Median tract 95% interval width divided by posterior median among ACS demand CV <= 0.40.",
    "Median tract 95% interval width divided by posterior median among ACS demand CV > 0.40."
  ), stringsAsFactors = FALSE
))

cat("Step 2.2 posterior uncertainty propagation: PASS\n")
