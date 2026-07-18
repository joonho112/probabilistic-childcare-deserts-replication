#!/usr/bin/env Rscript

# P07 Step 2.1 -- Intercept-only scaled BYM2 model for design-based areal coverage.
#
# Purpose:
#   Fits the single areal model at the heart of the P07 pipeline: a smoothed
#   posterior surface of child care coverage (accessible slots per eligible
#   child) across Alabama census tracts. Every later step -- posterior draws,
#   desert probabilities, FDR/FDX declarations -- consumes this fit, so its
#   spatial structure and hyperparameters propagate downstream.
#
# Method:
#   Response is the fixed-E2SFCA allocation of survey-calibrated expected
#   capacity to tracts; ACS under-five demand enters as an exposure offset,
#   offset(log(exposure)), so the linear predictor targets a log coverage rate.
#   The likelihood is TWEEDIE (compound Poisson-gamma) with a log link -- a
#   documented, registered deviation from the planned Poisson, adopted because
#   1,206 of 1,409 responses are fractional and 203 are structural zeros, and
#   rounding to integer counts was rejected as arbitrary data alteration.
#   The spatial term is the BYM2 reparameterization (Riebler et al. 2016) of the
#   Besag-York-Mollie convolution: total variance is split by a single mixing
#   parameter phi into a spatially structured (scaled Besag/ICAR) part and an
#   unstructured IID part. PC priors (Simpson et al. 2017) set P(sigma > 1)=0.01
#   and P(phi < 0.5)=0.5. Fit by INLA (Rue et al. 2009) with adaptive strategy,
#   CCD integration, METIS reordering, single-thread execution, BLAS/OpenMP
#   pinned to 1, and seed 20260716 for bit-reproducibility. Reported posterior:
#   phi=0.992 (near-fully structured), sigma=0.976, DIC 13,317.9, WAIC 13,262.6,
#   0 CPO failures.
#
# Reads:
#   data/analytic/01_demand_uncertainty.rds   (tract demand: mean, SE, CV, eligibility)
#   data/analytic/01_supply_uncertainty.rds   (providers + fixed E2SFCA allocation operator)
#   data/analytic/01_prereg_region.rds        (locked preregistered choices + hash)
#   <P01>/05_spatial_weights.rds              (Queen contiguity neighbor list)
#
# Writes:
#   data/analytic/02_bym2_fit.rds                    (fitted model + model_data + diagnostics)
#   outputs/tables/02_bym2_diagnostics.csv           (graph facts, alignment, DIC/WAIC, CPO)
#   outputs/tables/02_bym2_hyperparameters.csv       (sigma, phi, precision posteriors)
#   outputs/tables/02_model_deviation_register.csv   (Poisson -> Tweedie deviation record)
#   outputs/key_numbers.csv                          (appended single-source-of-truth numbers)
#
# References:
#   Besag, York & Mollie (1991); Riebler et al. (2016); Simpson et al. (2017);
#   Rue, Martino & Chopin (2009, INLA).

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_bym2.R"))

demand_bundle <- readRDS(p07_path("data", "analytic", "01_demand_uncertainty.rds"))
supply_bundle <- readRDS(p07_path("data", "analytic", "01_supply_uncertainty.rds"))
prereg <- readRDS(p07_path("data", "analytic", "01_prereg_region.rds"))
weights <- read_p01("05_spatial_weights.rds")

demand <- demand_bundle$tracts
eligible <- demand$eligible
# The model universe is the 1,409 nonzero-demand tracts (the eligible subset
# of the 1,436-tract region); zero-demand tracts carry no offset and are dropped.
assert_that(sum(eligible) == 1409L, "Model eligibility must contain 1,409 tracts.")
assert_that(identical(prereg$primary$threshold_A, 0.33), "Preregistered A changed before model fit.")
assert_that(identical(prereg$choices_sha256, file_sha256(p07_path("outputs", "tables", "PREREG_choices.csv"))), "Preregistration hash changed.")

# Subset the full Queen-contiguity graph to eligible tracts and build the INLA
# adjacency for the BYM2 term; records islands/components of the ICAR structure.
graph_info <- prepare_p07_graph(weights$full_nb, demand$GEOID, eligible)
assert_that(graph_info$n == 1409L && graph_info$islands == 1L && graph_info$components == 2L, "Model graph facts differ from Step 0.2 audit.")

providers <- supply_bundle$providers
# Point-estimate expected capacity per provider: licensed slots scaled by the
# survey-calibrated sector mean multiplier (utilized, not merely licensed, slots).
expected_capacity <- providers$licensed_capacity * providers$sector_mean_multiplier
# Apply the fixed E2SFCA allocation operator (a sparse tract-by-provider matrix)
# to map provider capacity into a per-tract accessible-slot supply rate.
expected_rate_all <- as.numeric(supply_bundle$operator %*% expected_capacity)
# Exposure = ACS under-five demand on eligible tracts; enters the model as
# offset(log(exposure)) so the linear predictor is a log coverage rate.
exposure <- demand$demand_mean[eligible]
# Response = coverage rate x demand = expected accessible slots. It is fractional
# with a point mass at zero, which is exactly what forces the Tweedie choice below.
response <- expected_rate_all[eligible] * exposure

# `response` is the fractional accessible-slot equivalent produced by the
# fixed FCA allocation, with a point mass at zero. Current INLA enforces an
# integer response for Poisson. A Tweedie compound Poisson-Gamma likelihood
# accepts nonnegative continuous allocations and zeros while retaining the
# log(exposure) offset; no arbitrary rounding or epsilon is introduced.
model <- fit_coverage_bym2(response, exposure, graph_info)
fit <- model$fit
assert_that(all(is.finite(fit$summary.linear.predictor$mean)), "BYM2 linear predictors are invalid.")
assert_that(all(is.finite(fit$summary.hyperpar$mean)), "BYM2 hyperparameter summaries are invalid.")
assert_that(is.finite(fit$mlik[1L, 1L]), "BYM2 marginal likelihood is invalid.")
assert_that(!is.null(fit$misc$configs), "BYM2 fit lacks posterior sampling configuration.")

# sigma is recovered by transforming the precision marginal (E[1/sqrt(prec)]), not
# by 1/sqrt(mean precision) which Jensen's inequality would bias.
hyper <- extract_bym2_hyperparameters(fit)
# Recover the fitted coverage rate by removing the exposure offset:
# fitted expected slots divided by demand.
posterior_rate <- fit$summary.fitted.values$mean / exposure
assert_that(all(is.finite(posterior_rate)) && all(posterior_rate > 0), "Posterior fitted coverage is invalid.")
# Validation gate: the smoothed posterior must stay directionally aligned with the
# raw input allocation (Spearman > 0.70), guarding against an over-smoothed field.
spearman <- stats::cor(posterior_rate, expected_rate_all[eligible], method = "spearman")
pearson <- stats::cor(posterior_rate, expected_rate_all[eligible], method = "pearson")
assert_that(is.finite(spearman) && spearman > 0.70, "BYM2 posterior coverage is not directionally aligned with input coverage.")

fit_bundle <- list(
  fit = fit,
  model_data = cbind(
    data.frame(GEOID = demand$GEOID[eligible], stringsAsFactors = FALSE),
    model$model_data,
    input_rate = expected_rate_all[eligible],
    fitted_rate_mean = posterior_rate
  ),
  graph = graph_info[c("region_id", "n", "islands", "components", "directed_links", "mean_neighbors")],
  priors = list(
    precision = "PC: P(sigma > 1) = 0.01; param c(1,0.01)",
    mixing = "PC: P(phi < 0.5) = 0.5; param c(0.5,0.5)"
  ),
  likelihood = list(
    family = "tweedie",
    role = "nonnegative continuous accessible-slot likelihood with zero mass",
    response_sum = sum(response),
    integer_response_rows = sum(abs(response - round(response)) < 1e-10),
    noninteger_response_rows = sum(abs(response - round(response)) >= 1e-10),
    reason = "fixed E2SFCA allocation yields continuous expected slot equivalents and structural zeros; no arbitrary rounding"
  ),
  hyperparameters = hyper,
  diagnostics = list(
    spearman_input_vs_posterior = spearman,
    pearson_input_vs_posterior = pearson,
    dic = fit$dic$dic,
    waic = fit$waic$waic,
    n_cpo_failures = sum(!is.finite(fit$cpo$cpo) | fit$cpo$cpo <= 0),
    inla_version = as.character(utils::packageVersion("INLA")),
    num_threads = "1:1"
  ),
  input_sha256 = list(
    demand = file_sha256(p07_path("data", "analytic", "01_demand_uncertainty.rds")),
    supply = file_sha256(p07_path("data", "analytic", "01_supply_uncertainty.rds")),
    prereg = prereg$choices_sha256,
    weights = file_sha256(file.path(P01_ANALYTIC, "05_spatial_weights.rds"))
  )
)
save_analytic(fit_bundle, "02_bym2_fit.rds")

diagnostic_table <- data.frame(
  metric = c(
    "model_nodes", "model_islands", "model_components", "directed_links",
    "mean_neighbors", "response_sum", "noninteger_response_rows",
    "spearman_input_posterior", "pearson_input_posterior", "DIC", "WAIC",
    "CPO_failures"
  ),
  value = c(
    graph_info$n, graph_info$islands, graph_info$components,
    graph_info$directed_links, graph_info$mean_neighbors, sum(response),
    fit_bundle$likelihood$noninteger_response_rows, spearman, pearson,
    fit$dic$dic, fit$waic$waic, fit_bundle$diagnostics$n_cpo_failures
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(diagnostic_table, p07_path("outputs", "tables", "02_bym2_diagnostics.csv"), row.names = FALSE, na = "")
utils::write.csv(hyper, p07_path("outputs", "tables", "02_bym2_hyperparameters.csv"), row.names = FALSE, na = "")
utils::write.csv(data.frame(
  plan_item = "response likelihood",
  planned = "Poisson count likelihood on accessible-slot allocation",
  implemented = "Tweedie compound Poisson-Gamma with log-demand offset",
  trigger = paste0(
    "INLA 26.6.8 rejects noninteger Poisson y; ",
    fit_bundle$likelihood$noninteger_response_rows,
    " of 1,409 model responses are fractional and structural zeros are present"
  ),
  rejected_workaround = "round fractional accessible-slot equivalents to integer counts",
  scientific_reason = "Tweedie preserves nonnegative continuous allocations, exact zeros, and exposure scaling without arbitrary data alteration",
  stringsAsFactors = FALSE
), p07_path("outputs", "tables", "02_model_deviation_register.csv"), row.names = FALSE, na = "")

append_key_numbers(data.frame(
  key = c("bym2_phi_post_mean", "bym2_sigma_post_mean", "n_graph_islands", "bym2_likelihood_role"),
  value = c(
    hyper$mean[hyper$parameter == "phi"],
    hyper$mean[hyper$parameter == "sigma"],
    graph_info$islands,
    "Tweedie log-link with log-demand offset"
  ),
  unit = c("proportion", "log-rate SD", "islands in 1,409-node graph", "likelihood"),
  source_script = "scripts/02-1_fit_bym2.R",
  note = c(
    "Posterior BYM2 structured-variance fraction.",
    "Posterior mean obtained by transforming the precision marginal, not inverse square root of mean precision.",
    "The full 1,436-node graph has two; one is zero-demand and excluded.",
    "Accessible-slot allocations are nonnegative continuous values with zeros and are not rounded."
  ), stringsAsFactors = FALSE
))

cat("Step 2.1 BYM2 model: PASS\n")
