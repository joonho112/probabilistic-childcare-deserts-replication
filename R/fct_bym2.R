# ============================================================================
# fct_bym2.R -- Bayesian spatial coverage model (BYM2) for P07: graph, fit,
#               hyperparameters, and reproducible posterior sampling.
# ----------------------------------------------------------------------------
# Purpose
#   Fits an intercept-only areal model of child-care coverage over Alabama
#   census tracts and draws reproducible posterior predictors that downstream
#   steps turn into desert probabilities and FDR/FDX declarations.
#
# Model
#   y_i ~ Tweedie(mu_i), log mu_i = beta0 + log(demand_i) + b_i, where b_i is a
#   BYM2 spatial random effect. BYM2 (Riebler et al. 2016) splits the effect into
#   a scaled ICAR (spatially structured) and an IID (unstructured) part with a
#   single marginal sd sigma and a mixing proportion phi in [0, 1]:
#     b = sigma * ( sqrt(phi) * u_scaled + sqrt(1 - phi) * v ).
#   scale.model = TRUE fixes the ICAR to unit generalized variance so sigma and
#   the priors are interpretable. Penalized-complexity (PC) priors
#   (Simpson et al. 2017): P(sigma > 1) = 0.01 (prec: pc.prec, param c(1, 0.01))
#   and P(phi < 0.5) = 0.5 (phi: pc, param c(0.5, 0.5)). The Tweedie (compound
#   Poisson-gamma) likelihood with a log link and offset(log(demand)) models a
#   nonnegative, zero-inflated coverage rate. Estimation is by INLA
#   (Rue et al. 2009).
#
# Determinism contract
#   INLA calls an external binary that can otherwise inherit multithreaded
#   BLAS/OpenMP ordering. Every fit/sample forces single-thread execution
#   (OMP/OPENBLAS/MKL/VECLIB/NUMEXPR = 1, num.threads = "1:1") under a fixed seed
#   (20260716) so clean reruns reproduce bit-for-bit.
#
# Graph
#   prepare_p07_graph() builds a Queen-contiguity neighbor graph on the 1,409
#   nonzero-demand tracts by subsetting the full graph, converts it to a sparse
#   binary adjacency, and reads it into an INLA graph object.
#
# Key functions
#   prepare_p07_graph()             Queen graph -> INLA graph (+ diagnostics)
#   fit_coverage_bym2()             fit the intercept-only Tweedie BYM2 model
#   extract_bym2_hyperparameters()  precision marginal -> sigma, plus phi
#   posterior_predictor_matrix()    pull the Predictor block from samples
#   sample_predictors_batched()     reproducible batched posterior draws
#
# References
#   Riebler, Sorbye, Simpson & Rue (2016), Stat. Methods Med. Res. 25(4), 1145.
#   Simpson, Rue, Riebler, Martins & Sorbye (2017), Statist. Sci. 32(1), 1-28.
#   Rue, Martino & Chopin (2009), J. R. Stat. Soc. B 71(2), 319-392 [INLA].
# ============================================================================

# Build the modeling neighbor graph: subset the full Queen-contiguity graph to
# the eligible (nonzero-demand) tracts and hand INLA a sparse binary adjacency.
prepare_p07_graph <- function(full_nb, demand_geoid, eligible) {
  assert_that(requireNamespace("spdep", quietly = TRUE), "Package 'spdep' is required.")
  assert_that(requireNamespace("INLA", quietly = TRUE), "Package 'INLA' is required.")
  assert_that(length(full_nb) == length(demand_geoid) && length(eligible) == length(demand_geoid), "Graph/demand lengths differ.")
  assert_that(identical(attr(full_nb, "region.id"), demand_geoid), "Full Queen graph region IDs/order mismatch.")
  # Restrict the full graph to the eligible (nonzero-demand) tracts.
  model_nb <- spdep::subset.nb(full_nb, subset = eligible)
  # Binary contiguity matrix (style = "B"); zero.policy tolerates islands.
  binary <- spdep::nb2mat(model_nb, style = "B", zero.policy = TRUE)
  sparse <- Matrix::Matrix(binary, sparse = TRUE)
  graph <- INLA::inla.read.graph(sparse)
  list(
    nb = model_nb,
    adjacency = sparse,
    inla_graph = graph,
    region_id = attr(model_nb, "region.id"),
    n = length(model_nb),
    islands = sum(spdep::card(model_nb) == 0L),
    components = spdep::n.comp.nb(model_nb)$nc,
    directed_links = sum(spdep::card(model_nb)),
    mean_neighbors = mean(spdep::card(model_nb))
  )
}

# Fit the intercept-only Tweedie BYM2 coverage model with INLA under the strict
# single-thread determinism contract.
fit_coverage_bym2 <- function(y, exposure, graph_info) {
  assert_that(length(y) == graph_info$n && length(exposure) == graph_info$n, "Model vectors/graph differ.")
  assert_that(all(is.finite(y)) && all(y >= 0), "Accessible-slot equivalents are invalid.")
  assert_that(all(is.finite(exposure)) && all(exposure > 0), "Exposure must be positive.")
  model_data <- data.frame(
    y = as.numeric(y), exposure = as.numeric(exposure), idx = seq_len(graph_info$n)
  )
  # Linear predictor: intercept + log-demand offset + BYM2 field. constr = TRUE
  # imposes the sum-to-zero identifiability constraint, adjust.for.con.comp applies
  # it per connected component, and scale.model = TRUE standardizes the ICAR part.
  formula <- y ~ 1 + offset(log(exposure)) + f(
    idx,
    model = "bym2",
    graph = graph_info$inla_graph,
    scale.model = TRUE,
    adjust.for.con.comp = TRUE,
    constr = TRUE,
    # PC priors: P(sigma > 1) = 0.01 shrinks the marginal sd, and P(phi < 0.5) =
    # 0.5 is neutral between structured and unstructured spatial variation.
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),
      phi = list(prior = "pc", param = c(0.5, 0.5))
    )
  )
  # Deterministic clean-process contract. INLA runs an external binary and can
  # otherwise inherit multithreaded BLAS/OpenMP ordering even when the outer
  # `num.threads` argument is serial.
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  INLA::inla.setOption(num.threads = "1:1", blas.num.threads = 1L)
  # Fixed seed for the integration/optimization path.
  set.seed(20260716L)
  # Tweedie (compound Poisson-gamma) likelihood, log link; the offset is carried
  # in the formula. config = TRUE stores the joint configuration needed for
  # posterior sampling, and DIC/WAIC/CPO are retained as fit diagnostics.
  fit <- INLA::inla(
    formula,
    family = "tweedie",
    data = model_data,
    control.predictor = list(compute = TRUE),
    control.compute = list(
      config = TRUE,
      return.marginals.predictor = TRUE,
      dic = TRUE,
      waic = TRUE,
      cpo = TRUE
    ),
    # Hyperparameter integration: adaptive Gaussian strategy on a central-
    # composite-design (ccd) grid; metis reordering speeds the sparse solve.
    control.inla = list(
      strategy = "adaptive", int.strategy = "ccd",
      stupid.search = FALSE, reordering = "metis"
    ),
    # Serial nested execution prevents thread-order floating-point drift from
    # changing the fitted integration configuration across clean reruns.
    num.threads = "1:1",
    verbose = FALSE
  )
  list(fit = fit, formula = formula, model_data = model_data)
}

# Summarize the BYM2 hyperparameters. INLA reports the precision of the field;
# sigma is its reciprocal square root, obtained by transforming the marginal.
extract_bym2_hyperparameters <- function(fit) {
  hyper <- fit$summary.hyperpar
  precision_name <- grep("^Precision for", rownames(hyper), value = TRUE)
  phi_name <- grep("^Phi for", rownames(hyper), value = TRUE)
  assert_that(length(precision_name) == 1L && length(phi_name) == 1L, "Could not identify BYM2 hyperparameters.")
  precision_marginal <- fit$marginals.hyperpar[[precision_name]]
  # Transform the precision marginal to the sd marginal, sigma = 1/sqrt(prec), so
  # sigma's mean and quantiles are exact rather than plug-in from the precision.
  sigma_marginal <- INLA::inla.tmarginal(function(x) 1 / sqrt(x), precision_marginal)
  data.frame(
    parameter = c("sigma", "phi", "precision"),
    mean = c(
      INLA::inla.emarginal(function(x) x, sigma_marginal),
      hyper[phi_name, "mean"], hyper[precision_name, "mean"]
    ),
    q025 = c(
      INLA::inla.qmarginal(0.025, sigma_marginal),
      hyper[phi_name, "0.025quant"], hyper[precision_name, "0.025quant"]
    ),
    q50 = c(
      INLA::inla.qmarginal(0.5, sigma_marginal),
      hyper[phi_name, "0.5quant"], hyper[precision_name, "0.5quant"]
    ),
    q975 = c(
      INLA::inla.qmarginal(0.975, sigma_marginal),
      hyper[phi_name, "0.975quant"], hyper[precision_name, "0.975quant"]
    ),
    stringsAsFactors = FALSE
  )
}

# Extract the linear-predictor block from a list of posterior samples, returning
# an (n_predictor x n_samples) matrix.
posterior_predictor_matrix <- function(samples, n_predictor) {
  assert_that(length(samples) >= 1L, "No posterior samples supplied.")
  one_names <- rownames(samples[[1L]]$latent)
  # The latent vector concatenates several effects; keep only the "Predictor:"
  # rows, i.e. the fitted linear predictor for each tract.
  predictor_rows <- grep("^Predictor:", one_names)
  assert_that(length(predictor_rows) == n_predictor, "Posterior sample predictor block has unexpected length.")
  out <- vapply(
    samples,
    function(s) as.numeric(s$latent[predictor_rows, 1L]),
    numeric(n_predictor)
  )
  if (is.null(dim(out))) out <- matrix(out, nrow = n_predictor)
  out
}

# Draw posterior linear predictors in fixed-size batches, re-seeding both R and
# INLA per batch so the full set of draws is reproducible regardless of batching.
sample_predictors_batched <- function(fit, n_predictor, n_draws, seed = 20260716L, batch_size = 100L) {
  assert_that(n_draws >= 1L && batch_size >= 1L, "Posterior draw and batch counts must be positive.")
  Sys.setenv(
    OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1", NUMEXPR_NUM_THREADS = "1"
  )
  INLA::inla.setOption(num.threads = "1:1", blas.num.threads = 1L)
  out <- matrix(NA_real_, nrow = n_predictor, ncol = n_draws)
  # Batch start indices; each batch draws at most batch_size samples.
  starts <- seq.int(1L, n_draws, by = batch_size)
  for (b in seq_along(starts)) {
    first <- starts[b]
    last <- min(n_draws, first + batch_size - 1L)
    # Deterministic per-batch seed (base seed offset by the batch index).
    batch_seed <- as.integer(seed + b - 1L)
    # INLA documents that both its internal seed and R's RNG state must be
    # reset for reproducible posterior samples. Keep configurations serial.
    set.seed(batch_seed)
    samples <- INLA::inla.posterior.sample(
      last - first + 1L,
      fit,
      seed = batch_seed,
      num.threads = "1:1",
      parallel.configs = FALSE
    )
    # Write this batch's predictor columns into their slice of the output.
    out[, first:last] <- posterior_predictor_matrix(samples, n_predictor)
    rm(samples)
    invisible(gc(FALSE))
  }
  out
}
