#!/usr/bin/env Rscript
# =============================================================================
# 99_reproduce_all.R : run the full analysis pipeline end to end (Track A)
# =============================================================================
#
# This orchestrator runs the eleven numbered pipeline scripts in order, each in
# a FRESH Rscript subprocess (so no state leaks between steps), and records the
# status of each step to results/tables/99_reproduction_status.csv. It is
# fail-closed: the first nonzero exit stops the whole run.
#
# Track A requires the RESTRICTED inputs (a built companion P01 codebase and the
# four DHR quarterly survey workbooks) and the INLA toolchain. It cannot run
# from a clean clone -- see data/DATA_ACCESS.md to obtain the inputs, then set
# P01_CODEBASE_ROOT and P07_DHR_SURVEY_DIR. To reproduce only the figures and
# tables WITHOUT the restricted data, use scripts/reproduce_exhibits.R (Track C)
# instead.
#
# Run from the package root:
#     Rscript scripts/99_reproduce_all.R
# =============================================================================

options(stringsAsFactors = FALSE)

# fct_io.R resolves paths and enforces that we are at the package root
# (README.md and _run_order.md must be present).
source(file.path("R", "fct_io.R"))

pipeline <- c(
  "scripts/00-2_audit_sources.R",       # audit inputs, graph, and environment
  "scripts/01-1_demand_uncertainty.R",  # ACS demand MOE -> sampling distribution
  "scripts/01-2_supply_uncertainty.R",  # survey-calibrated capacity error + fixed operator
  "scripts/01-3_preregister_decisions.R", # freeze + hash the primary specification
  "scripts/02-1_fit_bym2.R",            # fit the Tweedie BYM2 coverage model (INLA)
  "scripts/02-2_posterior_draws.R",     # modular joint posterior Monte Carlo
  "scripts/02-3_desert_probability.R",  # tract desert probability and LIS
  "scripts/03-1_sun_fdr.R",             # LIS step-up FDR, FDX, county selection
  "scripts/03-2_compare_p01.R",         # 2x2 vs the deterministic P01 map
  "scripts/03-3_sensitivity.R",         # preregistered sensitivity + triangulation
  "scripts/04-1_visualize.R"            # figures, tables, and the aggregate leaflet
)
if (!all(file.exists(pipeline))) {
  stop("Reproduction manifest contains a missing path: ",
       paste(pipeline[!file.exists(pipeline)], collapse = ", "), call. = FALSE)
}

rscript     <- file.path(R.home("bin"), "Rscript")
status_path <- p07_path("results", "tables", "99_reproduction_status.csv")
dir.create(dirname(status_path), recursive = TRUE, showWarnings = FALSE)
status_rows <- vector("list", length(pipeline))

for (i in seq_along(pipeline)) {
  script <- pipeline[[i]]
  message(sprintf("[%02d/%02d] %s", i, length(pipeline), script))
  started <- Sys.time()
  status  <- system2(rscript, shQuote(script))
  ended   <- Sys.time()
  status_rows[[i]] <- data.frame(
    sequence      = i,
    step          = script,
    started_utc   = format(started, tz = "UTC", usetz = TRUE),
    ended_utc     = format(ended, tz = "UTC", usetz = TRUE),
    elapsed_secs  = as.numeric(difftime(ended, started, units = "secs")),
    exit_status   = status,
    result        = ifelse(status == 0L, "pass", "fail"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(do.call(rbind, status_rows[seq_len(i)]), status_path, row.names = FALSE, na = "")
  if (status != 0L) stop("Pipeline failed at: ", script, call. = FALSE)
}

message("\nPipeline complete. Now rebuild the disclosure-safe layer and verify:")
message("  Rscript scripts/build_derived.R      # refresh data/derived/")
message("  Rscript manifest/verify_outputs.R    # check outputs against key_numbers.csv")
