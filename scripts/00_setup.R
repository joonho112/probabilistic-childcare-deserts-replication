#!/usr/bin/env Rscript
# =============================================================================
# 00_setup.R : check the environment and load the packages this package needs
# =============================================================================
#
# Source this once at the start of a session:
#
#     source("scripts/00_setup.R")
#
# It does three things:
#   1. checks the R version floor,
#   2. reports which required packages are installed (and, if you opt in,
#      installs the missing CRAN ones), and
#   3. prints the environment variables that point the code at the inputs it
#      cannot ship (see data/DATA_ACCESS.md).
#
# There are two reproduction tracks; you only need the packages for the one you
# are running (Chapter 2 of the guide explains the choice):
#
#   * Track C  -- rebuild the figures/tables from the shipped aggregate layer
#                 (data/derived/). CRAN only; no INLA, no restricted data.
#   * Track A  -- rerun the whole pipeline from the restricted inputs. Adds INLA
#                 (the Bayesian spatial fit) and the spatial stack.
#
# Nothing here is required to simply READ the code and the results.
# =============================================================================

# ---- 1. R version floor -----------------------------------------------------
if (getRversion() < "4.3") {
  stop("This package targets R >= 4.3 (the authors ran R 4.6.0). Please upgrade.",
       call. = FALSE)
}

# ---- 2. Packages, by track --------------------------------------------------
# Track C (reporting/reproduction of exhibits) -- all on CRAN.
pkgs_track_c <- c("sf", "dplyr", "tidyr", "readr", "ggplot2", "patchwork",
                  "scales", "gt", "leaflet", "htmlwidgets", "digest")

# Track A additionally needs the modeling stack. INLA is NOT on CRAN; install it
# from its own repository (see the note printed below and data/DATA_ACCESS.md).
pkgs_track_a_extra <- c("spdep", "Matrix", "truncnorm", "readxl", "furrr",
                        "future", "INLA")

want <- unique(c(pkgs_track_c, pkgs_track_a_extra))
installed <- vapply(want, requireNamespace, logical(1), quietly = TRUE)

cat("P07 replication -- environment check\n")
cat(sprintf("  R %s (floor 4.3, authors used 4.6.0)\n", getRversion()))
cat("  package status (Track C needs the first group; Track A adds the second):\n")
for (p in want) cat(sprintf("    [%s] %s\n", ifelse(installed[[p]], "x", " "), p))

missing_cran <- setdiff(names(installed)[!installed], "INLA")
if (length(missing_cran) &&
    isTRUE(as.logical(Sys.getenv("P07_INSTALL_MISSING", "FALSE")))) {
  message("Installing missing CRAN packages: ", paste(missing_cran, collapse = ", "))
  utils::install.packages(missing_cran)
}
if (!installed[["INLA"]]) {
  cat("\n  Note: INLA is only needed for Track A and is not on CRAN. Install with:\n",
      '    install.packages("INLA",\n',
      '      repos = c(INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)\n',
      "  The authors used INLA 26.6.8. See data/DATA_ACCESS.md.\n", sep = "")
}

# ---- 3. Environment variables the pipeline reads ----------------------------
# These have sensible defaults; set them only if your inputs live elsewhere.
cat("\n  environment variables (Track A only; defaults in brackets):\n")
cat("    P07_CODEBASE_ROOT   [getwd()]                 this package's root\n")
cat("    P01_CODEBASE_ROOT   [../codebase-P01]         the built companion P01 codebase\n")
cat("    P07_DHR_SURVEY_DIR  [restricted-data/dhr_surveys]  the four quarterly survey workbooks\n")
cat("\n  Ready. See _run_order.md for the canonical run order.\n")

invisible(TRUE)
