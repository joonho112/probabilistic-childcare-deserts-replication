#!/usr/bin/env Rscript
# =============================================================================
# reproduce_exhibits.R : rebuild the paper's figures from the shipped aggregate
#                        layer -- the "no restricted data" reproduction (Track C)
# =============================================================================
#
# What this does : reads ONLY the disclosure-safe aggregate layer in
#   data/derived/ (the tract/county result tables and the public simplified
#   tract geometry) and regenerates the six analysis figures F1-F6 into
#   results/figures/. It needs no confidential data, no Census API key, and no
#   spatial/Bayesian toolchain (sf for plotting only; no INLA) -- just a reader
#   who cloned the repository.
#
# How to read the result : compare results/figures/ against the canonical
#   outputs/figures/ shipped by the authors. They are built from the same
#   numbers; the only difference is that results/ uses the simplified public
#   geometry, so tract borders are very slightly coarser. The manuscript's
#   headline values all trace to outputs/key_numbers.csv (see verification/).
#
# Run : Rscript scripts/reproduce_exhibits.R      (from the package root)
# =============================================================================

options(stringsAsFactors = FALSE)
suppressWarnings(suppressMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(scales)
}))

# Palette and themes only -- fct_viz.R defines P07_COLORS, p07_theme(),
# p07_theme_map(). (Its save_p07_figure() helper is not used here; we ggsave to
# results/ directly so the shipped outputs/ reference is never overwritten.)
source(file.path("R", "fct_viz.R"))

derived <- "data/derived"
stopifnot("Run scripts/build_derived.R first (data/derived/ is missing)." =
            dir.exists(derived) && file.exists(file.path(derived, "tract_results.csv")))
dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

# Force the id columns to character so the leading zeros in GEOIDs and county
# FIPS codes survive the read (01001... must not become 1001...).
tracts  <- utils::read.csv(file.path(derived, "tract_results.csv"),
                           colClasses = c(GEOID = "character", county_fips = "character"))
county  <- utils::read.csv(file.path(derived, "county_results.csv"),
                           colClasses = c(county_fips = "character"))
gamma_s <- utils::read.csv(file.path(derived, "sensitivity_gamma.csv"))
weight_s <- utils::read.csv(file.path(derived, "sensitivity_county_weight.csv"))
triang  <- utils::read.csv(file.path(derived, "triangulation.csv"))

geom <- sf::st_read(file.path(derived, "al_tracts_2020_simplified.geojson"), quiet = TRUE)
geom$GEOID <- as.character(geom$GEOID)
geom$county_fips <- formatC(as.integer(geom$county_fips), width = 5, flag = "0")
geom$display_allowed <- as.logical(geom$display_allowed)

# ---- Assemble the tract map frame (mirror of scripts/04-1_visualize.R) ------
map_data <- geom |>
  dplyr::select("GEOID", "county_fips", "county_name", "display_allowed") |>
  dplyr::left_join(
    tracts |>
      dplyr::select("GEOID", "desert_probability", "fdr_desert", "fdx_desert",
                    "point_desert_e2sfca", "comparison_class"),
    by = "GEOID"
  ) |>
  dplyr::mutate(
    posterior_display = ifelse(.data$display_allowed, .data$desert_probability, NA_real_),
    fdr_display = factor(
      ifelse(!.data$display_allowed, "Suppressed",
             ifelse(.data$fdr_desert, "FDR desert", "Not declared")),
      levels = c("FDR desert", "Not declared", "Suppressed")),
    fdx_display = factor(
      ifelse(!.data$display_allowed, "Suppressed",
             ifelse(.data$fdx_desert, "FDX desert", "Not declared")),
      levels = c("FDX desert", "Not declared", "Suppressed")),
    comparison_display = factor(
      ifelse(!.data$display_allowed, "Suppressed", as.character(.data$comparison_class)),
      levels = c("both_point_and_fdr", "point_only", "fdr_only", "neither", "Suppressed"))
  )
masked_n <- sum(!map_data$display_allowed)
stopifnot("Expected 46 suppressed tracts." = masked_n == 46L)
comparison_counts <- table(map_data$comparison_class)

caption_common <- paste0(
  "Source: P01 fixed E2SFCA infrastructure and P07 posterior analysis.\n",
  "Rebuilt from the shipped aggregate layer. Tracts with fewer than 10 children ",
  "or zero demand are suppressed (n=", masked_n, ")."
)
ggsave2 <- function(plot, stem, width, height) {
  path <- file.path("results", "figures", paste0(stem, ".png"))
  ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = 300, bg = "white")
  cat(sprintf("  rebuilt %-32s\n", basename(path)))
}

# ---- F1: continuous posterior desert probability ----------------------------
f1 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$posterior_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::scale_fill_gradientn(
    colors = c(P07_COLORS$blue_xlight, P07_COLORS$blue_light, P07_COLORS$blue, P07_COLORS$blue_dark),
    limits = c(0, 1), breaks = c(0, .25, .5, .75, 1),
    labels = scales::label_percent(accuracy = 1), na.value = P07_COLORS$masked, name = "Desert probability") +
  ggplot2::labs(title = "Tract posterior desert probability",
                subtitle = "P(coverage < 0.33); 1,409 nonzero-demand Alabama tracts",
                caption = caption_common) + p07_theme_map(10)
ggsave2(f1, "F1_desert_probability", 8.2, 6.4)

outline <- map_data[map_data$display_allowed & map_data$point_desert_e2sfca %in% TRUE, ]

# ---- F2: FDR declarations (P01 point deserts outlined) ----------------------
f2 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$fdr_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::geom_sf(data = outline, fill = NA, color = P07_COLORS$ink, linewidth = 0.20) +
  ggplot2::scale_fill_manual(
    values = c("FDR desert" = P07_COLORS$orange, "Not declared" = "#F3F4F6", "Suppressed" = P07_COLORS$masked),
    drop = FALSE, name = NULL) +
  ggplot2::labs(title = "Tract FDR desert declarations",
                subtitle = "LIS step-up q=0.10; dark outlines mark P01 point deserts",
                caption = caption_common) + p07_theme_map(10)
ggsave2(f2, "F2_fdr_declarations", 8.2, 6.4)

# ---- F3: FDX headline declarations ------------------------------------------
f3 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$fdx_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::geom_sf(data = outline, fill = NA, color = P07_COLORS$ink, linewidth = 0.20) +
  ggplot2::scale_fill_manual(
    values = c("FDX desert" = P07_COLORS$blue, "Not declared" = "#F3F4F6", "Suppressed" = P07_COLORS$masked),
    drop = FALSE, name = NULL) +
  ggplot2::labs(title = "Tract FDX headline declarations",
                subtitle = "Empirical joint-posterior P(FDP>0.10) <= 0.05; dark outlines mark P01 point deserts",
                caption = caption_common) + p07_theme_map(10)
ggsave2(f3, "F3_fdx_declarations", 8.2, 6.4)

# ---- F4: reclassification vs the deterministic map --------------------------
comparison_labels <- c(both_point_and_fdr = "Both point and FDR", point_only = "Point only",
                       fdr_only = "FDR only", neither = "Neither", Suppressed = "Suppressed")
f4 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$comparison_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::scale_fill_manual(
    values = c(both_point_and_fdr = P07_COLORS$blue, point_only = P07_COLORS$gold,
               fdr_only = P07_COLORS$orange, neither = "#F3F4F6", Suppressed = P07_COLORS$masked),
    labels = comparison_labels, drop = FALSE, name = NULL) +
  ggplot2::labs(title = "P01 point map and P07 FDR reclassification",
                subtitle = paste0(comparison_counts[["both_point_and_fdr"]], " both, ",
                                  comparison_counts[["point_only"]], " point-only, ",
                                  comparison_counts[["fdr_only"]], " FDR-only, and ",
                                  comparison_counts[["neither"]], " neither"),
                caption = caption_common) + p07_theme_map(10)
ggsave2(f4, "F4_point_fdr_reclassification", 8.2, 6.4)

# ---- F5: child-weighted county declarations ---------------------------------
county_geometry <- map_data |>
  dplyr::group_by(.data$county_fips, .data$county_name) |>
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
  dplyr::left_join(county |> dplyr::select("county_fips", "county_fdr_desert"), by = "county_fips") |>
  dplyr::mutate(county_status = ifelse(.data$county_fdr_desert, "Declared", "Not declared"))
stopifnot("Expected 13 declared counties." = sum(county_geometry$county_fdr_desert) == 13L)
f5 <- ggplot2::ggplot(county_geometry) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$county_status), color = "#FFFFFF", linewidth = 0.35) +
  ggplot2::scale_fill_manual(values = c("Declared" = P07_COLORS$orange, "Not declared" = "#E5E7EB"), name = NULL) +
  ggplot2::labs(title = "Child-weighted county desert declarations",
                subtitle = paste0(sum(county_geometry$county_fdr_desert), " of ", nrow(county_geometry),
                                  " counties selected by under-five-weighted tract LIS at q=0.10"),
                caption = "County declaration uses child-weighted tract LIS.\nBoundaries are county aggregates and contain no provider points.") +
  p07_theme_map(10)
ggsave2(f5, "F5_county_declarations", 8.2, 6.4)

# ---- F6: preregistered sensitivity + triangulation (three panels) -----------
p6a <- ggplot2::ggplot(gamma_s, ggplot2::aes(x = .data$gamma, y = .data$n_fdr)) +
  ggplot2::geom_line(color = P07_COLORS$blue_dark, linewidth = 0.8) +
  ggplot2::geom_point(ggplot2::aes(fill = .data$role), shape = 21, size = 3, color = P07_COLORS$blue_dark) +
  ggplot2::geom_text(ggplot2::aes(label = .data$n_fdr), vjust = -0.8, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_fill_manual(values = c(primary_locked = P07_COLORS$blue, sensitivity_only = "white"), guide = "none") +
  ggplot2::scale_x_continuous(breaks = gamma_s$gamma) +
  ggplot2::scale_y_continuous(limits = c(0, max(gamma_s$n_fdr) * 1.15), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "FDR declarations by strict buffer", subtitle = "Filled point is primary", x = "Gamma", y = "Tracts") + p07_theme(9)

weight_plot <- weight_s |>
  dplyr::mutate(weight_scheme = factor(.data$weight_scheme, levels = c("under5", "equal", "area"),
                                       labels = c("Under-five", "Equal tract", "Area")))
p6b <- ggplot2::ggplot(weight_plot, ggplot2::aes(x = .data$weight_scheme, y = .data$n_declared)) +
  ggplot2::geom_col(ggplot2::aes(fill = .data$role), width = .65, color = P07_COLORS$ink, linewidth = .25) +
  ggplot2::geom_text(ggplot2::aes(label = .data$n_declared), vjust = -0.5, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_fill_manual(values = c(primary_locked = P07_COLORS$orange, sensitivity_only = P07_COLORS$orange_light), guide = "none") +
  ggplot2::scale_y_continuous(limits = c(0, max(weight_plot$n_declared) * 1.18), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "County declarations by weight", subtitle = "Primary uses child exposure", x = NULL, y = "Counties") + p07_theme(9)

tri_plot <- data.frame(
  set = factor(c("P01 always-desert", "In P07 FDR", "In P07 FDX"),
               levels = c("P01 always-desert", "In P07 FDR", "In P07 FDX")),
  tracts = c(519, triang$n_in_fdr, triang$n_in_fdx))
p6c <- ggplot2::ggplot(tri_plot, ggplot2::aes(x = .data$set, y = .data$tracts)) +
  ggplot2::geom_col(fill = c(P07_COLORS$gold_light, P07_COLORS$blue_light, P07_COLORS$blue), color = P07_COLORS$ink, linewidth = .25, width = .65) +
  ggplot2::geom_text(ggplot2::aes(label = .data$tracts), vjust = -0.5, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_y_continuous(limits = c(0, 590), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "Triangulation of the 519 anchor", subtitle = "Intersection, not nested sets", x = NULL, y = "Tracts") +
  p07_theme(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

f6 <- p6a + p6b + p6c + patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    title = "Preregistered sensitivity and stability triangulation",
    subtitle = "Primary choices are filled/darker; alternatives remain sensitivity-only",
    caption = "Source: P07 joint posterior draws and P01 always-desert stability anchor.")
ggsave2(f6, "F6_sensitivity_triangulation", 12, 4.8)

cat("\nreproduce_exhibits.R: rebuilt 6 figures into results/figures/ from data/derived/ only.\n")
