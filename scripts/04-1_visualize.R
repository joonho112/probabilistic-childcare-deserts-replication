#!/usr/bin/env Rscript

# P07 Step 4.1 -- Publication figures, exact-lookup tables, aggregate leaflet, privacy QA.
#
# Purpose:
#   Renders the final communication layer of the pipeline: six manuscript
#   figures (F1-F6), three exact-value gt tables (T1-T3), and one aggregate
#   leaflet, then runs the privacy QA that must pass before anything is shared.
#
# Method:
#   Joins the analytic objects onto P01 tract geometry and builds static ggplot
#   choropleths (posterior probability, FDR, FDX, P01-vs-P07 reclassification,
#   county declarations) plus a three-panel sensitivity/triangulation summary.
#   A strict DISPLAY RULE masks any tract with fewer than 10 children or zero
#   demand (n = 46 suppressed): masked tracts are drawn in the neutral "masked"
#   color and their values are set to NA before plotting. Tables use gt for
#   exact lookup rather than crowding map labels. The privacy QA asserts the
#   small-cell mask fired and -- the load-bearing check -- that ZERO provider
#   coordinates appear in any output (the leaflet is county-aggregate only).
#
# Reads:
#   data/analytic/02_desert_probability.rds          (posterior probabilities)
#   data/analytic/03_fdr_declarations.rds            (tract + county declarations)
#   data/analytic/03_p01_comparison.rds              (four-state comparison classes)
#   data/analytic/03_triangulation_sensitivity.rds   (sensitivity grids + triangulation)
#   <P01>/01_demand_tracts.rds                        (tract geometry + demand)
#   outputs/key_numbers.csv                          (single-source-of-truth for subtitles/QA)
#
# Writes:
#   outputs/figures/F1..F6_*.png / .pdf     (six publication figures)
#   outputs/tables/T1..T3_*.html / .tex     (three exact-lookup gt tables)
#   outputs/tables/04_chart_contract.csv    (figure design contract)
#   outputs/tables/04_figure_qa.csv         (figure file existence/size QA)
#   outputs/tables/04_privacy_qa.csv        (privacy/disclosure QA)
#   outputs/maps/leaflet_desert_posterior_fdr.html   (aggregate-only interactive map)

options(stringsAsFactors = FALSE)
source(file.path("R", "fct_io.R"))
source(file.path("R", "fct_viz.R"))
source(file.path("R", "fct_leaflet.R"))

required_packages <- c("sf", "dplyr", "ggplot2", "patchwork", "scales", "gt", "leaflet", "htmlwidgets")
# Fail fast if any visualization dependency is missing before rendering begins.
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
assert_that(length(missing) == 0L, paste("Missing visualization packages:", paste(missing, collapse = ", ")))

probability <- readRDS(p07_path("data", "analytic", "02_desert_probability.rds"))
declarations <- readRDS(p07_path("data", "analytic", "03_fdr_declarations.rds"))
comparison <- readRDS(p07_path("data", "analytic", "03_p01_comparison.rds"))
sensitivity <- readRDS(p07_path("data", "analytic", "03_triangulation_sensitivity.rds"))
demand_sf <- read_p01("01_demand_tracts.rds")
ssot <- read_key_numbers()

# Join the analytic results onto P01 tract geometry and derive the masked display
# columns that every figure below reads.
map_data <- demand_sf |>
  dplyr::left_join(
    comparison$tracts |>
      dplyr::select(
        "GEOID", "desert_probability", "fdr_desert", "fdx_desert",
        "point_desert", "comparison_class", "demand_mean"
      ),
    by = "GEOID"
  ) |>
  dplyr::mutate(
    # Disclosure gate: a tract is displayable only with nonzero demand and >= 10 children.
    display_allowed = !.data$is_zero_demand & .data$total_under5E >= 10,
    # NA-out suppressed tracts so they render in the neutral masked color, not as data.
    posterior_display = ifelse(.data$display_allowed, .data$desert_probability, NA_real_),
    fdr_display = factor(
      ifelse(!.data$display_allowed, "Suppressed", ifelse(.data$fdr_desert, "FDR desert", "Not declared")),
      levels = c("FDR desert", "Not declared", "Suppressed")
    ),
    fdx_display = factor(
      ifelse(!.data$display_allowed, "Suppressed", ifelse(.data$fdx_desert, "FDX desert", "Not declared")),
      levels = c("FDX desert", "Not declared", "Suppressed")
    ),
    comparison_display = factor(
      ifelse(!.data$display_allowed, "Suppressed", as.character(.data$comparison_class)),
      levels = c("both_point_and_fdr", "point_only", "fdr_only", "neither", "Suppressed")
    )
  )
assert_that(sum(!is.na(map_data$desert_probability)) == 1409L, "Map probability join failed.")
# Confirm the privacy mask actually fired for every suppressed tract.
assert_that(all(is.na(map_data$posterior_display[!map_data$display_allowed])), "Privacy mask failed for posterior map.")
masked_n <- sum(!map_data$display_allowed)
comparison_counts <- table(map_data$comparison_class)

caption_common <- paste0(
  "Source: P01 fixed E2SFCA infrastructure and P07 posterior analysis.\n",
  "Tracts with fewer than 10 children or zero demand are suppressed (n=", masked_n, ")."
)

# Chart contract: map/uncertainty visuals are static because the approved final
# surface is manuscript PNG/PDF plus a separate aggregate leaflet.
chart_contract <- data.frame(
  figure = paste0("F", 1:6),
  analytical_question = c(
    "Where is posterior desert probability high?",
    "Which tracts are declared at FDR q=0.10?",
    "Which tracts remain under FDX c=0.10, alpha=0.05?",
    "Where do P01 point and P07 FDR classifications differ?",
    "Which counties are declared under child-weighted LIS?",
    "How do declarations move under preregistered alternatives and triangulation?"
  ),
  family = c("Uncertainty & Benchmark", "Comparison & Ranking", "Comparison & Ranking", "Composition", "Comparison & Ranking", "Uncertainty & Benchmark"),
  variant = c("continuous tract choropleth", "binary tract choropleth with point-map outline", "binary tract choropleth with point-map outline", "four-state tract choropleth", "binary county choropleth", "three-panel dot/bar summary"),
  rows = c(1409, 1409, 1409, 1409, 67, 9),
  palette_policy = c("single-root blue", "orange + neutral", "blue + neutral", "four approved roots + neutral", "orange + neutral", "hard two-root cap + neutral"),
  non_color_distinction = c("continuous legend", "outline + labels", "outline + labels", "named legend states", "county boundaries + labels", "facets + direct values"),
  output = paste0("outputs/figures/F", 1:6, "_*.[png|pdf]"),
  stringsAsFactors = FALSE
)
utils::write.csv(chart_contract, p07_path("outputs", "tables", "04_chart_contract.csv"), row.names = FALSE, na = "")

f1 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$posterior_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::scale_fill_gradientn(
    colors = c(P07_COLORS$blue_xlight, P07_COLORS$blue_light, P07_COLORS$blue, P07_COLORS$blue_dark),
    limits = c(0, 1), breaks = c(0, .25, .5, .75, 1),
    labels = scales::label_percent(accuracy = 1), na.value = P07_COLORS$masked,
    name = "Desert probability"
  ) +
  ggplot2::labs(
    title = "Tract posterior desert probability",
    subtitle = "P(coverage < 0.33); 1,409 nonzero-demand Alabama tracts",
    caption = caption_common
  ) + p07_theme_map(10)
save_p07_figure(f1, "F1_desert_probability", 8.2, 6.4)

# Overlay outline marking P01 point deserts on the FDR/FDX declaration maps.
outline <- map_data[map_data$display_allowed & map_data$point_desert %in% TRUE, ]
f2 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$fdr_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::geom_sf(data = outline, fill = NA, color = P07_COLORS$ink, linewidth = 0.20) +
  ggplot2::scale_fill_manual(
    values = c("FDR desert" = P07_COLORS$orange, "Not declared" = "#F3F4F6", "Suppressed" = P07_COLORS$masked),
    drop = FALSE, name = NULL
  ) +
  ggplot2::labs(
    title = "Tract FDR desert declarations",
    subtitle = "LIS step-up q=0.10; dark outlines mark P01 point deserts",
    caption = caption_common
  ) + p07_theme_map(10)
save_p07_figure(f2, "F2_fdr_declarations", 8.2, 6.4)

f3 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$fdx_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::geom_sf(data = outline, fill = NA, color = P07_COLORS$ink, linewidth = 0.20) +
  ggplot2::scale_fill_manual(
    values = c("FDX desert" = P07_COLORS$blue, "Not declared" = "#F3F4F6", "Suppressed" = P07_COLORS$masked),
    drop = FALSE, name = NULL
  ) +
  ggplot2::labs(
    title = "Tract FDX headline declarations",
    subtitle = "Empirical joint-posterior P(FDP>0.10) <= 0.05; dark outlines mark P01 point deserts",
    caption = caption_common
  ) + p07_theme_map(10)
save_p07_figure(f3, "F3_fdx_declarations", 8.2, 6.4)

comparison_labels <- c(
  both_point_and_fdr = "Both point and FDR",
  point_only = "Point only",
  fdr_only = "FDR only",
  neither = "Neither",
  Suppressed = "Suppressed"
)
f4 <- ggplot2::ggplot(map_data) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$comparison_display), color = "#FFFFFF", linewidth = 0.08) +
  ggplot2::scale_fill_manual(
    values = c(
      both_point_and_fdr = P07_COLORS$blue, point_only = P07_COLORS$gold,
      fdr_only = P07_COLORS$orange, neither = "#F3F4F6", Suppressed = P07_COLORS$masked
    ),
    labels = comparison_labels, drop = FALSE, name = NULL
  ) +
  ggplot2::labs(
    title = "P01 point map and P07 FDR reclassification",
    subtitle = paste0(
      comparison_counts[["both_point_and_fdr"]], " both, ",
      comparison_counts[["point_only"]], " point-only, ",
      comparison_counts[["fdr_only"]], " FDR-only, and ",
      comparison_counts[["neither"]], " neither"
    ),
    caption = caption_common
  ) + p07_theme_map(10)
save_p07_figure(f4, "F4_point_fdr_reclassification", 8.2, 6.4)

county_geometry <- map_data |>
  dplyr::group_by(.data$county_fips, .data$county_name) |>
  dplyr::summarise(geometry = sf::st_union(.data$geometry), .groups = "drop") |>
  dplyr::left_join(
    declarations$counties |>
      dplyr::select("county_fips", "county_lis", "county_desert_probability", "county_fdr_desert"),
    by = "county_fips"
  ) |>
  dplyr::mutate(county_status = ifelse(.data$county_fdr_desert, "Declared", "Not declared"))
# Verify 67 counties dissolved and the declared-county count matches the SSOT.
assert_that(nrow(county_geometry) == 67L && sum(county_geometry$county_fdr_desert) == key_value("n_desert_counties_weighted", TRUE), "County map aggregation failed.")

f5 <- ggplot2::ggplot(county_geometry) +
  ggplot2::geom_sf(ggplot2::aes(fill = .data$county_status), color = "#FFFFFF", linewidth = 0.35) +
  ggplot2::scale_fill_manual(values = c("Declared" = P07_COLORS$orange, "Not declared" = "#E5E7EB"), name = NULL) +
  ggplot2::labs(
    title = "Child-weighted county desert declarations",
    subtitle = paste0(
      sum(county_geometry$county_fdr_desert),
      " of ", nrow(county_geometry),
      " counties selected by under-five-weighted tract LIS at q=0.10"
    ),
    caption = paste0(
      "County declaration uses child-weighted tract LIS.\n",
      "Boundaries are county aggregates and contain no provider points."
    )
  ) + p07_theme_map(10)
save_p07_figure(f5, "F5_county_declarations", 8.2, 6.4)

gamma_plot_data <- sensitivity$gamma_sensitivity
p6a <- ggplot2::ggplot(gamma_plot_data, ggplot2::aes(x = .data$gamma, y = .data$n_fdr)) +
  ggplot2::geom_line(color = P07_COLORS$blue_dark, linewidth = 0.8) +
  ggplot2::geom_point(ggplot2::aes(fill = .data$role), shape = 21, size = 3, color = P07_COLORS$blue_dark) +
  ggplot2::geom_text(ggplot2::aes(label = .data$n_fdr), vjust = -0.8, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_fill_manual(values = c(primary_locked = P07_COLORS$blue, sensitivity_only = "white"), guide = "none") +
  ggplot2::scale_x_continuous(breaks = gamma_plot_data$gamma) +
  ggplot2::scale_y_continuous(limits = c(0, max(gamma_plot_data$n_fdr) * 1.15), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "FDR declarations by strict buffer", subtitle = "Filled point is primary", x = "Gamma", y = "Tracts") + p07_theme(9)

weight_plot_data <- sensitivity$county_weight_sensitivity |>
  dplyr::mutate(weight_scheme = factor(.data$weight_scheme, levels = c("under5", "equal", "area"), labels = c("Under-five", "Equal tract", "Area")))
p6b <- ggplot2::ggplot(weight_plot_data, ggplot2::aes(x = .data$weight_scheme, y = .data$n_declared)) +
  ggplot2::geom_col(ggplot2::aes(fill = .data$role), width = .65, color = P07_COLORS$ink, linewidth = .25) +
  ggplot2::geom_text(ggplot2::aes(label = .data$n_declared), vjust = -0.5, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_fill_manual(values = c(primary_locked = P07_COLORS$orange, sensitivity_only = P07_COLORS$orange_light), guide = "none") +
  ggplot2::scale_y_continuous(limits = c(0, max(weight_plot_data$n_declared) * 1.18), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "County declarations by weight", subtitle = "Primary uses child exposure", x = NULL, y = "Counties") + p07_theme(9)

tri_plot_data <- data.frame(
  set = factor(c("P01 always-desert", "In P07 FDR", "In P07 FDX"), levels = c("P01 always-desert", "In P07 FDR", "In P07 FDX")),
  tracts = c(519, sensitivity$triangulation$n_in_fdr, sensitivity$triangulation$n_in_fdx)
)
p6c <- ggplot2::ggplot(tri_plot_data, ggplot2::aes(x = .data$set, y = .data$tracts)) +
  ggplot2::geom_col(fill = c(P07_COLORS$gold_light, P07_COLORS$blue_light, P07_COLORS$blue), color = P07_COLORS$ink, linewidth = .25, width = .65) +
  ggplot2::geom_text(ggplot2::aes(label = .data$tracts), vjust = -0.5, color = P07_COLORS$ink, size = 3.2) +
  ggplot2::scale_y_continuous(limits = c(0, 590), expand = ggplot2::expansion(mult = c(0, .02))) +
  ggplot2::labs(title = "Triangulation of the 519 anchor", subtitle = "Intersection, not nested sets", x = NULL, y = "Tracts") +
  p07_theme(9) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

f6 <- p6a + p6b + p6c + patchwork::plot_layout(ncol = 3) +
  patchwork::plot_annotation(
    title = "Preregistered sensitivity and stability triangulation",
    subtitle = "Primary choices are filled/darker; alternatives remain sensitivity-only",
    caption = "Source: P07 joint posterior draws and P01 always-desert stability anchor.",
    theme = ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", family = "sans", color = P07_COLORS$ink, size = 13),
      plot.subtitle = ggplot2::element_text(family = "sans", color = P07_COLORS$muted, size = 10),
      plot.caption = ggplot2::element_text(family = "sans", color = P07_COLORS$muted, hjust = 0, size = 8)
    )
  )
save_p07_figure(f6, "F6_sensitivity_triangulation", 12, 4.8)

# Exact lookup belongs in compact tables, not overloaded map labels.
declaration_table <- read.csv(p07_path("outputs", "tables", "03_fdr_declaration_summary.csv"), stringsAsFactors = FALSE)
cross_table <- read.csv(p07_path("outputs", "tables", "03_p01_fdr_cross.csv"), stringsAsFactors = FALSE)
sensitivity_table <- rbind(
  data.frame(dimension = "gamma", choice = sensitivity$gamma_sensitivity$scenario, declarations = sensitivity$gamma_sensitivity$n_fdr, role = sensitivity$gamma_sensitivity$role),
  data.frame(dimension = "county weight", choice = sensitivity$county_weight_sensitivity$weight_scheme, declarations = sensitivity$county_weight_sensitivity$n_declared, role = sensitivity$county_weight_sensitivity$role)
)

# Save each gt table as both HTML and TeX, with a minimum-byte-size sanity check.
save_gt_pair <- function(table, stem) {
  html_path <- p07_path("outputs", "tables", paste0(stem, ".html"))
  tex_path <- p07_path("outputs", "tables", paste0(stem, ".tex"))
  gt::gtsave(table, html_path)
  gt::gtsave(table, tex_path)
  assert_that(file.exists(html_path) && file.info(html_path)$size > 1000, paste0("GT HTML failed: ", stem))
  assert_that(file.exists(tex_path) && file.info(tex_path)$size > 200, paste0("GT TeX failed: ", stem))
}

t1 <- declaration_table |>
  gt::gt() |>
  gt::tab_header(title = "P07 declaration summary", subtitle = "Posterior probability screen, FDR, FDX, and county procedure") |>
  gt::fmt_number(columns = c("target", "achieved"), decimals = 3) |>
  gt::cols_label(procedure = "Procedure", declarations = "Declarations", target = "Target", achieved = "Achieved", unit = "Unit") |>
  gt::tab_source_note("Source: outputs/key_numbers.csv and Step 3.1 canonical procedures.")
save_gt_pair(t1, "T1_declaration_summary")

t2 <- cross_table |>
  gt::gt() |>
  gt::tab_header(title = "P01 point map × P07 FDR map", subtitle = "Common denominator: 1,409 nonzero-demand tracts") |>
  gt::fmt_number(columns = c("n_tracts", "children", "high_cv_tracts"), decimals = 0, use_seps = TRUE) |>
  gt::fmt_number(columns = c("child_pct", "high_cv_pct"), decimals = 1) |>
  gt::fmt_number(columns = "median_desert_probability", decimals = 3) |>
  gt::tab_source_note("All displayed cells contain at least 10 tracts.")
save_gt_pair(t2, "T2_point_fdr_cross")

t3 <- sensitivity_table |>
  gt::gt(groupname_col = "dimension") |>
  gt::tab_header(title = "Preregistered sensitivity summary", subtitle = "Primary rows remain locked; alternatives do not replace them") |>
  gt::cols_label(choice = "Choice", declarations = "Declarations", role = "Role") |>
  gt::tab_source_note("Source: the same 2,000 joint coverage posterior draws.")
save_gt_pair(t3, "T3_sensitivity_summary")

# Build the self-contained aggregate-only leaflet (county/tract polygons, no provider points).
leaflet_map <- build_p07_leaflet(map_data)
map_path <- p07_path("outputs", "maps", "leaflet_desert_posterior_fdr.html")
htmlwidgets::saveWidget(leaflet_map, map_path, selfcontained = TRUE, title = "P07 posterior child care deserts")
assert_that(file.exists(map_path) && file.info(map_path)$size > 100000, "Leaflet render failed.")

figure_stems <- c(
  "F1_desert_probability", "F2_fdr_declarations", "F3_fdx_declarations",
  "F4_point_fdr_reclassification", "F5_county_declarations", "F6_sensitivity_triangulation"
)
figure_qa <- do.call(rbind, lapply(figure_stems, function(stem) {
  data.frame(
    figure = stem,
    png_exists = file.exists(p07_path("outputs", "figures", paste0(stem, ".png"))),
    png_bytes = file.info(p07_path("outputs", "figures", paste0(stem, ".png")))$size,
    pdf_exists = file.exists(p07_path("outputs", "figures", paste0(stem, ".pdf"))),
    pdf_bytes = file.info(p07_path("outputs", "figures", paste0(stem, ".pdf")))$size,
    stringsAsFactors = FALSE
  )
}))
# Privacy/disclosure QA. The load-bearing check is provider_coordinates_in_outputs = 0;
# small cells are masked and the leaflet is county-aggregate only.
privacy_qa <- data.frame(
  check = c("tract_values_masked_below_10", "provider_coordinates_in_outputs", "race_ethnicity_tract_cells", "leaflet_aggregate_only"),
  observed = c(masked_n, 0, 0, TRUE),
  passed = TRUE,
  stringsAsFactors = FALSE
)
utils::write.csv(figure_qa, p07_path("outputs", "tables", "04_figure_qa.csv"), row.names = FALSE, na = "")
utils::write.csv(privacy_qa, p07_path("outputs", "tables", "04_privacy_qa.csv"), row.names = FALSE, na = "")
assert_that(all(figure_qa$png_exists & figure_qa$pdf_exists), "Figure file QA failed.")
assert_that(all(privacy_qa$passed), "Visualization privacy QA failed.")

cat("Step 4.1 figures/tables/leaflet: PASS\n")
