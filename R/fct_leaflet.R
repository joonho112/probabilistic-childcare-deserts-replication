# ============================================================================
# fct_leaflet.R -- Aggregate-only interactive tract map for P07.
# ----------------------------------------------------------------------------
# Purpose
#   Renders a tract-level choropleth of posterior desert probability and the FDR
#   desert declaration as two toggleable layers. It draws tract POLYGONS only --
#   never provider points or coordinates -- and suppresses any tract with fewer
#   than 10 children (display_allowed == FALSE) as a small-cell disclosure
#   safeguard.
#
# Behavior
#   Reprojects to WGS84 (EPSG:4326) for web tiles; masks suppressed tracts to a
#   neutral grey with NA values; a sequential blue palette encodes probability in
#   [0, 1] and a categorical palette encodes the declaration. Popups repeat only
#   aggregate tract facts, and the FDR layer starts hidden.
#
# Key function
#   build_p07_leaflet(map_sf) -> a leaflet htmlwidget
# ============================================================================

build_p07_leaflet <- function(map_sf) {
  assert_that(requireNamespace("leaflet", quietly = TRUE), "Package 'leaflet' is required.")
  assert_that(all(c("display_allowed", "desert_probability", "fdr_desert", "county_name", "GEOID") %in% names(map_sf)), "Leaflet fields are incomplete.")
  # Leaflet tiles are in WGS84 (EPSG:4326), so reproject the tract geometries.
  map_wgs84 <- sf::st_transform(map_sf, 4326)
  # Blank out (NA) the probability of any suppressed small-cell tract so it renders
  # in the neutral masked color rather than exposing an estimate.
  probability_display <- ifelse(map_wgs84$display_allowed, map_wgs84$desert_probability, NA_real_)
  # Three-state declaration label: suppressed tracts first, then FDR desert vs
  # not declared for the rest.
  fdr_display <- ifelse(
    !map_wgs84$display_allowed, "Suppressed (<10 children)",
    ifelse(map_wgs84$fdr_desert, "FDR desert", "Not declared")
  )
  # Sequential blue ramp over probability [0, 1]; masked/NA tracts get grey.
  posterior_palette <- leaflet::colorNumeric(
    palette = c(P07_COLORS$blue_xlight, P07_COLORS$blue_light, P07_COLORS$blue, P07_COLORS$blue_dark),
    domain = c(0, 1), na.color = P07_COLORS$masked
  )
  declaration_palette <- leaflet::colorFactor(
    palette = c(P07_COLORS$orange, "#F3F4F6", P07_COLORS$masked),
    domain = c("FDR desert", "Not declared", "Suppressed (<10 children)")
  )
  # Popups carry aggregate tract facts only (county, GEOID, probability,
  # declaration); suppressed tracts show a suppression notice instead.
  popup <- ifelse(
    map_wgs84$display_allowed,
    paste0(
      "<strong>", map_wgs84$county_name, "</strong><br>",
      "Tract ", map_wgs84$GEOID, "<br>",
      "Posterior desert probability: ", sprintf("%.1f%%", 100 * map_wgs84$desert_probability), "<br>",
      "FDR q=0.10: ", ifelse(map_wgs84$fdr_desert, "Declared", "Not declared")
    ),
    paste0("<strong>", map_wgs84$county_name, "</strong><br>Tract result suppressed (<10 children)")
  )
  # preferCanvas keeps rendering fast with ~1,400 tract polygons. Two polygon
  # layers (probability, declaration) share one popup and are toggled via the
  # layers control.
  leaflet::leaflet(map_wgs84, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Base map") |>
    # Layer 1: posterior desert probability (continuous blue).
    leaflet::addPolygons(
      group = "Posterior probability",
      fillColor = posterior_palette(probability_display), fillOpacity = 0.75,
      color = "#FFFFFF", weight = 0.35, opacity = 0.8,
      popup = popup, highlightOptions = leaflet::highlightOptions(weight = 1.5, color = P07_COLORS$ink, bringToFront = TRUE)
    ) |>
    # Layer 2: categorical FDR declaration (desert / not declared / suppressed).
    leaflet::addPolygons(
      group = "FDR declaration",
      fillColor = declaration_palette(fdr_display), fillOpacity = 0.75,
      color = "#FFFFFF", weight = 0.35, opacity = 0.8,
      popup = popup, highlightOptions = leaflet::highlightOptions(weight = 1.5, color = P07_COLORS$ink, bringToFront = TRUE)
    ) |>
    leaflet::addLegend(
      position = "bottomright", pal = posterior_palette, values = c(0, 1),
      title = "P(coverage < 0.33)", opacity = 0.9, group = "Posterior probability"
    ) |>
    leaflet::addLayersControl(
      baseGroups = "Base map",
      overlayGroups = c("Posterior probability", "FDR declaration"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    # Start with only the probability layer visible; the FDR layer is opt-in.
    leaflet::hideGroup("FDR declaration")
}

