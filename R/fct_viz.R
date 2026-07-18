# ============================================================================
# fct_viz.R -- Publication figure styling for P07.
# ----------------------------------------------------------------------------
# Purpose
#   One palette and one set of ggplot2 themes so every static figure in the
#   package reads as a single visual system, plus a save helper that writes a
#   matched PNG + PDF and gates on a minimum file size to catch silent render
#   failures (e.g. an empty device) before they reach the manuscript.
#
# Contents
#   P07_COLORS        named palette (ink/muted/grid/masked + blue, gold, orange, pink)
#   p07_theme()       minimal ggplot2 theme (bold title, bottom legend, faint grid)
#   p07_theme_map()   map variant that strips axes, ticks, and grid
#   save_p07_figure() write <stem>.png and <stem>.pdf; assert non-trivial size
# ============================================================================

# Single named palette referenced by every P07 figure and the leaflet map, so
# colors stay consistent across the whole package.
P07_COLORS <- list(
  ink = "#1F2937",
  muted = "#6B7280",
  grid = "#E5E7EB",
  masked = "#D1D5DB",
  blue_dark = "#173B6C",
  blue = "#2563EB",
  blue_light = "#93C5FD",
  blue_xlight = "#EFF6FF",
  gold = "#D9A514",
  gold_light = "#FDE68A",
  orange = "#C85A17",
  orange_light = "#FED7AA",
  pink = "#B83280"
)

# Base theme for non-map figures: bold dark title, muted subtitle/caption, a
# faint major grid only, and a bottom legend.
p07_theme <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = P07_COLORS$ink, size = base_size + 2),
      plot.subtitle = ggplot2::element_text(color = P07_COLORS$muted, size = base_size),
      plot.caption = ggplot2::element_text(color = P07_COLORS$muted, hjust = 0, size = base_size - 1),
      axis.title = ggplot2::element_text(color = P07_COLORS$ink),
      axis.text = ggplot2::element_text(color = P07_COLORS$ink),
      panel.grid.major = ggplot2::element_line(color = P07_COLORS$grid, linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}

# Map variant of the base theme: drop axes, ticks, and grid, which are noise on
# a choropleth.
p07_theme_map <- function(base_size = 10) {
  p07_theme(base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}

# Save a figure to both PNG (raster preview) and PDF (vector, for the paper)
# under outputs/figures, then assert each file is non-trivially large.
save_p07_figure <- function(plot, stem, width = 8, height = 6, dpi = 300) {
  png_path <- p07_path("outputs", "figures", paste0(stem, ".png"))
  pdf_path <- p07_path("outputs", "figures", paste0(stem, ".pdf"))
  ggplot2::ggsave(png_path, plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  ggplot2::ggsave(pdf_path, plot = plot, width = width, height = height, device = "pdf", bg = "white")
  # Size gate: a truncated or empty file is the usual signature of a failed
  # render, so require a sensible minimum byte count for each format.
  assert_that(file.exists(png_path) && file.info(png_path)$size > 10000, paste0("PNG render failed: ", stem))
  assert_that(file.exists(pdf_path) && file.info(pdf_path)$size > 5000, paste0("PDF render failed: ", stem))
  invisible(c(png = png_path, pdf = pdf_path))
}
