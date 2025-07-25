% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/artist_aux.R
\name{style_artist_common}
\alias{style_artist_common}
\title{Common core function for artist-inspired ggplot2 styles}
\usage{
style_artist_common(
  data,
  artist,
  obra_inspiracion,
  x = NULL,
  y = NULL,
  color_var = NULL,
  fill_var = NULL,
  label_var = NULL,
  plot_type = c("column", "scatter", "line", "map"),
  title,
  subtitle,
  caption,
  show_labels = FALSE,
  add_grid_lines = FALSE,
  show_background = TRUE,
  add_glow = FALSE,
  coord_flip = FALSE,
  theme_base = ggplot2::theme_void,
  grid_linetype = "dotted",
  grid_linewidth = 0.3,
  axis_line_linewidth = 0.8,
  panel_background_map_specific = FALSE,
  text_size = 12,
  add_texture = NULL,
  canvas = NULL,
  add_filter = FALSE,
  add_margins = TRUE
)
}
\arguments{
\item{data}{A data frame or `sf` object, depending on the plot type.}

\item{artist}{String. Artist name (e.g., `"van_gogh"`, `"da_vinci"`).}

\item{obra_inspiracion}{String. Specific artwork to define the palette/theme.}

\item{x, y}{Tidy-evaluated expressions for aesthetics.}

\item{color_var}{Optional. Tidy-evaluated expression for color mapping.}

\item{fill_var}{Optional. Tidy-evaluated expression for fill mapping.}

\item{label_var}{Optional. Tidy-evaluated expression for label text.}

\item{plot_type}{One of `"scatter"`, `"line"`, `"column"`, or `"map"`.}

\item{title, subtitle, caption}{Plot title, subtitle, and caption.}

\item{show_labels}{Logical. Add text labels.}

\item{add_grid_lines}{Logical. Show grid lines.}

\item{show_background}{Logical. Show background fill (for non-map plots).}

\item{add_glow}{Logical. Apply glow effect to geoms.}

\item{coord_flip}{Logical. Flip coordinates (for column plots).}

\item{theme_base}{Base ggplot2 theme. Default: `theme_void`.}

\item{grid_linetype}{Line type for grid. Default: `"dotted"`.}

\item{grid_linewidth}{Width of grid lines. Default: `0.3`.}

\item{axis_line_linewidth}{Width of axis lines. Default: `0.8`.}

\item{panel_background_map_specific}{Logical. Special map background fill. Default: `FALSE`.}

\item{text_size}{Numeric. Base text size for titles, labels, etc. Default: `12`.}

\item{add_texture}{Integer (1–3). Applies visual texture effects to geoms. Default: `NULL`.}

\item{canvas}{Integer (1–6). Adds canvas-style background image. Default: `NULL`.}

\item{add_filter}{Experimental. Logical. Applies oil effect to the full graph. Default: `FALSE`.}

\item{add_margins}{Logical. Applies margins to plot and labs. Default: `TRUE`.}
}
\value{
A `ggplot` object styled with artistic aesthetics.
}
\description{
Applies a visual style to ggplot2 charts inspired by an artist and specific artwork, with optional painterly effects like texture, canvas overlays, and glow.
}
