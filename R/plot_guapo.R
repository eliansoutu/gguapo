#' @title plot_guapo: Highly stylized data visualization function
#' @description Generates ggplot2 graphics with high visual impact, utilizing
#' modern color palettes, distinctive fonts, and advanced visual effects from `ggfx`.
#' @param data A data frame (or an `sf` object for `plot_type = "map"`).
#' @param x (Optional) Name of the column for the x-axis (can be unquoted for tidy evaluation). Defaults to NULL.
#' @param y (Optional) Name of the column for the y-axis (can be unquoted for tidy evaluation). Defaults to NULL.
#' @param color_var (Optional) Name of the column to map to color (can be unquoted). Defaults to NULL.
#' @param fill_var (Optional) Name of the column to map to fill (can be unquoted). Defaults to NULL.
#' @param label_var (Optional) Name of the column for data labels (can be unquoted). Defaults to NULL.
#' @param title The main title of the plot.
#' @param subtitle The subtitle of the plot.
#' @param caption The caption or source of the plot.
#' @param plot_type The type of plot:  "column", "scatter", "line", "map".
#' @param palette_name The name of the color palette to use: "guapo", "guapero", "guapon", "guapisimo".
#' @param font_title_name The Google Font for the plot title.
#' @param font_body_name The Google Font for the body text and axes.
#' @param show_labels Logical, whether to display data labels. Defaults to FALSE.
#' @param add_trend_line Logical, whether to add a trend line (only for scatter/line plots). Defaults to FALSE.
#' @param highlight_values A vector of values in `x`, `color_var`, or `label_var` to highlight. Defaults to NULL.
#' @param highlight_color The color for highlighted values. Defaults to "#FFD700" (Gold).
#' @param facet_var (Optional) Name of the column to facet the plot by (can be unquoted). Defaults to NULL.
#' @param dark_mode Logical, whether to use a dark color scheme. Defaults to FALSE.
#' @param apply_shadow Logical, whether to apply a shadow effect to geometric elements. Requires `ggfx`. Defaults to FALSE.
#' @param apply_glow Logical, whether to apply a glow effect to geometric elements. Requires `ggfx`. Defaults to FALSE.
#' @param apply_blur_background Logical, whether to apply a blur effect to the panel background. Requires `ggfx`. Defaults to FALSE.
#' @param gradient_fill Logical, whether to use a gradient fill for continuous variables (note: this parameter isn't directly used in the provided code for scale generation; scales are determined automatically based on variable type). Defaults to FALSE.
#' @param base_alpha The base transparency of the geometric elements. Defaults to 0.8.
#' @param geom_size The base size of points or thickness of lines. Defaults to 3.
#' @param geom_stroke The border thickness of points. Defaults to 0.5.
#' @param text_size The base text size for the plot. Defaults to 20.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col geom_smooth theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn facet_wrap geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf
#' @importFrom rlang .data !! sym enexpr as_string
#' @importFrom sysfonts font_add_google font_families
#' @importFrom showtext showtext_auto
#' @importFrom grDevices colorRampPalette
#' @importFrom ggfx with_shadow with_outer_glow with_blur
#' @export
plot_guapo <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                       title = "Visualización de Datos Impactante",
                       subtitle = "Con Estilo y Diseño Avanzado",
                       caption = "Generado con plot_guapo",
                       plot_type = c("column", "scatter", "line", "map"),
                       palette_name = c("guapo", "guapero", "guapon", "guapisimo"),
                       font_title_name = "Orbitron", font_body_name = "Poppins",
                       show_labels = FALSE, add_trend_line = FALSE,
                       highlight_values = NULL, highlight_color = "#FFD700", # Gold for highlighting
                       facet_var = NULL,
                       dark_mode = FALSE,
                       apply_shadow = FALSE, apply_glow = FALSE,
                       apply_blur_background = FALSE,
                       gradient_fill = FALSE, base_alpha = 0.8,
                       geom_size = 3, geom_stroke = 0.5,
                       text_size = 20) {

  plot_type <- match.arg(plot_type)
  palette_name <- match.arg(palette_name)


  if (sum(c("Orbitron","Poppins") %in% sysfonts::font_families()) == 0) {

    showtext::showtext_auto()

    sysfonts::font_add_google("Orbitron", "Orbitron")
    sysfonts::font_add_google("Poppins", "Poppins")

  }

  # Definición de paletas y ajustes
  palettes <- list(
    "guapo" = list(
      light = list(
        colors = c("#00FFFF", "#FF00FF", "#FFFF00", "#FF69B4", "#00FF7F"),
        background_fill = "#F0F8FF", panel_fill = "#E0FFFF",
        grid_color = "#D3D3D3", text_color = "#333333", geom_alpha = 0.9
      ),
      dark = list(
        colors = c("#00FFFF", "#FF00FF", "#FFFF00", "#FF69B4", "#00FF7F"),
        background_fill = "#1A1A2E", panel_fill = "#0F0F1A",
        grid_color = "#4A4A5C", text_color = "#E0FFFF", geom_alpha = 0.8
      )
    ),
    "guapero" = list(
      light = list(
        colors = c("#FF1493", "#8A2BE2", "#FFD700", "#00CED1", "#FF6347"),
        background_fill = "#FFF0F5", panel_fill = "#F5E0EB",
        grid_color = "#E6E6FA", text_color = "#4B0082", geom_alpha = 0.9
      ),
      dark = list(
        colors = c("#FF1493", "#8A2BE2", "#FFD700", "#00CED1", "#FF6347"),
        background_fill = "#2C003D", panel_fill = "#1A002A",
        grid_color = "#5A2A6B", text_color = "#FFD700", geom_alpha = 0.8
      )
    ),
    "guapon" = list(
      light = list(
        colors = c("#A3D9EE", "#C7F2A4", "#FFE180", "#FFB6C1", "#D8BFD8"),
        background_fill = "#F5F5F5", panel_fill = "#EFEFEF",
        grid_color = "#D0D0D0", text_color = "#555555", geom_alpha = 0.9
      ),
      dark = list(
        colors = c("#A3D9EE", "#C7F2A4", "#FFE180", "#FFB6C1", "#D8BFD8"),
        background_fill = "#2B2D42", panel_fill = "#1B1D32",
        grid_color = "#4A4C62", text_color = "#A3D9EE", geom_alpha = 0.8
      )
    ),
    "guapisimo" = list(
      light = list(
        colors = c("#FF4500", "#1E90FF", "#32CD32", "#FFD700", "#8A2BE2"),
        background_fill = "#FFFFFF", panel_fill = "#F8F8F8",
        grid_color = "#E0E0E0", text_color = "#000000", geom_alpha = 0.95
      ),
      dark = list(
        colors = c("#FF4500", "#1E90FF", "#32CD32", "#FFD700", "#8A2BE2"),
        background_fill = "#000000", panel_fill = "#111111",
        grid_color = "#333333", text_color = "#FFFFFF", geom_alpha = 0.9
      )
    )
  )

  current_settings <- if (dark_mode) palettes[[palette_name]]$dark else palettes[[palette_name]]$light

  # Capturar las expresiones de las variables
  x_sym <- rlang::enexpr(x)
  y_sym <- rlang::enexpr(y)
  color_var_sym <- rlang::enexpr(color_var)
  fill_var_sym <- rlang::enexpr(fill_var)
  label_var_sym <- rlang::enexpr(label_var)
  facet_var_sym <- rlang::enexpr(facet_var)

  # Base plot
  if (plot_type == "map") {
    p <- ggplot2::ggplot(data)
  } else {
    aes_mapping_list <- list()
    if (!is.null(x_sym)) aes_mapping_list$x <- x_sym
    if (!is.null(y_sym)) aes_mapping_list$y <- y_sym
    if (!is.null(color_var_sym)) aes_mapping_list$color <- color_var_sym
    if (!is.null(fill_var_sym)) aes_mapping_list$fill <- fill_var_sym

    p <- ggplot2::ggplot(data, ggplot2::aes(!!!aes_mapping_list))
  }

  # Dynamic color/fill scales (logic integrated from generate_color_scale)
  # Color scale
  if (!is.null(color_var_sym) && rlang::as_string(color_var_sym) %in% names(data)) {
    var_name_str <- rlang::as_string(color_var_sym)
    var_data <- data[[var_name_str]]
    if (is.numeric(var_data)) {
      p <- p + ggplot2::scale_color_gradientn(colors = current_settings$colors)
    } else {
      unique_levels <- unique(var_data)
      num_levels <- length(unique_levels)
      if (num_levels <= length(current_settings$colors)) {
        p <- p + ggplot2::scale_color_manual(values = current_settings$colors[1:num_levels])
      } else {
        extended_palette_fun <- grDevices::colorRampPalette(current_settings$colors)
        extended_colors <- extended_palette_fun(num_levels)
        p <- p + ggplot2::scale_color_manual(values = extended_colors)
      }
    }
  }

  # Fill scale
  if (!is.null(fill_var_sym) && rlang::as_string(fill_var_sym) %in% names(data)) {
    var_name_str <- rlang::as_string(fill_var_sym)
    var_data <- data[[var_name_str]]
    if (is.numeric(var_data)) {
      p <- p + ggplot2::scale_fill_gradientn(colors = current_settings$colors)
    } else {
      unique_levels <- unique(var_data)
      num_levels <- length(unique_levels)
      if (num_levels <= length(current_settings$colors)) {
        p <- p + ggplot2::scale_fill_manual(values = current_settings$colors[1:num_levels])
      } else {
        extended_palette_fun <- grDevices::colorRampPalette(current_settings$colors)
        extended_colors <- extended_palette_fun(num_levels)
        p <- p + ggplot2::scale_fill_manual(values = extended_colors)
      }
    }
  }

  # Geom layers with ggfx effects
  geom_layer_base <- NULL

  if (plot_type == "scatter") {
    point_aes_list <- list()
    if (!is.null(color_var_sym)) point_aes_list$color <- color_var_sym

    point_args <- list(
      mapping = ggplot2::aes(!!!point_aes_list),
      size = geom_size,
      alpha = base_alpha,
      stroke = geom_stroke
    )
    if (is.null(color_var_sym)) { # Only add 'color' arg if it's a fixed value
      point_args$color <- current_settings$colors[4]
    }
    geom_layer_base <- do.call(ggplot2::geom_point, point_args)

  } else if (plot_type == "line") {
    line_point_aes_list <- list()
    if (!is.null(color_var_sym)) line_point_aes_list$color <- color_var_sym

    line_args <- list(
      mapping = ggplot2::aes(!!!line_point_aes_list),
      size = geom_size / 2,
      alpha = base_alpha
    )
    point_args <- list(
      mapping = ggplot2::aes(!!!line_point_aes_list),
      size = geom_size,
      alpha = base_alpha,
      stroke = geom_stroke
    )

    if (is.null(color_var_sym)) { # Apply fixed color to both if not mapped
      line_args$color <- current_settings$colors[1]
      point_args$color <- current_settings$colors[1]
    }

    geom_layer_base <- list(
      do.call(ggplot2::geom_line, line_args),
      do.call(ggplot2::geom_point, point_args)
    )

    if (add_trend_line) {
      geom_layer_base <- c(geom_layer_base, ggplot2::geom_smooth(method = "lm", se = FALSE, color = current_settings$text_color, linetype = "dashed", size = 0.5))
    }

  } else if (plot_type == "column") {
    # Ensure x is discrete for columns
    if (!is.null(x_sym) && rlang::as_string(x_sym) %in% names(data)) {
      data[[rlang::as_string(x_sym)]] <- factor(data[[rlang::as_string(x_sym)]])
    }

    column_aes_list <- list()
    if (!is.null(fill_var_sym)) column_aes_list$fill <- fill_var_sym

    col_args <- list(
      mapping = ggplot2::aes(!!!column_aes_list),
      width = 0.7,
      alpha = base_alpha
    )
    if (is.null(fill_var_sym)) { # Only add 'fill' arg if it's a fixed value
      col_args$fill <- current_settings$colors[1]
    }
    geom_layer_base <- do.call(ggplot2::geom_col, col_args)

  } else if (plot_type == "map") {
    if (!is.null(fill_var_sym)) {
      map_aes_list <- list(fill = fill_var_sym)
      geom_layer_base <- ggplot2::geom_sf(
        ggplot2::aes(!!!map_aes_list),
        alpha = base_alpha,
        color = current_settings$grid_color, linewidth = 0.1
      )
    } else {
      geom_layer_base <- ggplot2::geom_sf(
        fill = current_settings$colors[1], # Fixed fill
        alpha = base_alpha,
        color = current_settings$grid_color, linewidth = 0.1
      )
    }
  }

  # Apply ggfx effects
  if (!is.null(geom_layer_base)) {
    if (apply_shadow) {
      if (apply_glow) {
        p <- p + ggfx::with_shadow(ggfx::with_outer_glow(geom_layer_base, colour = highlight_color, sigma = 8, expand = 5),
                                   colour = "black", x_offset = 3, y_offset = 3, sigma = 5)
      } else {
        p <- p + ggfx::with_shadow(geom_layer_base, colour = "black", x_offset = 3, y_offset = 3, sigma = 5)
      }
    } else if (apply_glow) {
      p <- p + ggfx::with_outer_glow(geom_layer_base, colour = highlight_color, sigma = 8, expand = 5)
    } else {
      p <- p + geom_layer_base
    }
  }

  # Highlight specific values
  if (!is.null(highlight_values)) {
    # Initialize a logical vector for filtering
    filter_condition <- rep(FALSE, nrow(data))

    # Add condition for x-axis variable if x is not NULL and exists in data
    if (!is.null(x_sym) && rlang::as_string(x_sym) %in% names(data)) {
      filter_condition <- filter_condition | (data[[rlang::as_string(x_sym)]] %in% highlight_values)
    }

    # Add condition for color_var if color_var is not NULL and exists in data
    if (!is.null(color_var_sym) && rlang::as_string(color_var_sym) %in% names(data)) {
      filter_condition <- filter_condition | (data[[rlang::as_string(color_var_sym)]] %in% highlight_values)
    }

    # Add condition for label_var if label_var is not NULL and exists in data (useful for maps/categorical data)
    if (!is.null(label_var_sym) && rlang::as_string(label_var_sym) %in% names(data)) {
      filter_condition <- filter_condition | (data[[rlang::as_string(label_var_sym)]] %in% highlight_values)
    }

    # Only proceed if there's at least one true in the filter_condition
    if (any(filter_condition)) {
      highlight_data <- dplyr::filter(data, filter_condition)

      if (nrow(highlight_data) > 0) {
        if (plot_type == "scatter" || plot_type == "line") {
          p <- p + ggplot2::geom_point(data = highlight_data, size = geom_size * 1.8, shape = 21,
                                       fill = highlight_color, color = current_settings$text_color, stroke = 1.2, alpha = 0.9)
        } else if (plot_type == "column") {
          p <- p + ggplot2::geom_col(data = highlight_data, width = 0.7, fill = highlight_color, alpha = 0.9)
        } else if (plot_type == "map") {
          p <- p + ggplot2::geom_sf(data = highlight_data, fill = highlight_color,
                                    color = current_settings$text_color, linewidth = 0.5, alpha = 0.9)
        }
      } else {
        warning("No data points found to highlight based on provided 'highlight_values'.")
      }
    } else {
      warning("No valid variable (x, color_var, or label_var) found in data to apply 'highlight_values'.")
    }
  }

  # Labels
  if (show_labels && !is.null(label_var_sym)) {
    label_aes_list <- list(label = label_var_sym)
    if (plot_type == "column") {
      p <- p + ggplot2::geom_text(ggplot2::aes(!!!label_aes_list), vjust = -0.5, hjust = 0.5,
                                  size = 3.5, color = current_settings$text_color, family = font_body_name)
    } else if (plot_type == "scatter" || plot_type == "line") {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(!!!label_aes_list), size = 3.5, box.padding = 0.5,
                                        min.segment.length = 0.3, segment.color = current_settings$grid_color,
                                        segment.size = 0.3, max.overlaps = 50, direction = "y",
                                        family = font_body_name, color = current_settings$text_color)
    } else if (plot_type == "map") {
      p <- p + ggplot2::geom_sf_text(ggplot2::aes(!!!label_aes_list),
                                     size = 3.5, color = current_settings$text_color, family = font_body_name,
                                     check_overlap = TRUE)
    }
  }

  # Faceting
  if (!is.null(facet_var_sym)) {
    p <- p + ggplot2::facet_wrap(facet_var_sym)
  }

  # Theme
  final_theme <- ggplot2::theme_void() +
    ggplot2::theme(
      text = element_text(size = text_size),
      plot.title = ggtext::element_markdown(face = "bold", hjust = 0.5, family = font_title_name, color = current_settings$text_color, margin = ggplot2::margin(b = 20)),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, family = font_body_name, color = current_settings$text_color, margin = ggplot2::margin(b = 30)),
      plot.caption = ggtext::element_markdown( hjust = 1, family = font_body_name, color = current_settings$text_color, margin = ggplot2::margin(t = 20)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(family = font_body_name, color = current_settings$text_color),
      legend.text = ggplot2::element_text(family = font_body_name, color = current_settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      plot.background = ggplot2::element_rect(fill = current_settings$background_fill, colour = NA),
      axis.text = ggplot2::element_text(color = current_settings$text_color, family = font_body_name),
      axis.title = ggtext::element_markdown(color = current_settings$text_color, family = font_body_name),
      panel.grid.major = ggplot2::element_line(color = current_settings$grid_color, linetype = "dotted", linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = current_settings$grid_color, linewidth = 0.8)
    )

  # Apply blur to panel background
  if (apply_blur_background) {
    final_theme <- final_theme + ggplot2::theme(
      panel.background = ggfx::with_blur(ggplot2::element_rect(fill = current_settings$panel_fill, colour = NA),
                                         sigma = 10)
    )
  } else {
    final_theme <- final_theme + ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = current_settings$panel_fill, colour = NA)
    )
  }

  # Specific adjustments for map plots
  if (plot_type == "map") {
    final_theme <- final_theme +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  # Construct the labs arguments dynamically to avoid "argument x is missing" error
  lab_args <- list(
    title = paste0("<span style='font-family:\"", font_title_name, "\";'>", title, "</span>"),
    subtitle = paste0("<span style='font-family:\"", font_body_name, "\";'>", subtitle, "</span>"),
    caption = paste0("<span style='font-family:\"", font_body_name, "\";'>", caption, "</span>")
  )

  if (!is.null(x_sym) && plot_type != "map") { # x is relevant for non-map plots
    lab_args$x = paste0("<span style='font-family:\"", font_body_name, "\";'>", tools::toTitleCase(gsub("_", " ", rlang::as_string(x_sym))), "</span>")
  }

  if (!is.null(y_sym) && plot_type != "map") { # y is relevant for non-map plots
    lab_args$y = paste0("<span style='font-family:\"", font_body_name, "\";'>", tools::toTitleCase(gsub("_", " ", rlang::as_string(y_sym))), "</span>")
  }

  p + final_theme + do.call(ggplot2::labs, lab_args)
}

