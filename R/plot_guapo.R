#' @title plot_guapo: Highly Stylized Data Visualization with ggplot2
#'
#' @description
#' Creates high-impact, visually appealing plots using `ggplot2`, supporting multiple plot types (`"column"`, `"scatter"`, `"line"`, and `"map"`), modern color palettes, Google Fonts, and advanced visual effects like glow, shadow, and background blur.
#'
#' @param data A data frame (`data.frame`) or an `sf` object (for `plot_type = "map"`).
#' @param x Variable for the x-axis (unquoted).
#' @param y Variable for the y-axis (unquoted).
#' @param color_var Variable used for color aesthetics.
#' @param fill_var Variable used for fill aesthetics.
#' @param label_var Variable used for text labels.
#' @param title Main plot title.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption text displayed below the plot.
#' @param plot_type Type of plot: `"column"`, `"scatter"`, `"line"`, or `"map"`.
#' @param palette_name Color palette to use: `"guapo"`, `"guapero"`, `"guapon"`, or `"guapisimo"`.
#' @param font_title_name Font for the title (Google Fonts, default: `"Orbitron"`).
#' @param font_body_name Font for the body and labels (default: `"Poppins"`).
#' @param show_labels Logical. If `TRUE`, displays text labels on bars or points.
#' @param coord_flip Logical. If `TRUE`, flips x and y axes (useful for bar plots).
#' @param show_baseline Logical. If `TRUE`, draws a baseline at y = 0 for column, scatter and line plots.
#' @param highlight_values A vector of values to highlight (based on `color_var` or `fill_var`).
#' @param highlight_color Color used for highlighting specific values.
#' @param facet_var Variable used for faceting (unquoted).
#' @param dark_mode Logical. If `TRUE`, applies a dark-themed background.
#' @param apply_shadow Logical. If `TRUE`, applies a shadow effect to geoms.
#' @param apply_glow Logical. If `TRUE`, applies a glow effect to geoms.
#' @param apply_blur_background Logical. If `TRUE`, adds a blurred background to the plot.
#' @param gradient_fill Logical. If `TRUE`, fills bars/points with a color gradient instead of solid colors.
#' @param base_alpha Alpha transparency applied to geoms (default: 0.8).
#' @param geom_size Size of geom elements (points, lines, or bars).
#' @param geom_stroke Stroke width for geoms (e.g., outline of points).
#' @param text_size Base size for plot text (titles, labels, etc.).
#'
#' @return A stylized `ggplot` object.
#'
#' @export
plot_guapo <- function(data, x = NULL, y = NULL,
                       color_var = NULL, fill_var = NULL, label_var = NULL,
                       title = "Visualización de Datos Impactante",
                       subtitle = "Con Estilo y Diseño Avanzado",
                       caption = "Generado con plot_guapo",
                       plot_type = c("column", "scatter", "line", "map"),
                       palette_name = c("guapo", "guapero", "guapon", "guapisimo"),
                       font_title_name = "Orbitron", font_body_name = "Poppins",
                       show_labels = FALSE,
                       coord_flip = FALSE,
                       show_baseline = FALSE,
                       highlight_values = NULL,
                       highlight_color = "#FFD700",
                       facet_var = NULL,
                       dark_mode = FALSE,
                       apply_shadow = FALSE,
                       apply_glow = FALSE,
                       apply_blur_background = FALSE,
                       gradient_fill = FALSE,
                       base_alpha = 0.8,
                       geom_size = 3,
                       geom_stroke = 0.5,
                       text_size = 20) {
  plot_type <- match.arg(plot_type)
  palette_name <- match.arg(palette_name)

  if (sum(c(font_title_name, font_body_name) %in% sysfonts::font_families()) < 2) {
    showtext::showtext_auto()
    sysfonts::font_add_google(font_title_name, font_title_name)
    sysfonts::font_add_google(font_body_name, font_body_name)
  }

  # Paletas
  palettes <- list(
    "guapo" = list(
      light = list(colors = c("#04dede", "#FF00FF", "#FFFF00", "#FF69B4", "#00FF7F"),
                   background_fill = "#F0F8FF", panel_fill = "#E0FFFF",
                   grid_color = "#D3D3D3", text_color = "#333333", geom_alpha = 0.9),
      dark = list(colors = c("#00FFFF", "#FF00FF", "#FFFF00", "#FF69B4", "#00FF7F"),
                  background_fill = "#1A1A2E", panel_fill = "#0F0F1A",
                  grid_color = "#4A4A5C", text_color = "#E0FFFF", geom_alpha = 0.8)
    ),
    "guapero" = list(
      light = list(colors = c("#FF1493", "#8A2BE2", "#FFD700", "#00CED1", "#FF6347"),
                   background_fill = "#FFF0F5", panel_fill = "#F5E0EB",
                   grid_color = "#E6E6FA", text_color = "#4B0082", geom_alpha = 0.9),
      dark = list(colors = c("#FF1493", "#8A2BE2", "#FFD700", "#00CED1", "#FF6347"),
                  background_fill = "#2C003D", panel_fill = "#1A002A",
                  grid_color = "#5A2A6B", text_color = "#FFD700", geom_alpha = 0.8)
    ),
    "guapon" = list(
      light = list(colors = c("#A3D9EE", "#C7F2A4", "#FFE180", "#FFB6C1", "#D8BFD8"),
                   background_fill = "#F5F5F5", panel_fill = "#EFEFEF",
                   grid_color = "#D0D0D0", text_color = "#555555", geom_alpha = 0.9),
      dark = list(colors = c("#A3D9EE", "#C7F2A4", "#FFE180", "#FFB6C1", "#D8BFD8"),
                  background_fill = "#2B2D42", panel_fill = "#1B1D32",
                  grid_color = "#4A4C62", text_color = "#A3D9EE", geom_alpha = 0.8)
    ),
    "guapisimo" = list(
      light = list(colors = c("#FF4500", "#1E90FF", "#32CD32", "#FFD700", "#8A2BE2"),
                   background_fill = "#FFFFFF", panel_fill = "#F8F8F8",
                   grid_color = "#E0E0E0", text_color = "#000000", geom_alpha = 0.95),
      dark = list(colors = c("#FF4500", "#1E90FF", "#32CD32", "#FFD700", "#8A2BE2"),
                  background_fill = "#000000", panel_fill = "#111111",
                  grid_color = "#333333", text_color = "#FFFFFF", geom_alpha = 0.9)
    )
  )

  current_settings <- if (dark_mode) palettes[[palette_name]]$dark else palettes[[palette_name]]$light

  # Capturar expresiones
  x_sym <- rlang::enexpr(x)
  y_sym <- rlang::enexpr(y)
  color_var_sym <- rlang::enexpr(color_var)
  fill_var_sym <- rlang::enexpr(fill_var)
  label_var_sym <- rlang::enexpr(label_var)
  facet_var_sym <- rlang::enexpr(facet_var)

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

  # Escalas de color y fill
  for (var in c("color", "fill")) {
    var_sym <- if (var == "color") color_var_sym else fill_var_sym
    if (!is.null(var_sym) && rlang::as_string(var_sym) %in% names(data)) {
      var_data <- data[[rlang::as_string(var_sym)]]
      if (is.numeric(var_data)) {
        p <- p + do.call(get(paste0("scale_", var, "_gradientn")), list(colors = current_settings$colors))
      } else {
        unique_levels <- unique(var_data)
        num_levels <- length(unique_levels)
        pal_fun <- grDevices::colorRampPalette(current_settings$colors)
        colors <- if (num_levels <= length(current_settings$colors)) {
          current_settings$colors[1:num_levels]
        } else {
          pal_fun(num_levels)
        }
        p <- p + do.call(get(paste0("scale_", var, "_manual")), list(values = colors))
      }
    }
  }

  # Geoms principales
  geom_layer_base <- NULL
  if (plot_type == "scatter") {
    args <- list(size = geom_size, alpha = base_alpha, stroke = geom_stroke)
    if (is.null(color_var_sym)) args$color <- current_settings$colors[1]
    geom_layer_base <- do.call(ggplot2::geom_point, args)
  } else if (plot_type == "line") {
    args_line <- list(size = geom_size / 2, alpha = base_alpha)
    args_point <- list(size = geom_size, alpha = base_alpha, stroke = geom_stroke)
    if (is.null(color_var_sym)) {
      args_line$color <- current_settings$colors[1]
      args_point$color <- current_settings$colors[1]
    }
    geom_layer_base <- list(
      do.call(ggplot2::geom_line, args_line),
      do.call(ggplot2::geom_point, args_point)
    )
  } else if (plot_type == "column") {
    if (!is.null(x_sym)) {
      x_str <- rlang::as_string(x_sym)
      data[[x_str]] <- factor(data[[x_str]])
    }
    args <- list(width = 0.7, alpha = base_alpha)
    if (is.null(fill_var_sym)) args$fill <- current_settings$colors[1]
    geom_layer_base <- do.call(ggplot2::geom_col, args)
  } else if (plot_type == "map") {
    if (!is.null(fill_var_sym)) {
      geom_layer_base <- ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var_sym),
                                          alpha = base_alpha, color = current_settings$grid_color, linewidth = 0.1)
    } else {
      geom_layer_base <- ggplot2::geom_sf(fill = current_settings$colors[1],
                                          alpha = base_alpha, color = current_settings$grid_color, linewidth = 0.1)
    }
  }



  # Aplicar efectos
  if (!is.null(geom_layer_base)) {
    if (apply_shadow) {
      layer <- if (apply_glow) ggfx::with_outer_glow(geom_layer_base, colour = highlight_color, sigma = 8, expand = 5) else geom_layer_base
      p <- p + ggfx::with_shadow(layer, colour = "black", x_offset = 3, y_offset = 3, sigma = 5)
    } else if (apply_glow) {
      p <- p + ggfx::with_outer_glow(geom_layer_base, colour = highlight_color, sigma = 8, expand = 5)
    } else {
      p <- p + geom_layer_base
    }
  }

  # Línea base
  if (show_baseline && plot_type %in% c("column", "line","scatter")) {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                                 color = current_settings$grid_color, linewidth = 0.4)
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

  # Etiquetas
  if (show_labels && !is.null(label_var_sym)) {
    label_str <- rlang::as_string(label_var_sym)
    if (label_str %in% names(data)) {
      label_aes <- ggplot2::aes(label = !!label_var_sym)
      if (plot_type == "column") {
        p <- p + ggplot2::geom_text(
          mapping = label_aes,
          vjust = if (coord_flip) 0.5 else -0.5,
          hjust = if (coord_flip) -0.2 else 0.5,
          size = text_size * 0.3, color = current_settings$text_color,
          family = font_body_name
        )
      } else if (plot_type %in% c("scatter", "line")) {
        p <- p + ggrepel::geom_text_repel(
          mapping = label_aes, max.overlaps = 50,
          size = text_size * 0.3, box.padding = 0.5, direction = "y",
          segment.color = current_settings$grid_color, segment.size = 0.3,
          family = font_body_name, color = current_settings$text_color
        )
      } else if (plot_type == "map") {
        p <- p + ggplot2::geom_sf_text(mapping = label_aes,
                                       size = text_size * 0.3, color = current_settings$text_color,
                                       family = font_body_name, check_overlap = TRUE)
      }
    }

  }

  if (!is.null(facet_var_sym)) {
    p <- p + ggplot2::facet_wrap(facet_var_sym)
  }

  if (coord_flip && plot_type != "map") {
    p <- p + ggplot2::coord_flip()
  }

  # Expansión automática del eje numérico (solo límite superior)
  expand_axis_var <- if (coord_flip) as_label(x_sym) else as_label(y_sym)

  if (!is.null(expand_axis_var) && is.numeric(data[[expand_axis_var]])) {
    axis_range <- range(data[[expand_axis_var]], na.rm = TRUE)
    if (diff(axis_range) > 0) {
      # Solo expandir en el extremo superior
      if (coord_flip) {
        p <- p + scale_x_continuous(expand = expansion(mult = c(0.05, 0.15)))
      } else {
        p <- p + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15)))
      }
    } else {
      # Todos los valores son iguales, agregar pequeño buffer
      buffer <- ifelse(axis_range[1] == 0, 0.5, abs(axis_range[1]) * 0.05)
      lim <- c(axis_range[1], axis_range[2] + buffer)
      if (coord_flip) {
        p <- p + scale_x_continuous(limits = lim)
      } else {
        p <- p + scale_y_continuous(limits = lim)
      }
    }
  }

  # Tema y labs
  theme_final <- ggplot2::theme_void() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      plot.title = ggtext::element_markdown(face = "bold", hjust = 0.5, family = font_title_name, color = current_settings$text_color),
      plot.subtitle = ggtext::element_markdown(hjust = 0.5, family = font_body_name, color = current_settings$text_color),
      plot.caption = ggtext::element_markdown(hjust = 1, family = font_body_name, color = current_settings$text_color),
      legend.position = "bottom",
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      plot.background = ggplot2::element_rect(fill = current_settings$background_fill, colour = NA),
      panel.background = if (apply_blur_background)
        ggfx::with_blur(ggplot2::element_rect(fill = current_settings$panel_fill, colour = NA), sigma = 10)
      else
        ggplot2::element_rect(fill = current_settings$panel_fill, colour = NA),
      axis.text = ggplot2::element_text(color = current_settings$text_color, family = font_body_name),
      axis.title = ggtext::element_markdown(color = current_settings$text_color, family = font_body_name),
      panel.grid.major = if (plot_type == "map") ggplot2::element_blank() else ggplot2::element_line(color = current_settings$grid_color, linetype = "dotted", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type == "map") ggplot2::element_blank() else ggplot2::element_line(color = current_settings$grid_color, linewidth = 0.8)
    )

  if (plot_type == "map") {
    theme_final <- theme_final + ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  }

  labs_list <- list(
    title = paste0("<span style='font-family:\"", font_title_name, "\";'>", title, "</span>"),
    subtitle = paste0("<span style='font-family:\"", font_body_name, "\";'>", subtitle, "</span>"),
    caption = paste0("<span style='font-family:\"", font_body_name, "\";'>", caption, "</span>")
  )

  if (!is.null(x_sym) && plot_type != "map") {
    labs_list$x <- paste0("<span style='font-family:\"", font_body_name, "\";'>",
                          tools::toTitleCase(gsub("_", " ", rlang::as_string(x_sym))), "</span>")
  }

  if (!is.null(y_sym) && plot_type != "map") {
    labs_list$y <- paste0("<span style='font-family:\"", font_body_name, "\";'>",
                          tools::toTitleCase(gsub("_", " ", rlang::as_string(y_sym))), "</span>")
  }

  p + theme_final + do.call(ggplot2::labs, labs_list)
}
