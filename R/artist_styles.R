# artist_styles.R

# 1. Estilo Leonardo da Vinci
# Inspiración: Claroscuro, sfumato, anatomía, tonos terrosos.
#' @title Style for Leonardo da Vinci inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Leonardo da Vinci's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "mona_lisa", "last_supper", or "vitruvian_man".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE.
#' @param add_glow Logical, whether to add a subtle shadow effect (sfumato) for maps. Defaults to FALSE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_da_vinci <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Maestría de Da Vinci",
                           subtitle = "Un estudio de proporciones y armonía",
                           caption = "Análisis Renacentista",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("mona_lisa", "last_supper", "vitruvian_man"),
                           show_labels = FALSE, add_grid_lines = FALSE,
                           show_background = TRUE,
                           add_glow = FALSE,
                           coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("da_vinci", work_inspired_by)

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    p <- p + ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white",
        bg.r = 0.05
      )
    }

    # Efecto Sfumato (sombra sutil) para mapas
    if (add_glow) {
      p <- p + ggfx::with_shadow(
        sigma = 3, x_offset = 2, y_offset = 2, colour = "grey30", alpha = 0.5
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is.null(color_var)) {
        p <- p + ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        p <- p + ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1]) +
          ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.5, alpha = settings$geom_alpha) +
          ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        p <- p + ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }

      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_void, # Tema base para Da Vinci
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

# 2. Estilo Miguel Ángel
# Inspiración: Escultura, tonos marmóreos, fuerza, anatomía, el claro del Renacimiento.
#' @title Style for Michelangelo inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Michelangelo's works.
#' @param data A data frame.
#' @param x A string specifying the column name for the x-axis.
#' @param y A string specifying the column name for the y-axis.
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", or "column".
#' @param work_inspired_by Specific work for aesthetic inspiration: "david", "sistine_chapel", or "pieta".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines. Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_michelangelo <- function(data, x, y, color_var = NULL, fill_var = NULL, label_var = NULL,
                               title = "Magnificencia de Datos al Estilo de Miguel Ángel",
                               subtitle = "Una composición de fuerza y detalle",
                               caption = "Obra Maestra del Análisis",
                               plot_type = c("scatter", "line", "column","map"),
                               work_inspired_by = c("david", "sistine_chapel", "pieta"),
                               show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE,
                               show_background = TRUE,
                               coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("michelangelo", work_inspired_by)


  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- p + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is.null(color_var)) {
        geom_layer <- ggplot2::geom_point(size = 4.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4.5, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.6, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.6, alpha = settings$geom_alpha)
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3.5, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.75, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.75, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_minimal, # Tema base para Michelangelo
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Rembrandt inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Rembrandt's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "night_watch", "self_portrait", or "storm_sea".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_rembrandt <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Iluminando Datos al Estilo de Rembrandt",
                            subtitle = "Profundidad y contraste en la visualización",
                            caption = "Maestría en la Sombra",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("night_watch", "self_portrait", "storm_sea"),
                            show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE,
                            show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("rembrandt", work_inspired_by)

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- p + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is.null(color_var)) {
        geom_layer <- ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.5, alpha = settings$geom_alpha)
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_dark, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Vincent van Gogh inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Vincent van Gogh's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "starry_night", "sunflowers", or "irises".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_texture Logical, whether to add texture (density contours) to scatter plots (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_van_gogh <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Pasión de Van Gogh",
                           subtitle = "Un remolino de información",
                           caption = "Impresiones de Datos",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("starry_night", "sunflowers", "irises"),
                           show_labels = FALSE, add_glow = FALSE, add_texture = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("van_gogh", work_inspired_by)

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- p + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (add_texture) {
        p <- p + ggplot2::geom_density_2d(color = settings$grid_color, linewidth = 0.5, alpha = 0.6)
      }
      if (is.null(color_var)) {
        geom_layer <- ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.5, alpha = settings$geom_alpha)
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_dark, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Claude Monet inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Claude Monet's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "water_lilies", "impression_sunrise", or "poppy_fields".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_texture Logical, whether to add texture (density contours) to scatter plots (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_monet <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                        title = "La Impresión de los Datos al Estilo de Monet",
                        subtitle = "Armonía de luz y color",
                        caption = "Análisis Impresionista",
                        plot_type = c("scatter", "line", "column", "map"),
                        work_inspired_by = c("water_lilies", "impression_sunrise", "poppy_fields"),
                        show_labels = FALSE, add_glow = FALSE, add_texture = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("monet", work_inspired_by)

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- p + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is.null(color_var)) {
        geom_layer <- ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
      if (add_texture) {
        p <- p + ggplot2::geom_density_2d(color = settings$grid_color, linetype = "dotted", alpha = 0.3)
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.5, alpha = settings$geom_alpha)
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_minimal, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Banksy inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Banksy's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "girl_with_balloon", "flower_thrower", or "rat_graffiti".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_banksy <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                         title = "Datos con la Calle de Banksy",
                         subtitle = "Un mensaje subversivo en cada punto",
                         caption = "Arte Urbano de Datos",
                         plot_type = c("scatter", "line", "column", "map"),
                         work_inspired_by = c("girl_with_balloon", "flower_thrower", "rat_graffiti"),
                         show_labels = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("banksy", work_inspired_by)

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Añadir capa geom_sf
    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.8, # Líneas más gruesas para efecto stencil
      colour = settings$grid_color, # Color de contorno consistente
      ggplot2::aes_string(fill = fill_var, color = color_var) # Mapeo de relleno y color
    )

    p <- p + geom_sf_layer

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    }
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text) - crucial para el estilo de Banksy
    if (show_labels && !is.null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4, # Texto más grande para impacto
        check_overlap = TRUE, # Prevenir solapamiento de texto
        nudge_x = 0.01, nudge_y = 0.01 # Ligeramente desplazado para apariencia de stencil
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is.null(color_var)) {
      p <- p + generate_color_scale(data, color_var, settings$colors, "color")
    }
    if (!is.null(fill_var)) {
      p <- p + generate_color_scale(data, fill_var, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is.null(color_var)) {
        p <- p + ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "line") {
      if (is.null(color_var)) {
        p <- p + ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, linetype = "dashed", color = settings$colors[1]) +
          ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 1.5, alpha = settings$geom_alpha, linetype = "dashed") +
          ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 3, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      if (is.null(fill_var)) {
        p <- p + ggplot2::geom_col(width = 0.8, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.8, alpha = settings$geom_alpha)
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # Fin del bloque de gráficos no-mapa

  # Temas comunes para todos los tipos de gráficos
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_void, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Salvador Dalí inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Salvador Dalí's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "persistence_memory", "elephants", or "swans_reflecting_elephants".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_jitter Logical, whether to add a jitter effect to scatter points (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_salvador_dali <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                title = "Datos Derretidos al Estilo de Dalí",
                                subtitle = "La realidad se dobla para mostrar la verdad",
                                caption = "Análisis Surrealista",
                                plot_type = c("scatter", "line", "column", "map"),
                                work_inspired_by = c("persistence_memory", "elephants", "swans_reflecting_elephants"),
                                show_labels = FALSE, add_glow = FALSE, add_jitter = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("dali", work_inspired_by)

  # Initialize plot based on type
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Geom_sf for polygons/lines/points
    geom_sf_layer_params <- list(
      lwd = 0.3, # Finer lines, less defined for melting effect
      colour = settings$grid_color, # Consistent border color, perhaps translucent
      alpha = settings$geom_alpha, # Apply general alpha
      ggplot2::aes_string(fill = fill_var, color = color_var) # Use aes_string for flexibility
    )
    geom_sf_layer <- do.call(ggplot2::geom_sf, geom_sf_layer_params)

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 8, expand = 5)
    } else {
      p <- p + geom_sf_layer
    }

    # Apply dynamic color/fill scales for maps
    color_scale <- generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    # Labels for maps (geom_sf_text) - symbolic text, part of the surreal landscape
    if (show_labels && !is.null(label_var)) {
      geom_sf_text_layer_params <- list(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4, # Text size
        check_overlap = TRUE, # Prevent text overlap
        nudge_x = 0.01, nudge_y = 0.01 # Slightly nudge for surreal look
      )
      geom_sf_text_layer <- do.call(ggplot2::geom_sf_text, geom_sf_text_layer_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_sf_text_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_sf_text_layer
      }
    }

  } else { # Existing scatter, line, column plots
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Apply dynamic color/fill scales for non-maps
    color_scale <- generate_color_scale(data, color_var, settings$colors, "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data, fill_var, settings$colors, "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    if (plot_type == "scatter") {
      point_params <- list(
        size = 4,
        alpha = settings$geom_alpha
      )
      if (!is.null(color_var)) {
        point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        point_params$color <- settings$colors[1]
      }
      geom_layer <- do.call(ggplot2::geom_point, point_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
      if (add_jitter) {
        p <- p + ggplot2::geom_jitter(width = 0.1, height = 0.1, alpha = settings$geom_alpha * 0.3, color = settings$text_color)
      }
    } else if (plot_type == "line") {
      line_params <- list(
        size = 1.5,
        alpha = settings$geom_alpha,
        linetype = "dotted"
      )
      point_params <- list(
        size = 3,
        alpha = settings$geom_alpha
      )

      if (!is.null(color_var)) {
        line_params$mapping <- ggplot2::aes_string(color = color_var)
        point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        line_params$color <- settings$colors[1]
        point_params$color <- settings$colors[1]
      }
      geom_line_layer <- do.call(ggplot2::geom_line, line_params)
      geom_point_layer <- do.call(ggplot2::geom_point, point_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      col_params <- list(
        width = 0.7,
        alpha = settings$geom_alpha
      )
      if (!is.null(fill_var)) {
        col_params$mapping <- ggplot2::aes_string(fill = fill_var)
      } else {
        col_params$fill <- settings$colors[1]
      }
      geom_col_layer <- do.call(ggplot2::geom_col, col_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_dark, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Joan Miró inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Joan Miró's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "the_farm", "constellations", or "blue_series".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_shapes Logical, whether to use varied shapes for scatter points (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_miro <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                       title = "Datos con la Fantasía de Miró",
                       subtitle = "Un universo de formas y color",
                       caption = "Análisis Surrealista Abstracto",
                       plot_type = c("scatter", "line", "column", "map"),
                       work_inspired_by = c("the_farm", "constellations", "blue_series"),
                       show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("miro", work_inspired_by)

  # Initialize plot based on type
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Geom_sf for map polygons - Miró style: bold outlines, vibrant fills
    geom_sf_layer_params <- list(
      lwd = settings$geom_stroke, # Thicker lines for bold outlines
      colour = settings$grid_color, # Often black outlines
      alpha = settings$geom_alpha, # Miró's colors are typically opaque
      ggplot2::aes_string(fill = fill_var, color = color_var) # Use aes_string for flexibility
    )
    geom_sf_layer <- do.call(ggplot2::geom_sf, geom_sf_layer_params)
    p <- p + geom_sf_layer

    # Apply dynamic color/fill scales for maps
    color_scale <- generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    # Labels for maps (geom_sf_text) - symbolic text
    if (show_labels && !is.null(label_var)) {
      geom_sf_text_layer_params <- list(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 5, # Larger text for Miró's bold style
        check_overlap = TRUE, # Prevent text overlap
        nudge_x = 0.05, nudge_y = 0.05 # Nudge for a slightly playful placement
      )
      geom_sf_text_layer <- do.call(ggplot2::geom_sf_text, geom_sf_text_layer_params)
      p <- p + geom_sf_text_layer
    }

  } else { # Existing scatter, line, column plots
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escala de color si color_var es proporcionado
    color_scale <- generate_color_scale(data, color_var, settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }

    # Aplicar escala de relleno si fill_var es proporcionado
    fill_scale <- generate_color_scale(data, fill_var, settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    if (plot_type == "scatter") {
      point_params <- list(
        size = settings$geom_size,
        alpha = settings$geom_alpha,
        stroke = settings$geom_stroke,
        fill = "white" # Miro often uses filled shapes with outlines
      )

      if (!is.null(color_var)) {
        point_params$mapping <- ggplot2::aes_string(color = color_var)
        if (add_shapes) {
          point_params$mapping$shape <- ggplot2::aes_string(shape = color_var)$shape
          if (length(unique(data[[color_var]])) > 0) {
            p <- p + ggplot2::scale_shape_manual(values = settings$shapes[seq_along(unique(data[[color_var]]))])
          }
        } else {
          point_params$shape <- 21 # Default shape if not mapping
        }
      } else {
        point_params$color <- settings$colors[1]
        point_params$shape <- if (add_shapes) settings$shapes[1] else 21
      }
      p <- p + do.call(ggplot2::geom_point, point_params)

      segment_params <- list(
        x = x, y = 0, xend = x, yend = y,
        size = settings$geom_stroke * 0.5,
        alpha = settings$geom_alpha * 0.5,
        linetype = "solid"
      )
      if (!is.null(color_var)) {
        segment_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        segment_params$color <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_segment, segment_params)
    } else if (plot_type == "line") {
      line_params <- list(
        size = settings$geom_stroke,
        alpha = settings$geom_alpha,
        linetype = "solid"
      )
      point_params <- list(
        size = settings$geom_size * 0.7,
        alpha = settings$geom_alpha,
        shape = 21,
        stroke = settings$geom_stroke,
        fill = "white"
      )

      if (!is.null(color_var)) {
        line_params$mapping <- ggplot2::aes_string(color = color_var)
        point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        line_params$color <- settings$colors[1]
        point_params$color <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_line, line_params) +
        do.call(ggplot2::geom_point, point_params)
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      col_params <- list(
        width = 0.7,
        alpha = settings$geom_alpha
      )
      if (!is.null(fill_var)) {
        col_params$mapping <- ggplot2::aes_string(fill = fill_var)
      } else {
        col_params$fill <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_col, col_params)
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_void, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Artemisia Gentileschi inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Artemisia Gentileschi's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "judith_beheading_holofernes", "mary_magdalene", or "self_portrait_lute_player".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_artemisia_gentileschi <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                        title = "Datos con la Fuerza de Artemisia Gentileschi",
                                        subtitle = "Un contraste dramático en la visualización",
                                        caption = "Barroco de Datos",
                                        plot_type = c("scatter", "line", "column", "map"),
                                        work_inspired_by = c("judith_beheading_holofernes", "mary_magdalene", "self_portrait_lute_player"),
                                        show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("artemisia", work_inspired_by)

  # Initialize plot based on type
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Geom_sf for map polygons - Artemisia style: deep colors, potential glow
    geom_sf_layer_params <- list(
      lwd = settings$geom_lwd, # Moderate line width for forms
      colour = settings$grid_color, # Dark outline for chiaroscuro
      alpha = settings$geom_alpha, # Opaque fills
      ggplot2::aes_string(fill = fill_var, color = color_var) # Use aes_string for flexibility
    )
    geom_sf_layer <- do.call(ggplot2::geom_sf, geom_sf_layer_params)

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 6, expand = 4)
    } else {
      p <- p + geom_sf_layer
    }

    # Apply dynamic color/fill scales for maps
    color_scale <- generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    # Labels for maps (geom_sf_text) - strong figures
    if (show_labels && !is.null(label_var)) {
      geom_sf_text_layer_params <- list(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4, # Clear text
        check_overlap = TRUE
      )
      geom_sf_text_layer <- do.call(ggplot2::geom_sf_text, geom_sf_text_layer_params)
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_sf_text_layer, colour = settings$glow_color, sigma = 6, expand = 4)
      } else {
        p <- p + geom_sf_text_layer
      }
    }

  } else { # Existing scatter, line, column plots
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escala de color si color_var es proporcionado
    color_scale <- generate_color_scale(data, color_var, settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    # Aplicar escala de relleno si fill_var es proporcionado
    fill_scale <- generate_color_scale(data, fill_var, settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    if (plot_type == "scatter") {
      point_params <- list(
        size = 4.5,
        alpha = settings$geom_alpha
      )
      if (!is.null(color_var)) {
        point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        point_params$color <- settings$colors[1]
      }
      geom_layer <- do.call(ggplot2::geom_point, point_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 6, expand = 4)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      line_params <- list(
        size = 1.8,
        alpha = settings$geom_alpha
      )
      point_params <- list(
        size = 3.5,
        alpha = settings$geom_alpha
      )

      if (!is.null(color_var)) {
        line_params$mapping <- ggplot2::aes_string(color = color_var)
        point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        line_params$color <- settings$colors[1]
        point_params$color <- settings$colors[1]
      }
      geom_line_layer <- do.call(ggplot2::geom_line, line_params)
      geom_point_layer <- do.call(ggplot2::geom_point, point_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 6, expand = 4) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 6, expand = 4)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]])
      col_params <- list(
        width = 0.7,
        alpha = settings$geom_alpha
      )
      if (!is.null(fill_var)) {
        col_params$mapping <- ggplot2::aes_string(fill = fill_var)
      } else {
        col_params$fill <- settings$colors[1]
      }
      geom_col_layer <- do.call(ggplot2::geom_col, col_params)

      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 6, expand = 4)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_dark, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}

#' @title Style for Wassily Kandinsky inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Wassily Kandinsky's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots). Defaults to NULL.
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots). Defaults to NULL.
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "composition_viii", "yellow_red_blue", or "on_white_ii".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_shapes Logical, whether to use varied shapes for scatter points. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_kandinsky <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Datos con la Abstracción de Kandinsky",
                            subtitle = "Una sinfonía visual de formas y colores",
                            caption = "Composición de Datos",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("composition_viii", "yellow_red_blue", "on_white_ii"),
                            show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("kandinsky", work_inspired_by)

  # Initialize plot based on type
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Geom_sf for map polygons - Kandinsky style: vibrant colors, strong outlines
    geom_sf_layer_params <- list(
      lwd = settings$geom_lwd, # Substantial line width for clear shapes
      colour = settings$text_color, # Often black or strong contrast outlines
      alpha = settings$geom_alpha, # Solid fills
      ggplot2::aes_string(fill = fill_var, color = color_var) # Use aes_string for flexibility
    )
    geom_sf_layer <- do.call(ggplot2::geom_sf, geom_sf_layer_params)
    p <- p + geom_sf_layer

    # Apply dynamic color/fill scales for maps
    color_scale <- generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    # Labels for maps (geom_sf_text) - clear, structured elements
    if (show_labels && !is.null(label_var)) {
      geom_sf_text_layer_params <- list(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4, # Clear text
        check_overlap = TRUE
      )
      p <- p + do.call(ggplot2::geom_sf_text, geom_sf_text_layer_params)
    }

  } else { # Existing scatter, line, column plots
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escala de color si color_var es proporcionado
    color_scale <- generate_color_scale(data, color_var, settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }

    # Aplicar escala de relleno si fill_var es proporcionado
    fill_scale <- generate_color_scale(data, fill_var, settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    if (plot_type == "scatter") {
      # Preparar parámetros para geom_point
      point_params <- list(
        size = settings$geom_size,
        alpha = settings$geom_alpha,
        stroke = settings$geom_stroke
      )

      if (!is.null(color_var)) {
        point_params$mapping <- ggplot2::aes_string(color = color_var)
        if (add_shapes) {
          # Añadir mapeo de forma si add_shapes es TRUE
          point_params$mapping$shape <- ggplot2::aes_string(shape = color_var)$shape
          # Asegurarse de que scale_shape_manual se aplique solo si hay mapeo de formas
          if (length(unique(data[[color_var]])) > 0) { # Solo si hay niveles únicos para mapear
            p <- p + ggplot2::scale_shape_manual(values = settings$shapes[seq_along(unique(data[[color_var]]))])
          }
        } else {
          point_params$shape <- 16 # Forma predeterminada si no hay mapeo de formas
        }
      } else {
        # Usar color estático si no hay color_var
        point_params$color <- settings$colors[1]
        point_params$shape <- if (add_shapes) settings$shapes[1] else 16 # Forma estática
      }
      p <- p + do.call(ggplot2::geom_point, point_params)

      # Preparar parámetros para geom_segment
      segment_params <- list(
        x = x, y = 0, xend = x, yend = y,
        size = settings$geom_stroke * 0.5,
        alpha = settings$geom_alpha * 0.5,
        linetype = "solid"
      )
      if (!is.null(color_var)) {
        segment_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        segment_params$color <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_segment, segment_params)

    } else if (plot_type == "line") {
      # Preparar parámetros para geom_line
      line_params <- list(
        size = settings$geom_stroke,
        alpha = settings$geom_alpha,
        linetype = "solid"
      )
      # Preparar parámetros para geom_point en líneas
      line_point_params <- list(
        size = settings$geom_size * 0.7,
        alpha = settings$geom_alpha,
        shape = 16,
        stroke = settings$geom_stroke
      )

      if (!is.null(color_var)) {
        line_params$mapping <- ggplot2::aes_string(color = color_var)
        line_point_params$mapping <- ggplot2::aes_string(color = color_var)
      } else {
        line_params$color <- settings$colors[1]
        line_point_params$color <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_line, line_params) +
        do.call(ggplot2::geom_point, line_point_params)

    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]]) # Asegura que X sea un factor para gráficos de columnas
      # Preparar parámetros para geom_col
      col_params <- list(
        width = 0.7,
        alpha = settings$geom_alpha
      )
      if (!is.null(fill_var)) {
        col_params$mapping <- ggplot2::aes_string(fill = fill_var)
      } else {
        col_params$fill <- settings$colors[1]
      }
      p <- p + do.call(ggplot2::geom_col, col_params)
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_minimal, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}


#' @title Style for Andy Warhol inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Andy Warhol's works.
#' @param data A data frame or an sf object.
#' @param x A string specifying the column name for the x-axis (for scatter, line, column plots). Defaults to NULL.
#' @param y A string specifying the column name for the y-axis (for scatter, line, column plots). Defaults to NULL.
#' @param color_var An optional string specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional string specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional string specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "soup_cans", "marilyn_monroe", or "cow_wallpaper".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @export
style_andy_warhol <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                              title = "Datos como Pop Art de Andy Warhol",
                              subtitle = "La repetición se convierte en arte",
                              caption = "Consumo de Datos",
                              plot_type = c("scatter", "line", "column", "map"),
                              work_inspired_by = c("soup_cans", "marilyn_monroe", "cow_wallpaper"),
                              show_labels = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("warhol", work_inspired_by)

  # Initialize plot based on type
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data)

    # Geom_sf for map polygons - Warhol style: flat colors, strong outlines
    geom_sf_layer_params <- list(
      lwd = settings$geom_lwd, # Substantial line width for clear shapes
      colour = settings$text_color, # Often black or strong contrast outlines
      alpha = settings$geom_alpha, # Solid fills
      ggplot2::aes_string(fill = fill_var, color = color_var) # Use aes_string for flexibility
    )
    geom_sf_layer <- do.call(ggplot2::geom_sf, geom_sf_layer_params)
    p <- p + geom_sf_layer

    # Apply dynamic color/fill scales for maps
    color_scale <- generate_color_scale(data = data, var_name = color_var, current_colors = settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }
    fill_scale <- generate_color_scale(data = data, var_name = fill_var, current_colors = settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    # Labels for maps (geom_sf_text) - clear, structured elements, Pop Art typography
    if (show_labels && !is.null(label_var)) {
      geom_sf_text_layer_params <- list(
        ggplot2::aes_string(label = label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4, # Clear text
        check_overlap = TRUE
      )
      p <- p + do.call(ggplot2::geom_sf_text, geom_sf_text_layer_params)
    }

  } else { # Existing scatter, line, column plots
    if (is.null(x) || is.null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y))

    # Aplicar escala de color si color_var es proporcionado
    color_scale <- generate_color_scale(data, color_var, settings$colors, type = "color")
    if (!is.null(color_scale)) {
      p <- p + color_scale
    }

    # Aplicar escala de relleno si fill_var es proporcionado (principalmente para gráficos de columnas)
    fill_scale <- generate_color_scale(data, fill_var, settings$colors, type = "fill")
    if (!is.null(fill_scale)) {
      p <- p + fill_scale
    }

    if (plot_type == "scatter") {
      if (!is.null(color_var)) {
        p <- p + ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 6, alpha = settings$geom_alpha, shape = 15)
      } else {
        # Si no hay color_var, usa el primer color del setting de forma estática
        p <- p + ggplot2::geom_point(color = settings$colors[1], size = 6, alpha = settings$geom_alpha, shape = 15)
      }
    } else if (plot_type == "line") {
      if (!is.null(color_var)) {
        p <- p + ggplot2::geom_line(ggplot2::aes_string(color = color_var), size = 2, alpha = settings$geom_alpha) +
          ggplot2::geom_point(ggplot2::aes_string(color = color_var), size = 4, alpha = settings$geom_alpha, shape = 15)
      } else {
        p <- p + ggplot2::geom_line(color = settings$colors[1], size = 2, alpha = settings$geom_alpha) +
          ggplot2::geom_point(color = settings$colors[1], size = 4, alpha = settings$geom_alpha, shape = 15)
      }
    } else if (plot_type == "column") {
      data[[x]] <- factor(data[[x]]) # Asegura que X sea un factor para gráficos de columnas
      if (!is.null(fill_var)) {
        p <- p + ggplot2::geom_col(ggplot2::aes_string(fill = fill_var), width = 0.9, alpha = settings$geom_alpha)
      } else {
        # Si no hay fill_var, usa el primer color del setting de forma estática
        p <- p + ggplot2::geom_col(fill = settings$colors[1], width = 0.9, alpha = settings$geom_alpha)
      }
    }

    if (show_labels) {
      label_col <- if (!is.null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes_string(label = label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x,
    y = y,
    base_theme_fun = ggplot2::theme_minimal, # Tema base
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE # No tiene lógica especial de fondo para mapas
  )
}


# Helper function for `|||` operator (from rlang, but not exporting rlang just for this)
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

