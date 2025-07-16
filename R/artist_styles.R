# artist_styles.R

# 1. Estilo Leonardo da Vinci
# Inspiración: Claroscuro, sfumato, anatomía, tonos terrosos.
#' @title Style for Leonardo da Vinci inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Leonardo da Vinci's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @importFrom rlang sym enquo as_label quo_is_null
#' @export
style_da_vinci <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Maestría de Da Vinci",
                           subtitle = "Un estudio de proporciones y armonía",
                           caption = "Análisis Renacentista",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("mona_lisa", "last_supper", "vitruvian_man"),
                           show_labels = FALSE, add_grid_lines = FALSE,
                           show_background = TRUE, add_glow = FALSE,
                           coord_flip = FALSE, text_size = 12) {
  style_artist_common(
    data = data,
    artist = "da_vinci",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = match.arg(plot_type),
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )
}

#' @title Style for Michelangelo inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Michelangelo's works.
#' @param data A data frame.
#' @param x (Tidy-evaluated) Name of the column for the x-axis.
#' @param y (Tidy-evaluated) Name of the column for the y-axis.
#' @param color_var (Tidy-evaluated) Optional name of the column for color mapping. Defaults to NULL.
#' @param fill_var (Tidy-evaluated) Optional name of the column for fill mapping (for column charts). Defaults to NULL.
#' @param label_var (Tidy-evaluated) Optional name of the column for text labels (for maps or other plots). Defaults to NULL.
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
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang .data enquo as_string quo_is_null quo_is_missing quo_get_expr
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

  # Capturar las expresiones de las variables como quosures para tidy evaluation
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  color_quo <- rlang::enquo(color_var)
  fill_quo <- rlang::enquo(fill_var)
  label_quo <- rlang::enquo(label_var)

  # Función auxiliar para verificar si un quosure es nulo o faltante
  is_quo_missing_or_null <- function(quo) {
    rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)
  }

  # Obtener el nombre de la variable como string solo si no es missing/null
  x_var_name <- if (!is_quo_missing_or_null(x_quo)) rlang::as_string(rlang::quo_get_expr(x_quo)) else NULL
  y_var_name <- if (!is_quo_missing_or_null(y_quo)) rlang::as_string(rlang::quo_get_expr(y_quo)) else NULL
  color_var_name <- if (!is_quo_missing_or_null(color_quo)) rlang::as_string(rlang::quo_get_expr(color_quo)) else NULL
  fill_var_name <- if (!is_quo_missing_or_null(fill_quo)) rlang::as_string(rlang::quo_get_expr(fill_quo)) else NULL
  label_var_name <- if (!is_quo_missing_or_null(label_quo)) rlang::as_string(rlang::quo_get_expr(label_quo)) else NULL


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
      ggplot2::aes(fill = !!fill_quo, color = !!color_quo) # Mapeo de relleno y color
    )

    if (add_glow) {
      p <- p + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- p + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!is_quo_missing_or_null(fill_quo)) {
      p <- p + generate_color_scale(data = data, var_name = fill_var_name, current_colors = settings$colors, type = "fill")
    }
    if (!is_quo_missing_or_null(color_quo)) {
      p <- p + generate_color_scale(data = data, var_name = color_var_name, current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !is_quo_missing_or_null(label_quo)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes(label = !!label_quo),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (is_quo_missing_or_null(x_quo) || is_quo_missing_or_null(y_quo)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x_quo, y = !!y_quo))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!is_quo_missing_or_null(color_quo)) {
      p <- p + generate_color_scale(data, color_var_name, settings$colors, "color")
    }
    if (!is_quo_missing_or_null(fill_quo)) {
      p <- p + generate_color_scale(data, fill_var_name, settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (is_quo_missing_or_null(color_quo)) {
        geom_layer <- ggplot2::geom_point(size = 4.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes(color = !!color_quo), size = 4.5, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (is_quo_missing_or_null(color_quo)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.6, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes(color = !!color_quo), size = 1.6, alpha = settings$geom_alpha)
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes(color = !!color_quo), size = 3.5, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      # Asegurar que la columna x sea un factor si es para un gráfico de barras
      data[[x_var_name]] <- factor(data[[x_var_name]])
      if (is_quo_missing_or_null(fill_quo)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.75, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes(fill = !!fill_quo), width = 0.75, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      # Mapeo de etiqueta: label_var si existe, sino y
      label_mapping_quo <- if (!is_quo_missing_or_null(label_quo)) label_quo else y_quo
      if (plot_type == "column") {
        p <- p + ggplot2::geom_text(ggplot2::aes(label = !!label_mapping_quo), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_mapping_quo), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
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
    x_label = x_var_name, # Se pasa el nombre de la variable como string
    y_label = y_var_name, # Se pasa el nombre de la variable como string
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
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_density_2d
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang sym enquo as_label quo_is_null expr inject !!!
#' @export
style_rembrandt <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Iluminando Datos al Estilo de Rembrandt",
                            subtitle = "Profundidad y contraste en la visualización",
                            caption = "Maestría en la Sombra",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("night_watch", "self_portrait", "storm_sea"),
                            show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE,
                            show_background = TRUE, coord_flip = FALSE) {

  # Capturar las expresiones de las variables para tidy evaluation
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  color_var_quo <- rlang::enquo(color_var)
  fill_var_quo <- rlang::enquo(fill_var)
  label_var_quo <- rlang::enquo(label_var)

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("rembrandt", work_inspired_by)

  # Para el título de los ejes en labs, necesitamos el nombre de la columna como string
  x_label_str <- if (!rlang::quo_is_null(x_quo) && plot_type != "map") rlang::as_label(x_quo) else NULL
  y_label_str <- if (!rlang::quo_is_null(y_quo) && plot_type != "map") rlang::as_label(y_quo) else NULL

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }

    # Definir los mappings de fill y color para geom_sf si las variables existen
    fill_map <- if (!rlang::quo_is_null(fill_var_quo)) rlang::expr(fill = {{fill_var}}) else NULL
    color_map <- if (!rlang::quo_is_null(color_var_quo)) rlang::expr(color = {{color_var}}) else NULL

    # Combinar los mappings de forma segura
    aes_map_sf <- rlang::inject(ggplot2::aes(!!!c(fill_map, color_map)))

    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      aes_map_sf # Mapeo de relleno y color con tidy evaluation
    )

    if (add_glow) {
      p <- ggplot2::ggplot(data = data) + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- ggplot2::ggplot(data = data) + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data = data, var_name = rlang::as_label(fill_var_quo), current_colors = settings$colors, type = "fill")
    }
    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data = data, var_name = rlang::as_label(color_var_quo), current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !rlang::quo_is_null(label_var_quo)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes(label = {{label_var}}), # Usar {{}} para label_var
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (rlang::quo_is_null(x_quo) || rlang::quo_is_null(y_quo)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    # Inicializar ggplot con {{}} para x e y
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, y = {{y}}))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(color_var_quo), settings$colors, "color")
    }
    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(fill_var_quo), settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (rlang::quo_is_null(color_var_quo)) {
        geom_layer <- ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 4, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (rlang::quo_is_null(color_var_quo)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes(color = {{color_var}}), size = 1.5, alpha = settings$geom_alpha) # Usar {{}}
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 3, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      # Acceder a la columna por su nombre string para convertirla a factor
      data[[rlang::as_label(x_quo)]] <- factor(data[[rlang::as_label(x_quo)]])
      if (rlang::quo_is_null(fill_var_quo)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes(fill = {{fill_var}}), width = 0.7, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col_quo <- if (!rlang::quo_is_null(label_var_quo)) label_var_quo else y_quo # Usar el quosure de y
      if (plot_type == "column") {
        if (coord_flip) {
          # Ajustar geom_text para coord_flip, invirtiendo x e y para las etiquetas
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}, x = {{y}}, y = {{x}}), hjust = -0.3,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}), vjust = -0.5, hjust = 0.5, # Usar {{}}
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }

      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = {{label_col_quo}}), size = 3.5, box.padding = 0.5, min.segment.length = 0.3, # Usar {{}}
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
    x_label = x_label_str, # Pasar el string del nombre de la columna
    y_label = y_label_str, # Pasar el string del nombre de la columna
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
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_density_2d
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang sym enquo as_label quo_is_null expr inject !!!
#' @export
style_van_gogh <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Pasión de Van Gogh",
                           subtitle = "Un remolino de información",
                           caption = "Impresiones de Datos",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("starry_night", "sunflowers", "irises"),
                           show_labels = FALSE, add_glow = FALSE, add_texture = FALSE, add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {

  # Capturar las expresiones de las variables para tidy evaluation
  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  color_var_quo <- rlang::enquo(color_var)
  fill_var_quo <- rlang::enquo(fill_var)
  label_var_quo <- rlang::enquo(label_var)

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("van_gogh", work_inspired_by)

  # Para el título de los ejes en labs, necesitamos el nombre de la columna como string
  x_label_str <- if (!rlang::quo_is_null(x_quo) && plot_type != "map") rlang::as_label(x_quo) else NULL
  y_label_str <- if (!rlang::quo_is_null(y_quo) && plot_type != "map") rlang::as_label(y_quo) else NULL

  # Inicializar el objeto ggplot
  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }

    # Definir los mappings de fill y color para geom_sf si las variables existen
    fill_map <- if (!rlang::quo_is_null(fill_var_quo)) rlang::expr(fill = {{fill_var}}) else NULL
    color_map <- if (!rlang::quo_is_null(color_var_quo)) rlang::expr(color = {{color_var}}) else NULL

    # Combinar los mappings de forma segura
    aes_map_sf <- rlang::inject(ggplot2::aes(!!!c(fill_map, color_map)))

    geom_sf_layer <- ggplot2::geom_sf(
      lwd = 0.6,
      colour = settings$grid_color, # Color de contorno predeterminado para sf
      aes_map_sf # Mapeo de relleno y color con tidy evaluation
    )

    if (add_glow) {
      p <- ggplot2::ggplot(data = data) + ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      p <- ggplot2::ggplot(data = data) + geom_sf_layer
    }

    # Aplicar escalas de color y relleno dinámicamente para mapas
    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data = data, var_name = rlang::as_label(fill_var_quo), current_colors = settings$colors, type = "fill")
    }
    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data = data, var_name = rlang::as_label(color_var_quo), current_colors = settings$colors, type = "color")
    }

    # Etiquetas para mapas (geom_sf_text)
    if (show_labels && !rlang::quo_is_null(label_var_quo)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes(label = {{label_var}}), # Usar {{}} para label_var
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white", # Fondo blanco para las etiquetas para mejorar la legibilidad
        bg.r = 0.05
      )
    }

  } else { # Lógica para gráficos de dispersión, línea y columna
    if (rlang::quo_is_null(x_quo) || rlang::quo_is_null(y_quo)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    # Inicializar ggplot con {{}} para x e y
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, y = {{y}}))

    # Aplicar escalas de color y relleno dinámicamente para no-mapas
    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(color_var_quo), settings$colors, "color")
    }
    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(fill_var_quo), settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      # geom_density_2d no usa aes_string, así que no necesita {{}}
      if (add_texture) {
        p <- p + ggplot2::geom_density_2d(color = settings$grid_color, linewidth = 0.5, alpha = 0.6)
      }
      if (rlang::quo_is_null(color_var_quo)) {
        geom_layer <- ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_layer <- ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 4, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_layer
      }
    } else if (plot_type == "line") {
      if (rlang::quo_is_null(color_var_quo)) {
        geom_line_layer <- ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        geom_point_layer <- ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_line_layer <- ggplot2::geom_line(ggplot2::aes(color = {{color_var}}), size = 1.5, alpha = settings$geom_alpha) # Usar {{}}
        geom_point_layer <- ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 3, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_line_layer + geom_point_layer
      }
    } else if (plot_type == "column") {
      # Acceder a la columna por su nombre string para convertirla a factor
      data[[rlang::as_label(x_quo)]] <- factor(data[[rlang::as_label(x_quo)]])
      if (rlang::quo_is_null(fill_var_quo)) {
        geom_col_layer <- ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col_layer <- ggplot2::geom_col(ggplot2::aes(fill = {{fill_var}}), width = 0.7, alpha = settings$geom_alpha) # Usar {{}}
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        p <- p + geom_col_layer
      }
    }

    if (show_labels) {
      label_col_quo <- if (!rlang::quo_is_null(label_var_quo)) label_var_quo else y_quo # Usar el quosure de y
      if (plot_type == "column") {
        if (coord_flip) {
          # Ajustar geom_text para coord_flip, invirtiendo x e y para las etiquetas
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}, x = {{y}}, y = {{x}}), hjust = -0.3,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}), vjust = -0.5, hjust = 0.5, # Usar {{}}
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = {{label_col_quo}}), size = 3.5, box.padding = 0.5, min.segment.length = 0.3, # Usar {{}}
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
    x_label = x_label_str, # Pasar el string del nombre de la columna
    y_label = y_label_str, # Pasar el string del nombre de la columna
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
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
#' @param coord_flip Logical, whether to flip labels coords in column chart. Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_density_2d
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_monet <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                        title = "La Impresión de los Datos al Estilo de Monet",
                        subtitle = "Armonía de luz y color",
                        caption = "Análisis Impresionista",
                        plot_type = c("scatter", "line", "column", "map"),
                        work_inspired_by = c("water_lilies", "impression_sunrise", "poppy_fields"),
                        show_labels = FALSE, add_glow = FALSE, add_texture = FALSE,
                        add_grid_lines = FALSE, show_background = TRUE, coord_flip = FALSE) {

  x_quo <- rlang::enquo(x)
  y_quo <- rlang::enquo(y)
  color_var_quo <- rlang::enquo(color_var)
  fill_var_quo <- rlang::enquo(fill_var)
  label_var_quo <- rlang::enquo(label_var)

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("monet", work_inspired_by)

  x_label_str <- if (!rlang::quo_is_null(x_quo) && plot_type != "map") rlang::as_label(x_quo) else NULL
  y_label_str <- if (!rlang::quo_is_null(y_quo) && plot_type != "map") rlang::as_label(y_quo) else NULL

  if (plot_type == "map") {
    if (!inherits(data, "sf")) stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")

    fill_map <- if (!rlang::quo_is_null(fill_var_quo)) rlang::expr(fill = {{fill_var}}) else NULL
    color_map <- if (!rlang::quo_is_null(color_var_quo)) rlang::expr(color = {{color_var}}) else NULL
    aes_map_sf <- rlang::inject(ggplot2::aes(!!!c(fill_map, color_map)))

    geom_sf_layer <- ggplot2::geom_sf(aes_map_sf, lwd = 0.6, colour = settings$grid_color)

    p <- ggplot2::ggplot(data = data) +
      if (add_glow) ggfx::with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    else geom_sf_layer

    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(fill_var_quo), settings$colors, "fill")
    }
    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(color_var_quo), settings$colors, "color")
    }

    if (show_labels && !rlang::quo_is_null(label_var_quo)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes(label = {{label_var}}),
        color = settings$text_color,
        family = settings$font_body,
        size = 3,
        bg.colour = "white",
        bg.r = 0.05
      )
    }

  } else {
    if (rlang::quo_is_null(x_quo) || rlang::quo_is_null(y_quo)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, y = {{y}}))

    if (!rlang::quo_is_null(color_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(color_var_quo), settings$colors, "color")
    }
    if (!rlang::quo_is_null(fill_var_quo)) {
      p <- p + generate_color_scale(data, rlang::as_label(fill_var_quo), settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (add_texture) {
        p <- p + ggplot2::geom_density_2d(color = settings$grid_color, linetype = "dotted", alpha = 0.3)
      }
      geom_layer <- if (rlang::quo_is_null(color_var_quo)) {
        ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 4, alpha = settings$geom_alpha)
      }
      p <- p + if (add_glow) ggfx::with_outer_glow(geom_layer, colour = settings$glow_color, sigma = 5, expand = 3) else geom_layer

    } else if (plot_type == "line") {
      geom_line_layer <- if (rlang::quo_is_null(color_var_quo)) {
        ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = {{color_var}}), size = 1.5, alpha = settings$geom_alpha)
      }
      geom_point_layer <- if (rlang::quo_is_null(color_var_quo)) {
        ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = {{color_var}}), size = 3, alpha = settings$geom_alpha)
      }
      p <- p + if (add_glow) {
        ggfx::with_outer_glow(geom_line_layer, colour = settings$glow_color, sigma = 5, expand = 3) +
          ggfx::with_outer_glow(geom_point_layer, colour = settings$glow_color, sigma = 5, expand = 3)
      } else {
        geom_line_layer + geom_point_layer
      }

    } else if (plot_type == "column") {
      data[[rlang::as_label(x_quo)]] <- factor(data[[rlang::as_label(x_quo)]])
      geom_col_layer <- if (rlang::quo_is_null(fill_var_quo)) {
        ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = {{fill_var}}), width = 0.7, alpha = settings$geom_alpha)
      }
      p <- p + if (add_glow) ggfx::with_outer_glow(geom_col_layer, colour = settings$glow_color, sigma = 5, expand = 3) else geom_col_layer
    }

    if (show_labels) {
      label_col_quo <- if (!rlang::quo_is_null(label_var_quo)) label_var_quo else y_quo
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}, x = {{y}}, y = {{x}}), hjust = -0.3,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(label = {{label_col_quo}}), vjust = -0.5, hjust = 0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = {{label_col_quo}}), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = x_label_str,
    y_label = y_label_str,
    base_theme_fun = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}

#' @title Style for Banksy inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Banksy's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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

  x <- enquo(x)
  y <- enquo(y)
  color_var <- enquo(color_var)
  fill_var <- enquo(fill_var)
  label_var <- enquo(label_var)

  if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = !!fill_var, color = !!color_var),
        lwd = 0.8,
        colour = settings$grid_color
      )

    if (!quo_is_null(fill_var)) {
      p <- p + generate_color_scale(data = data, var_name = as_label(fill_var), current_colors = settings$colors, type = "fill")
    }
    if (!quo_is_null(color_var)) {
      p <- p + generate_color_scale(data = data, var_name = as_label(color_var), current_colors = settings$colors, type = "color")
    }

    if (show_labels && !quo_is_null(label_var)) {
      p <- p + ggplot2::geom_sf_text(
        ggplot2::aes(label = !!label_var),
        color = settings$text_color,
        family = settings$font_body,
        size = 4,
        check_overlap = TRUE,
        nudge_x = 0.01,
        nudge_y = 0.01
      )
    }

  } else {
    if (quo_is_null(x) || quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (!quo_is_null(color_var)) {
      p <- p + generate_color_scale(data, as_label(color_var), settings$colors, "color")
    }
    if (!quo_is_null(fill_var)) {
      p <- p + generate_color_scale(data, as_label(fill_var), settings$colors, "fill")
    }

    if (plot_type == "scatter") {
      if (quo_is_null(color_var)) {
        p <- p + ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 4, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "line") {
      if (quo_is_null(color_var)) {
        p <- p +
          ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, linetype = "dashed", color = settings$colors[1]) +
          ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        p <- p +
          ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = 1.5, alpha = settings$geom_alpha, linetype = "dashed") +
          ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 3, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "column") {
      data[[as_label(x)]] <- factor(data[[as_label(x)]])
      if (quo_is_null(fill_var)) {
        p <- p + ggplot2::geom_col(width = 0.8, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        p <- p + ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.8, alpha = settings$geom_alpha)
      }
    }

    if (show_labels) {
      label_col <- if (!quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        if (coord_flip) {
          p <- p +
            ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3,
                               size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          p <- p + ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5,
                                      size = 3.5, color = settings$text_color, family = settings$font_body)
        }
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = as_label(x),
    y_label = as_label(y),
    base_theme_fun = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}


#' @title Style for Salvador Dalí inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Salvador Dalí's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_var <- rlang::enquo(color_var)
  fill_var <- rlang::enquo(fill_var)
  label_var <- rlang::enquo(label_var)

  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    base <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var, color = !!color_var), lwd = 0.3, colour = settings$grid_color, alpha = settings$geom_alpha)
    if (add_glow) base <- ggfx::with_outer_glow(base, colour = settings$glow_color, sigma = 8, expand = 5)

    if (show_labels && !rlang::quo_is_null(label_var)) {
      layer <- ggplot2::geom_sf_text(ggplot2::aes(label = !!label_var), color = settings$text_color, family = settings$font_body, size = 4, check_overlap = TRUE, nudge_x = 0.01, nudge_y = 0.01)
      base <- base + if (add_glow) ggfx::with_outer_glow(layer, colour = settings$glow_color, sigma = 5, expand = 3) else layer
    }
    base
  } else {
    if (rlang::quo_is_null(x) || rlang::quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (plot_type == "scatter") {
      geom <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 4, alpha = settings$geom_alpha)
      }
      if (add_glow) p <- p + ggfx::with_outer_glow(geom, colour = settings$glow_color, sigma = 5, expand = 3) else p <- p + geom
      if (add_jitter) p <- p + ggplot2::geom_jitter(width = 0.1, height = 0.1, alpha = settings$geom_alpha * 0.3, color = settings$text_color)
    } else if (plot_type == "line") {
      line <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_line(size = 1.5, alpha = settings$geom_alpha, linetype = "dotted", color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = 1.5, alpha = settings$geom_alpha, linetype = "dotted")
      }
      point <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 3, alpha = settings$geom_alpha)
      }
      p <- if (add_glow) p + ggfx::with_outer_glow(line, colour = settings$glow_color, sigma = 5, expand = 3) + ggfx::with_outer_glow(point, colour = settings$glow_color, sigma = 5, expand = 3) else p + line + point
    } else if (plot_type == "column") {
      data[[rlang::as_label(x)]] <- factor(data[[rlang::as_label(x)]])
      geom <- if (rlang::quo_is_null(fill_var)) {
        ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      p <- if (add_glow) p + ggfx::with_outer_glow(geom, colour = settings$glow_color, sigma = 5, expand = 3) else p + geom
    }

    if (show_labels) {
      label_col <- if (!rlang::quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        text_layer <- if (coord_flip) {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3, size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5, size = 3.5, color = settings$text_color, family = settings$font_body)
        }
        p <- p + text_layer
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3, segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y", family = settings$font_body, color = settings$text_color)
      }
    }
    p
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = rlang::as_label(x),
    y_label = rlang::as_label(y),
    base_theme_fun = ggplot2::theme_dark,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}

#' @title Style for Joan Miró inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Joan Miró's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_var <- rlang::enquo(color_var)
  fill_var <- rlang::enquo(fill_var)
  label_var <- rlang::enquo(label_var)

  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    base <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var, color = !!color_var), lwd = settings$geom_stroke, colour = settings$grid_color, alpha = settings$geom_alpha)

    if (show_labels && !rlang::quo_is_null(label_var)) {
      base <- base + ggplot2::geom_sf_text(ggplot2::aes(label = !!label_var), color = settings$text_color, family = settings$font_body, size = 5, check_overlap = TRUE, nudge_x = 0.05, nudge_y = 0.05)
    }
    base
  } else {
    if (rlang::quo_is_null(x) || rlang::quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (plot_type == "scatter") {
      geom <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = settings$geom_size, alpha = settings$geom_alpha, stroke = settings$geom_stroke, fill = "white", shape = if (add_shapes) settings$shapes[1] else 21, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = settings$geom_size, alpha = settings$geom_alpha, stroke = settings$geom_stroke, fill = "white", shape = if (add_shapes) settings$shapes[1] else 21)
      }
      p <- p + geom
    } else if (plot_type == "line") {
      line <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_line(size = settings$geom_stroke, alpha = settings$geom_alpha, linetype = "solid", color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = settings$geom_stroke, alpha = settings$geom_alpha, linetype = "solid")
      }
      point <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = settings$geom_size * 0.7, alpha = settings$geom_alpha, shape = 21, stroke = settings$geom_stroke, fill = "white", color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = settings$geom_size * 0.7, alpha = settings$geom_alpha, shape = 21, stroke = settings$geom_stroke, fill = "white")
      }
      p <- p + line + point
    } else if (plot_type == "column") {
      data[[rlang::as_label(x)]] <- factor(data[[rlang::as_label(x)]])
      geom <- if (rlang::quo_is_null(fill_var)) {
        ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      p <- p + geom
    }

    if (show_labels) {
      label_col <- if (!rlang::quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        text_layer <- if (coord_flip) {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3, size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5, size = 3.5, color = settings$text_color, family = settings$font_body)
        }
        p <- p + text_layer
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3, segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y", family = settings$font_body, color = settings$text_color)
      }
    }
    p
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = rlang::as_label(x),
    y_label = rlang::as_label(y),
    base_theme_fun = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}

#' @title Style for Artemisia Gentileschi inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Artemisia Gentileschi's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_var <- rlang::enquo(color_var)
  fill_var <- rlang::enquo(fill_var)
  label_var <- rlang::enquo(label_var)

  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    base <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var, color = !!color_var), lwd = settings$geom_lwd, colour = settings$grid_color, alpha = settings$geom_alpha)

    if (show_labels && !rlang::quo_is_null(label_var)) {
      label_layer <- ggplot2::geom_sf_text(ggplot2::aes(label = !!label_var), color = settings$text_color, family = settings$font_body, size = 4, check_overlap = TRUE)
      base <- if (add_glow) base + ggfx::with_outer_glow(label_layer, colour = settings$glow_color, sigma = 6, expand = 4) else base + label_layer
    }
    base
  } else {
    if (rlang::quo_is_null(x) || rlang::quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (plot_type == "scatter") {
      geom <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 4.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 4.5, alpha = settings$geom_alpha)
      }
      p <- if (add_glow) p + ggfx::with_outer_glow(geom, colour = settings$glow_color, sigma = 6, expand = 4) else p + geom

    } else if (plot_type == "line") {
      line <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_line(size = 1.8, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = 1.8, alpha = settings$geom_alpha)
      }
      point <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 3.5, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 3.5, alpha = settings$geom_alpha)
      }
      if (add_glow) {
        p <- p + ggfx::with_outer_glow(line, colour = settings$glow_color, sigma = 6, expand = 4) +
          ggfx::with_outer_glow(point, colour = settings$glow_color, sigma = 6, expand = 4)
      } else {
        p <- p + line + point
      }

    } else if (plot_type == "column") {
      data[[rlang::as_label(x)]] <- factor(data[[rlang::as_label(x)]])
      geom <- if (rlang::quo_is_null(fill_var)) {
        ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      p <- if (add_glow) p + ggfx::with_outer_glow(geom, colour = settings$glow_color, sigma = 6, expand = 4) else p + geom
    }

    if (show_labels) {
      label_col <- if (!rlang::quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        text_layer <- if (coord_flip) {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3, size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5, size = 3.5, color = settings$text_color, family = settings$font_body)
        }
        p <- p + text_layer
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3, segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y", family = settings$font_body, color = settings$text_color)
      }
    }
    p
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = rlang::as_label(x),
    y_label = rlang::as_label(y),
    base_theme_fun = ggplot2::theme_dark,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}


#' @title Style for Wassily Kandinsky inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Wassily Kandinsky's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
                            show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE,
                            show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("kandinsky", work_inspired_by)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_var <- rlang::enquo(color_var)
  fill_var <- rlang::enquo(fill_var)
  label_var <- rlang::enquo(label_var)

  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    base <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var, color = !!color_var),
                       lwd = settings$geom_lwd, colour = settings$text_color,
                       alpha = settings$geom_alpha)

    if (show_labels && !rlang::quo_is_null(label_var)) {
      base <- base + ggplot2::geom_sf_text(ggplot2::aes(label = !!label_var),
                                           color = settings$text_color,
                                           family = settings$font_body,
                                           size = 4,
                                           check_overlap = TRUE)
    }
    base
  } else {
    if (rlang::quo_is_null(x) || rlang::quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (plot_type == "scatter") {
      shape_val <- if (add_shapes && !rlang::quo_is_null(color_var)) 21 else 16
      point_layer <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = settings$geom_size, alpha = settings$geom_alpha,
                            shape = shape_val, stroke = settings$geom_stroke,
                            color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var),
                            size = settings$geom_size, alpha = settings$geom_alpha,
                            shape = shape_val, stroke = settings$geom_stroke)
      }
      segment_layer <- ggplot2::geom_segment(ggplot2::aes(x = !!x, xend = !!x, y = 0, yend = !!y),
                                             size = settings$geom_stroke * 0.5,
                                             alpha = settings$geom_alpha * 0.5,
                                             linetype = "solid",
                                             color = if (rlang::quo_is_null(color_var)) settings$colors[1] else NULL)
      p <- p + point_layer + segment_layer

    } else if (plot_type == "line") {
      line <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_line(size = settings$geom_stroke, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = settings$geom_stroke, alpha = settings$geom_alpha)
      }
      point <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = settings$geom_size * 0.7, alpha = settings$geom_alpha,
                            shape = 16, stroke = settings$geom_stroke, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = settings$geom_size * 0.7,
                            alpha = settings$geom_alpha, shape = 16, stroke = settings$geom_stroke)
      }
      p <- p + line + point

    } else if (plot_type == "column") {
      data[[rlang::as_label(x)]] <- factor(data[[rlang::as_label(x)]])
      geom <- if (rlang::quo_is_null(fill_var)) {
        ggplot2::geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.7, alpha = settings$geom_alpha)
      }
      p <- p + geom
    }

    if (show_labels) {
      label_col <- if (!rlang::quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        text_layer <- if (coord_flip) {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3,
                             size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5,
                             size = 3.5, color = settings$text_color, family = settings$font_body)
        }
        p <- p + text_layer
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5,
                                          box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3,
                                          max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
    p
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = rlang::as_label(x),
    y_label = rlang::as_label(y),
    base_theme_fun = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}



#' @title Style for Andy Warhol inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Andy Warhol's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
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
                              show_labels = FALSE, add_grid_lines = FALSE,
                              show_background = TRUE, coord_flip = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)
  settings <- get_artist_settings("warhol", work_inspired_by)

  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  color_var <- rlang::enquo(color_var)
  fill_var <- rlang::enquo(fill_var)
  label_var <- rlang::enquo(label_var)

  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) {
      stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    }
    base <- ggplot2::ggplot(data) +
      ggplot2::geom_sf(ggplot2::aes(fill = !!fill_var, color = !!color_var),
                       lwd = settings$geom_lwd, colour = settings$text_color,
                       alpha = settings$geom_alpha)

    if (show_labels && !rlang::quo_is_null(label_var)) {
      base <- base + ggplot2::geom_sf_text(ggplot2::aes(label = !!label_var),
                                           color = settings$text_color,
                                           family = settings$font_body,
                                           size = 4,
                                           check_overlap = TRUE)
    }
    base
  } else {
    if (rlang::quo_is_null(x) || rlang::quo_is_null(y)) {
      stop("Para plot_type = 'scatter', 'line', o 'column', 'x' e 'y' deben ser especificados.")
    }
    p <- ggplot2::ggplot(data, ggplot2::aes(x = !!x, y = !!y))

    if (plot_type == "scatter") {
      geom <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 6, alpha = settings$geom_alpha,
                            shape = 15, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var),
                            size = 6, alpha = settings$geom_alpha, shape = 15)
      }
      p <- p + geom

    } else if (plot_type == "line") {
      line <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_line(size = 2, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        ggplot2::geom_line(ggplot2::aes(color = !!color_var), size = 2, alpha = settings$geom_alpha)
      }
      point <- if (rlang::quo_is_null(color_var)) {
        ggplot2::geom_point(size = 4, alpha = settings$geom_alpha, shape = 15, color = settings$colors[1])
      } else {
        ggplot2::geom_point(ggplot2::aes(color = !!color_var), size = 4, alpha = settings$geom_alpha, shape = 15)
      }
      p <- p + line + point

    } else if (plot_type == "column") {
      data[[rlang::as_label(x)]] <- factor(data[[rlang::as_label(x)]])
      geom <- if (rlang::quo_is_null(fill_var)) {
        ggplot2::geom_col(width = 0.9, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        ggplot2::geom_col(ggplot2::aes(fill = !!fill_var), width = 0.9, alpha = settings$geom_alpha)
      }
      p <- p + geom
    }

    if (show_labels) {
      label_col <- if (!rlang::quo_is_null(label_var)) label_var else y
      if (plot_type == "column") {
        text_layer <- if (coord_flip) {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), hjust = -0.3,
                             size = 3.5, color = settings$text_color, family = settings$font_body)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = !!label_col), vjust = -0.5,
                             size = 3.5, color = settings$text_color, family = settings$font_body)
        }
        p <- p + text_layer
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = !!label_col), size = 3.5,
                                          box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3,
                                          max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
    p
  }

  apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = rlang::as_label(x),
    y_label = rlang::as_label(y),
    base_theme_fun = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE
  )
}


# Helper function for `|||` operator (from rlang, but not exporting rlang just for this)
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

