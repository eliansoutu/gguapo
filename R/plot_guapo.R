#' @title plot_guapo: Función de visualización de datos altamente estilizada
#' @description Genera gráficos ggplot2 con un alto impacto visual, utilizando paletas de colores
#'   modernas, tipografías distintivas y efectos visuales avanzados de `ggfx`.
#' @param data Un data frame (o un objeto `sf` para `plot_type = "map"`).
#' @param x Una cadena de texto con el nombre de la columna para el eje x. **Ahora opcional, por defecto es NULL.**
#' @param y Una cadena de texto con el nombre de la columna para el eje y. **Ahora opcional, por defecto es NULL.**
#' @param color_var (Opcional) Una cadena de texto para mapear color. Por defecto es NULL.
#' @param fill_var (Opcional) Una cadena de texto para mapear relleno (para gráficos de barras/áreas/mapas). Por defecto es NULL.
#' @param label_var (Opcional) Una cadena de texto para etiquetas de datos. Por defecto es NULL.
#' @param title El título del gráfico.
#' @param subtitle El subtítulo del gráfico.
#' @param caption La leyenda o fuente del gráfico.
#' @param plot_type El tipo de gráfico: "scatter", "line", "column", "area", "map".
#' @param palette_name El nombre de la paleta de colores a usar: "neoflash", "glamour", "soft_futuristic", "bold_minimal".
#' @param font_title_name La fuente de Google Fonts para el título.
#' @param font_body_name La fuente de Google Fonts para el cuerpo del texto y ejes.
#' @param show_labels Lógico, si mostrar etiquetas de datos. Por defecto es FALSE.
#' @param add_trend_line Lógico, si añadir una línea de tendencia (solo para scatter/line). Por defecto es FALSE.
#' @param highlight_values Un vector de valores en `x`, `color_var` o `label_var` a resaltar. Por defecto es NULL.
#' @param highlight_color El color para los valores resaltados. Por defecto es "cyan".
#' @param facet_var (Opcional) Una cadena de texto para facetear el gráfico. Por defecto es NULL.
#' @param dark_mode Lógico, si usar un esquema de colores oscuro. Por defecto es TRUE.
#' @param apply_shadow Lógico, si aplicar una sombra a los elementos geométricos. Requiere `ggfx`. Por defecto es FALSE.
#' @param apply_glow Lógico, si aplicar un efecto de brillo a los elementos geométricos. Requiere `ggfx`. Por defecto es FALSE.
#' @param apply_blur_background Lógico, si aplicar un desenfoque al fondo del panel. Requiere `ggfx`. Por defecto es FALSE.
#' @param gradient_fill Lógico, si usar un relleno degradado para variables continuas. Por defecto es FALSE.
#' @param base_alpha La transparencia base de los elementos geométricos. Por defecto es 0.8.
#' @param geom_size El tamaño base de los puntos o el grosor de las líneas. Por defecto es 3.
#' @param geom_stroke El grosor del borde de los puntos. Por defecto es 0.5.
#' @return Un objeto ggplot2.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_area geom_smooth theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn facet_wrap geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf # Aunque no se usa directamente en la función, se importa para coherencia con sf objects
#' @export
plot_guapo <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL, # x e y ahora son opcionales
                       title = "Visualización de Datos Impactante",
                       subtitle = "Con Estilo y Diseño Avanzado",
                       caption = "Generado con plot_guapo",
                       plot_type = c("scatter", "line", "column", "area", "map"),
                       palette_name = c("neoflash", "glamour", "soft_futuristic", "bold_minimal"),
                       font_title_name = "Orbitron", font_body_name = "Poppins",
                       show_labels = FALSE, add_trend_line = FALSE,
                       highlight_values = NULL, highlight_color = "#FFD700", # Gold for highlighting
                       facet_var = NULL,
                       dark_mode = TRUE,
                       apply_shadow = FALSE, apply_glow = FALSE, apply_blur_background = FALSE,
                       gradient_fill = FALSE, base_alpha = 0.8, geom_size = 3, geom_stroke = 0.5) {

  plot_type <- match.arg(plot_type)
  palette_name <- match.arg(palette_name)

  # Definición de paletas y ajustes
  palettes <- list(
    "neoflash" = list(
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
    "glamour" = list(
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
    "soft_futuristic" = list(
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
    "bold_minimal" = list(
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

  # Base plot
  if (plot_type == "map") {
    p <- ggplot2::ggplot(data)
  } else {
    # Only map x and y if they are provided and not for map type
    aes_mapping <- list()
    if (!is.null(x)) aes_mapping$x <- x
    if (!is.null(y)) aes_mapping$y <- y
    p <- ggplot2::ggplot(data, do.call(ggplot2::aes_string, aes_mapping))
  }

  # Dynamic color/fill scales
  if (!is.null(color_var)) {
    if (gradient_fill && is.numeric(data[[color_var]])) {
      p <- p + ggplot2::scale_color_gradientn(colors = current_settings$colors)
    } else {
      p <- p + generate_color_scale(data, color_var, current_settings$colors, "color")
    }
  }
  if (!is.null(fill_var)) {
    if (gradient_fill && is.numeric(data[[fill_var]])) {
      p <- p + ggplot2::scale_fill_gradientn(colors = current_settings$colors)
    } else {
      p <- p + generate_color_scale(data, fill_var, current_settings$colors, "fill")
    }
  }

  # Geom layers with ggfx effects
  geom_layer_base <- NULL
  if (plot_type == "scatter") {
    geom_layer_base <- ggplot2::geom_point(ggplot2::aes_string(color = color_var),
                                           size = geom_size, alpha = base_alpha, stroke = geom_stroke,
                                           color = if (is.null(color_var)) current_settings$colors[1] else NULL)
  } else if (plot_type == "line") {
    geom_layer_base <- list(
      ggplot2::geom_line(ggplot2::aes_string(color = color_var),
                         size = geom_size / 2, alpha = base_alpha,
                         color = if (is.null(color_var)) current_settings$colors[1] else NULL),
      ggplot2::geom_point(ggplot2::aes_string(color = color_var),
                          size = geom_size, alpha = base_alpha, stroke = geom_stroke,
                          color = if (is.null(color_var)) current_settings$colors[1] else NULL)
    )
    if (add_trend_line) {
      geom_layer_base <- c(geom_layer_base, ggplot2::geom_smooth(method = "lm", se = FALSE, color = current_settings$text_color, linetype = "dashed", size = 0.5))
    }
  } else if (plot_type == "column") {
    data[[x]] <- factor(data[[x]]) # Ensure x is discrete for columns
    geom_layer_base <- ggplot2::geom_col(ggplot2::aes_string(fill = fill_var),
                                         width = 0.7, alpha = base_alpha,
                                         fill = if (is.null(fill_var)) current_settings$colors[1] else NULL)
  } else if (plot_type == "area") {
    geom_layer_base <- ggplot2::geom_area(ggplot2::aes_string(fill = fill_var),
                                          alpha = base_alpha,
                                          fill = if (is.null(fill_var)) current_settings$colors[1] else NULL)
  } else if (plot_type == "map") {
    # CORRECTED: Handle fill_var mapping for geom_sf to avoid aesthetic conflict
    if (!is.null(fill_var)) {
      geom_layer_base <- ggplot2::geom_sf(ggplot2::aes_string(fill = fill_var),
                                          alpha = base_alpha,
                                          color = current_settings$grid_color, linewidth = 0.1)
    } else {
      geom_layer_base <- ggplot2::geom_sf(fill = current_settings$colors[1], # Default static fill if no fill_var
                                          alpha = base_alpha,
                                          color = current_settings$grid_color, linewidth = 0.1)
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
    if (!is.null(x) && x %in% names(data)) {
      filter_condition <- filter_condition | (data[[x]] %in% highlight_values)
    }

    # Add condition for color_var if color_var is not NULL and exists in data
    if (!is.null(color_var) && color_var %in% names(data)) {
      filter_condition <- filter_condition | (data[[color_var]] %in% highlight_values)
    }

    # Add condition for label_var if label_var is not NULL and exists in data (useful for maps/categorical data)
    if (!is.null(label_var) && label_var %in% names(data)) {
      filter_condition <- filter_condition | (data[[label_var]] %in% highlight_values)
    }

    # Only proceed if there's at least one true in the filter_condition
    if (any(filter_condition)) {
      highlight_data <- data %>%
        dplyr::filter(filter_condition)

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
  if (show_labels && !is.null(label_var)) {
    if (plot_type == "column" || plot_type == "area") {
      p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_var), vjust = -0.5, hjust = 0.5,
                                  size = 3.5, color = current_settings$text_color, family = font_body_name)
    } else if (plot_type == "scatter" || plot_type == "line") {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_var), size = 3.5, box.padding = 0.5,
                                        min.segment.length = 0.3, segment.color = current_settings$grid_color,
                                        segment.size = 0.3, max.overlaps = 50, direction = "y",
                                        family = font_body_name, color = current_settings$text_color)
    } else if (plot_type == "map") {
      p <- p + ggplot2::geom_sf_text(ggplot2::aes_string(label = label_var),
                                     size = 3.5, color = current_settings$text_color, family = font_body_name,
                                     check_overlap = TRUE)
    }
  }

  # Faceting
  if (!is.null(facet_var)) {
    p <- p + ggplot2::facet_wrap(ggplot2::sym(facet_var))
  }

  # Theme
  final_theme <- ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 32, face = "bold", hjust = 0.5, family = font_title_name, color = current_settings$text_color, margin = ggplot2::margin(b = 20)),
      plot.subtitle = ggtext::element_markdown(size = 20, hjust = 0.5, family = font_body_name, color = current_settings$text_color, margin = ggplot2::margin(b = 30)),
      plot.caption = ggtext::element_markdown(size = 14, hjust = 1, family = font_body_name, color = current_settings$text_color, margin = ggplot2::margin(t = 20)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 16, family = font_body_name, color = current_settings$text_color),
      legend.text = ggplot2::element_text(size = 14, family = font_body_name, color = current_settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      plot.background = ggplot2::element_rect(fill = current_settings$background_fill, colour = NA),
      axis.text = ggplot2::element_text(size = 12, color = current_settings$text_color, family = font_body_name),
      axis.title = ggtext::element_markdown(size = 16, color = current_settings$text_color, family = font_body_name),
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

  if (!is.null(x) && plot_type != "map") { # x is relevant for non-map plots
    lab_args$x = paste0("<span style='font-family:\"", font_body_name, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>")
  }

  if (!is.null(y) && plot_type != "map") { # y is relevant for non-map plots
    lab_args$y = paste0("<span style='font-family:\"", font_body_name, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>")
  }

  p + final_theme + do.call(ggplot2::labs, lab_args)
}
