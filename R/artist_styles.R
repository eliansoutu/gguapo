# artist_styles.R

# Función auxiliar para generar las escalas de color y relleno dinámicamente
#' @title Generate ggplot2 color and fill scales dynamically
#' @description This function intelligently applies manual or automatically generated
#'   color/fill scales based on the number of unique levels in the specified variable
#'   and the available custom colors. If the variable is continuous, it defaults to a viridis scale.
#' @param data A data frame containing the variable.
#' @param var_name A string specifying the column name for color or fill mapping.
#' @param current_colors A vector of preferred colors from the style settings.
#' @param type The type of scale to generate: "color" or "fill".
#' @return A ggplot2 scale object (e.g., `scale_color_manual`, `scale_fill_manual`) or NULL if no variable to map.
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_color_viridis_d scale_fill_viridis_d scale_color_viridis_c scale_fill_viridis_c
#' @export
generate_color_scale <- function(data, var_name, current_colors, type = c("color", "fill")) {
  type <- match.arg(type)

  if (is.null(var_name)) {
    return(NULL) # No variable to map, use default static color
  }

  # Check if the variable exists in the data
  if (!var_name %in% names(data)) {
    warning(paste0("Variable '", var_name, "' not found in data. Using default colors."))
    return(NULL)
  }

  var_data <- data[[var_name]]

  if (is.numeric(var_data)) {
    # Numeric variable: use gradient scale
    if (type == "color") {
      return(ggplot2::scale_color_gradientn(colors = current_colors))
    } else {
      return(ggplot2::scale_fill_gradientn(colors = current_colors))
    }
  } else {
    # Discrete variable (factor, character, logical)
    unique_levels <- unique(var_data)
    num_levels <- length(unique_levels)

    if (num_levels <= length(current_colors)) {
      # Enough predefined colors
      if (type == "color") {
        return(ggplot2::scale_color_manual(values = current_colors[1:num_levels]))
      } else {
        return(ggplot2::scale_fill_manual(values = current_colors[1:num_levels]))
      }
    } else {
      # Not enough predefined colors, generate more using colorRampPalette
      extended_palette_fun <- grDevices::colorRampPalette(current_colors)
      extended_colors <- extended_palette_fun(num_levels)
      if (type == "color") {
        return(ggplot2::scale_color_manual(values = extended_colors))
      } else {
        return(ggplot2::scale_fill_manual(values = extended_colors))
      }
    }
  }
}

#' @title Load all Google Fonts used in artist styles
#' @description This function loads all necessary Google Fonts for the artist-inspired ggplot2 styles,
#'   preventing redundant loading when multiple style functions are called.
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
load_all_artist_fonts <- function() {
  # Call showtext_auto once to enable font rendering
  showtext::showtext_auto()

  # Load all unique fonts used across the artist styles
  sysfonts::font_add_google("Permanent Marker", "Permanent Marker")
  sysfonts::font_add_google("Cabin Sketch", "Cabin Sketch")
  sysfonts::font_add_google("Cutive Mono", "Cutive Mono")
  sysfonts::font_add_google("Playfair Display", "Playfair Display")
  sysfonts::font_add_google("Special Elite", "Special Elite")
  sysfonts::font_add_google("Merriweather", "Merriweather")
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  sysfonts::font_add_google("Lato", "Lato")
  sysfonts::font_add_google("Amatic SC", "Amatic SC")
  sysfonts::font_add_google("Indie Flower", "Indie Flower")
  sysfonts::font_add_google("Architects Daughter", "Architects Daughter")
  sysfonts::font_add_google("Pacifico", "Pacifico")
  sysfonts::font_add_google("Libre Barcode 39 Text", "Libre Barcode 39 Text")
  sysfonts::font_add_google("Oswald", "Oswald")
  sysfonts::font_add_google("Bebas Neue", "Bebas Neue")
  sysfonts::font_add_google("Bangers", "bangers")
  sysfonts::font_add_google("Poppins", "Poppins")
  sysfonts::font_add_google("Orbitron", "orbitron")

  message("Todas las fuentes de los estilos de artistas han sido cargadas.")
}

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
#' @param show_background Logical, whether to show the panel background. Defaults to FALSE.
#' @param add_glow Logical, whether to add a subtle shadow effect (sfumato) for maps. Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox geom_sf geom_sf_text # Added for maps
#' @importFrom ggfx with_shadow # Added for glow effect
#' @export
style_da_vinci <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Maestría de Da Vinci",
                           subtitle = "Un estudio de proporciones y armonía",
                           caption = "Análisis Renacentista",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("mona_lisa", "last_supper", "vitruvian_man"),
                           show_labels = FALSE, add_grid_lines = FALSE, show_background = FALSE,
                           add_glow = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  # Paletas y ajustes según la obra
  settings <- list(
    "mona_lisa" = list(
      colors = c("#8B4513", "#CD853F", "#A0522D", "#694F4F", "#D2B48C"), # Tonos tierra, marrones y ocres
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#F5DEB3", panel_fill = "#FAEBD7",
      grid_color = "#D2B48C", text_color = "#4F4F4F", geom_alpha = 0.8
    ),
    "last_supper" = list(
      colors = c("#5C4033", "#A9A9A9", "#708090", "#D3D3D3", "#BDB76B"), # Tonos oscuros, grises piedra, toques oliva
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#36454F", panel_fill = "#2F4F4F",
      grid_color = "#5C4033", text_color = "#D3D3D3", geom_alpha = 0.7
    ),
    "vitruvian_man" = list(
      colors = c("#4B0082", "#7B68EE", "#8A2BE2", "#C0C0C0", "#F0E68C"), # Morados, grises, toques de amarillo/marrón
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#F5F5DC", panel_fill = "#E0E0E0",
      grid_color = "#A9A9A9", text_color = "#4B0082", geom_alpha = 0.9
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA), # Transparente
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA),
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula condicionales
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (add_grid_lines && plot_type != "map") ggplot2::element_line(color = settings$grid_color, linetype = "dotted", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank() # Línea de eje para no-mapas
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", or "column".
#' @param work_inspired_by Specific work for aesthetic inspiration: "david", "sistine_chapel", or "pieta".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines. Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_minimal theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @export
style_michelangelo <- function(data, x, y, color_var = NULL, fill_var = NULL, label_var = NULL,
                               title = "Magnificencia de Datos al Estilo de Miguel Ángel",
                               subtitle = "Una composición de fuerza y detalle",
                               caption = "Obra Maestra del Análisis",
                               plot_type = c("scatter", "line", "column","map"),
                               work_inspired_by = c("david", "sistine_chapel", "pieta"),
                               show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE, show_background = FALSE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)


  settings <- list(
    "david" = list(
      colors = c("#B0C4DE", "#DCDCDC", "#E6E6FA", "#708090", "#8470FF"), # Tonos fríos de mármol y azules pálidos
      font_title = "Merriweather", font_body = "Open Sans",
      background_fill = "#F0F8FF", panel_fill = "#E0FFFF",
      grid_color = "#A9A9A9", text_color = "#4682B4", geom_alpha = 0.9,
      glow_color = "#ADD8E6"
    ),
    "sistine_chapel" = list(
      colors = c("#8B0000", "#FFD700", "#483D8B", "#DAA520", "#B22222"), # Rojos, dorados, azules profundos
      font_title = "Playfair Display", font_body = "Roboto Condensed",
      background_fill = "#2F4F4F", panel_fill = "#4F4F4F",
      grid_color = "#696969", text_color = "#FFD700", geom_alpha = 0.8,
      glow_color = "#DAA520"
    ),
    "pieta" = list(
      colors = c("#D3D3D3", "#A9A9A9", "#C0C0C0", "#778899", "#5F9EA0"), # Grises, platas, toques de verde azulado
      font_title = "Merriweather", font_body = "Lato",
      background_fill = "#E6E6FA", panel_fill = "#D8BFD8",
      grid_color = "#B0C4DE", text_color = "#6A5ACD", geom_alpha = 0.9,
      glow_color = "#C0C0C0"
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_minimal() + # Michelangelo usa theme_minimal
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      # Unificar fondo del panel y del plot para mapas, condicional para otros
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula basados en plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (add_grid_lines && plot_type != "map") ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.5) else ggplot2::element_blank() # Línea de eje para no-mapas
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_dark theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf geom_sf geom_sf_text # Add sf imports for maps
#' @export
style_rembrandt <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Iluminando Datos al Estilo de Rembrandt",
                            subtitle = "Profundidad y contraste en la visualización",
                            caption = "Maestría en la Sombra",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("night_watch", "self_portrait", "storm_sea"),
                            show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE, show_background = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "night_watch" = list(
      colors = c("#4B3621", "#8B4513", "#CD853F", "#DAA520", "#FFFACD"), # Marrones oscuros, dorados, crema
      font_title = "Merriweather", font_body = "Roboto Condensed",
      background_fill = "#2F2F2F", panel_fill = "#3A3A3A", # panel_fill se usará solo para no-mapas
      grid_color = "#5A5A5A", text_color = "#FFFACD", geom_alpha = 0.8,
      glow_color = "#DAA520"
    ),
    "self_portrait" = list(
      colors = c("#362B2B", "#6B4226", "#A0522D", "#D2B48C", "#E0B388"), # Tonos sepia, marrones, piel
      font_title = "Playfair Display", font_body = "Lato",
      background_fill = "#252525", panel_fill = "#303030",
      grid_color = "#454545", text_color = "#E0B388", geom_alpha = 0.9,
      glow_color = "#D2B48C"
    ),
    "storm_sea" = list(
      colors = c("#102027", "#2F4F4F", "#708090", "#C0C0C0", "#F0FFFF"), # Azules-grises oscuros, blanco tormenta
      font_title = "Special Elite", font_body = "Cutive Mono",
      background_fill = "#0A1215", panel_fill = "#1A2225",
      grid_color = "#3A454A", text_color = "#F0FFFF", geom_alpha = 0.7,
      glow_color = "#C0C0C0"
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_dark() + # Rembrandt usa theme_dark
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      # Unificar fondo del panel y del plot para mapas, condicional para otros
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula basados en plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (add_grid_lines && plot_type != "map") ggplot2::element_line(color = settings$grid_color, linetype = "dotted", linewidth = 0.4) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.5) else ggplot2::element_blank() # Línea de eje para no-mapas
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to FALSE.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_density_2d theme_dark theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_van_gogh <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Pasión de Van Gogh",
                           subtitle = "Un remolino de información",
                           caption = "Impresiones de Datos",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("starry_night", "sunflowers", "irises"),
                           show_labels = FALSE, add_glow = FALSE, add_texture = FALSE, add_grid_lines = FALSE, show_background = FALSE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "starry_night" = list(
      colors = c("#2A4480", "#4A70B0", "#80B0D8", "#F0D050", "#E0A030", "#C07020"), # Azules profundos, amarillos y naranjas intensos
      font_title = "Playfair Display", font_body = "Roboto Condensed",
      background_fill = "#1A2A50", panel_fill = "#1A2A50", # panel_fill se usará solo para no-mapas
      grid_color = "#3A5A90", text_color = "#F0D050", geom_alpha = 0.7,
      glow_color = "#F0D050"
    ),
    "sunflowers" = list(
      colors = c("#FFD700", "#FFA500", "#FFC107", "#FF8C00", "#B8860B"), # Amarillos, naranjas, tonos mostaza
      font_title = "Permanent Marker", font_body = "Cabin Sketch",
      background_fill = "#FFFACD", panel_fill = "#FFFDD0",
      grid_color = "#E0B900", text_color = "#B8860B", geom_alpha = 0.9,
      glow_color = "#FFD700"
    ),
    "irises" = list(
      colors = c("#4B0082", "#8A2BE2", "#9370DB", "#6A5ACD", "#D8BFD8"), # Morados, lilas, lavandas
      font_title = "Lato", font_body = "Open Sans",
      background_fill = "#E6E6FA", panel_fill = "#D8BFD8",
      grid_color = "#B0C4DE", text_color = "#4B0082", geom_alpha = 0.8,
      glow_color = "#8A2BE2"
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_dark() + # Van Gogh usa theme_dark
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      # Unificar fondo del panel y del plot para mapas, condicional para otros
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula basados en plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (add_grid_lines && plot_type != "map") ggplot2::element_line(color = settings$grid_color, linetype = "dotted", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank() # Línea de eje para no-mapas
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_density_2d theme_minimal theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_monet <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                        title = "La Impresión de los Datos al Estilo de Monet",
                        subtitle = "Armonía de luz y color",
                        caption = "Análisis Impresionista",
                        plot_type = c("scatter", "line", "column", "map"),
                        work_inspired_by = c("water_lilies", "impression_sunrise", "poppy_fields"),
                        show_labels = FALSE, add_glow = FALSE, add_texture = FALSE, add_grid_lines = FALSE, show_background = TRUE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "water_lilies" = list(
      colors = c("#87CEEB", "#ADD8E6", "#B0E0E6", "#6A5ACD", "#483D8B"), # Tonos azules, púrpuras suaves
      font_title = "Playfair Display", font_body = "Lato",
      background_fill = "#E0F2F7", panel_fill = "#F0FFFF", # panel_fill se usará solo para no-mapas
      grid_color = "#C0D9E0", text_color = "#4682B4", geom_alpha = 0.7,
      glow_color = "#ADD8E6"
    ),
    "impression_sunrise" = list(
      colors = c("#FF8C00", "#FF4500", "#FFA07A", "#FFD700", "#FFEA00"), # Naranjas, rojos, dorados amanecer
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#FFF0D9", panel_fill = "#FFF5E0",
      grid_color = "#FFD700", text_color = "#D2691E", geom_alpha = 0.8,
      glow_color = "#FFD700"
    ),
    "poppy_fields" = list(
      colors = c("#B22222", "#DC143C", "#FF6347", "#556B2F", "#8FBC8F"), # Rojos amapola, verdes campestres
      font_title = "Open Sans", font_body = "Roboto Condensed",
      background_fill = "#F5FFFA", panel_fill = "#F0FFF0",
      grid_color = "#98FB98", text_color = "#8B0000", geom_alpha = 0.8,
      glow_color = "#DC143C"
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_minimal() + # Monet usa theme_minimal
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      # Unificar fondo del panel y del plot para mapas, condicional para otros
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula basados en plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.3) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank() # Línea de eje para no-mapas
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_void theme element_text element_rect unit labs
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_banksy <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                         title = "Datos con la Calle de Banksy",
                         subtitle = "Un mensaje subversivo en cada punto",
                         caption = "Arte Urbano de Datos",
                         plot_type = c("scatter", "line", "column", "map"),
                         work_inspired_by = c("girl_with_balloon", "flower_thrower", "rat_graffiti"),
                         show_labels = FALSE, add_grid_lines = FALSE, show_background = TRUE) {

  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "girl_with_balloon" = list(
      colors = c("#000000", "#FFFFFF", "#B22222", "#D3D3D3", "#696969"), # Negro, blanco, rojo
      font_title = "Permanent Marker", font_body = "Cabin Sketch",
      background_fill = "#D3D3D3", panel_fill = "#A9A9A9", # panel_fill se usará solo para no-mapas
      grid_color = "#696969", text_color = "#000000", geom_alpha = 0.9
    ),
    "flower_thrower" = list(
      colors = c("#000000", "#FFFFFF", "#32CD32", "#4169E1", "#FFD700"), # Negro, blanco, toques primarios
      font_title = "Cabin Sketch", font_body = "Cutive Mono",
      background_fill = "#B0C4DE", panel_fill = "#778899",
      grid_color = "#D3D3D3", text_color = "#000000", geom_alpha = 0.8
    ),
    "rat_graffiti" = list(
      colors = c("#000000", "#FFFFFF", "#FF4500", "#FFD700", "#8B0000"), # Negro, blanco, naranjas/rojos
      font_title = "Cutive Mono", font_body = "Permanent Marker",
      background_fill = "#808080", panel_fill = "#696969",
      grid_color = "#D3D3D3", text_color = "#FFFFFF", geom_alpha = 0.9
    )
  )[[work_inspired_by]]

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
  p + ggplot2::theme_void() + # Banksy usa theme_void
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      # Unificar fondo del panel y del plot para mapas, condicional para otros
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Ajustes de ejes y cuadrícula basados en plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.5) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_jitter theme_dark theme element_text element_rect unit labs
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_salvador_dali <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                title = "Datos Derretidos al Estilo de Dalí",
                                subtitle = "La realidad se dobla para mostrar la verdad",
                                caption = "Análisis Surrealista",
                                plot_type = c("scatter", "line", "column", "map"),
                                work_inspired_by = c("persistence_memory", "elephants", "swans_reflecting_elephants"),
                                show_labels = FALSE, add_glow = FALSE, add_jitter = FALSE, add_grid_lines = FALSE, show_background = TRUE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  # Carga de fuentes
  sysfonts::font_add_google("Playfair Display", "Playfair Display")
  sysfonts::font_add_google("Special Elite", "Special Elite")
  sysfonts::font_add_google("Merriweather", "Merriweather")
  sysfonts::font_add_google("Roboto Condensed", "Roboto Condensed")
  sysfonts::font_add_google("Lato", "Lato")
  showtext::showtext_auto()

  settings <- list(
    "persistence_memory" = list(
      colors = c("#8B4513", "#CD853F", "#A0522D", "#ADD8E6", "#FFDAB9"), # Marrones, ocres, azules suaves, cremas
      font_title = "Playfair Display", font_body = "Special Elite",
      background_fill = "#ADD8E6", panel_fill = "#B0C4DE",
      grid_color = "#CD853F", text_color = "#8B4513", geom_alpha = 0.6,
      glow_color = "#FFDAB9"
    ),
    "elephants" = list(
      colors = c("#483D8B", "#8B0000", "#CD853F", "#D3D3D3", "#B0C4DE"), # Morados oscuros, rojos, marrones, grises
      font_title = "Merriweather", font_body = "Roboto Condensed",
      background_fill = "#2F4F4F", panel_fill = "#4F4F4F",
      grid_color = "#778899", text_color = "#D3D3D3", geom_alpha = 0.7,
      glow_color = "#CD853F"
    ),
    "swans_reflecting_elephants" = list(
      colors = c("#228B22", "#4682B4", "#DAA520", "#CD853F", "#8B4513"), # Verdes, azules, dorados, marrones
      font_title = "Playfair Display", font_body = "Lato",
      background_fill = "#A0C4FF", panel_fill = "#B0E0E6",
      grid_color = "#DAA520", text_color = "#228B22", geom_alpha = 0.6,
      glow_color = "#DAA520"
    )
  )[[work_inspired_by]]

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
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  p + ggplot2::theme_dark() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "dashed", linewidth = 0.4) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.5) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_segment theme_void theme element_text element_rect unit labs scale_shape_manual
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_miro <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                       title = "Datos con la Fantasía de Miró",
                       subtitle = "Un universo de formas y color",
                       caption = "Análisis Surrealista Abstracto",
                       plot_type = c("scatter", "line", "column", "map"),
                       work_inspired_by = c("the_farm", "constellations", "blue_series"),
                       show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE, show_background = TRUE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "the_farm" = list(
      colors = c("#B22222", "#32CD32", "#4169E1", "#FFD700", "#FF4500"), # Rojos, verdes, azules, amarillos, naranjas (vibrantes y primarios)
      font_title = "Amatic SC", font_body = "Indie Flower",
      background_fill = "#F5F5DC", panel_fill = "#FFFACD",
      grid_color = "#000000", text_color = "#000000", geom_alpha = 0.9,
      geom_size = 5, geom_stroke = 1.5,
      shapes = c(21, 22, 23, 24, 25) # Círculo, cuadrado, diamante, triángulo, triángulo invertido
    ),
    "constellations" = list(
      colors = c("#000000", "#FFFFFF", "#4169E1", "#B22222", "#FFD700"), # Negro, blanco, azul, rojo, amarillo (cielo estrellado)
      font_title = "Architects Daughter", font_body = "Indie Flower",
      background_fill = "#2F4F4F", panel_fill = "#1A2225",
      grid_color = "#FFFFFF", text_color = "#FFFFFF", geom_alpha = 0.8,
      geom_size = 6, geom_stroke = 1.8,
      shapes = c(8, 4, 15, 16, 17) # Estrellas, cruces, cuadrados, círculos, triángulos
    ),
    "blue_series" = list(
      colors = c("#000080", "#4169E1", "#ADD8E6", "#87CEFA", "#6A5ACD"), # Tonos de azul profundo
      font_title = "Pacifico", font_body = "Lato",
      background_fill = "#F0FFFF", panel_fill = "#E0FFFF",
      grid_color = "#000080", text_color = "#000080", geom_alpha = 0.7,
      geom_size = 4, geom_stroke = 1.2,
      shapes = c(21, 24, 25, 16, 17)
    )
  )[[work_inspired_by]]

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
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  p + ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$background_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_dark theme element_text element_rect unit labs
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom ggfx with_outer_glow
#' @importFrom tools toTitleCase
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_artemisia_gentileschi <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                        title = "Datos con la Fuerza de Artemisia Gentileschi",
                                        subtitle = "Un contraste dramático en la visualización",
                                        caption = "Barroco de Datos",
                                        plot_type = c("scatter", "line", "column", "map"),
                                        work_inspired_by = c("judith_beheading_holofernes", "mary_magdalene", "self_portrait_lute_player"),
                                        show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE, show_background = TRUE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "judith_beheading_holofernes" = list(
      colors = c("#8B0000", "#5C4033", "#0A0A0A", "#CD853F", "#C0C0C0"), # Rojos sangre, marrones oscuros, negros, dorados
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#1A1A1A", panel_fill = "#2A2A2A",
      grid_color = "#3A3A3A", text_color = "#CD853F", geom_alpha = 0.8,
      glow_color = "#FFD700", geom_lwd = 1.0 # Line width for map outlines
    ),
    "mary_magdalene" = list(
      colors = c("#4B0082", "#800080", "#C0C0C0", "#CD853F", "#A0522D"), # Morados profundos, grises, marrones
      font_title = "Merriweather", font_body = "Lato",
      background_fill = "#2F2F2F", panel_fill = "#3F3F3F",
      grid_color = "#696969", text_color = "#C0C0C0", geom_alpha = 0.7,
      glow_color = "#DAA520", geom_lwd = 0.8
    ),
    "self_portrait_lute_player" = list(
      colors = c("#362B2B", "#6B4226", "#A0522D", "#D2B48C", "#FFFACD"), # Tonos terrosos, piel, crema
      font_title = "Playfair Display", font_body = "Roboto Condensed",
      background_fill = "#252525", panel_fill = "#303030",
      grid_color = "#454545", text_color = "#D2B48C", geom_alpha = 0.9,
      glow_color = "#FFFACD", geom_lwd = 1.2
    )
  )[[work_inspired_by]]

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
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  p + ggplot2::theme_dark() + # Theme dark for chiaroscuro effect
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (plot_type == "map") ggplot2::element_rect(fill = settings$panel_fill, colour = NA) else {
        if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.5) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col geom_segment theme_minimal theme element_text element_rect unit labs scale_shape_manual
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add_google
#' @importFrom sf geom_sf geom_sf_text # Add sf imports for maps
#' @export
style_kandinsky <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Datos con la Abstracción de Kandinsky",
                            subtitle = "Una sinfonía visual de formas y colores",
                            caption = "Composición de Datos",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("composition_viii", "yellow_red_blue", "on_white_ii"),
                            show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE, show_background = TRUE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "composition_viii" = list(
      colors = c("#FF0000", "#0000FF", "#FFFF00", "#000000", "#808080", "#FF4500", "#32CD32"), # Primarios, negro, gris, naranjas, verdes
      font_title = "Oswald", font_body = "Bebas Neue",
      background_fill = "#F5F5F5", panel_fill = "#FFFFFF",
      grid_color = "#D3D3D3", text_color = "#000000", geom_alpha = 0.9,
      geom_size = 5, geom_stroke = 1.5, geom_lwd = 1.5, # geom_lwd for map outlines
      shapes = c(15, 16, 17, 18, 19, 21, 22) # Cuadrados, círculos, triángulos, diamantes, formas variadas
    ),
    "yellow_red_blue" = list(
      colors = c("#FFFF00", "#FF0000", "#0000FF", "#000000", "#FFFFFF"), # Amarillo, rojo, azul, negro, blanco (colores fundamentales)
      font_title = "Bebas Neue", font_body = "Oswald",
      background_fill = "#D3D3D3", panel_fill = "#F0F0F0",
      grid_color = "#A9A9A9", text_color = "#000000", geom_alpha = 0.8,
      geom_size = 6, geom_stroke = 2, geom_lwd = 2.0,
      shapes = c(15, 16, 17, 18)
    ),
    "on_white_ii" = list(
      colors = c("#FFFFFF", "#000000", "#808080", "#FFD700", "#4169E1"), # Blanco, negro, gris, toques de color
      font_title = "Oswald", font_body = "Lato",
      background_fill = "#FFFFFF", panel_fill = "#F8F8F8",
      grid_color = "#E0E0E0", text_color = "#000000", geom_alpha = 0.7,
      geom_size = 4, geom_stroke = 1.2, geom_lwd = 1.0,
      shapes = c(15, 16, 17)
    )
  )[[work_inspired_by]]

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
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  p + ggplot2::theme_minimal() + # Kandinsky often had lighter backgrounds than Gentileschi
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA),
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
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
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line geom_col theme_minimal theme element_text element_rect unit labs
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom showtext showtext_auto
#' @importFrom sysfonts font_add_google
#' @importFrom sf geom_sf geom_sf_text
#' @export
style_andy_warhol <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                              title = "Datos como Pop Art de Andy Warhol",
                              subtitle = "La repetición se convierte en arte",
                              caption = "Consumo de Datos",
                              plot_type = c("scatter", "line", "column", "map"),
                              work_inspired_by = c("soup_cans", "marilyn_monroe", "cow_wallpaper"),
                              show_labels = FALSE, add_grid_lines = FALSE, show_background = TRUE) {
  plot_type <- match.arg(plot_type)
  work_inspired_by <- match.arg(work_inspired_by)

  settings <- list(
    "soup_cans" = list(
      colors = c("#FF0000", "#FFFFFF", "#000000", "#FFD700", "#808080"), # Rojo Campbell, blanco, negro, amarillo, gris
      font_title = "Libre Barcode 39 Text", font_body = "Oswald",
      background_fill = "#D3D3D3", panel_fill = "#C0C0C0",
      grid_color = "#A9A9A9", text_color = "#000000", geom_alpha = 1, # Opacidad total para colores planos
      geom_lwd = 1.0 # Line width for map outlines
    ),
    "marilyn_monroe" = list(
      colors = c("#FF00FF", "#00FFFF", "#FFD700", "#B22222", "#4169E1"), # Colores pop, fucsia, cian, amarillo, rojo, azul
      font_title = "Permanent Marker", font_body = "Cabin Sketch",
      background_fill = "#F5F5F5", panel_fill = "#F8F8F8",
      grid_color = "#E0E0E0", text_color = "#000000", geom_alpha = 0.9,
      geom_lwd = 0.8
    ),
    "cow_wallpaper" = list(
      colors = c("#006400", "#9ACD32", "#FFFFFF", "#000000", "#8B4513"), # Verdes vibrantes, blanco, negro, marrón (vacas)
      font_title = "Bebas Neue", font_body = "Cutive Mono",
      background_fill = "#E6E6FA", panel_fill = "#D8BFD8",
      grid_color = "#A9A9A9", text_color = "#000000", geom_alpha = 0.9,
      geom_lwd = 0.6
    )
  )[[work_inspired_by]]

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
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = label_col), vjust = -0.5, hjust = 0.5,
                                    size = 3.5, color = settings$text_color, family = settings$font_body)
      } else {
        p <- p + ggrepel::geom_text_repel(ggplot2::aes_string(label = label_col), size = 3.5, box.padding = 0.5, min.segment.length = 0.3,
                                          segment.color = settings$grid_color, segment.size = 0.3, max.overlaps = 50, direction = "y",
                                          family = settings$font_body, color = settings$text_color)
      }
    }
  } # End of non-map plot_type block

  # Common theming for all plot types
  p + ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      panel.background = if (!show_background) ggplot2::element_blank() else ggplot2::element_rect(fill = settings$panel_fill, colour = NA),
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (plot_type != "map" && add_grid_lines) ggplot2::element_line(color = settings$grid_color, linetype = "solid", linewidth = 0.5) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = 0.8) else ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
      subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
      caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>"),
      x = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>") else NULL,
      y = if (plot_type != "map") paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>") else NULL
    )
}


# Helper function for `|||` operator (from rlang, but not exporting rlang just for this)
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

