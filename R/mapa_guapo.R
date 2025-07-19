#' @title Mapa Guapo
#' @description Genera mapas estéticamente agradables utilizando `ggplot2` y `sf`, con opciones para diferentes tipos de geometría y personalización visual.
#' @param data Un objeto `sf` (simple features) que contiene los datos geográficos a visualizar.
#' @param tipo Un string que especifica el tipo de geometría a graficar. Puede ser "poligono", "linea" o "punto".
#' @param fill (Tidy-evaluated) Nombre de la columna para mapear el color de relleno de los polígonos. Solo aplica para `tipo = "poligono"`.
#' @param color (Tidy-evaluated) Nombre de la columna para mapear el color de las líneas o puntos. Solo aplica para `tipo = "linea"` o `tipo = "punto"`.
#' @param size Un valor numérico que especifica el tamaño de las líneas o puntos. Por defecto es 1.
#' @param titulo Un string opcional que especifica el título principal del mapa.
#' @param subtitulo Un string opcional que especifica el subtítulo del mapa.
#' @param etiqueta (Tidy-evaluated) Nombre de la columna a usar para etiquetar las geometrías en el mapa.
#' @param palette Un string que especifica la paleta de colores a usar para el relleno de polígonos (ej. "Blues", "Reds"). Compatible con `scale_fill_distiller`.
#' @param font Un string que especifica la familia de fuente a usar para el texto del mapa (título, etiquetas). Por defecto es "Poppins". Asegúrate de que la fuente esté disponible.
#' @param show_shadow_polygons Lógico. Si aplicar una sombra a los polígonos. Por defecto es TRUE.
#' @return Un objeto `ggplot2` que representa el mapa generado.
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_distiller scale_color_viridis_c labs theme_void theme element_text element_rect element_blank scale_fill_manual scale_color_manual after_stat
#' @importFrom ggfx with_shadow
#' @importFrom ggtext geom_richtext element_markdown
#' @importFrom rlang .data enquo as_string quo_is_null quo_is_missing quo_get_expr
#' @importFrom sf st_geometry st_drop_geometry
#' @importFrom grDevices colorRampPalette
#' @importFrom tools toTitleCase
#' @export
mapa_guapo <- function(data, tipo = "poligono", fill = NULL, color = NULL, size = 1,
                       titulo = NULL, subtitulo = NULL, etiqueta = NULL, palette = "Blues", font = "Poppins",
                       show_shadow_polygons = TRUE) {

  # Validar que 'data' sea un objeto sf
  if (!inherits(data, "sf")) {
    stop("El objeto 'data' debe ser de la clase 'sf'.")
  }

  tipo <- tolower(tipo)

  # Ajustes de estilo predefinidos
  map_settings <- list(
    background_fill = "#FDFDFD",
    panel_fill = "#F5F5F5",
    geom_border_color = "white",
    geom_default_fill = "#87CEEB", # Sky Blue
    geom_default_line_color = "#1E90FF", # Dodger Blue
    geom_default_point_color = "#FF4500", # Orange Red
    text_color_title = "#333333",
    text_color_subtitle = "#666666",
    text_color_legend = "#555555",
    shadow_color = "grey30"
  )


  if (font == "Poppins") {

    sysfonts::font_add_google("Poppins", "Poppins")
    showtext::showtext_auto()
  }

  # Capturar las expresiones de las variables como quosures para tidy evaluation
  fill_quo <- rlang::enquo(fill)
  color_quo <- rlang::enquo(color)
  etiqueta_quo <- rlang::enquo(etiqueta)

  # Función auxiliar para verificar si un quosure es nulo o faltante
  is_quo_missing_or_null <- function(quo) {
    rlang::quo_is_null(quo) || rlang::quo_is_missing(quo)
  }

  # Obtener el nombre de la variable como string solo si no es missing/null
  # Usar quo_get_expr() antes de as_string()
  fill_var_name <- if (!is_quo_missing_or_null(fill_quo)) rlang::as_string(rlang::quo_get_expr(fill_quo)) else NULL
  color_var_name <- if (!is_quo_missing_or_null(color_quo)) rlang::as_string(rlang::quo_get_expr(color_quo)) else NULL
  etiqueta_var_name <- if (!is_quo_missing_or_null(etiqueta_quo)) rlang::as_string(rlang::quo_get_expr(etiqueta_quo)) else NULL

  # Verificar si las columnas existen en los datos (excluyendo la columna de geometría)
  data_names <- names(sf::st_drop_geometry(data))
  if (!is.null(fill_var_name) && !(fill_var_name %in% data_names)) {
    stop(paste0("La columna '", fill_var_name, "' especificada para 'fill' no se encuentra en los datos."))
  }
  if (!is.null(color_var_name) && !(color_var_name %in% data_names)) {
    stop(paste0("La columna '", color_var_name, "' especificada para 'color' no se encuentra en los datos."))
  }
  if (!is.null(etiqueta_var_name) && !(etiqueta_var_name %in% data_names)) {
    stop(paste0("La columna '", etiqueta_var_name, "' especificada para 'etiqueta' no se encuentra en los datos."))
  }

  # Inicializar el objeto ggplot
  base <- ggplot2::ggplot(data = data)

  # --- Construcción dinámica de la capa geom_sf ---
  geom_args <- list(alpha = 0.8) # Alpha común para todas las geometrías

  if (tipo == "poligono") {
    geom_args$mapping <- if (!is_quo_missing_or_null(fill_quo)) ggplot2::aes(fill = !!fill_quo) else NULL
    geom_args$color <- map_settings$geom_border_color # Borde del polígono
    geom_args$linewidth <- 0.3 # Grosor del borde

    if (is_quo_missing_or_null(fill_quo)) { # Si no hay mapeo de 'fill', usar color por defecto
      geom_args$fill <- map_settings$geom_default_fill
    }

    geom_layer <- do.call(ggplot2::geom_sf, geom_args)

    if (show_shadow_polygons) {
      base <- base + ggfx::with_shadow(
        geom_layer,
        sigma = 3, x_offset = 1, y_offset = 1,
        colour = map_settings$shadow_color, alpha = 0.2
      )
    } else {
      base <- base + geom_layer
    }

    # Escala de relleno para polígonos
    if (!is_quo_missing_or_null(fill_quo)) {
      if (is.numeric(data[[fill_var_name]])) {
        base <- base + ggplot2::scale_fill_distiller(palette = palette, direction = 1)
      } else {
        # Para variables categóricas, usar scale_fill_manual
        unique_levels <- unique(data[[fill_var_name]])
        num_levels <- length(unique_levels)
        # Definir una paleta base para categóricas, extender si es necesario
        categorical_colors <- c("#1E90FF", "#FF4500", "#32CD32", "#8A2BE2", "#FFD700", "#00FFFF", "#FF69B4")
        if (num_levels > length(categorical_colors)) {
          extended_palette_fun <- grDevices::colorRampPalette(categorical_colors)
          categorical_colors <- extended_palette_fun(num_levels)
        }
        base <- base + ggplot2::scale_fill_manual(values = categorical_colors[1:num_levels])
      }
    }

  } else if (tipo == "linea") {
    geom_args$mapping <- if (!is_quo_missing_or_null(color_quo)) ggplot2::aes(color = !!color_quo) else NULL
    geom_args$linewidth <- size # Grosor de la línea

    if (is_quo_missing_or_null(color_quo)) { # Si no hay mapeo de 'color', usar color por defecto
      geom_args$color <- map_settings$geom_default_line_color
    }
    base <- base + do.call(ggplot2::geom_sf, geom_args)

    # Escala de color para líneas
    if (!is_quo_missing_or_null(color_quo)) {
      if (is.numeric(data[[color_var_name]])) {
        base <- base + ggplot2::scale_color_viridis_c()
      } else {
        unique_levels <- unique(data[[color_var_name]])
        num_levels <- length(unique_levels)
        categorical_colors <- c("#1E90FF", "#FF4500", "#32CD32", "#8A2BE2", "#FFD700", "#00FFFF", "#FF69B4")
        if (num_levels > length(categorical_colors)) {
          extended_palette_fun <- grDevices::colorRampPalette(categorical_colors)
          categorical_colors <- extended_palette_fun(num_levels)
        }
        base <- base + ggplot2::scale_color_manual(values = categorical_colors[1:num_levels])
      }
    }

  } else if (tipo == "punto") {
    geom_args$mapping <- if (!is_quo_missing_or_null(color_quo)) ggplot2::aes(color = !!color_quo) else NULL
    geom_args$size <- size # Tamaño del punto
    geom_args$stroke <- 0.5 # Borde del punto

    if (is_quo_missing_or_null(color_quo)) { # Si no hay mapeo de 'color', usar color por defecto
      geom_args$color <- map_settings$geom_default_point_color
    }
    base <- base + do.call(ggplot2::geom_sf, geom_args)

    # Escala de color para puntos
    if (!is_quo_missing_or_null(color_quo)) {
      if (is.numeric(data[[color_var_name]])) {
        base <- base + ggplot2::scale_color_viridis_c()
      } else {
        unique_levels <- unique(data[[color_var_name]])
        num_levels <- length(unique_levels)
        categorical_colors <- c("#1E90FF", "#FF4500", "#32CD32", "#8A2BE2", "#FFD700", "#00FFFF", "#FF69B4")
        if (num_levels > length(categorical_colors)) {
          extended_palette_fun <- grDevices::colorRampPalette(categorical_colors)
          categorical_colors <- extended_palette_fun(num_levels)
        }
        base <- base + ggplot2::scale_color_manual(values = categorical_colors[1:num_levels])
      }
    }

  } else {
    stop("El argumento 'tipo' debe ser 'poligono', 'linea' o 'punto'.")
  }

  # --- Etiquetas ---
  if (!is_quo_missing_or_null(etiqueta_quo)) {
    # geom_richtext requiere 'geometry' en aes si stat="sf_coordinates"
    base <- base +
      ggtext::geom_richtext(
        ggplot2::aes(label = !!etiqueta_quo, geometry = ggplot2::after_stat(geometry)),
        stat = "sf_coordinates",
        family = font, size = 3, fill = NA, label.color = NA,
        color = map_settings$text_color_title # Color para las etiquetas
      )
  }

  # --- Tema ---
  base +
    ggplot2::labs(title = titulo, subtitle = subtitulo) +
    ggplot2::theme_void(base_family = font) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(
        size = 22, hjust = 0.5, face = "bold",
        color = map_settings$text_color_title,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggtext::element_markdown(
        size = 14, hjust = 0.5,
        color = map_settings$text_color_subtitle,
        margin = ggplot2::margin(b = 15)
      ),
      legend.position = "right",
      legend.title = ggplot2::element_text(
        size = 12, face = "bold", color = map_settings$text_color_legend
      ),
      legend.text = ggplot2::element_text(
        size = 10, color = map_settings$text_color_legend
      ),
      plot.background = ggplot2::element_rect(
        fill = map_settings$background_fill, color = NA
      ),
      panel.background = ggplot2::element_rect( # Fondo del panel del mapa
        fill = map_settings$panel_fill, color = NA
      ),
      legend.background = ggplot2::element_rect(
        fill = map_settings$panel_fill, color = NA, size = 0.5, colour = map_settings$grid_color
      ),
      legend.key = ggplot2::element_rect(fill = map_settings$panel_fill, color = NA),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "cm") # Margen alrededor de todo el plot
    )
}
