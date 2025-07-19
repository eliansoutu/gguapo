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

#' @title Load Google Fonts used in artist styles
#' @description Load only the Google Fonts used in a specific artist's visual styles,
#' or all fonts if 'artist = "all"'. This avoids unnecessary font loading and speeds up rendering.
#'
#' @param artist Character. One of the supported artists (e.g., "dali", "monet") or "all" to load all fonts. Default is "all".
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @export
load_all_fonts <- function(artist = "all") {
  showtext::showtext_auto()

  fonts_by_artist <- list(
    da_vinci     = c("Playfair Display", "Merriweather"),
    michelangelo = c("Merriweather", "Open Sans", "Roboto Condensed", "Lato"),
    rembrandt    = c("Merriweather", "Roboto Condensed", "Playfair Display", "Lato", "Special Elite", "Cutive Mono"),
    van_gogh     = c("Playfair Display", "Roboto Condensed", "Permanent Marker", "Cabin Sketch", "Lato", "Open Sans"),
    monet        = c("Playfair Display", "Poppins", "Open Sans", "Roboto Condensed"),
    banksy       = c("Permanent Marker", "Cabin Sketch", "Cutive Mono"),
    dali         = c("Playfair Display", "Special Elite", "Merriweather", "Roboto Condensed", "Lato"),
    miro         = c("Amatic SC", "Indie Flower", "Architects Daughter", "Pacifico", "Lato"),
    gentileschi  = c("Playfair Display", "Merriweather", "Lato", "Roboto Condensed"),
    kandinsky    = c("Oswald", "Bebas Neue", "Lato"),
    warhol       = c("Bangers", "Oswald", "Permanent Marker", "Cabin Sketch", "Cutive Mono", "Libre Barcode 39 Text", "Bebas Neue")
  )

  if (!artist %in% c("all", names(fonts_by_artist))) {
    stop("Artista no reconocido. Usa 'all' o uno de: ", paste(names(fonts_by_artist), collapse = ", "))
  }

  fonts_to_load <- if (artist == "all") {
    unique(unlist(fonts_by_artist))
  } else {
    unique(fonts_by_artist[[artist]])
  }

  for (f in fonts_to_load) {
    sysfonts::font_add_google(f, f)
  }

  message("Fuentes cargadas: ", paste(fonts_to_load, collapse = ", "))
}



#' @title Configura los estilos visuales según el artista y obra
#' @description Devuelve una lista con parámetros visuales como paleta, tipografía, colores y más, según el artista y la obra elegida.
#' @param artist Nombre del artista: "da_vinci" o "michelangelo".
#' @param work Nombre de la obra específica del artista.
#' @return Una lista con los parámetros del estilo visual.
#' @export
get_artist_settings <- function(artist = c("da_vinci", "michelangelo", "rembrandt", "van_gogh", "monet",
                                           "banksy", "dali", "gentileschi", "miro", "kandinsky", "warhol"), work) {
  artist <- match.arg(artist)

  da_vinci_settings <- list(
    "mona_lisa" = list(
      colors = c("#8B4513", "#CD853F", "#A0522D", "#694F4F", "#D2B48C"),
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#F5DEB3", panel_fill = "#FAEBD7",
      grid_color = "#D2B48C", text_color = "#4F4F4F", geom_alpha = 0.8
    ),
    "last_supper" = list(
      colors = c("#5C4033", "#A9A9A9", "#708090", "#D3D3D3", "#BDB76B"),
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#36454F", panel_fill = "#2F4F4F",
      grid_color = "#5C4033", text_color = "#D3D3D3", geom_alpha = 0.7
    ),
    "vitruvian_man" = list(
      colors = c("#4B0082", "#7B68EE", "#8A2BE2", "#C0C0C0", "#F0E68C"),
      font_title = "Playfair Display", font_body = "Merriweather",
      background_fill = "#F5F5DC", panel_fill = "#E0E0E0",
      grid_color = "#A9A9A9", text_color = "#4B0082", geom_alpha = 0.9
    )
  )

  michelangelo_settings <- list(
    "david" = list(
      colors = c("#B0C4DE", "#DCDCDC", "#E6E6FA", "#708090", "#8470FF"),
      font_title = "Merriweather", font_body = "Open Sans",
      background_fill = "#F0F8FF", panel_fill = "#E0FFFF",
      grid_color = "#A9A9A9", text_color = "#4682B4", geom_alpha = 0.9,
      glow_color = "#ADD8E6"
    ),
    "sistine_chapel" = list(
      colors = c("#8B0000", "#FFD700", "#483D8B", "#DAA520", "#B22222"),
      font_title = "Playfair Display", font_body = "Roboto Condensed",
      background_fill = "#2F4F4F", panel_fill = "#4F4F4F",
      grid_color = "#696969", text_color = "#FFD700", geom_alpha = 0.8,
      glow_color = "#DAA520"
    ),
    "pieta" = list(
      colors = c("#D3D3D3", "#A9A9A9", "#C0C0C0", "#778899", "#5F9EA0"),
      font_title = "Merriweather", font_body = "Lato",
      background_fill = "#E6E6FA", panel_fill = "#D8BFD8",
      grid_color = "#B0C4DE", text_color = "#6A5ACD", geom_alpha = 0.9,
      glow_color = "#C0C0C0"
    )
  )

  rembrandt_settings <- list(
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
  )

  van_gogh_settings <- list(
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
  )

  monet_settings <- list(
    "water_lilies" = list(
      colors = c("#87CEEB", "#ADD8E6", "#B0E0E6", "#6A5ACD", "#483D8B"),
      font_title = "Playfair Display", font_body = "Poppins",  # actualizado
      background_fill = "#E0F2F7", panel_fill = "#F0FFFF",
      grid_color = "#C0D9E0", text_color = "#4682B4", geom_alpha = 0.7,
      glow_color = "#ADD8E6"
    ),
    "impression_sunrise" = list(
      colors = c("#FF8C00", "#FF4500", "#FFA07A", "#FFD700", "#FFEA00"),
      font_title = "Playfair Display", font_body = "Poppins",  # actualizado
      background_fill = "#FFF0D9", panel_fill = "#FFF5E0",
      grid_color = "#FFD700", text_color = "#D2691E", geom_alpha = 0.8,
      glow_color = "#FFD700"
    ),
    "poppy_fields" = list(
      colors = c("#B22222", "#DC143C", "#FF6347", "#556B2F", "#8FBC8F"),
      font_title = "Open Sans", font_body = "Poppins",  # actualizado
      background_fill = "#F5FFFA", panel_fill = "#F0FFF0",
      grid_color = "#98FB98", text_color = "#8B0000", geom_alpha = 0.8,
      glow_color = "#DC143C"
    )
  )

  banksy_settings <- list(
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
  )


  dali_settings <- list(
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
  )

  miro_settings <- list(
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
  )

  gentileschi_settings <- list(
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
  )

  kandinsky_settings <- list(
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
  )

  warhol_settings <- list(
    "soup_cans" = list(
      colors = c("#FF0000", "#FFFFFF", "#000000", "#FFD700", "#808080"),
      font_title = "Bangers", font_body = "Oswald",  # actualizado
      background_fill = "#D3D3D3", panel_fill = "#C0C0C0",
      grid_color = "#A9A9A9", text_color = "#000000", geom_alpha = 1,
      geom_lwd = 1.0
    ),
    "marilyn_monroe" = list(
      colors = c("#FF00FF", "#00FFFF", "#FFD700", "#B22222", "#4169E1"),
      font_title = "Permanent Marker", font_body = "Cabin Sketch",
      background_fill = "#F5F5F5", panel_fill = "#F8F8F8",
      grid_color = "#E0E0E0", text_color = "#000000", geom_alpha = 0.9,
      geom_lwd = 0.8
    ),
    "cow_wallpaper" = list(
      colors = c("#006400", "#9ACD32", "#FFFFFF", "#000000", "#8B4513"),
      font_title = "Bebas Neue", font_body = "Cutive Mono",
      background_fill = "#E6E6FA", panel_fill = "#D8BFD8",
      grid_color = "#A9A9A9", text_color = "#000000", geom_alpha = 0.9,
      geom_lwd = 0.6
    )
  )

  settings <- switch(artist,
                     "da_vinci" = da_vinci_settings[[work]],
                     "michelangelo" = michelangelo_settings[[work]],
                     "rembrandt" = rembrandt_settings[[work]],
                     "van_gogh" = van_gogh_settings[[work]],
                     "monet" = monet_settings[[work]],
                     "banksy" = banksy_settings[[work]],
                     "dali" = dali_settings[[work]],
                     "miro" = miro_settings[[work]],
                     "gentileschi" = gentileschi_settings[[work]],
                     "kandinsky" = kandinsky_settings[[work]],
                     "warhol" = warhol_settings[[work]]
                     )

  if (is.null(settings)) stop("Obra no reconocida para el artista.")
  return(settings)
}

#' @title Función auxiliar para aplicar temas y etiquetas comunes
#' @description Encapsula la lógica de ggplot2::theme() y ggplot2::labs() para reducir la duplicación de código.
#' @param p Objeto ggplot2 actual.
#' @param settings Lista de configuraciones de estilo (colores, fuentes, etc.).
#' @param plot_type Tipo de gráfico (para ajustes condicionales de ejes/cuadrícula).
#' @param add_grid_lines Lógico, si mostrar líneas de cuadrícula.
#' @param show_background Lógico, si mostrar el fondo del panel.
#' @param title Título del gráfico.
#' @param subtitle Subtítulo del gráfico.
#' @param caption Leyenda del gráfico.
#' @param x_label Nombre de la variable del eje x (para etiquetas).
#' @param y_label Nombre de la variable del eje y (para etiquetas).
#' @param base_theme_fun Función de tema base de ggplot2 (ej. `ggplot2::theme_void`).
#' @param grid_linetype Tipo de línea para la cuadrícula principal.
#' @param grid_linewidth Ancho de línea para la cuadrícula principal.
#' @param axis_line_linewidth Ancho de línea para los ejes.
#' @param panel_background_map_specific Lógico, si el tema del panel tiene lógica especial para mapas.
#' @return Objeto ggplot2 con el tema y las etiquetas aplicadas.
apply_common_theme_and_labs <- function(p, settings, plot_type, add_grid_lines, show_background,
                                        title, subtitle, caption, x_label, y_label,
                                        base_theme_fun, grid_linetype, grid_linewidth,
                                        axis_line_linewidth, panel_background_map_specific = FALSE,
                                        text_size = 12) {

  # Escalado proporcional según text_size base
  size_title <- text_size * 2.2
  size_subtitle <- text_size * 1.5
  size_caption <- text_size * 1.1
  size_legend_title <- text_size * 1.2
  size_legend_text <- text_size
  size_axis_text <- text_size
  size_axis_title <- text_size * 1.25

  current_theme <- base_theme_fun() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = size_title, face = "bold", hjust = 0.5,
                                            family = settings$font_title, color = settings$text_color,
                                            margin = ggplot2::margin(b = text_size * 1.2)),
      plot.subtitle = ggtext::element_markdown(size = size_subtitle, hjust = 0.5,
                                               family = settings$font_body, color = settings$text_color,
                                               margin = ggplot2::margin(b = text_size * 1.8)),
      plot.caption = ggtext::element_markdown(size = size_caption, hjust = 1,
                                              family = settings$font_body, color = settings$text_color,
                                              margin = ggplot2::margin(t = text_size * 1.2)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = size_legend_title,
                                              family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = size_legend_text,
                                          family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(rep(1.5, 4), "cm"),
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),

      panel.background = if (panel_background_map_specific && plot_type == "map") {
        ggplot2::element_rect(fill = settings$background_fill, colour = NA)
      } else if (!show_background) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },

      axis.text = if (plot_type != "map") ggplot2::element_text(size = size_axis_text,
                                                                color = settings$text_color,
                                                                family = settings$font_body) else ggplot2::element_blank(),

      axis.title = if (plot_type != "map") ggtext::element_markdown(size = size_axis_title,
                                                                    color = settings$text_color,
                                                                    family = settings$font_body) else ggplot2::element_blank(),

      panel.grid.major = if (add_grid_lines && plot_type != "map") {
        ggplot2::element_line(color = settings$grid_color, linetype = grid_linetype, linewidth = grid_linewidth)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") {
        ggplot2::element_line(color = settings$grid_color, linewidth = axis_line_linewidth)
      } else {
        ggplot2::element_blank()
      }
    )

  # Construcción dinámica de labels
  lab_args <- list(
    title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
    subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
    caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>")
  )

  if (!is.null(x_label) && plot_type != "map") {
    lab_args$x <- paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x_label)), "</span>")
  }

  if (!is.null(y_label) && plot_type != "map") {
    lab_args$y <- paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y_label)), "</span>")
  }

  p + current_theme + do.call(ggplot2::labs, lab_args)
}


#' @title Common core function for artist-inspired ggplot2 styles
#' @description Applies a visual style to ggplot2 charts inspired by an artist and specific artwork, with optional painterly effects like texture, canvas overlays, and glow.
#'
#' @param data A data frame or `sf` object, depending on the plot type.
#' @param artist String. Artist name (e.g., `"van_gogh"`, `"da_vinci"`).
#' @param obra_inspiracion String. Specific artwork to define the palette/theme.
#' @param x,y Tidy-evaluated expressions for aesthetics.
#' @param color_var Optional. Tidy-evaluated expression for color mapping.
#' @param fill_var Optional. Tidy-evaluated expression for fill mapping.
#' @param label_var Optional. Tidy-evaluated expression for label text.
#' @param plot_type One of `"scatter"`, `"line"`, `"column"`, or `"map"`.
#' @param title,subtitle,caption Plot title, subtitle, and caption.
#' @param show_labels Logical. Add text labels.
#' @param add_grid_lines Logical. Show grid lines.
#' @param show_background Logical. Show background fill (for non-map plots).
#' @param add_glow Logical. Apply glow effect to geoms.
#' @param coord_flip Logical. Flip coordinates (for column plots).
#' @param theme_base Base ggplot2 theme. Default: `theme_void`.
#' @param grid_linetype Line type for grid. Default: `"dotted"`.
#' @param grid_linewidth Width of grid lines. Default: `0.3`.
#' @param axis_line_linewidth Width of axis lines. Default: `0.8`.
#' @param panel_background_map_specific Logical. Special map background fill. Default: `FALSE`.
#' @param text_size Numeric. Base text size for titles, labels, etc. Default: `12`.
#' @param add_texture Integer (1–3). Applies visual texture effects to geoms. Default: `NULL`.
#' @param canvas Integer (1–6). Adds canvas-style background image. Default: `NULL`.
#' @param add_filter Experimental. Logical. Applies oil effect to the full graph. Default: `FALSE`.
#'
#' @return A `ggplot` object styled with artistic aesthetics.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank
#' @importFrom ggplot2 scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip
#' @importFrom ggplot2 geom_sf geom_sf_text geom_text annotation_custom
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow with_blur
#' @importFrom rlang sym enquo as_label quo_is_null inject expr
#' @importFrom purrr map
#' @importFrom grid rasterGrob unit
#' @importFrom magick image_read image_colorize image_graph image_oilpaint image_ggplot
#' @importFrom ggpattern geom_col_pattern
#' @importFrom grDevices dev.off
#' @export
style_artist_common <- function(data, artist, obra_inspiracion,
                                x = NULL, y = NULL,
                                color_var = NULL, fill_var = NULL, label_var = NULL,
                                plot_type = c("scatter", "line", "column", "map"),
                                title, subtitle, caption,
                                show_labels = FALSE, add_grid_lines = FALSE,
                                show_background = TRUE, add_glow = FALSE,
                                coord_flip = FALSE,
                                theme_base = ggplot2::theme_void,
                                grid_linetype = "dotted",
                                grid_linewidth = 0.3,
                                axis_line_linewidth = 0.8,
                                panel_background_map_specific = FALSE,
                                text_size = 12,
                                add_texture = NULL,
                                canvas = NULL,
                                add_filter = FALSE) {
  plot_type <- match.arg(plot_type)
  settings <- get_artist_settings(artist, obra_inspiracion)

  # Captura de quosures
  x_quo <- enquo(x)
  y_quo <- enquo(y)
  color_quo <- enquo(color_var)
  fill_quo <- enquo(fill_var)
  label_quo <- enquo(label_var)

  # Labels como strings
  x_label <- if (!quo_is_null(x_quo) && plot_type != "map") as_label(x_quo) else NULL
  y_label <- if (!quo_is_null(y_quo) && plot_type != "map") as_label(y_quo) else NULL

  # Construcción del gráfico
  p <- if (plot_type == "map") {
    if (!inherits(data, "sf")) stop("Para plot_type = 'map', 'data' debe ser un objeto 'sf'.")
    fill_map <- if (!quo_is_null(fill_quo)) expr(fill = !!fill_quo) else NULL
    color_map <- if (!quo_is_null(color_quo)) expr(color = !!color_quo) else NULL
    aes_map_sf <- inject(aes(!!!c(fill_map, color_map)))

    geom_sf_layer <- geom_sf(aes_map_sf, lwd = 0.6, colour = settings$grid_color)

    base <- ggplot(data)
    if (add_glow) {
      base + with_outer_glow(geom_sf_layer, colour = settings$glow_color, sigma = 5, expand = 3)
    } else {
      base + geom_sf_layer
    }
  } else {
    if (quo_is_null(x_quo) || quo_is_null(y_quo)) stop("Debe especificar x e y para este tipo de gráfico.")
    ggplot(data, aes(x = !!x_quo, y = !!y_quo))
  }

  # Escalas de color
  if (!quo_is_null(color_quo)) {
    p <- p + generate_color_scale(data, as_label(color_quo), settings$colors, "color")
  }
  if (!quo_is_null(fill_quo)) {
    p <- p + generate_color_scale(data, as_label(fill_quo), settings$colors, "fill")
  }

  if (!is.null(canvas)) {

    if (canvas %in% 1:6) {

    img_path <- system.file("extdata", paste0("texture", canvas, ".png"), package = "gguapo")

    if (file.exists(img_path)) {
      textura <- magick::image_read(img_path)
      textura <- magick::image_colorize(textura, opacity = 70, color = settings$panel_fill)
      raster <- grid::rasterGrob(textura, width = unit(1, "npc"), height = unit(1, "npc"))
      p <- p + ggplot2::annotation_custom(raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    } else {
      warning("No se encontró la imagen de textura correspondiente al canvas seleccionado.")
    }

  } else {
    warning("Las opciones de canvas van del 1 al 6")
  }

  }
  # Geoms según tipo
  if (plot_type %in% c("scatter", "line", "column")) {
    if (plot_type == "scatter") {
      geom_layer <- if(!is.null(add_texture) && add_texture %in% 1:3) {

        with_blur(geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1]), sigma = 2)

      } else if (quo_is_null(color_quo)) {
        geom_point(size = 4, alpha = settings$geom_alpha, color = settings$colors[1])
      } else {
        geom_point(aes(color = !!color_quo), size = 4, alpha = settings$geom_alpha)
      }
    } else if (plot_type == "line") {
      geom_layer <- list(
        if (!is.null(add_texture) && add_texture %in% 1:3) {

          list(with_blur(geom_line(aes(group = 1), size = 1.5, color = settings$colors[1]), sigma = 1),  # línea tipo pincel
          with_blur(geom_point(size = 2, alpha = settings$geom_alpha, color = settings$colors[1]), sigma = 2))

        } else if (quo_is_null(color_quo)) {
          geom_line(aes(group = 1), size = 1.5, alpha = settings$geom_alpha, color = settings$colors[1])
        } else {
          geom_line(aes(color = !!color_quo, group = !!color_quo), size = 1.5, alpha = settings$geom_alpha)
        },
        if (quo_is_null(color_quo)) {
          geom_point(size = 3, alpha = settings$geom_alpha, color = settings$colors[1])
        } else {
          geom_point(aes(color = !!color_quo), size = 3, alpha = settings$geom_alpha)
        }
      )
    } else if (plot_type == "column") {
      if (!quo_is_null(x_quo)) {
        x_var <- as_label(x_quo)
        data[[x_var]] <- factor(data[[x_var]])
      }
      geom_layer <- if (!is.null(add_texture) && add_texture %in% 1:3) {

        ggpattern::geom_col_pattern(
              #pattern_density = 0.3,
              #pattern_spacing = 0.02,
              pattern_alpha = .7,
              pattern = "image",
              pattern_type = "expand",
              pattern_filename = system.file("extdata", paste0("column_texture",add_texture,".png"), package = "gguapo"),
              fill = settings$colors[1],
              colour = settings$colors[3],
              pattern_fill = settings$colors[1],
              pattern_fill2 = settings$colors[2]
            )

      } else if (quo_is_null(fill_quo)) {
        geom_col(width = 0.7, alpha = settings$geom_alpha, fill = settings$colors[1])
      } else {
        geom_col(aes(fill = !!fill_quo), width = 0.7, alpha = settings$geom_alpha)
      }

      if (coord_flip) p <- p + coord_flip()

    }

    # Aplicar glow
    if (add_glow) {
      if (is.list(geom_layer)) {
        for (layer in geom_layer) {
          p <- p + ggfx::with_outer_glow(layer, colour = settings$colors[1], sigma = 5, expand = 3)
        }
      } else {
        p <- p + ggfx::with_outer_glow(geom_layer, colour = settings$colors[1], sigma = 5, expand = 3)
      }
    } else {
      if (is.list(geom_layer)) {
        for (layer in geom_layer) {
          p <- p + layer
        }
      } else {
        p <- p + geom_layer
      }
    }

  }

  # Etiquetas
  if (show_labels) {
    label_quo_final <- if (!quo_is_null(label_quo)) label_quo else y_quo
    label_size <- text_size * 0.28
    if (plot_type == "map") {
      p <- p + geom_sf_text(
        aes(label = !!label_quo_final),
        color = settings$text_color,
        family = settings$font_body,
        size = label_size,
        bg.colour = "white", bg.r = 0.05
      )
    } else if (plot_type == "column" && coord_flip) {
      p <- p + geom_text(
        aes(label = !!label_quo_final),
        hjust = -0.3,
        size = label_size,
        color = settings$text_color, family = settings$font_body
      )
    } else if (plot_type == "column") {
      p <- p + geom_text(
        aes(label = !!label_quo_final),
        vjust = -0.5,
        size = label_size,
        color = settings$text_color, family = settings$font_body
      )
    } else {
      p <- p + ggrepel::geom_text_repel(
        aes(label = !!label_quo_final),
        size = label_size,
        box.padding = 0.5, segment.color = settings$grid_color,
        segment.size = 0.3, max.overlaps = 50,
        family = settings$font_body, color = settings$text_color
      )
    }
  }

  # Tema común final
  p <- apply_common_theme_and_labs(
    p = p,
    settings = settings,
    plot_type = plot_type,
    add_grid_lines = add_grid_lines,
    show_background = show_background,
    title = title,
    subtitle = subtitle,
    caption = caption,
    x_label = x_label,
    y_label = y_label,
    base_theme_fun = theme_base,
    grid_linetype = grid_linetype,
    grid_linewidth = grid_linewidth,
    axis_line_linewidth = axis_line_linewidth,
    panel_background_map_specific = panel_background_map_specific,
    text_size = text_size
  )

  # Opciones de textura/difuminacion
  # if (add_texture) {
  #   # 1. Aplicar blur a las capas geométricas
  #   if (plot_type %in% c("scatter", "column", "line")) {
  #     p <- ggfx::with_blur(p, sigma = .5)
  #   }
  # }


  if (add_filter) {

    img_magick <- magick::image_graph(width = 1800, height = 1000)
    print(p)   # importante: usar print()
    dev.off()  # cer

    #temp <- magick::image_noise(img_magick, "Uniform")
    temp <- magick::image_oilpaint(img_magick, radius = 1)

    p <- magick::image_ggplot(temp)

  }


  return(p)
}
