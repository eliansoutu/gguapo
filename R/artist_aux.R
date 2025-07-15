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
load_all_fonts <- function() {
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
  sysfonts::font_add_google("Bangers", "Bangers")
  sysfonts::font_add_google("Poppins", "Poppins")
  sysfonts::font_add_google("Orbitron", "Orbitron")
  sysfonts::font_add_google("Open Sans", "Open Sans")

  message("Todas las fuentes de los estilos de artistas han sido cargadas.")
}

#' @title Configura los estilos visuales según el artista y obra
#' @description Devuelve una lista con parámetros visuales como paleta, tipografía, colores y más, según el artista y la obra elegida.
#' @param artist Nombre del artista: "da_vinci" o "michelangelo".
#' @param work Nombre de la obra específica del artista.
#' @return Una lista con los parámetros del estilo visual.
#' @export
get_artist_settings <- function(artist = c("da_vinci", "michelangelo", "rembrandt", "van_gogh", "monet",
                                           "banksy", "dali", "artemisia", "miro", "kandinsky", "warhol"), work) {
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

  artemisia_settings <- list(
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
                     "artemisia" = artemisia_settings[[work]],
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
#' @param x Nombre de la variable del eje x (para etiquetas).
#' @param y Nombre de la variable del eje y (para etiquetas).
#' @param base_theme_fun Función de tema base de ggplot2 (ej. `ggplot2::theme_void`).
#' @param grid_linetype Tipo de línea para la cuadrícula principal.
#' @param grid_linewidth Ancho de línea para la cuadrícula principal.
#' @param axis_line_linewidth Ancho de línea para los ejes.
#' @param panel_background_map_specific Lógico, si el tema del panel tiene lógica especial para mapas.
#' @return Objeto ggplot2 con el tema y las etiquetas aplicadas.
apply_common_theme_and_labs <- function(p, settings, plot_type, add_grid_lines, show_background,
                                        title, subtitle, caption, x, y,
                                        base_theme_fun, grid_linetype, grid_linewidth,
                                        axis_line_linewidth, panel_background_map_specific = FALSE) {

  # Base theme (e.g., theme_void or theme_minimal)
  current_theme <- base_theme_fun() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 28, face = "bold", hjust = 0.5, family = settings$font_title, color = settings$text_color, margin = ggplot2::margin(b = 15)),
      plot.subtitle = ggtext::element_markdown(size = 18, hjust = 0.5, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(b = 25)),
      plot.caption = ggtext::element_markdown(size = 12, hjust = 1, family = settings$font_body, color = settings$text_color, margin = ggplot2::margin(t = 15)),
      legend.position = "bottom",
      legend.title = ggtext::element_markdown(size = 14, family = settings$font_body, color = settings$text_color),
      legend.text = ggplot2::element_text(size = 12, family = settings$font_body, color = settings$text_color),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      plot.margin = ggplot2::unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
      plot.background = ggplot2::element_rect(fill = settings$background_fill, colour = NA),
      # Conditional panel.background logic
      panel.background = if (panel_background_map_specific && plot_type == "map") {
        ggplot2::element_rect(fill = settings$background_fill, colour = NA)
      } else if (!show_background) {
        ggplot2::element_blank()
      } else {
        ggplot2::element_rect(fill = settings$panel_fill, colour = NA)
      },
      # Axis and grid adjustments based on plot_type
      axis.text = if (plot_type != "map") ggplot2::element_text(size = 12, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      axis.title = if (plot_type != "map") ggtext::element_markdown(size = 15, color = settings$text_color, family = settings$font_body) else ggplot2::element_blank(),
      panel.grid.major = if (add_grid_lines && plot_type != "map") ggplot2::element_line(color = settings$grid_color, linetype = grid_linetype, linewidth = grid_linewidth) else ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = if (plot_type != "map") ggplot2::element_line(color = settings$grid_color, linewidth = axis_line_linewidth) else ggplot2::element_blank()
    )

  # Construct the labs arguments dynamically
  lab_args <- list(
    title = paste0("<span style='font-family:\"", settings$font_title, "\";'>", title, "</span>"),
    subtitle = paste0("<span style='font-family:\"", settings$font_body, "\";'>", subtitle, "</span>"),
    caption = paste0("<span style='font-family:\"", settings$font_body, "\";'>", caption, "</span>")
  )

  # Only add x and y to labs if they are not NULL and plot_type is not "map"
  if (!is.null(x) && plot_type != "map") {
    lab_args$x = paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", x)), "</span>")
  } else {
    lab_args$x = NULL # Explicitly set to NULL if not applicable
  }

  if (!is.null(y) && plot_type != "map") {
    lab_args$y = paste0("<span style='font-family:\"", settings$font_body, "\";'>", tools::toTitleCase(gsub("_", " ", y)), "</span>")
  } else {
    lab_args$y = NULL # Explicitly set to NULL if not applicable
  }

  p + current_theme + do.call(ggplot2::labs, lab_args)
}
