#' @title Previsualizar Estilos de Gráficos
#' @description Genera y muestra ejemplos de gráficos utilizando cada uno de los estilos definidos (Leonardo da Vinci, Artemisia Gentileschi, Cómic, Mapa Guapo, Porcentaje, Robótico, Elegante). Esto permite al usuario previsualizar y elegir el estilo que mejor se adapta a sus necesidades.
#' @param style_name Un string opcional para previsualizar un estilo específico. Si es "all" (por defecto), se previsualizan todos los estilos. Otras opciones válidas son: "da vinci"
#' @param display_plots Logical. Si es TRUE (por defecto), los gráficos se imprimen en el dispositivo actual uno por uno. Si es FALSE, se retorna una lista de objetos ggplot.
#' @return Una lista de objetos ggplot si `display_plots` es FALSE. Si `display_plots` es TRUE, los gráficos se imprimen y la función retorna `NULL` de forma invisible.
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point coord_flip labs theme_void theme element_text element_rect
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom ggtext geom_richtext element_markdown
#' @importFrom grid unit
#' @importFrom rlang .data
#' @importFrom stats reorder
#' @importFrom sf st_sf st_sfc st_polygon st_linestring st_point
#' @export
preview_styles <- function(style_name = "all", display_plots = TRUE) {

  plots <- list()

  # --- Datos de Mentira ---
  data_default <- data.frame(
    category = LETTERS[1:5],
    value = c(25, 45, 60, 30, 75),
    time = 1:5,
    group = c("A", "B", "A", "B", "A")
  )

  # 1. Estilo Leonardo da Vinci
  if (style_name %in% c("all", "da vinci")) {
    if (exists("style_da_vinci") && is.function(style_da_vinci)) {

      plots[["Leonardo - Column - mona_lisa"]] <- style_da_vinci(
        data_default, x = "category", y = "value", plot_type = "column",
        title = "Columnas al Estilo de Leonardo: mona_lisa", subtitle = "Magnitud de categorías"
      )
      plots[["Leonardo - Line - last_supper"]] <- style_da_vinci(
        data_default, x = "time", y = "value", plot_type = "line",
        work_inspired_by = "last_supper",
        title = "Lineas al Estilo de Leonardo: last_supper", subtitle = "Magnitud de categorías"
      )
      plots[["Leonardo - Point - vitruvian_man"]] <- style_da_vinci(
        data_default, x = "time", y = "value", plot_type = "scatter",
        work_inspired_by = "vitruvian_man",
        title = "Puntos al Estilo de Leonardo: vitruvian_man", subtitle = "Magnitud de categorías"
      )
    } else {
      warning("La función 'style_da_vinci' no fue encontrada. Se omite la previsualización del estilo Leonardo.")
    }
  }

  # 2. Estilo Miguel Ángel
  if (style_name %in% c("all", "michelangelo")) {
    if (exists("style_michelangelo") && is.function(style_michelangelo)) {

      plots[["Miguel Ángel - Column - david"]] <- style_michelangelo(
        data_default, x = "category", y = "value", plot_type = "column",
        title = "Columnas al Estilo de Miguel Ángel: david", subtitle = "Magnitud de categorías"
      )
      plots[["Miguel Ángel - Line - sistine_chapel"]] <- style_michelangelo(
        data_default, x = "time", y = "value", plot_type = "line",
        work_inspired_by = "sistine_chapel",
        title = "Lineas al Estilo de Miguel Ángel: sistine_chapel", subtitle = "Magnitud de categorías"
      )
      plots[["Miguel Ángel - Point - pieta"]] <- style_michelangelo(
        data_default, x = "time", y = "value", plot_type = "scatter",
        work_inspired_by = "pieta",
        title = "Puntos al Estilo de Miguel Ángel: pieta", subtitle = "Magnitud de categorías"
      )
    } else {
      warning("La función 'style_michelangelo' no fue encontrada. Se omite la previsualización del estilo Leonardo.")
    }
  }

  # Mostrar/Retornar gráficos
  if (display_plots) {
    for (i in seq_along(plots)) {
      cat(paste0("\n--- Previsualizando: ", names(plots)[i], " ---\n"))
      print(plots[[i]])
    }
    invisible(NULL) # Retorna NULL de forma invisible cuando se muestran los plots
  } else {
    return(plots) # Retorna la lista de objetos ggplot
  }
}
