#' @title Plot Elegante
#' @description Genera gráficos con una estética elegante y sofisticada utilizando ggplot2, ideal para presentaciones formales.
#' @param data Un data frame que contiene los datos a graficar.
#' @param tipo Un string que especifica el tipo de gráfico a generar. Puede ser "barra" para un gráfico de barras (horizontal) o "linea" para un gráfico de líneas.
#' @param x Un string que especifica el nombre de la columna para el eje x. Para gráficos de barras, esta será la variable categórica a reordenar. Para gráficos de línea, la variable continua/temporal.
#' @param y Un string que especifica el nombre de la columna para el eje y. Para gráficos de barras, esta será la variable numérica a graficar. Para gráficos de línea, la variable numérica.
#' @param titulo Un string opcional que especifica el título principal del gráfico.
#' @return Un objeto ggplot2 que representa el gráfico con estilo elegante.
#' @importFrom ggplot2 ggplot aes reorder geom_col geom_text coord_flip geom_line geom_point labs theme_minimal theme element_blank element_text element_line element_rect
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom ggtext element_markdown
#' @importFrom rlang .data
#' @export
plot_elegante <- function(data, tipo = "barra", x = NULL, y = NULL, titulo = NULL) {

  # Carga la fuente Playfair Display de Google Fonts
  sysfonts::font_add_google("Playfair Display", "playfair")
  showtext::showtext_auto() # Activa el renderizado de texto con showtext

  tipo <- tolower(tipo) # Normaliza el tipo a minúsculas

  p <- switch(tipo,
              "barra" = ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x]], .data[[y]]), y = .data[[y]])) +
                ggplot2::geom_col(fill = "#bfa46f", width = 0.7) + # Barras de color dorado/beige
                ggplot2::geom_text(ggplot2::aes(label = .data[[y]]), hjust = -0.1, size = 4, family = "playfair") + # Etiquetas de texto en la barra
                ggplot2::coord_flip(), # Voltea las coordenadas para barras horizontales
              "linea" = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
                ggplot2::geom_line(color = "#2e2e2e", linewidth = 1) + # Línea de color gris oscuro
                ggplot2::geom_point(color = "#bfa46f", size = 3), # Puntos de color dorado/beige
              stop("Tipo de gráfico no soportado") # Mensaje de error para tipos no válidos
  )

  p + ggplot2::labs(title = titulo) + # Añade el título
    ggplot2::theme_minimal(base_family = "playfair") + # Tema minimalista con fuente Playfair
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5), # Título del gráfico
      axis.title = ggplot2::element_blank(), # Elimina los títulos de los ejes
      axis.text = ggplot2::element_text(color = "#444", size = 12), # Texto de los ejes en gris oscuro
      panel.grid = ggplot2::element_line(color = "#dddddd"), # Líneas de cuadrícula en gris claro
      plot.background = ggplot2::element_rect(fill = "#fcfcfa", color = NA) # Fondo del gráfico en un blanco cálido
    )
}
