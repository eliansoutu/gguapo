
#' @title Plot Robótico
#' @description Genera gráficos con una estética futurista y robótica utilizando ggplot2, con colores brillantes y una fuente de estilo tecnológico.
#' @param data Un data frame que contiene los datos a graficar.
#' @param tipo Un string que especifica el tipo de gráfico a generar. Puede ser "barra" para un gráfico de barras (horizontal) o "linea" para un gráfico de líneas.
#' @param x Un string que especifica el nombre de la columna para el eje x. Para gráficos de barras, esta será la variable categórica a reordenar. Para gráficos de línea, la variable continua/temporal.
#' @param y Un string que especifica el nombre de la columna para el eje y. Para gráficos de barras, esta será la variable numérica a graficar. Para gráficos de línea, la variable numérica.
#' @param titulo Un string opcional que especifica el título principal del gráfico.
#' @return Un objeto ggplot2 que representa el gráfico con estilo robótico.
#' @importFrom ggplot2 ggplot aes reorder geom_col geom_text coord_flip geom_line geom_point labs theme_minimal theme element_blank element_text element_line element_rect
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom ggtext element_markdown
#' @importFrom rlang .data
#' @export
plot_robotico <- function(data, tipo = "barra", x = NULL, y = NULL, titulo = NULL) {

  # Carga la fuente Orbitron de Google Fonts
  sysfonts::font_add_google("Orbitron", "orbitron")
  showtext::showtext_auto() # Activa el renderizado de texto con showtext

  tipo <- tolower(tipo) # Normaliza el tipo a minúsculas

  p <- switch(tipo,
              "barra" = ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x]], .data[[y]]), y = .data[[y]])) +
                ggplot2::geom_col(fill = "#00e5ff", width = 0.7) + # Barras de color cian brillante
                ggplot2::geom_text(ggplot2::aes(label = .data[[y]]), hjust = -0.2, size = 4, color = "white", family = "orbitron") + # Etiquetas blancas en las barras
                ggplot2::coord_flip(), # Voltea las coordenadas para barras horizontales
              "linea" = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
                ggplot2::geom_line(color = "#00bcd4", linewidth = 1.3) + # Línea de color azul brillante
                ggplot2::geom_point(color = "#ff4081", size = 3), # Puntos de color rosa neón
              stop("Tipo de gráfico no soportado") # Mensaje de error para tipos no válidos
  )

  p + ggplot2::labs(title = titulo) + # Añade el título
    ggplot2::theme_minimal(base_family = "orbitron") + # Tema minimalista con fuente Orbitron
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5, color = "#00e5ff"), # Título en cian brillante
      axis.title = ggplot2::element_blank(), # Elimina los títulos de los ejes
      axis.text = ggplot2::element_text(color = "#B0BEC5", size = 12), # Texto de los ejes en gris claro
      panel.grid.major = ggplot2::element_line(color = "#37474F"), # Líneas de cuadrícula principales en gris oscuro
      panel.grid.minor = ggplot2::element_blank(), # Elimina las líneas de cuadrícula menores
      plot.background = ggplot2::element_rect(fill = "#212121", color = NA), # Fondo del gráfico en gris muy oscuro
      legend.background = ggplot2::element_rect(fill = "#212121", color = NA), # Fondo de la leyenda en gris muy oscuro
      legend.text = ggplot2::element_text(color = "#ECEFF1") # Texto de la leyenda en blanco/gris muy claro
    )
}
