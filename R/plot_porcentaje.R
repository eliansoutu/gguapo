
#' @title Gráfico de Barras de Porcentaje Redondeadas
#' @description Genera un gráfico de barras horizontales con esquinas redondeadas, ideal para visualizar porcentajes o proporciones de forma clara y moderna.
#' @param data Un data frame que contiene los datos a graficar.
#' @param categoria Un string que especifica el nombre de la columna que contiene las categorías (variable cualitativa) para el eje y.
#' @param porcentaje Un string que especifica el nombre de la columna que contiene los valores de porcentaje (variable numérica entre 0 y 1) para el eje x.
#' @param titulo Un string opcional que especifica el título principal del gráfico.
#' @return Un objeto ggplot2 que representa el gráfico de barras de porcentaje redondeadas.
#' @importFrom ggplot2 ggplot aes geom_text coord_flip labs theme_minimal theme element_blank element_text element_rect
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom ggrounded geom_col_rounded
#' @importFrom grid unit
#' @importFrom ggtext element_markdown
#' @importFrom rlang .data
#' @export
plot_porcentaje <- function(data, categoria, porcentaje, titulo = NULL) {
  sysfonts::font_add_google("Poppins", "poppins")
  showtext::showtext_auto()
  # Asegura que los datos estén ordenados por el porcentaje para una visualización ascendente/descendente
  data <- data[order(data[[porcentaje]]), ]
  data$max <- 1 # Columna auxiliar para la barra de fondo del 100%

  ggplot2::ggplot(data) +
    ggrounded::geom_col_rounded(
      ggplot2::aes(x = .data[[categoria]], y = max),
      fill = "#90CAF9", alpha = 0.2, # Color de fondo claro
      radius = grid::unit(8, "pt"),
      width = 0.7
    ) +
    ggrounded::geom_col_rounded(
      ggplot2::aes(x = .data[[categoria]], y = .data[[porcentaje]]),
      fill = "#42A5F5", # Color de la barra de porcentaje
      radius = grid::unit(8, "pt"),
      width = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = .data[[categoria]], y = .data[[porcentaje]],
                   label = paste0(round(.data[[porcentaje]] * 100), "%")),
      hjust = 1.1, color = "white", size = 4.2, family = "poppins" # Etiqueta de porcentaje
    ) +
    ggplot2::coord_flip() + # Voltea las coordenadas para barras horizontales
    ggplot2::labs(title = titulo, x = NULL, y = NULL) + # Sin etiquetas de eje
    ggplot2::theme_minimal(base_family = "poppins") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5),
      axis.text.x = ggplot2::element_blank(), # Quita texto del eje x
      axis.text.y = ggplot2::element_text(size = 12, color = "#333333", face = "bold"),
      panel.grid = ggplot2::element_blank(), # Quita las líneas de la cuadrícula
      plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA), # Fondo del gráfico
      axis.ticks = ggplot2::element_blank() # Quita los ticks de los ejes
    )
}
