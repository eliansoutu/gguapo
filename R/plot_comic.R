#' @title Plot estilo Cómic
#' @description Genera gráficos con estética de cómic utilizando ggplot2, ideal para visualizaciones impactantes y divertidas.
#' @param data Un data frame que contiene los datos a graficar.
#' @param tipo Un string que especifica el tipo de gráfico a generar. Puede ser "barra" para un gráfico de barras (horizontal) o "linea" para un gráfico de líneas.
#' @param x Un string que especifica el nombre de la columna para el eje x. Para gráficos de barras, esta será la variable categórica a reordenar. Para gráficos de línea, la variable continua/temporal.
#' @param y Un string que especifica el nombre de la columna para el eje y. Para gráficos de barras, esta será la variable numérica a graficar. Para gráficos de línea, la variable numérica.
#' @param fill Un string opcional que especifica el nombre de la columna para mapear el relleno (actualmente no utilizado directamente en la implementación, los colores son fijos).
#' @param titulo Un string que especifica el título principal del gráfico.
#' @return Un objeto ggplot2 que representa el gráfico con estilo de cómic.
#' @importFrom ggplot2 ggplot aes reorder geom_col geom_line geom_point coord_flip labs theme_void theme element_text element_rect
#' @importFrom ggtext geom_richtext element_markdown
#' @importFrom grid unit
#' @export
plot_comic <- function(data, tipo = "barra", x = NULL, y = NULL, fill = NULL, titulo = NULL) {

  tipo <- tolower(tipo)

  p <- switch(tipo,
              "barra" = ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x]], .data[[y]]), y = .data[[y]])) +
                ggplot2::geom_col(fill = "#F44336") +
                ggtext::geom_richtext(ggplot2::aes(label = paste0("<b>", .data[[y]], "</b>")),
                                      fill = "#FFEB3B", color = "#000000", size = 4,
                                      label.padding = grid::unit(0.2, "lines"), family = "bangers",
                                      label.size = 0.5) +
                ggplot2::coord_flip(),
              "linea" = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
                ggplot2::geom_line(color = "#00BCD4", linewidth = 1.5) + # Changed size to linewidth for ggplot2 3.4.0+
                ggplot2::geom_point(color = "#FFEB3B", size = 4, shape = 21, stroke = 1.2),
              stop("Tipo no soportado")
  )

  p + ggplot2::labs(title = titulo) +
    ggplot2::theme_void(base_family = "bangers") +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 24, hjust = 0.5, face = "bold", color = "#F44336"),
      axis.text = ggplot2::element_text(size = 12, color = "#333333"),
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA)
    )
}
