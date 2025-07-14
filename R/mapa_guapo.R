#' @title Mapa Guapo
#' @description Genera mapas estéticamente agradables utilizando `ggplot2` y `sf`, con opciones para diferentes tipos de geometría y personalización visual.
#' @param data Un objeto `sf` (simple features) que contiene los datos geográficos a visualizar.
#' @param tipo Un string que especifica el tipo de geometría a graficar. Puede ser "poligono", "linea" o "punto".
#' @param fill Un string opcional que especifica el nombre de la columna para mapear el color de relleno de los polígonos. Solo aplica para `tipo = "poligono"`.
#' @param color Un string opcional que especifica el nombre de la columna para mapear el color de las líneas o puntos. Solo aplica para `tipo = "linea"` o `tipo = "punto"`.
#' @param size Un valor numérico que especifica el tamaño de las líneas o puntos. Por defecto es 1.
#' @param titulo Un string opcional que especifica el título principal del mapa.
#' @param etiqueta Un string opcional que especifica el nombre de la columna a usar para etiquetar las geometrías en el mapa.
#' @param palette Un string que especifica la paleta de colores a usar para el relleno de polígonos (ej. "Blues", "Reds"). Compatible con `scale_fill_distiller`.
#' @param font Un string que especifica la familia de fuente a usar para el texto del mapa (título, etiquetas). Por defecto es "Poppins". Asegúrate de que la fuente esté disponible.
#' @return Un objeto `ggplot2` que representa el mapa generado.
#' @importFrom ggplot2 ggplot aes geom_sf scale_fill_distiller scale_color_viridis_c labs theme_void theme element_text element_rect
#' @importFrom ggfx with_shadow
#' @importFrom ggtext geom_richtext element_markdown
#' @importFrom rlang .data
#' @export
mapa_guapo <- function(data, tipo = "poligono", fill = NULL, color = NULL, size = 1,
                       titulo = NULL, etiqueta = NULL, palette = "Blues", font = "Poppins") {

  tipo <- tolower(tipo)
  base <- ggplot2::ggplot(data = data)

  if (tipo == "poligono") {
    base <- base +
      ggfx::with_shadow(
        ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill]]), color = "white", linewidth = 0.3), # Changed size to linewidth
        sigma = 3, x_offset = 1, y_offset = 1, colour = "grey30", alpha = 0.2
      ) +
      ggplot2::scale_fill_distiller(palette = palette, direction = 1)
  } else if (tipo == "linea") {
    base <- base +
      ggplot2::geom_sf(ggplot2::aes(color = .data[[color]]), linewidth = size) + # Changed size to linewidth
      ggplot2::scale_color_viridis_c()
  } else if (tipo == "punto") {
    base <- base +
      ggplot2::geom_sf(ggplot2::aes(color = .data[[color]]), size = size, alpha = 0.8) +
      ggplot2::scale_color_viridis_c()
  } else {
    stop("Tipo debe ser 'poligono', 'linea' o 'punto'")
  }

  if (!is.null(etiqueta)) {
    base <- base +
      ggtext::geom_richtext(
        ggplot2::aes(label = .data[[etiqueta]], geometry = geometry),
        stat = "sf_coordinates",
        family = font, size = 3, fill = NA, label.color = NA
      )
  }

  base +
    ggplot2::labs(title = titulo) +
    ggplot2::theme_void(base_family = font) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5, color = "#666"),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 10),
      plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA),
      legend.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA)
    )
}
