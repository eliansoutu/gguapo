
plot_guapo <- function(data, tipo = "barra", x = NULL, y = NULL, titulo = NULL,
                       font = "Poppins", palette = "#42A5F5") {
  sysfonts::font_add_google(font, font)
  showtext::showtext_auto()
  tipo <- tolower(tipo)

  p <- switch(tipo,
    "barra" = ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x]], .data[[y]]), y = .data[[y]])) +
      ggplot2::geom_col(fill = palette, width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = .data[[y]]), hjust = -0.1, size = 4, family = font) +
      ggplot2::coord_flip(),
    "linea" = ggplot2::ggplot(data, ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
      ggplot2::geom_line(color = palette, size = 1.2) +
      ggplot2::geom_point(color = palette, size = 3),
    stop("Tipo no soportado")
  )

  p + ggplot2::labs(title = titulo) +
    ggplot2::theme_minimal(base_family = font) +
    ggplot2::theme(
      plot.title = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12, color = "#333"),
      panel.grid.major = ggplot2::element_line(color = "#E0E0E0"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#FAFAFA", color = NA)
    )
}
