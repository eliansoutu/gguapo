#' @title Previsualizar Estilos de Gráficos
#' @description Genera y muestra ejemplos de gráficos utilizando cada uno de los estilos definidos. Esto permite al usuario previsualizar y elegir el estilo que mejor se adapta a sus necesidades.
#' @param style_name Un string opcional para previsualizar un estilo específico. Si es "all" (por defecto), se previsualizan todos los estilos disponibles. Otras opciones válidas: "da_vinci", "michelangelo", etc.
#' @param display_plots Logical. Si es TRUE (por defecto), los gráficos se imprimen en el dispositivo actual. Si es FALSE, se retorna una lista de objetos ggplot.
#' @return Una lista de objetos ggplot si `display_plots` es FALSE. Si `display_plots` es TRUE, los gráficos se imprimen y la función retorna `NULL` de forma invisible.
#' @export
preview_styles <- function(style_name = "all", display_plots = TRUE) {
  plots <- list()

  # --- Datos de ejemplo ---
  data_default <- data.frame(
    category = LETTERS[1:5],
    value = c(25, 45, 60, 30, 75),
    time = 1:5,
    group = c("A", "B", "A", "B", "A")
  )

  # --- Estilos disponibles ---
  valid_styles <- c("da_vinci", "michelangelo")  # Agregá aquí más artistas

  if (!style_name %in% c("all", valid_styles)) {
    stop(glue::glue("Estilo '{style_name}' no reconocido. Opciones válidas: {paste(c('all', valid_styles), collapse = ', ')}"))
  }

  load_all_fonts(artist = style_name)

  # === Leonardo da Vinci ===
  if (style_name %in% c("all", "da_vinci")) {
    if (exists("style_da_vinci") && is.function(style_da_vinci)) {
      plots[["Leonardo - Column - mona_lisa"]] <- style_da_vinci(
        data_default, x = category, y = value, plot_type = "column",
        title = "Leonardo: mona_lisa (column)", subtitle = "Magnitud de categorías"
      )
      plots[["Leonardo - Line - last_supper"]] <- style_da_vinci(
        data_default, x = time, y = value, plot_type = "line",
        work_inspired_by = "last_supper",
        title = "Leonardo: last_supper (line)", subtitle = "Evolución en el tiempo"
      )
      plots[["Leonardo - Point - vitruvian_man"]] <- style_da_vinci(
        data_default, x = time, y = value, plot_type = "scatter",
        work_inspired_by = "vitruvian_man",
        title = "Leonardo: vitruvian_man (scatter)", subtitle = "Distribución"
      )
    } else {
      warning("La función 'style_da_vinci' no fue encontrada. Se omite Leonardo.")
    }
  }

  # === Michelangelo ===
  if (style_name %in% c("all", "michelangelo")) {
    if (exists("style_michelangelo") && is.function(style_michelangelo)) {
      plots[["Michelangelo - Column - david"]] <- style_michelangelo(
        data_default, x = category, y = value, plot_type = "column",
        title = "Miguel Ángel: david (column)", subtitle = "Magnitud de categorías"
      )
      plots[["Michelangelo - Line - sistine_chapel"]] <- style_michelangelo(
        data_default, x = time, y = value, plot_type = "line",
        work_inspired_by = "sistine_chapel",
        title = "Miguel Ángel: sistine_chapel (line)", subtitle = "Evolución en el tiempo"
      )
      plots[["Michelangelo - Point - pieta"]] <- style_michelangelo(
        data_default, x = time, y = value, plot_type = "scatter",
        work_inspired_by = "pieta",
        title = "Miguel Ángel: pieta (scatter)", subtitle = "Distribución"
      )
    } else {
      warning("La función 'style_michelangelo' no fue encontrada. Se omite Michelangelo.")
    }
  }

  # Mostrar o retornar
  if (display_plots) {
    for (i in seq_along(plots)) {
      #cat(paste0("\n--- Previsualizando: ", names(plots)[i], " ---\n"))
      print(plots[[i]])
    }
    invisible(NULL)
  } else {
    return(plots)
  }
}

