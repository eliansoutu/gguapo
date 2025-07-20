#' Previsualiza los estilos gráficos disponibles inspirados en artistas
#'
#' Esta función genera gráficos de ejemplo utilizando los estilos definidos para distintos artistas.
#' Para cada artista se grafican tres obras representativas (columna, línea, dispersión).
#'
#' @param style_name Nombre del estilo a previsualizar. Puede ser un artista específico (por ejemplo, `"da_vinci"`)
#'   o `"all"` para mostrar todos los estilos disponibles.
#' @param display_plots Lógico. Si `TRUE`, se muestran los gráficos en pantalla. Si `FALSE`, se devuelve una lista
#'   de objetos `ggplot`.
#'
#' @return Invisiblemente `NULL` si `display_plots = TRUE`, o una lista de objetos `ggplot` si `FALSE`.
#' @export
#'
#' @import ggplot2
#' @importFrom glue glue
#'
#' @examples
#' # Mostrar todos los estilos
#' preview_styles("all")
#'
#' # Solo el estilo de Monet
#' preview_styles("monet")
#'
#' # Obtener los gráficos sin mostrarlos
#' plots <- preview_styles("warhol", display_plots = FALSE)
preview_styles <- function(style_name = "all", display_plots = TRUE) {
  plots <- list()

  # --- Datos de ejemplo ---
  data_default <- data.frame(
    category = LETTERS[1:5],
    value = c(25, 45, 60, 30, 75),
    time = 1:5,
    group = c("A", "B", "A", "B", "A")
  )

  # --- Estilos y obras disponibles ---
  works <- list(
    "da_vinci" = c("mona_lisa","last_supper","vitruvian_man"),
    "michelangelo" = c("david","sistine_chapel","pieta"),
    "rembrandt" = c("night_watch","self_portrait","storm_sea"),
    "van_gogh" = c("starry_night","sunflowers","irises"),
    "monet" = c("water_lilies","impression_sunrise","poppy_fields"),
    "miro" = c("the_farm","constellations","blue_series"),
    "gentileschi" = c("judith_beheading_holofernes","mary_magdalene","self_portrait_lute_player"),
    "dali" = c("persistence_memory","elephants","swans_reflecting_elephants"),
    "warhol" = c("soup_cans","marilyn_monroe","cow_wallpaper"),
    "banksy" = c("girl_with_balloon","flower_thrower","rat_graffiti"),
    "kandinsky" = c("composition_viii","yellow_red_blue","on_white_ii")
  )
  valid_styles <- names(works)

  # --- Validación ---
  if (!style_name %in% c("all", valid_styles)) {
    stop(glue::glue("Estilo '{style_name}' no reconocido. Opciones válidas: {paste(c('all', valid_styles), collapse = ', ')}"))
  }

  # --- Cargar fuentes necesarias ---
  load_all_fonts(artist = style_name)

  # --- Artistas a mostrar ---
  estilos_a_mostrar <- if (style_name == "all") valid_styles else style_name

  for (artist in estilos_a_mostrar) {
    style_fun_name <- paste0("style_", artist)
    if (!exists(style_fun_name)) {
      warning(glue::glue("La función '{style_fun_name}' no fue encontrada. Se omite '{artist}'."))
      next
    }

    style_fun <- get(style_fun_name)
    obras <- works[[artist]]
    tipos_plot <- c("column", "line", "scatter")

    for (i in seq_along(obras)) {
      tipo <- tipos_plot[(i - 1) %% length(tipos_plot) + 1]
      obra <- obras[i]
      title_base <- stringr::str_to_title(gsub("_", " ", artist))
      plot_name <- glue::glue("{title_base} - {stringr::str_to_title(tipo)} - {obra}")

      if (tipo == "column") {
        plots[[plot_name]] <- style_fun(
          data_default,
          x =  category,
          y = value,
          plot_type = tipo,
          work_inspired_by = obra,
          title = glue::glue("{title_base}: {obra} ({tipo})")
        )
      } else {

        plots[[plot_name]] <- style_fun(
          data_default,
          x = time,
          y = value,
          plot_type = tipo,
          work_inspired_by = obra,
          title = glue::glue("{title_base}: {obra} ({tipo})")
        )
      }
    }
  }

  # --- Mostrar o retornar ---
  if (display_plots) {
    for (p in plots) print(p)
    invisible(NULL)
  } else {
    return(plots)
  }
}
