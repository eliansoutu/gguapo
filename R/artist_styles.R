# artist_styles.R

#' @title Style for Leonardo da Vinci inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Leonardo da Vinci's works.
#'
#' @param data A data frame or an sf object containing the data to be visualized.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, or column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, or column plots).
#' @param color_var Optional. A tidy-evaluated expression specifying the column name used for color mapping. Defaults to NULL.
#' @param fill_var Optional. A tidy-evaluated expression specifying the column name used for fill mapping (used in column or map plots). Defaults to NULL.
#' @param label_var Optional. A tidy-evaluated expression specifying the column name used for text labels. Defaults to NULL.
#' @param title The plot title as a string.
#' @param subtitle The plot subtitle as a string.
#' @param caption The plot caption as a string.
#' @param plot_type The type of plot to create: one of `"scatter"`, `"line"`, `"column"`, or `"map"`.
#' @param work_inspired_by A specific Da Vinci work to guide aesthetic inspiration: `"mona_lisa"`, `"last_supper"`, or `"vitruvian_man"`.
#' @param show_labels Logical. If TRUE, data labels are displayed. Defaults to FALSE.
#' @param add_grid_lines Logical. If TRUE, major grid lines are shown (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical. If TRUE, shows the plot panel background. Defaults to TRUE.
#' @param add_glow Logical. If TRUE, adds a soft glow effect to map geometries. Defaults to FALSE.
#' @param coord_flip Logical. If TRUE, flips coordinates (e.g., for horizontal column charts with matching label orientation). Defaults to FALSE.
#' @param text_size Base size for text elements (titles, labels, legend). Labels and annotations scale proportionally from this value. Defaults to 12.
#'
#' @return A ggplot2 object styled with Da Vinci-inspired visual elements.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow
#' @importFrom rlang sym enquo as_label quo_is_null
#' @export
style_da_vinci <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Maestría de Da Vinci",
                           subtitle = "Un estudio de proporciones y armonía",
                           caption = "Análisis Renacentista",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("mona_lisa", "last_supper", "vitruvian_man"),
                           show_labels = FALSE, add_grid_lines = FALSE,
                           show_background = TRUE, add_glow = FALSE,
                           coord_flip = FALSE, text_size = 12) {
  style_artist_common(
    data = data,
    artist = "da_vinci",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = match.arg(plot_type),
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )
}

#' @title Style for Michelangelo inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Michelangelo's works, such as David, the Sistine Chapel, and the Pietà. Supports scatter, line, column, and map visualizations with customizable theming and effects.
#'
#' @param data A data frame (or sf object if `plot_type = "map"`).
#' @param x Tidy-evaluated expression specifying the variable for the x-axis.
#' @param y Tidy-evaluated expression specifying the variable for the y-axis.
#' @param color_var Optional tidy-evaluated expression for color mapping. Defaults to NULL.
#' @param fill_var Optional tidy-evaluated expression for fill mapping (used in column or map plots). Defaults to NULL.
#' @param label_var Optional tidy-evaluated expression for data labels (used in column or map plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type Type of plot to produce: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "david", "sistine_chapel", or "pieta".
#' @param show_labels Logical. Whether to display text labels on the plot. Defaults to FALSE.
#' @param add_grid_lines Logical. Whether to display major grid lines (non-map plots only). Defaults to FALSE.
#' @param show_background Logical. Whether to show the panel background. Defaults to TRUE.
#' @param add_glow Logical. Whether to apply an outer glow effect (for maps or specific geoms). Defaults to FALSE.
#' @param coord_flip Logical. If TRUE and plot_type is "column", flips axes and label positions. Defaults to FALSE.
#' @param text_size Numeric. Base size for titles, subtitles, labels, and annotations. Defaults to 12.
#'
#' @return A ggplot2 object styled with Michelangelo-inspired aesthetics.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang sym enquo as_label quo_is_null inject
#' @export
style_michelangelo <- function(data, x, y, color_var = NULL, fill_var = NULL, label_var = NULL,
                               title = "Magnificencia de Datos al Estilo de Miguel Ángel",
                               subtitle = "Una composición de fuerza y detalle",
                               caption = "Obra Maestra del Análisis",
                               plot_type = c("scatter", "line", "column","map"),
                               work_inspired_by = c("david", "sistine_chapel", "pieta"),
                               show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE,
                               show_background = TRUE,
                               coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "michelangelo",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = match.arg(plot_type),
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )

}

#' @title Style for Rembrandt inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Rembrandt's works, such as *The Night Watch*, *Self-Portrait*, and *The Storm on the Sea of Galilee*. Supports scatter, line, column, and map visualizations with rich contrast and depth, reminiscent of chiaroscuro.
#'
#' @param data A data frame (or an sf object if `plot_type = "map"`).
#' @param x Tidy-evaluated expression specifying the variable for the x-axis.
#' @param y Tidy-evaluated expression specifying the variable for the y-axis.
#' @param color_var Optional tidy-evaluated expression for color mapping. Defaults to NULL.
#' @param fill_var Optional tidy-evaluated expression for fill mapping (used in column or map plots). Defaults to NULL.
#' @param label_var Optional tidy-evaluated expression for data labels (used in column or map plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type Type of plot to produce: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "night_watch", "self_portrait", or "storm_sea".
#' @param show_labels Logical. Whether to display text labels on the plot. Defaults to FALSE.
#' @param add_glow Logical. Whether to apply an outer glow effect to geoms. Defaults to FALSE.
#' @param add_grid_lines Logical. Whether to show major grid lines (non-map plots only). Defaults to FALSE.
#' @param show_background Logical. Whether to show the panel background. Defaults to TRUE.
#' @param coord_flip Logical. If TRUE and `plot_type` is "column", flips axes and label positions. Defaults to FALSE.
#' @param text_size Numeric. Base size for titles, subtitles, labels, and annotations. Defaults to 12.
#'
#' @return A ggplot2 object styled with Rembrandt-inspired aesthetics.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang sym enquo as_label quo_is_null expr inject !!!
#' @export
style_rembrandt <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Iluminando Datos al Estilo de Rembrandt",
                            subtitle = "Profundidad y contraste en la visualización",
                            caption = "Maestría en la Sombra",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("night_watch", "self_portrait", "storm_sea"),
                            show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE,
                            show_background = TRUE, coord_flip = FALSE,
                            text_size = 12) {

  style_artist_common(
    data = data,
    artist = "rembrandt",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = match.arg(plot_type),
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_dark,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )


}

#' @title Style for Vincent van Gogh inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Vincent van Gogh's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "starry_night", "sunflowers", or "irises".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coords in column chart (and so labels). Defaults to FALSE.
#' @param text_size Numeric, base size for all text elements in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_density_2d geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang sym enquo as_label quo_is_null expr inject !!!
#' @export
style_van_gogh <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                           title = "Datos con la Pasión de Van Gogh",
                           subtitle = "Un remolino de información",
                           caption = "Impresiones de Datos",
                           plot_type = c("scatter", "line", "column", "map"),
                           work_inspired_by = c("starry_night", "sunflowers", "irises"),
                           show_labels = FALSE, add_glow = FALSE,
                           add_grid_lines = FALSE, show_background = TRUE,
                           coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "van_gogh",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_dark,
    grid_linetype = "solid",
    grid_linewidth = 0.4,
    axis_line_linewidth = 1,
    panel_background_map_specific = TRUE,
    text_size = text_size
  )

}


#' @title Style for Claude Monet inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Claude Monet's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels. Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "water_lilies", "impression_sunrise", or "poppy_fields".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_monet <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                        title = "La Impresión de los Datos al Estilo de Monet",
                        subtitle = "Armonía de luz y color",
                        caption = "Análisis Impresionista",
                        plot_type = c("scatter", "line", "column", "map"),
                        work_inspired_by = c("water_lilies", "impression_sunrise", "poppy_fields"),
                        show_labels = FALSE, add_glow = FALSE,
                        add_grid_lines = FALSE, show_background = TRUE,
                        coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "monet",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )
}


#' @title Style for Banksy inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Banksy's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "girl_with_balloon", "flower_thrower", or "rat_graffiti".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_banksy <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                         title = "Datos con la Calle de Banksy",
                         subtitle = "Un mensaje subversivo en cada punto",
                         caption = "Arte Urbano de Datos",
                         plot_type = c("scatter", "line", "column", "map"),
                         work_inspired_by = c("girl_with_balloon", "flower_thrower", "rat_graffiti"),
                         show_labels = FALSE, add_grid_lines = FALSE, show_background = TRUE,
                         coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "banksy",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )
}


#' @title Style for Salvador Dalí inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Salvador Dalí's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "persistence_memory", "elephants", or "swans_reflecting_elephants".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_jitter Logical, whether to add a jitter effect to scatter points (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_salvador_dali <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                title = "Datos Derretidos al Estilo de Dalí",
                                subtitle = "La realidad se dobla para mostrar la verdad",
                                caption = "Análisis Surrealista",
                                plot_type = c("scatter", "line", "column", "map"),
                                work_inspired_by = c("persistence_memory", "elephants", "swans_reflecting_elephants"),
                                show_labels = FALSE, add_glow = FALSE, add_jitter = FALSE, add_grid_lines = FALSE, show_background = TRUE,
                                coord_flip = FALSE, text_size = 12) {


   style_artist_common(
    data = data,
    artist = "dali",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_dark,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )

  #if (add_jitter) p <- p + ggplot2::geom_jitter(width = 0.1, height = 0.1, alpha = 0.4)

  #return(p)
}

#' @title Style for Joan Miró inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Joan Miró's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "the_farm", "constellations", or "blue_series".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_shapes Logical, whether to use varied shapes for scatter points (only for scatter plots). Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_miro <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                       title = "Datos con la Fantasía de Miró",
                       subtitle = "Un universo de formas y color",
                       caption = "Análisis Surrealista Abstracto",
                       plot_type = c("scatter", "line", "column", "map"),
                       work_inspired_by = c("the_farm", "constellations", "blue_series"),
                       show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE, show_background = TRUE,
                       coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "miro",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_void,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )

}

#' @title Style for Artemisia Gentileschi inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Artemisia Gentileschi's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "judith_beheading_holofernes", "mary_magdalene", or "self_portrait_lute_player".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_glow Logical, whether to add an outer glow effect. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background (for non-map plots). Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_artemisia_gentileschi <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                                        title = "Datos con la Fuerza de Artemisia Gentileschi",
                                        subtitle = "Un contraste dramático en la visualización",
                                        caption = "Barroco de Datos",
                                        plot_type = c("scatter", "line", "column", "map"),
                                        work_inspired_by = c("judith_beheading_holofernes", "mary_magdalene", "self_portrait_lute_player"),
                                        show_labels = FALSE, add_glow = FALSE, add_grid_lines = FALSE, show_background = TRUE,
                                        coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "artemisia",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_dark,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )

}


#' @title Style for Wassily Kandinsky inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Wassily Kandinsky's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "composition_viii", "yellow_red_blue", or "on_white_ii".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_shapes Logical, whether to use varied shapes for scatter points. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_kandinsky <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                            title = "Datos con la Abstracción de Kandinsky",
                            subtitle = "Una sinfonía visual de formas y colores",
                            caption = "Composición de Datos",
                            plot_type = c("scatter", "line", "column", "map"),
                            work_inspired_by = c("composition_viii", "yellow_red_blue", "on_white_ii"),
                            show_labels = FALSE, add_shapes = FALSE, add_grid_lines = FALSE,
                            show_background = TRUE, coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "kandinsky",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )

}


#' @title Style for Andy Warhol inspired plots
#' @description Generates ggplot2 plots with aesthetic elements inspired by Andy Warhol's works.
#' @param data A data frame or an sf object.
#' @param x A tidy-evaluated expression specifying the column name for the x-axis (for scatter, line, column plots).
#' @param y A tidy-evaluated expression specifying the column name for the y-axis (for scatter, line, column plots).
#' @param color_var An optional tidy-evaluated expression specifying the column name for color mapping. Defaults to NULL.
#' @param fill_var An optional tidy-evaluated expression specifying the column name for fill mapping (for column charts or map polygons). Defaults to NULL.
#' @param label_var An optional tidy-evaluated expression specifying the column name for text labels (for maps or other plots). Defaults to NULL.
#' @param title The plot title.
#' @param subtitle The plot subtitle.
#' @param caption The plot caption.
#' @param plot_type The type of plot: "scatter", "line", "column", or "map".
#' @param work_inspired_by Specific work for aesthetic inspiration: "soup_cans", "marilyn_monroe", or "cow_wallpaper".
#' @param show_labels Logical, whether to show data labels. Defaults to FALSE.
#' @param add_grid_lines Logical, whether to show major grid lines (only for non-map plots). Defaults to FALSE.
#' @param show_background Logical, whether to show the panel background. Defaults to TRUE.
#' @param coord_flip Logical, whether to flip coordinates in column charts. Defaults to FALSE.
#' @param text_size Numeric, base text size in the plot. Defaults to 12.
#' @return A ggplot2 object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_col theme_void theme element_text element_rect unit labs element_blank scale_color_manual scale_fill_manual scale_color_gradientn scale_fill_gradientn coord_flip geom_sf geom_sf_text geom_text
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggtext element_markdown
#' @importFrom tools toTitleCase
#' @importFrom grDevices colorRampPalette
#' @importFrom sf st_bbox
#' @importFrom ggfx with_shadow with_outer_glow
#' @importFrom rlang enquo quo_is_null as_label expr inject
#' @export
style_andy_warhol <- function(data, x = NULL, y = NULL, color_var = NULL, fill_var = NULL, label_var = NULL,
                              title = "Datos como Pop Art de Andy Warhol",
                              subtitle = "La repetición se convierte en arte",
                              caption = "Consumo de Datos",
                              plot_type = c("scatter", "line", "column", "map"),
                              work_inspired_by = c("soup_cans", "marilyn_monroe", "cow_wallpaper"),
                              show_labels = FALSE, add_grid_lines = FALSE,
                              show_background = TRUE, coord_flip = FALSE, text_size = 12) {

  style_artist_common(
    data = data,
    artist = "warhol",
    obra_inspiracion = match.arg(work_inspired_by),
    x = {{x}}, y = {{y}}, color_var = {{color_var}}, fill_var = {{fill_var}}, label_var = {{label_var}},
    title = title, subtitle = subtitle, caption = caption,
    plot_type = plot_type,
    show_labels = show_labels, add_grid_lines = add_grid_lines,
    show_background = show_background, add_glow = add_glow,
    coord_flip = coord_flip,
    theme_base = ggplot2::theme_minimal,
    grid_linetype = "dotted",
    grid_linewidth = 0.3,
    axis_line_linewidth = 0.8,
    panel_background_map_specific = FALSE,
    text_size = text_size
  )
}


# Helper function for `|||` operator (from rlang, but not exporting rlang just for this)
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

