
devtools::load_all()
library(tidyverse)

data <- comunicacion::toy_evyth

locs <- data %>%
  summarise(turistas = sum(pondera), .by = localidad_destino) %>%
  slice_max(turistas, n = 15) %>%
  mutate(porcentaje = turistas/sum(turistas))


anio <- data %>%
  summarise(turistas = sum(pondera), .by = anio)

data_geo <- geoAr::get_geo("SANTA FE") %>% geoAr::add_geo_codes() %>%
  mutate(cantidad = row_number()*100)


load_all_artist_fonts()

locs %>%
  #rowwise() %>%
  #mutate(categoria = sample(c("Hola","Chau"), 1)) %>%
  style_michelangelo(plot_type = "column", #fill_var = "turistas",
               x = "localidad_destino",
               y = "turistas",
               show_background = T,
               fill_var = "localidad_destino",
               work_inspired_by = "pieta")
locs %>%
  plot_guapo("localidad_destino", "turistas",
             plot_type = "column", palette_name = "neoflash", dark_mode = F,
             highlight_values = "Pilar")

anio %>%
  plot_guapo(plot_type = "line", "anio", "turistas",dark_mode = F, highlight_values = 2021,
               title = "Este es un graph copado")

data_geo %>%
  plot_guapo(plot_type = "map", dark_mode = T, fill_var = "cantidad",show_labels =T,
             label_var = "coddepto")

