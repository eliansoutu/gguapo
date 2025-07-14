

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
               x = "localidad_destino", show_background = T,
               fill_var = "localidad_destino",
               work_inspired_by = "pieta",
               y = "turistas")



anio %>%
  style_banksy(plot_type = "line", "anio", "turistas",title = "Este es un graph copado", show_labels = T)

anio %>%
  plot_robotico(tipo = "linea", "anio", "turistas")

data_geo %>%
  mapa_guapo(fill = "cantidad")


