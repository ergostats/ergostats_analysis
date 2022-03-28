
# ------------------------------------------------------------------------- #
#     RECURSOS ERGOSTATS: Mapas con división politico - administrativa      #
#           Versiones limpias, simplificadas y listas para el uso           #
# ------------------------------------------------------------------------- #
# Fuente: Inec
# Año del Shapefile original: 2014


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(janitor)

# Mapas de provincias -----------------------------------------------------

mapa_ecuador <- read_rds("general/mapas/shapefiles_simplificados_dpa.rds")

mapa_provincia <- mapa_ecuador[[1]]

mapa_provincia <- mapa_provincia %>% 
  clean_names() %>% 
  tibble()

write_rds(x = mapa_provincia,
          "general/mapas/dpa/shapefile_provincias.rds")


# Centroides de la provincia ----------------------------------------------

centroides_ecuador <- read_rds("general/mapas/centroides_simp.rds")

centroides_provincia <- centroides_ecuador$provincias

centroides_provincia <- centroides_provincia %>% 
  mutate(DPA_DESPRO = replace_na(DPA_DESPRO,"CAÑAR")) %>% 
  clean_names()

write_rds(x = centroides_provincia,
          "general/mapas/dpa/centroides_provincias.rds")
