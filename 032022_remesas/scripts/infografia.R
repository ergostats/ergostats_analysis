
# Librerias ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)
library(fuzzyjoin)
library(patchwork)
library(scales)

# Lectura del archivo -----------------------------------------------------

excel_sheets("032022_remesas/data/raw/RemesasPub.xlsm")

remesas_anuales_prov <- read_excel("032022_remesas/RemesasPub.xlsm",
                                   sheet = "ConsultaProvincia",
                                   skip = 8)

remesas_anuales_pais <- read_excel("032022_remesas/RemesasPub.xlsm",
                                   sheet = "ConsultaPaisOrigen",
                                   skip = 6)

# Corte de la tabla: 19 de marzo de 2022

# Auxiliares --------------------------------------------------------------

clean_bce <- function(tabla,
                      variable_grupo){
  
  var <- sym(variable_grupo)
  tabla %>% 
    clean_names() %>% 
    pivot_longer(cols = x2007:x2020,names_to = "periodo",values_to = "remesas") %>% 
    mutate(periodo = str_remove(periodo,"[:alpha:]"),
           periodo = as.numeric(periodo)) %>% 
    group_by(!!var) %>% 
    arrange(periodo) %>% 
    mutate(evolucion = (remesas/lag(remesas))- 1)
}

# Limpieza de la tabla: ---------------------------------------------------

clean_remesas <- remesas_anuales_prov %>%  clean_bce(variable_grupo = "provincia")

clean_remesas_pais <- remesas_anuales_pais %>%  clean_bce(variable_grupo = "pais")

# Guardamos el archivo temporal:

write_rds(clean_remesas,"032022_remesas/data/clean/remesas_prov_anual_25032022.rds")


# Remesas tras el primer año de la pandemia -------------------------------

remesas_2020 <- clean_remesas %>% 
  filter(periodo >= 2019)


rm(clean_remesas)


# Coordenadas provincias --------------------------------------------------

mapa_ecuador <- read_rds("general/mapas/shapefiles_simplificados_dpa.rds")

mapa_provincia <- mapa_ecuador[[1]]

mapa_provincia <- mapa_provincia %>% 
  clean_names() %>% 
  tibble()

centroides_ecuador <- read_rds("general/mapas/centroides_simp.rds")

centroides_provincia <- centroides_ecuador$provincias

centroides_provincia <- centroides_provincia %>% 
  mutate(DPA_DESPRO = replace_na(DPA_DESPRO,"CAÑAR")) %>% 
  clean_names()

remesas_2020 <- fuzzyjoin::regex_left_join(x = remesas_2020,
                                           y = centroides_provincia,
                                           by = c(provincia = "dpa_despro")) 



mapa_remesas_20 <- remesas_2020 %>% 
  right_join(mapa_provincia)



plots_map <- 
  mapa_remesas_20 %>% 
  filter(!is.na(periodo)) %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = evolucion)) +
  geom_polygon(
    color = "grey",
    size = 0.2,
    alpha = 0.9
  ) +
  coord_fixed()+
  facet_wrap(.~periodo) +
  scale_fill_gradient(low = "#03071e",
                      high =  "#9d0208",labels = percent)  +
  theme_light(base_size = 12) +
  theme(title = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank()
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA))



# Series de tiempo --------------------------------------------------------




plots_map[[1]] | plots_map[[2]] + plot_layout(guides = "collect") & theme(legend.position = "bottom")
