
# Librerias ---------------------------------------------------------------

library(readxl)
library(tidyverse)
library(janitor)
library(fuzzyjoin)
library(patchwork)
library(scales)
library(ggalt)

# Lectura del archivo -----------------------------------------------------

excel_sheets("032022_remesas/data/raw/RemesasPub.xlsm")

remesas_anuales_prov <- read_excel("032022_remesas/RemesasPub.xlsm",
                                   sheet = "ConsultaProvincia",
                                   skip = 8)

remesas_anuales_pais <- read_excel("032022_remesas/RemesasPub.xlsm",
                                   sheet = "ConsultaPaisOrigen",
                                   skip = 6)

remesas_anuales_area <- read_excel("032022_remesas/RemesasPub.xlsm",
                                   sheet = "ConsultaSectorMonto" ,
                                   skip = 8)



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
    mutate(evolucion = (remesas/lag(remesas))- 1,
           evolucion_fact = cut(evolucion,c(-Inf,-1.5,-1,-0.5,0,0.5,1,1.5,Inf)),
           evolucion_fact = fct_relabel(evolucion_fact,.fun = ~c("< -150%",
                                                                 "-150%",
                                                                 "-100%",
                                                                 "-50%",
                                                                 "50%",
                                                                 "100%",
                                                                 "150%",
                                                                 "> 150%")))
}

paletas_color <- function(paleta = "default",
                          ncolor = 5){
  
  if(paleta  == "default"){
    
    palette_bar <- c("#655D8A",
                     "#7897AB",
                     "#D885A3",
                     "#FDCEB9")
  }else if(paleta  == "greens"){
    
    palette_bar <- c("#557B83",
                     "#39AEA9",
                     "#A2D5AB",
                     "#E5EFC1")
  }else if(paleta  == "blues"){
    
    
    palette_bar <- c("#1C658C",
                     "#398AB9",
                     "#D8D2CB",
                     "#EEEEEE")
  }else if(paleta  == "contrast"){
    
    palette_bar <- c("#533E85",
                     "#488FB1",
                     "#4FD3C4",
                     "#C1F8CF")
    
  }else if(paleta == "ergos"){
    
    palette_bar <- c("#03071e",
                     "#9d0208",
                     "#f48c06")
  }
  
  paleta <- unikn::newpal(col = palette_bar)
  
  
  paleta <- unikn::usecol(pal = paleta,n = ncolor)
  
  
  return(paleta)
}
# Limpieza de la tabla: ---------------------------------------------------

clean_remesas <- remesas_anuales_prov %>%  clean_bce(variable_grupo = "provincia")

clean_remesas_pais <- remesas_anuales_pais %>%  clean_bce(variable_grupo = "pais")

clean_remesas_area <- remesas_anuales_area %>%  clean_bce(variable_grupo = "sector")

# Guardamos el archivo temporal:

write_rds(clean_remesas,"032022_remesas/data/clean/remesas_prov_anual_25032022.rds")


# Lectura de shapefiles ---------------------------------------------------

centroides_provincia <-read_rds("general/mapas/dpa/centroides_provincias.rds")

mapa_provincia <-read_rds("general/mapas/dpa/shapefile_provincias.rds")


# Remesas tras el primer año de la pandemia -------------------------------

remesas_2020 <- clean_remesas %>% 
  filter(periodo >= 2019)


# Coordenadas provincias --------------------------------------------------

remesas_2020 <- regex_left_join(x = remesas_2020,
                                y = centroides_provincia,
                                by = c(provincia = "dpa_despro")) 



mapa_remesas_20 <- remesas_2020 %>% 
  right_join(mapa_provincia)


# Paleta de colores: ------------------------------------------------------

intervalos <- clean_remesas %>% pull(evolucion_fact) %>% levels

paleta <- paletas_color(paleta = "default",ncolor = length(intervalos))

paleta <- set_names(x = paleta,nm = intervalos)

plots_map <- 
  mapa_remesas_20 %>% 
  filter(!is.na(periodo)) %>% 
  mutate(periodo = str_c("Tasa de cambio de las remesas",periodo,"respecto al año",periodo - 1,sep = " ")) %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group,
             fill = evolucion_fact)) +
  geom_polygon(
    color = "grey",
    size = 0.2,
    # alpha = 0.9
  ) +
  coord_fixed()+
  facet_wrap(.~periodo) +
  scale_fill_manual(values = paleta,
                    guide = guide_legend(nrow = 1, direction = 'horizontal',
                                         label.hjust = 0.5, 
                                         label.position = 'bottom',
                                         keywidth = 5,
                                         keyheight = 0.5,
                                         title = ""))  +
  labs(title = "Recepción de remesas en Ecuador antes y durante la crisis sanitaria",
       caption = "Fuente: Banco Central del Ecuador | Elaboración: Alex Bajaña") + 
  theme_minimal(base_size = 14) +
  theme(title = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(hjust = 0,color = "#03071e",size = 16),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0,-1,0,-1,"cm"),
        legend.spacing.x = unit(-0.01,"cm"),
        # panel.grid.major = element_blank(),
        # panel.border = element_blank(),
        plot.title = element_text(size = 18,face = "bold"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
  ) 


ggsave(plot = plots_map,filename = "032022_remesas/output/mapa_remesas_19_20.png",
       width = 1203,height = 617,units = "px",dpi = 90)


plots_map
# Series de tiempo --------------------------------------------------------

plots_area <- clean_remesas_area %>% 
  group_by(sector) %>% 
  mutate(remesas_scaled = scale(remesas),
         sector = str_to_sentence(sector)) %>% 
  filter(str_detect(sector,"Total",negate = T)) %>% 
  ggplot(mapping = aes(x = periodo,y = evolucion,color = sector))+
  geom_point() + 
  geom_xspline() + 
geom_hline(mapping = aes(yintercept = 0))+ 
  scale_color_manual(values = c(Rural = "#03071e",
                                Urbano = "#9d0208"))+
  scale_y_continuous(labels = percent) +
  labs(title = "Evolución de las remesas por area de residencia:")+
  theme_minimal(base_size = 30) +
  theme(title = element_text(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.margin = margin(0,2,0,2,"cm"),
        legend.spacing.x = unit(0.5,"cm"),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        # plot.title = element_text(size = 20),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)
  ) 
  

ggsave(plot = plots_area,filename = "032022_remesas/output/series_remesas.png",
       width = 1203,height = 500,units = "px",dpi = 90)
  



