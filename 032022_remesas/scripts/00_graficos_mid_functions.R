# ------------------------------------------------------------------------- #
#                Funciones graficas para el análisis del MID                #
# ------------------------------------------------------------------------- #
# Autor: Alex Bajaña
# Última actualiación: Marzo 2021


# Librerias ---------------------------------------------------------------


library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(rworldmap)
library(cowplot)
library(showtext)

# # Lectura de las tablas de resumen ----------------------------------------
#
# table_isd <- list.files("data/MID/",pattern = ".rds$",full.names =  T) %>%
#   set_names() %>%
#   map(~{
#     read_rds(.x) %>%
#       mutate(AÑO = str_extract(.x,"[:digit:]{4}"))})
#
# table_isd <- table_isd %>%
#   map(ungroup) %>%
#   map(mutate,CODIGO_PAIS = if_else(nchar(CODIGO_PAIS) == 2,
#                                    true = str_c("0",CODIGO_PAIS),
#                                    false = as.character(CODIGO_PAIS))) %>%
#   map(filter,!is.na(CODIGO_PAIS)) %>%
#   reduce(bind_rows)
#
# table_isd <- table_isd %>%
#   mutate(FORMA_PAGO_SUPER_BANCOS = as.character(FORMA_PAGO_SUPER_BANCOS),
#          MOTIVO_ECO_SUPER_BANCOS = as.character(MOTIVO_ECO_SUPER_BANCOS))
#
#
# # Merge con diccionarios --------------------------------------------------
#
# diccionario <- read_excel("data/MID/diccionario_mid.xlsx")
#
# diccionario <- diccionario %>%
#   fill(Variable,Etiqueta,DESCRIPCIÓN,.direction = "down")
# # %>%
# #   mutate(Grupo = if_else(Variable == "MOTIVO_ECO_SUPER_BANCOS",Grupo,NA_character_))
#
# diccionario_list <- diccionario %>% split(.$Variable)

join_tables <- function(init,x,y){

  # browser()

  etiqueta <- x %>%
    pull(Etiqueta) %>%
    unique()

  x <- x %>%
    rename_with(.cols = "CÓDIGO",.fn = ~ y) %>%
    select(one_of(y),DESCRIPCIÓN)

  join_table <- init %>%
    left_join(x,by = y ) %>%
    rename_with(.cols = "DESCRIPCIÓN",.fn = ~ etiqueta)



  return(join_table)
}


# table_isd <- reduce2(.x = diccionario_list,
#                      .y = names(diccionario_list),
#                      .init = table_isd,
#                      .f = join_tables) %>%
#   select(CODIGO_PAIS,PAIS_SUPER_BANCOS,MONTO_TRANSFERIDO:`CÓDIGOS DE PAÍSES`)
#
#
# # Datos del mundo ---------------------------------------------------------
#
# map_inter <- maps::iso3166 %>%
#   select(PAIS_SUPER_BANCOS = a2,ISOname) %>%
#   unique %>%
#   filter(!duplicated(PAIS_SUPER_BANCOS))
#
# table_isd <- table_isd %>%
#   inner_join(map_inter)
#
# world_map <- map_data("world") %>%
#   filter(region != "Antarctica")

# Función barras 100% -----------------------------------------------------

taxform_barras_100 <- function(tabla = tibble(),
                               x_var = character(),
                               y_var = character(),
                               fill_var = character(),
                               descripcion = character()){


plot <- tabla %>%
  ggplot(mapping = aes_string(x=x_var,
                              y = y_var,
                              fill= fill_var))+
  geom_bar(stat = "identity",
           position="fill")+
  labs(title = str_c( y_var, "by",x_var,sep = " "),
       subtitle = descripcion,
       x="",
       y = "Porcentaje",
       fill= "Categoría")+
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d()+
  theme_light()

return(plot)

}

# Ejemplo de uso
#
# taxform_barras_100(tabla = iris_2,
#                    x_var = "Species",
#                    y_var = "Sepal.Width",
#                    fill_var = "samp",
#                    descripcion= "Con una variable numerica se calcula el 100%")
#

# Funcion linea tiempo a nivel --------------------------------------------

taxform_lineas_tiempo <- function(tabla = tibble(),
                                  x_var = character(),
                                  y_var = character(),
                                  color_var = character(),
                                  descripcion = character(),
                                  y_desc = "Millions USD",
                                  y_lab = dollar){



  # browser()

  plot <- tabla %>%
    # filter(if_all(~!is.na(.x))) %>%
    # ungroup %>%
    ggplot()+
    geom_line(mapping = aes_string(x=x_var,
                                   y = y_var,
                                   color= color_var,
                                   group = color_var))+
    geom_vline(aes(xintercept = "2009",linetype = "dotted"))+
    # geom_text(aes(x = 2009 + 0.4 , y = 5.5,label = "1%"),size = 7)+
    geom_vline(aes(xintercept = "2010",linetype = "longdash"))+
    # geom_text(aes(x = 2010 + 0.4 , y = 6,label = "2%"),size = 7)+
    geom_vline(aes(xintercept = "2012",linetype = "solid"))+
    # geom_text(aes(x = 2012 + 0.4 , y = 6.5,label = "5%"),size = 7)+
    labs(title = str_c( y_var, "by",x_var,sep = " "),
         subtitle = descripcion,
         x="",
         y = y_desc,
         fill= "Categoría",linetype = "Tasa:")+
    scale_y_continuous(labels = y_lab,n.breaks = 10) +
    scale_linetype(labels = c("0.1%","1%", "5%")) +
    scale_color_viridis_d(end =  0.8)+
    theme_light()

  return(plot)

}

# Ejemplo de uso:

# taxform_lineas_tiempo(tabla = iris_2,
#                       x_var = "Species",
#                       y_var = "Sepal.Width",
#                       color_var = "samp",
#                       descripcion= "Linea de tiempo a nivel",
#                       y_lab = percent)



taxform_map <- function(tabla = tibble(),
                        group_var = character(),
                        join_var = character(),
                        resu_var = character(),
                        title_var = character(),
                        descripcion = character(),
                        fill_var = "Millions USD",
                        y_lab = dollar,
                        log = T){

  group_var <- sym(group_var)

  resu_var <- sym(resu_var)


  join_var <- sym(join_var)

  resumen <- tabla %>%
    group_by(!!group_var,!!join_var) %>%
    summarise(valor = sum(!!resu_var,na.rm = T))

  result <- list(res = resumen)

  if(log){

    resumen <- resumen %>%
      mutate(valor = log(valor))
    }

  merged_resumen <- full_join(resumen,
                               world_map,
                               by = c("region_ggplot" = "region"))


  merge_check <- full_join(resumen,
                           world_map %>% distinct(region) %>% rowid_to_column(),
                           by = c("region_ggplot" = "region"))

  breaks <- quantile(resumen$valor,probs = c(15,30,45,60,75,90)/100)
  #

  if(log){

    labels_2 <- y_lab(exp(breaks))
  }else{

    labels_2 <- y_lab(breaks)
  }
  #
  # browser()

  plot_mapa <- ggplot(merged_resumen,
         aes(long,
             lat,
             group = group))+
    geom_polygon(aes(fill = valor),color = "grey")+
    scale_fill_viridis_c(option = "C",
                         guide = guide_legend(nrow = 1, direction = 'horizontal',
                                              label.hjust = 0, label.position = 'bottom',
                                              keywidth = 4,
                                              keyheight = 0.31,
                                              title = ""),
                         breaks = as.numeric(breaks),
                         labels = labels_2) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    # coord_fixed(clip = "off") +
    # theme_minimal() +
    theme(legend.position = 'top',
          legend.key = element_blank(),
          # legend.text = element_text(size = 40),
          # legend.margin = margin(0,0,4,0,unit = "cm"),
          axis.text = element_blank(),
          axis.title =  element_blank(),
          plot.margin = unit(c(0,0,0,0),"cm")
          )

  # plot_mapa <- ggdraw(plot_mapa) +
  #   draw_text(text = title_var,
  #             fontface = "bold",hjust = 0,size = 60,
  #             x = 0.02,
  #             y = 0.96,
  #             color = "#0C007C") +
  #   draw_text(text = descripcion,
  #             hjust = 0,size = 40,
  #             x = 0.02,
  #             y = 0.93)

  result <- append(result,list(plot = plot_mapa,merge_check = merge_check))




  return(result)

  }



# aa <- taxform_map(tabla = table_isd,
#          group_var = "ISOname",
#          resu_var = "MONTO_TRANSFERIDO",
#          title_var = "Monto transferido",
#          descripcion = "Todos los años",
#          fill_var = "Millions USD",
#          y_lab = dollar)



tax_form_double_axis <- function(dataset,
                                 x_var = "fecha_mes",
                                 label_fun_1 = dollar,
                                 label_fun_2 = dollar,
                                 color_1,
                                 color_2,
                                 title_plot,
                                 subtitle_plot,
                                 panel,
                                 dat){


  axis <- dataset %>%
    ungroup() %>%
    select(-all_of(x_var))

  date_var <- sym(x_var)


  max_1 <-  max(dataset[[names(axis)[1]]],na.rm = T)

  max_2 <- max(dataset[[names(axis)[2]]],na.rm = T)

  ratio <- max_1/max_2

  ratio_2 <- 1/ratio

  maxims <- c(max_1,max_2)

  maxims_names <- c(names(axis)[1],
                    names(axis)[2])

  max_max <- which.max(maxims)

  axis_1 <- sym(maxims_names[max_max])

  axis_2 <- sym(maxims_names[-max_max])

  dataset <- dataset %>%
    select(all_of(c(x_var)),!!axis_1,!!axis_2)

  ratios <- c(ratio,ratio_2)

  max_ratio <- which.max(ratios)

  # browser()

  dataset %>%
    # mutate(across(.cols = c(maxims_names[max_max],
    #                         maxims_names[-max_max]),
    #               .fns = ~datawizard::winsorize(replace_na(.x,0),0.01))) %>%
    ggplot() +
    geom_line(mapping = aes(x = !!date_var,
                            y = !!axis_1,
                            color = maxims_names[max_max]),
              size = 1) +
    geom_line(mapping = aes(x = !!date_var,
                            y = !!axis_2*ratios[max_ratio],
                            color = maxims_names[-max_max]),
              size = 1) +
    geom_point(mapping = aes(x = !!date_var,
                             y = !!axis_1,
                             color = maxims_names[max_max]),
               size = 2) +
    geom_point(mapping = aes(x = !!date_var,
                             y = !!axis_2*ratios[max_ratio],
                             color = maxims_names[-max_max]),
               size = 2) +
    geom_vline(aes(xintercept = lubridate::ymd("20090101")),size = 1.5)+
    geom_text(aes(x = lubridate::ymd("20090101") + 90 ,
                  y = 0.8*maxims[max_max],label = "1%"),size = 7)+
    geom_vline(aes(xintercept = lubridate::ymd("20100101")) ,size = 1.5)+
    geom_text(aes(x = lubridate::ymd("20100101") + 90 ,
                  y = 0.85*maxims[max_max],label = "2%"),size = 7)+
    geom_vline(aes(xintercept = lubridate::ymd("20120101")),size = 1.5)+
    geom_text(aes(x = lubridate::ymd("20120101") + 90 ,
                  y = 0.9*maxims[max_max],label = "5%"),size = 7)+
    labs(title = title_plot,
         subtitle = subtitle_plot) +
    # scale_color_manual(values = c(color_1,color_2),
    #                    labels = c(maxims_names[max_max],
    #                               maxims_names[-max_max])) +
    scale_x_date(breaks = date_breaks(dat),
                 date_labels = "%Y")+
    scale_y_continuous(sec.axis = sec_axis(trans = ~.*ratios[-max_ratio],
                                           labels = label_fun_2,
                                           name = maxims_names[-max_max]),
                       name = maxims_names[max_max],
                       labels = label_fun_1) +
    theme_minimal()  +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = panel
            )

}



# Evolución porcentual, barras, porcentaje por años -----------------------
#
# tabla <- tablas$monto_lista_tipo_2
#
#
#
# evolucion <- tabla %>%
#   filter(tipo_factor == "Entrada de divisas") %>%
#   select(sum,year) %>%
#   mutate(diff = sum/lag(sum) - 1,
#          posx = if_else(diff<0,diff-0.15,diff+0.1),
#          label = percent(diff),
#          indicador = if_else(diff<0,"red","blue"),
#          ajuste = if_else(diff<0,1.1,-0.23),
#          shape = if_else(diff<0,"down","up"),
#          color = if_else(diff<0,"red","green"))

porcentual_change <- function(evolucion,title,subtitle,y_limits){

  plot_evolucion <- evolucion %>%
  ggplot(aes(x = year, y = diff,color=indicador,fill = indicador)) +
  geom_text(aes(label = label,hjust = ajuste),color = "black",size = 11
            ) +
  geom_point(aes(y = posx,shape = shape,fill = color),size = 11
             )+
  geom_col() +
  scale_x_date(breaks = "1 year",date_labels  = "%Y") +
  scale_y_continuous(limits = y_limits)+
  scale_shape_manual(values = c("up" = 2,"down" = 6))+
  scale_fill_manual(values = c("blue" = "blue","red" = "red","green" = "green"))+
  scale_color_manual(values = c("blue" = "blue","red" = "red","green" = "green"))+
  coord_flip(clip = "off") +
  theme_minimal()+
  theme(axis.title = element_blank(),
        # axis.text.y = element_text(size = 40),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white")
        # ,
        # plot.margin = unit(c(10.3,2,0,2),"cm")
          )


  # plot_evolucion <- ggdraw(plot_evolucion) +
  #   draw_text(text = title,
  #             fontface = "bold",hjust = 0,size = 60,
  #             x = 0.02,
  #             y = 0.97,
  #             color = "#0C007C") +
  #   draw_text(text = subtitle,
  #             hjust = 0,size = 40,
  #             x = 0.02,
  #             y = 0.94)

  return(plot_evolucion)
}


# parte_1 <-porcentual_change(evolucion)
#
#
# ggsave(filename = "remesas_analysis/graficos/poster_general/parte_1_ejemplo.png",
#        plot = parte_1,
#        width = 21.3,
#        height = 25.31,units = "in",
#        dpi = 150)



medias_con_intervalo <- function(tabla, mean, sd, n, tipo_factor, tiempo,title,subtitle) {

  media_sym <- sym(mean)
  sd_sym <- sym(sd)
  n_sym <- sym(n)
  tipo_sym <- sym(tipo_factor)
  tiempo_sym <- sym(tiempo)

  # browser()

  plot_medias <- tabla %>%
    mutate(superior = !!media_sym + ((1.96 * !!sd_sym)/sqrt(!!n_sym)),
           inferior = !!media_sym - ((1.96 * !!sd_sym)/sqrt(!!n_sym))) %>%
    ggplot() +
    geom_line(aes(x = !!tiempo_sym,y = !!media_sym,color = !!tipo_sym),size = 1) +
    geom_point(aes(x = !!tiempo_sym,y = !!media_sym,color = !!tipo_sym),size = 1) +
    geom_segment(mapping = aes(x = !!tiempo_sym,
                               xend = !!tiempo_sym,
                               y = inferior,
                               yend = superior),alpha = 0.7,size = 1.5,lineend = "butt",linejoin = "round") +
    facet_wrap(facets = tipo_factor,ncol = 3,scales = "free") +
    scale_y_continuous(labels = dollar) +
    scale_x_date(date_breaks = "2 year",date_labels = "%Y")+
    labs(y = "Dólares USD") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          # axis.text.y = element_text(size = 40),
          # axis.text.x = element_text(size = 40),
          legend.position = "none",
          plot.background = element_rect(fill = "white"),
          # plot.margin = unit(c(8.3,2,2,2),"cm"),
          # strip.text = element_text(size = 40)
    )

  # plot_medias <- ggdraw(plot_medias) +
  #   draw_text(text = title,
  #             fontface = "bold",hjust = 0,size = 60,
  #             x = 0.02,
  #             y = 0.95,
  #             color = "#0C007C") +
  #   draw_text(text = subtitle,
  #             hjust = 0,size = 40,
  #             x = 0.02,
  #             y = 0.90)

    return(plot_medias)

}

monto_quarter_evolucion_anual <- function(dataset,
                                          variable,
                                          variable_percent,
                                          tiempo,
                                          title,
                                          subtitle,
                                          indicador,
                                          variable_label){



  variable_sym <- sym(variable)
  variable_percent_sym <- sym(variable_percent)
  variable_label_sym <- sym(variable_label)
  tiempo_sym <- sym(tiempo)
  indicador <- sym(indicador)


  # browser()

  plot_barras <- dataset %>%
    mutate(label_2 = !!variable_label_sym,
           label_2 = dollar(label_2),
           label_1 = percent(!!variable_percent_sym,accuracy = 0.01),
           aux = !!tiempo_sym,
           across(c(label_1,
                    label_2),
                  ~if_else(aux %in% seq(ymd("2008-01-01"),ymd("2019-01-01"),"1 year"),.x,NA_character_))) %>%
    ggplot() +
    geom_col(aes(x = !!tiempo_sym,y = !!variable_sym),alpha = 0.6) +
    geom_text(aes(x = !!tiempo_sym,y = !!variable_sym,label = label_1,color = !!indicador),size = 6,vjust = -1) +
    geom_text(aes(x = !!tiempo_sym,y = !!variable_sym,label = label_2),size = 6,vjust = -3) +
    scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
    scale_y_continuous(labels = dollar)+
    scale_color_manual(values = c("blue" = "blue","red" = "red"))+
    theme_minimal() +
    theme(axis.title = element_blank(),
          # axis.text.y = element_text(size = 40),
          # axis.text.x = element_text(size = 40),
          legend.position = "none",
          plot.background = element_rect(fill = "white")
          # ,
          # plot.margin = unit(c(8.3,2,2,2),"cm"),
          # strip.text = element_text(size = 40)
    )

  # plot_barras <- ggdraw(plot_barras) +
  #   draw_text(text = title,
  #             fontface = "bold",hjust = 0,size = 60,
  #             x = 0.02,
  #             y = 0.95,
  #             color = "#0C007C") +
  #   draw_text(text = subtitle,
  #             hjust = 0,size = 40,
  #             x = 0.02,
  #             y = 0.90)

  return(plot_barras)


}


