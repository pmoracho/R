library("tidyverse")
library("ggrepel")
library("countrycode")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
  base_familiy = "Raleway"
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
  base_familiy = ""
}

#' get_prepared_data
#' Prepara la información final para los reportes
#'
#' @param covid.data 
#' @param paises 
#'
#' @return
#' @export
#'
#' @examples
#'   datos <- get_prepared_data(covid.data, paises)
#'   
get_prepared_data <-  function(covid.data) {
  
  require("dplyr")
  require("magrittr")
  require("countrycode")

  rango_fechas <- range(unique(as.Date(covid.data$dateRep,"%d/%m/%Y")))
  todas_las_fecha <- seq(from=rango_fechas[1], to=rango_fechas[2], by=1)
  todos_los_paises <- unique(covid.data$countryterritoryCode)
  todos_los_paises <- todos_los_paises[!is.na(todos_los_paises)]

  expand_grid(countryterritoryCode=todos_los_paises, 
              fecha=todas_las_fecha) %>% 
    left_join(covid.data %>% 
                mutate(fecha = as.Date(dateRep, "%d/%m/%Y")), 
              by=c("countryterritoryCode", "fecha")) %>%  
    inner_join(codelist, by = c("countryterritoryCode" = "iso3c")) %>% 
    mutate(pais = ifelse(is.na(un.name.es), iso.name.en, un.name.es),
           continente = continent) %>% 
    group_by(pais, continente) %>% 
    mutate(casos = cumsum(replace_na(cases, 0)),
           fallecidos = cumsum(replace_na(deaths, 0))) %>% 
    select(pais, continente, fecha, casos, fallecidos)  %>% 
    filter(casos > 0) %>%  
    mutate(dia=row_number()) %>% 
    ungroup() %>% 
    select(pais, continente, dia, fecha, fallecidos, casos) -> datos

  datos %>% 
    mutate(la_mitad = casos / 2) %>% 
    left_join(datos,  by = "pais", suffix = c("", ".y")) %>% 
    group_by(pais, continente, dia, fecha, fallecidos, casos) %>% 
    summarize(dia_2 = max(ifelse(casos.y <= casos / 2,  dia.y, 0))) %>% 
    mutate(dia_2 = dia - dia_2) %>% 
    ungroup() -> datos


  dia_en_arg <- max(datos$dia[datos$pais == 'Argentina'])
  
  datos %>% 
    group_by(pais) %>% 
    arrange(fecha) %>% 
    slice(n()) %>% 
    ungroup()  %>% 
    left_join(datos %>% 
                filter(dia <= dia_en_arg) %>% 
                group_by(pais) %>% 
                slice(n()),
              by = c('pais')) %>% 
    
    mutate(fallecidos_en_dia = ifelse(dia.x <= dia_en_arg, NA, fallecidos.y),
           casos_en_dia = ifelse(dia.x <= dia_en_arg, NA, casos.y),
           dia.arg = dia_en_arg) %>% 
    select(pais,
           dia = dia.x,
           dia.arg,
           fallecidos = fallecidos.x,
           fallecidos_en_dia,
           casos = casos.x,
           casos_en_dia,
           dia_2.x
    ) -> ultimo_reporte
  
    list(datos_totales = datos, ultimo_reporte = ultimo_reporte)
}


plot_curva_casos_y_fallecidos <- function(data, paises_de_interes, hasta_dia=NA) {
  
  cfg <- list(alpha = .5,
              point.size = 4,
              breaks.x = 10,
              breaks.y = 10,
              label.size = 4,
              segment.color = "#666666",
              segment.size = .2,
              family = base_familiy )
  
  
  paises_de_interes <- c('Argentina', paises_de_interes)
  cantidad_paises <- length(paises_de_interes)
  data[[1]] %>% 
    filter(pais %in% paises_de_interes) -> datos
            
  data[[2]] %>%
    filter(pais %in% paises_de_interes) -> ultimo_reporte

  ultima_fecha <- max(datos$fecha[datos$pais=='Argentina'])
  files <- c(paste0(ultima_fecha, '-fallecidos.png'),
             paste0(ultima_fecha, '-casos.png'))
  
  dimensiones <- c(12,7)
  
  # Para ajustar el gráfico a un área en paticular
  zoom <- list(x=NULL, y=NULL)
  zoom <- list(x=c(0, max(datos$dia) + 5), y=NULL)
  

  
  ###############################################################
  # FALLECIDOS
  ###############################################################
  datos %>% 
    ggplot() +
    geom_line(mapping=aes(x=dia, y=fallecidos, color=pais, size=pais, alpha=pais)) +
    geom_point(data=ultimo_reporte, mapping=aes(x=dia, y=fallecidos, color=pais), size = cfg$point.size) -> p
  
    if (!all(is.na(ultimo_reporte$fallecidos_en_dia))) {
      p <- p + 
        geom_point(data=ultimo_reporte, mapping=aes(x=dia.arg, y=fallecidos_en_dia, color=pais), size = cfg$point.size) +
        geom_label_repel(data = ultimo_reporte,
                       mapping = aes(x= dia.arg, y = fallecidos_en_dia, label = paste(pais, "día", dia.arg, ":", format(fallecidos_en_dia, big.mark = ',')), color=pais, fill=pais),
                       nudge_x = 1, 
                       nudge_y = 5,
                       segment.color = cfg$segment.color,
                       segment.size = cfg$segment.size,
                       size = cfg$label.size,
                       vjust = -2,
                       family = base_familiy, fontface = 'bold', color = 'white',
                       direction  = "y",
                       hjust = 2
      ) 
    }
  
    p <- p +
    scale_x_discrete(breaks = scales::pretty_breaks(n = cfg$breaks.x), limits=zoom$x) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = cfg$breaks.y), limits=zoom$y) +
    geom_label_repel(data = ultimo_reporte,
                     mapping = aes(x= dia, y = fallecidos, label = paste(pais, "día", dia, ":", format(fallecidos, big.mark = ',')), color=pais, fill=pais),
                     nudge_x = -1,
                     box.padding = unit(0.25, "lines"),
                     point.padding = unit(0.5, "lines"),
                     segment.color=cfg$segment.color,
                     segment.size = cfg$segment.size,
                     nudge_y = -5,
                     size = cfg$label.size,
                     vjust = 2,
                     family = base_familiy, fontface = 'bold', color = 'white',
                     direction = "y",
                     hjust = -2) +
    # Etiqueta de "Los primeros n días"
    geom_label_repel(data = ultimo_reporte %>% mutate(max_casos=max(ultimo_reporte$fallecidos)) %>% filter(pais=="Argentina"),
                     mapping = aes(x= dia.arg, 
                                   y = max_casos, 
                                   label = paste( "los primeros", dia.arg, "dias"), 
                                   color=factor("Argentina"), 
                                   fill=factor("Argentina")
                                   ),
                     arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "last"),
                     segment.color=cfg$segment.color,
                     segment.size = cfg$segment.size,
                     family = base_familiy, fontface = 'bold', color = 'white',
                     direction  = "y",
                     vjust = -2,
                     hjust = 2,
                     nudge_x = 1,
                     nudge_y = 5
    ) +      
 
    
    geom_vline(data=ultimo_reporte,
               linetype="dotted", 
               size=1.5,
               mapping=aes(xintercept=max(dia.arg), color="Argentina", alpha="Argentina")) +
    
    labs(title = paste("Fallecidos - COVID-19"), 
         subtitle = paste("Acumulados al", ultima_fecha) , 
         caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
         y = "Fallecidos", 
         x = "Número de día",
         color = NULL,
         alpha = NULL,
         size = NULL,
         fill = NULL
        
    ) +
    scale_size_manual(values=rep(1,cantidad_paises)) +
    scale_alpha_manual(values=c(1, rep(cfg$alpha, cantidad_paises - 1))) +
    theme_elegante_std(base_family = base_familiy) +
    theme(legend.position = "none")
  
    ggsave(filename = files[1], plot = p, device = "png", width = dimensiones[1], height = dimensiones[2])

  ###############################################################
  # CASOS
  ###############################################################  
  datos %>% 
    ggplot() +
    geom_line(mapping=aes(x=dia, y=casos, color=pais, size=pais, alpha=pais)) +
    geom_point(data=ultimo_reporte, mapping=aes(x=dia, y=casos, color=pais), size = cfg$point.size) -> p
    
    if (!all(is.na(ultimo_reporte$casos_en_dia))) {
      p <- p + geom_point(data=ultimo_reporte, mapping=aes(x=dia.arg, y=casos_en_dia, color=pais), size = cfg$point.size) +
        geom_label_repel(data = ultimo_reporte,
                         mapping = aes(x= dia.arg, y = casos_en_dia, label = paste(pais, "día", dia.arg, ":", format(casos_en_dia, big.mark = ',')), color=pais, fill=pais),
                         nudge_x = 1, 
                         nudge_y = 5,
                         segment.color=cfg$segment.color,
                         segment.size = cfg$segment.size,
                         size = cfg$label.size,
                         vjust = -2,
                         family = base_familiy, fontface = 'bold', color = 'white',
                         direction  = "y",
                         hjust = 2
        )   
    }
    
    p <- p +
    scale_x_discrete(breaks = scales::pretty_breaks(n = cfg$breaks.x), limits=zoom$x) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = cfg$breaks.y), limits=zoom$y) +
    geom_label_repel(data = ultimo_reporte,
                     mapping = aes(x= dia, y = casos, label = paste(pais, "día", dia,  ":", format(casos, big.mark = ',')), color=pais, fill=pais),
                     nudge_x = -1, 
                     segment.color=cfg$segment.color,
                     segment.size = cfg$segment.size,
                     nudge_y = -5,
                     size = cfg$label.size,
                     vjust = 2,
                     family = base_familiy, fontface = 'bold', color = 'white',
                     direction = "y",
                     hjust = -2) +
        
      geom_label_repel(data = ultimo_reporte %>% mutate(max_casos=max(ultimo_reporte$casos)) %>% filter(pais=="Argentina"),
                       mapping = aes(x= dia.arg, 
                                     y = max_casos, 
                                     label = paste( "los primeros", dia.arg, "dias"), 
                                     color=factor("Argentina"), 
                                     fill=factor("Argentina")
                                     ),
                       arrow = arrow(length = unit(0.02, "npc"), type = "closed", ends = "last"),
                       segment.color=cfg$segment.color,
                       segment.size = cfg$segment.size,
                       family = base_familiy, fontface = 'bold', color = 'white',
                       direction  = "y",
                       vjust = -2,
                       hjust = 2,
                       nudge_x = 1,
                       nudge_y = 5
      ) +      
 
    geom_vline(data=ultimo_reporte,
               linetype="dotted", 
               size=1.5,
               mapping=aes(xintercept=max(dia.arg), color="Argentina", alpha="Argentina")) +
    
    labs(title = paste("Infectados - COVID-19"), 
         subtitle = paste("Acumulados al", ultima_fecha) , 
         caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
         y = "Casos reportados", 
         x = "Número de día",
         color = NULL,
         alpha = NULL,
         size = NULL,
         fill = NULL
         
    ) +
    scale_size_manual(values=rep(1,cantidad_paises)) +
    scale_alpha_manual(values=c(1, rep(cfg$alpha, cantidad_paises - 1))) +
    theme_elegante_std(base_family = base_familiy) +
    theme(legend.position = "none") 

    ggsave(filename = files[2], plot = p, device = "png", width = dimensiones[1], height = dimensiones[2])
}

###############################################################
# Obtenemos los acumulados mundiales
###############################################################
covid.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM",
                 stringsAsFactors = FALSE)

data <- get_prepared_data(covid.data)

# paises_de_interes <- c('Brasil', 'Chile', 'México', 'Ecuador', 'República Dominicana')
# paises_de_interes <- c('Brasil', 'Chile', 'México', 'Ecuador', 'República Dominicana', 'Colombia', 'Paraguay', 'Uruguay')
# paises_de_interes <- c('Brasil', 'Perú')
# paises_de_interes <- c('Paraguay', 'Uruguay', 'Bolivia', 'Chile')
# paises_de_interes <- c('Chile')

paises_de_interes <- c('Brasil', 'Chile', 'Paraguay', 'Uruguay', 'Bolivia')
plot_curva_casos_y_fallecidos(data, paises_de_interes)

# lapply(1:51, function(x) plot_curva_casos_y_fallecidos(covid.data, paises, paises_de_interes, x))


data$datos_totales %>% 
  filter(continente == 'Americas') %>%
  group_by(pais) %>%
  summarise(total_fallecidos = max(fallecidos),
            total_casos = max(casos),
            fallecidos_c_1000 = total_fallecidos/(total_casos/1000)) %>% 
  filter(total_casos > 1000) %>% 
  mutate(pais = fct_reorder(pais, fallecidos_c_1000)) %>% 
  ggplot() +
    geom_col(aes(x=pais,y=-(total_casos/1000)), fill='#8BC540') +
    geom_col(aes(x=pais,y=fallecidos_c_1000), fill='#25AAE2') +
    geom_text(aes(x=pais, y=-(total_casos/1000), label = format(total_casos/1000, digits=0, big.mark = ',')), 
              vjust = 0.5, hjust=1.5) +
   geom_text(aes(x=pais, y=fallecidos_c_1000, label = format(fallecidos_c_1000, digits=0, big.mark = ',')), 
            vjust = 0.5, hjust=1.5) +
    coord_flip() +
    labs(title = paste("Fallecidos - COVID-19"), 
       subtitle = paste("cada 1000 infectados (Al", max(data$datos_totales$fecha),")") , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "fallecidos c/1000 infectados", 
       x = "",
       color = NULL,
       alpha = NULL,
       size = NULL,
       fill = NULL) +
    theme_elegante_std() -> p
    ggsave(filename = 'relacion.png', plot = p, device = "png", width =18, height = 15)

