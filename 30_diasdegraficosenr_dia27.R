# 30díasdegráficos con R - día 26 - Marimeko
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggmosaic + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Animamos una de las anteriores gráficas, a la vieja usanza: 
# generamos un conjunto de archivos y los montamos con ffmpeg. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia27.R

library("tidyverse")
library("countrycode")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", 
                       fileEncoding = "UTF-8-BOM",
                       stringsAsFactors = FALSE)

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
  mutate(dia=row_number()) %>% 
  ungroup() %>% 
  select(pais, dia, fecha, casos) %>% 
  group_by(fecha) %>% 
  arrange(-casos) %>% 
  mutate(nr = row_number(),
         media = mean(casos),
         sobre_media = casos > media) %>% 
  select(pais, fecha, casos, media, sobre_media, nr) %>% 
  filter(nr <= 50,
         fecha >= '2020-02-01') %>% 
  arrange(fecha, nr) -> data

out_folder <- tempdir(check = TRUE)
fechas <- unique(data$fecha)

for (f in 1:length(fechas)) {
  
  fecha_actual =  fechas[f]
  max_casos = max(data$casos[data$fecha == fecha_actual])
  data %>% 
    filter(fecha == fecha_actual) %>% 
    ggplot(aes(x=fct_reorder(pais, -nr), y = casos, color = sobre_media)) +
    geom_segment(aes( y=0 , xend = pais, yend = casos)) + 
    geom_point() +
    coord_flip() +
    geom_text(aes(label = casos,
                  vjust = .5,
                  hjust = -.1),
              position = position_dodge(width=1)) +
    labs(title = paste("COVID-19 en el mundo"), 
         subtitle = paste("Casos por pais al: ", fecha_actual, '\nLos primeros 50 países') , 
         caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
         y = "Casos", 
         x = ""
    ) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, max_casos * 1.03),
                       labels = scales::comma) +
    
    scale_color_manual(labels = c("Sobre la media", "Debajo de la media"), values = c("#67a9cf", "#ef8a62")) +
    theme_elegante_std(base_family = "Ralleway") -> p
  
    ggsave(filename = paste0(out_folder, "/", str_pad(f, 6, pad = "0"), ".png"), plot =p, width = 36, height = 22, units = "cm", dpi = 100)
}

system(paste0('ffmpeg -framerate 5 -i ', out_folder, '/%06d.png -c:v libx264 -r 30 ', getwd(), '/out.mp4'))