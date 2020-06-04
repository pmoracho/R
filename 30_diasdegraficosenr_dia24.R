# 30díasdegráficos con R - día 24 - coropletas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + sf + Algo de dplyr y viridis (paleta)
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 24 coropletas que mapea geograficamente la cantidad de delitos
# por comuna de la Ciudad de Buenos Aires
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia24.R

library("tidyverse")
library("sf")
library("viridis")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

delitos <- read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv", 
                    na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
comunas <- st_read('https://bitsandbricks.github.io/data/CABA_comunas.geojson')


delitos %>% 
  na.exclude() %>% 
  st_as_sf(coords = c("long","lat"), remove = FALSE,  crs = 4326) %>% 
  st_join(comunas)-> delitos_puntos

delitos_puntos %>% 
  mutate(n_delito = match(tipo_delito, c("Lesiones", "Hurto (sin violencia)","Robo (con violencia)","Homicidio")),
         comunas = factor(comuna)) %>% 
  group_by(barrios,comunas) %>% 
  summarize(nrank = n()) %>% 
  as.data.frame() %>% 
  select(barrios, comunas, nrank) -> ranking_comunas

comunas %>% 
  left_join(ranking_comunas,  by = "comunas") %>% 
  ggplot() +
    geom_sf(aes(fill = nrank), color="gray60") +
  scale_fill_viridis(option = "inferno", direction = -1) +
  theme_elegante_std(base_family = "Ralleway") + 
  labs(title = paste("CABA - Mapa del delito"), 
       subtitle = paste0("Nivel de peligrosidad por comuna - Año 2018\n") , 
       caption = "Fuente: data.buenosaires.gob.ar", 
       y = "", 
       x = "",
       fill = "Delitos"
  ) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_text(face="bold"),
        legend.position = "right") 
