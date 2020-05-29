# 30díasdegráficos con R - día 18 - Mapas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + sf + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 18: Un mapa del delito en la ciudad de Buenos Aires con datos del año 2018
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia18.R

library("tidyverse")
library("sf")

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

ggplot(delitos_puntos) +
  geom_sf(data = comunas, fill = "white") +
  geom_sf(data = delitos_puntos, aes(color=tipo_delito, alpha = 1), size=1) +
  theme_elegante_std(base_family = "Ralleway") + 
  labs(title = paste("CABA - Mapa del delito"), 
       subtitle = paste0("Delitos por tipo en la Ciudad de Buenos Aires - Año 2018\n") , 
       caption = "Fuente: data.buenosaires.gob.ar", 
       y = "", 
       x = ""
  ) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  facet_grid(cols = vars(tipo_delito))