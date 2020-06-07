# 30díasdegráficos con R - día 26 - Marimeko
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggmosaic + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 26 una gráfica tipo marimeko de los delitos ocurridos en la Ciudad de Buenos Aires
# en el año 2018, muy sencillo todo gracias a "ggmosaic"
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia26.R

library("tidyverse")
library("ggmosaic") # devtools::install_github("haleyjeppson/ggmosaic")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

delitos <- read.csv("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/mapa-del-delito/delitos_2019.csv", 
                    na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

delitos %>% 
  mutate(tipo_delito=factor(tipo_delito, levels = c("Lesiones", "Hurto (sin violencia)", "Robo (con violencia)", "Homicidio")),
         comuna = factor(comuna)) %>% 
  na.exclude() %>% 
  ggplot() +
  geom_mosaic(aes(x = product(comuna), fill=tipo_delito), offset= 0.003) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = "Delitos en CABA", 
       subtitle = paste0("Por Comuna (año 2018)"),  
       caption = "Fuente: data.buenosaires.gob.ar",
       y = "",
       x = "Comunas"
  ) +
  scale_fill_manual(values=c("#56B4E9", "#009E73",  "#0072B2", "#E69F00"))

