# 30díasdegráficos con R - día 2 - líneas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 1: Un gráfico de líneas con ggplot2 + tema propio + fuente: Ralleway. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia02.R

library("tidyverse")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM",
                       stringsAsFactors = FALSE)

last_date <- max(as.Date(covid.data$dateRep,"%d/%m/%Y"))

covid.data %>% 
  filter(countriesAndTerritories %in% c('Argentina')) %>% 
  mutate(fecha = as.Date(dateRep, "%d/%m/%Y")) %>% 
  arrange(fecha) %>% 
  mutate(dia = row_number()) %>% 
  select(dia, casos=cases, fallecidos=deaths) %>% 
  gather(referencia, cantidad, -dia) %>% 
  ggplot() +
    geom_line(aes(x=dia, color=referencia, y=cantidad)) +
    labs(title = paste("COVID-19"), 
       subtitle = paste("Variación de casos y fallecidos por día - al: ", last_date) , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "Cantidades", 
       x = "# de días desde el 1er caso"
  ) +
  scale_color_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  theme_elegante_std(base_family = "Ralleway") 
