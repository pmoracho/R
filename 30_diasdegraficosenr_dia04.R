# 30díasdegráficos con R - día 4- Facetas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 4: Un gráfico de líneas con facetas y  con ggplot2 + tema propio + fuente: Ralleway. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia03.R

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
  filter(countriesAndTerritories %in% c('Argentina','Brazil', 'Chile', 'Bolivia', 'Paraguay', 'Uruguay')) %>% 
  mutate(fecha = as.Date(dateRep, "%d/%m/%Y"), pais = countriesAndTerritories) %>% 
  arrange(fecha) %>% 
  group_by(pais) %>% 
  mutate(dia = row_number()) %>% 
  select(pais, dia, casos=cases, fallecidos=deaths) %>% 
  gather(referencia, cantidad, -pais, -dia) %>% 
  ggplot() +
  geom_line(aes(x=dia, color=referencia, y=cantidad)) +
  labs(title = paste("COVID-19"), 
       subtitle = paste("Variación de casos y fallecidos por día - al: ", last_date) , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "Cantidades", 
       x = "# de días desde el 1er caso"
  ) +
  facet_wrap(~pais, scales="free") +
  scale_color_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  theme_elegante_std(base_family = "Ralleway") 
