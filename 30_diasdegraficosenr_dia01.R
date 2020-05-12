# 30díasdegráficos con R - día 1 - barras / columnas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 1: Un gráfico de barras con ggplot2 + tema propio + fuente: Ralleway. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia01.R

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
  group_by(countriesAndTerritories) %>% 
  summarize(casos = sum(cases), fallecidos = sum(deaths)) %>% 
  ungroup() %>% 
  select(pais = countriesAndTerritories, casos, fallecidos) %>% 
  gather(referencia, cantidad, -pais) %>% 
  ggplot(aes(x=pais, fill=referencia, y=cantidad)) +
    geom_col(position=position_dodge(width=1)) +
    geom_text(aes(label = format(cantidad, digits=0, big.mark = ',')),  vjust = .6, hjust=1.1,
              position = position_dodge(width=1)) +
    coord_flip() +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    labs(title = paste("COVID-19"), 
       subtitle = paste("Relación Casos / fallecidos Argentina y vecinos al: ", last_date) , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "log10(Cantidad)", 
       x = "País"
    ) +
    scale_fill_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
    theme_elegante_std(base_family = "Ralleway") 
