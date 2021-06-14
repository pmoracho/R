# 30díasdegráficos con R - día 29
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + GGally + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 29: gráficos de coordenadas paralelas del top 10 de paíse Americanos con 
# mayor número de casos de COVID-19
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia29.R

library("tidyverse")
library("GGally")
if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

# Dataos originales
# covid.data <- read.csv(file='https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-16")
# Datos reproducibles
covid.data <- readRDS('./data/covid.mundial.Rda')

covid.data %>% 
  mutate(fecha = as.Date(dateRep, '%d/%m/%Y'),
         pais = countriesAndTerritories) %>% 
  filter(continentExp %in% c('America')) %>% 
  # filter(pais %in% c('Argentina', 'Brazil', 'Chile', 'Paraguay', 'Uruguay', 'Colombia', 'Bolivia', 'Ecuador', 'Peru', 'Venezuela', 'México')) %>% 
  group_by(pais, popData2019) %>% 
  summarise(casos = sum(cases), fallecidos = sum(deaths)) %>% 
  arrange(-casos) %>% 
  head(10) %>% 
  ggparcoord(columns = 2:4, groupColumn = 1) +
    theme_elegante_std(base_family = "Ralleway") +
    scale_x_discrete(labels = c("Población", "Casos", "Fallecidos")) +
    labs(title = paste("COVID-19"), 
       subtitle = paste("Relación Población / Casos / fallecidos (Top 10 América)") , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "", 
       x = ""
  )
