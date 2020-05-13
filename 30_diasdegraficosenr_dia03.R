# 30díasdegráficos con R - día 3 - Puntos
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 3: ¿Hay alguna relación entre el índice de desarrollo humano de cada país y 
# la cantidad de infectados?
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

hdi <- read.csv("https://data.humdata.org/dataset/05b5d8f1-9e7f-4379-9958-125c203d12ac/resource/4a7fd374-7e35-4c04-b7c8-25e5943aa476/download/hdi_human_development_index_hdig_value.csv", stringsAsFactors = FALSE)
hdi %>% 
  group_by(country_code) %>% 
  arrange(year) %>% 
  slice(n()) %>% 
  select(country_code, country, year, value) -> last_hdi

last_date <- max(as.Date(covid.data$dateRep,"%d/%m/%Y"))
paises_de_interes <- c('Argentina','Brazil', 'Chile', 'Bolivia', 'Paraguay', 'Uruguay')

covid.data %>% 
  group_by(countriesAndTerritories, countryterritoryCode) %>% 
  summarize(casos = sum(cases), fallecidos = sum(deaths)) %>% 
  ungroup() %>% 
  inner_join(last_hdi,
            by = c("countryterritoryCode" = "country_code")
            ) %>% 
  select(pais = countriesAndTerritories, casos, fallecidos, HDI = value) %>% 
  ggplot(aes(x=HDI, y=casos)) +
    geom_point(color = "#67a9cf", alpha=.5, size=3) +
    geom_smooth(method = 'lm',formula='y ~ x', se=FALSE, color="#ef8a62") +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    labs(title = paste("COVID-19"), 
         subtitle = paste0("¿Hay relación entre el desarrollo humano y la cantidad de infecciones?\n (Datos al: ", last_date, ")") , 
         caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
         y = "log10(Cantidad de infectados)", 
         x = "Human development Index (2013)"
    ) +
    theme_elegante_std(base_family = "Ralleway") 