# 30díasdegráficos con R - día 8 - Contornos
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggrepel + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 8: Me siento un poco tramposo, voy a reusar ¿Hay alguna relación entre el índice de 
# desarrollo humano de cada país y la cantidad de infectados? agregando una "2D kernel density estimation" de geom_density2d.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia08.R

library("tidyverse")
library("ggrepel")

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
paises_de_interes <- c( 'Argentina',
                        "Niger", "Norway", "EEUU", "Mauritania")
covid.data %>% 
  group_by(countriesAndTerritories, countryterritoryCode) %>% 
  summarize(casos = sum(cases), fallecidos = sum(deaths)) %>% 
  ungroup() %>% 
  inner_join(last_hdi,
             by = c("countryterritoryCode" = "country_code")
  ) %>% 
  mutate(pais = ifelse(countriesAndTerritories == 'United_States_of_America', 'EEUU', countriesAndTerritories)) %>% 
  select(pais, casos, fallecidos, HDI = value) %>% 
  mutate(pais_etiquetado = ifelse(pais %in% paises_de_interes, paste0(pais, " (casos: ", format(casos, digits=0, big.mark = ',', trim=TRUE), " hdi: ", HDI, ")"), NA)) %>% 
  ggplot(aes(x=HDI, y=casos)) +
  geom_point(color = "#67a9cf", alpha=.5, size=3) +
  geom_smooth(method = 'lm',formula='y ~ x', se=FALSE, color="#ef8a62") +
  geom_density2d(contour = TRUE, n = 1000) +
  geom_label_repel(mapping = aes(label = pais_etiquetado),
                   color="#67a9cf",family = "Ralleway", vjust = -1.2, hjust = 1.1, fontface="bold") +

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

