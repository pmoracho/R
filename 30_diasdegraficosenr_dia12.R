# 30díasdegráficos con R - día 12 - Lollipop Charts
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 12: El top 50 de países según casos de COVID - 19, destacando los que están sobre la 
# media mundial y los que están debajo
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia12.R

library("tidyverse")
library("forcats")
library("countrycode")

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
  group_by(countryterritoryCode) %>% 
  summarize(casos = sum(cases), fallecidos = sum(deaths)) %>% 
  arrange(-casos) %>% 
  mutate(nr = row_number()) %>% 
  select(code_pais = countryterritoryCode, casos, fallecidos, nr) -> data

media_casos <- mean(data$casos)
media_fallecidos <- mean(data$fallecidos)

data %>% 
  filter(nr <= 50) %>% 
  mutate(sobre_media = casos > media_casos,
         pais = countrycode(code_pais, origin = 'iso3c', destination = 'un.name.es')) %>% 
  ggplot(aes(x=fct_reorder(pais, -nr), y=casos, color = sobre_media)) +
  geom_segment(aes( y=0 , xend = pais, yend = casos)) + 
  geom_point() +
  coord_flip() +
  geom_text(aes(label = format(abs(casos), digits=0, big.mark = ','),
                vjust = .5,
                hjust = -.1),
            position = position_dodge(width=1)) +
  labs(title = paste("COVID-19 en el mundo"), 
       subtitle = paste("Casos por pais al: ", last_date, '\nLos primeros 50 países') , 
       caption = "Fuente: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "Casos", 
       x = "País"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), limits = c(0, max(data$casos) * 1.01),
                     labels = scales::comma) +

  scale_color_manual(labels = c("Sobre la media", "Debajo de la media"), values = c("#67a9cf", "#ef8a62")) +
  theme_elegante_std(base_family = "Ralleway") 

