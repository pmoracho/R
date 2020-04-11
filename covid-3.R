library("tidyverse")
library("ggrepel")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
  base_familiy = "Raleway"
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
  base_familiy = ""
}

###############################################################
# Obtenemos los acumulados mundiales
###############################################################
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM",
                 stringsAsFactors = FALSE)

paises_de_interes <- c('Argentina', 'Brazil', 'Spain', 'Italy', 'United_States_of_America')
data %>% 
  filter(countriesAndTerritories %in% paises_de_interes & cases > 0) %>% 
  mutate(fecha = as.Date(dateRep, "%d/%m/%Y")) %>% 
  arrange(countriesAndTerritories, fecha) %>% 
  select(countriesAndTerritories, fecha, deaths, cases) -> deaths

deaths %>% 
  arrange(countriesAndTerritories, fecha) %>% 
  group_by(countriesAndTerritories) %>% 
  mutate(pais = case_when(countriesAndTerritories == 'Argentina' ~ 'Argentina',
                          countriesAndTerritories == 'Brazil' ~ 'Brasil',
                          countriesAndTerritories == 'Spain' ~ 'España',
                          countriesAndTerritories == 'Italy' ~ 'Italia',
                          countriesAndTerritories == 'United_States_of_America' ~ 'Estados Unidos'),
         fallecidos = cumsum(replace_na(deaths,0)),
         casos = cumsum(replace_na(cases, 0)),
         dia=row_number()
  ) %>% 
  ungroup() %>% 
  select(pais, dia, fecha, fallecidos, casos) -> datos

datos %>% 
  group_by(pais) %>% 
  arrange(fecha) %>% 
  slice(n()) %>% 
  ungroup() -> ultimo_reporte

ultima_fecha <- max(datos$fecha)
###############################################################
# El Plot final
###############################################################
zoom <- list(x=NULL, y=NULL)

# Para ajustar el gráfico a un área en paticular
#zoom <- list(x=c(0, 45), y=c(0,1000))


datos %>% 
  ggplot() +
  geom_line(mapping=aes(x=dia, y=fallecidos, color=pais, size=pais, alpha=pais)) +
  geom_point(data=ultimo_reporte, mapping=aes(x=dia, y=fallecidos, color=pais), size=4) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits=zoom$x) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits=zoom$y) +
  geom_label_repel(data = ultimo_reporte,
                   mapping = aes(x= dia, y = fallecidos, label = fallecidos, color=pais),
                   size = 4,
                   vjust = -2,
                   family = base_familiy,
                   colour = "#666666") +
  labs(title = paste("Fallecidos - COVID-19"), 
       subtitle = paste("Acumulados al", ultima_fecha) , 
       caption = "Fuente: hhttps://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
       y = "Fallecidos", 
       x = "Número de día (desde el 1er caso)",
       color = NULL,
       alpha = NULL,
       size = NULL,
       fill = NULL
  ) +
  scale_size_manual(values=c(1, 1, 1, 1, 1)) +
  scale_alpha_manual(values=c(1, .5, .5, .5, .5)) +
  theme_elegante_std(base_family = base_familiy)

