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
url <- "https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv"

file <- file.path(".", basename(url))
download.file(url, file)
covid <- read.csv(file, stringsAsFactors = FALSE)

###############################################################
# Renombramos las columnas
###############################################################
rename_col <- grep("X\\d", colnames(covid))
new_col_names <- format(as.Date('2020-01-22')+rename_col-min(rename_col), "d%Y-%m-%d")
colnames(covid) <- c(colnames(covid[, -rename_col]), new_col_names)

covid %>% View()

covid %>% 
  group_by(Country.Region) %>% 
  summarise_at(vars(new_col_names), sum) %>% 
  gather(Fecha, casos, -Country.Region) %>% 
  mutate(Fecha = as.Date(gsub('d','', Fecha))) -> covid

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

covid %>% 
  filter(casos > 0) %>% 
  arrange(Fecha) %>% 
  arrange(Country.Region, Fecha) %>% 
  group_by(Country.Region) %>% 
  mutate(ndia = row_number()) -> datos

datos %>% 
  filter(Country.Region %in% c('Argentina', 'Brazil', 'Spain', 'Italy', 'US', '')) %>% 
  ggplot(mapping=aes(x=ndia, y=(casos), color=Country.Region)) +
  geom_line() +
  labs(title = paste("Curvas de casos"), 
       subtitle = "Infecciones reportadas por día", 
       caption = "Fuente: https://data.world/covid-19-data-resource-hub/covid-19-case-counts", 
       y = "Casos", 
       x = "Número de día",
       color = NULL) +
  theme_elegante_std(base_family = base_familiy) 


covid %>% 
  filter(casos > 0, Country.Region == 'Argentina') %>% 
  arrange(Fecha) %>% 
  mutate(ndia = row_number(),
         casos_norm = normalize(casos)) -> arg

ndias <- nrow(arg)


covid %>% 
  filter(casos > 0, 
         Country.Region != 'Argentina') %>% 
  arrange(Country.Region, Fecha) %>% 
  group_by(Country.Region) %>% 
  mutate(ndia = row_number(),
         casos_norm = normalize(casos)) %>% 
  filter(ndia <= ndias) -> resto

resto %>% 
  semi_join(resto %>% 
               filter(ndia==ndias),
             by = c("Country.Region")
             ) -> resto


resto %>% filter(Country.Region == "Brazil") %>% View()
arg %>% View()
plot(arg$ndia, arg$casos)
resto %>% View
arg %>% 
  ggplot(mapping=aes(x=ndia, y=casos_norm)) +
  geom_point()

resto %>% 
  union(arg) %>% 
  filter(Country.Region %in% c('Argentina', 'Brazil', 'Chile', 'Mexico', 'Ecuador', '')) %>% 
  ggplot(mapping=aes(x=ndia, y=(casos), color=Country.Region)) +
  geom_point() +
  geom_smooth(method = 'loess', alpha=.1, size = .5, se=FALSE) +
  labs(title = paste("Los primeros", ndias, "dias"), 
       subtitle = "Infecciones reportadas por día", 
       caption = "Fuente: https://data.world/covid-19-data-resource-hub/covid-19-case-counts", 
       y = "Casos", 
       x = "Número de día",
       color = NULL) +
  theme_elegante_std(base_family = base_familiy) 


###############################################################
# Las muertes
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
zoom <- list(x=c(0, 45), y=c(0,1000))


datos %>% 
  ggplot() +
  geom_line(mapping=aes(x=dia, y=fallecidos, color=pais, size=pais, alpha=pais)) +
  geom_point(data=ultimo_reporte, mapping=aes(x=dia, y=fallecidos, color=pais), size=4) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),
                     limits=zoom$x) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
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

