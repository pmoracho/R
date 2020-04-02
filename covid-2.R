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

covid %>% 
  group_by(Country.Region) %>% 
  summarise_at(vars(new_col_names), sum) %>% 
  gather(Fecha, casos, -Country.Region) %>% 
  mutate(Fecha = as.Date(gsub('d','', Fecha))) -> covid
str(covid)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

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

arg %>% View

unique(resto$Country.Region)
