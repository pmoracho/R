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

covid %>% 
  filter(casos > 0, Country.Region == 'Argentina')




  spread(Country.Region, Fecha)
  -> covid.data

xx <- sapply(covid.data[1,-1], function(x) unname(unlist(x), force=TRUE))

xx
class(as.vector(xx))
apply(covid.data[,-1],  1, function(x) as.vector(t(x[ min( which( x != 0 )):length(x) ])))

covid.data %>% 
  slice(2) %>% 
  unlist(., use.names=FALSE)

  group_by(Country.Region) %>% 
  do(vec = unlist(t(.[,-1]), use.names=FALSE))  -> covid.data

covid %>% 
  group_by(Country.Region) %>% 
  summarise_each(funs(sum),
                 new_col_names) %>% 
  group_by(Country.Region) %>% 
  summarize(vec = c(everything()))

covid.data %>% 
  map_dbl(min( which( .$vec != 0 )))

covid.data %>% 
  group_by(Country.Region) %>% 
  do(vec2 = .[,-1][ min( which( .x != 0 )) ]) %>% 
  View
