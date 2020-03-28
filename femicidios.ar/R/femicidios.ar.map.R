library(rgdal)
library(leaflet)
library(dplyr)
library(magrittr)

# ====================================================================================================
# Descarga del registro de femicidios de Argentina
# Buscar en lace al archivo en esta url: http://datos.jus.gob.ar/dataset/registro-sistematizacion-y-seguimiento-de-femicidios-y-homicidios-agravados-por-el-genero
# ====================================================================================================
url <- "http://datos.jus.gob.ar/dataset/27bb9b2c-521b-406c-bdf9-98110ef73f34/resource/583cec9a-3022-4234-8b32-17692a267aac/download/registro-de-femicidios-20200109.csv"
file <- file.path(".", "data",basename(url))
download.file(url, file)

# ====================================================================================================
# Carga el Csv a un data.frame
# ====================================================================================================
femicidios <- read.table(file = file, header = TRUE, sep = ',', stringsAsFactors = FALSE)

# ====================================================================================================
# Estandarización de los nombres de provincias
# ====================================================================================================
femicidios$hecho_provincia <- gsub('Ciudad Autónoma de Bs.As.', 'Ciudad de Buenos Aires', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Entre Rios', 'Entre Ríos', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Santa Fé', 'Santa Fe', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Tucuman', 'Tucumán', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Neuquen', 'Neuquén', femicidios$hecho_provincia)
femicidios_x_provincia <- as.data.frame(table(femicidios$hecho_provincia), stringsAsFactors = FALSE)

# ====================================================================================================
# Descarga del Shapefile de Argentina
# ====================================================================================================
tmp <- tempdir()
url <- "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip"
file <- file.path(".", "data",basename(url))
download.file(url, file)
unzip(file, exdir = tmp)
argentina <- readOGR(dsn = tmp, layer = "ARG_adm1", use_iconv=TRUE, encoding='UTF-8', stringsAsFactors=FALSE)
  
# Renombrar las columnas para hacer merge
colnames(femicidios_x_provincia) <- c("NAME_1", "Femicidios")
# Merge de los datos
argentina@data <- left_join(argentina@data,femicidios_x_provincia, by = c("NAME_1"))

# ====================================================================================================
# Primer versión del gráfico
# ====================================================================================================
pal <- colorQuantile("YlGn", NULL, n = 5)
state_popup <- paste0("<strong>Estado: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Femicidios: </strong>", 
                      argentina$Femicidios)

leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(femicidios_x_provincia$Femicidios), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup)

# ====================================================================================================
# Cuantas mujeres hay en cada provincia según proyecciones del indec
# Los datos están en excel por los que los preprocesé
# ====================================================================================================
url <- "https://raw.githubusercontent.com/pmoracho/R/master/femicidios.ar/data/poblacion.csv"
file <- file.path(".", "data", basename(url))
download.file(url, file)
poblacion <- read.csv(file, stringsAsFactors = FALSE)

# ====================================================================================================
# Merge de la data geográfica, los femicidios por provincia y la proyección de mujeres
# ====================================================================================================
nyear = 2020
argentina@data %>% 
  left_join(poblacion %>% 
              filter(year == nyear) %>% 
              select(provincia, mujeres), by= c("NAME_1" = "provincia")) %>% 
  mutate(femPob = round(mujeres/Femicidios)) %>% 
  select(-mujeres) -> argentina@data

pal <- colorQuantile(palette = "Blues", domain = NULL, n = 5, reverse = TRUE)
state_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>1 femicidio cada </strong>", 
                      argentina$femPob,
                      " <strong>mujeres</strong>")

leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(femPob), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup)
