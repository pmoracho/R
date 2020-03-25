# ====================================================================================================
# Descarga del registro de femicidios de Argentina
# Buscar en lace al archivo en esta url: http://datos.jus.gob.ar/dataset/registro-sistematizacion-y-seguimiento-de-femicidios-y-homicidios-agravados-por-el-genero
# ====================================================================================================
url <- "http://datos.jus.gob.ar/dataset/27bb9b2c-521b-406c-bdf9-98110ef73f34/resource/583cec9a-3022-4234-8b32-17692a267aac/download/registro-de-femicidios-20200109.csv"
file <- file.path("..", "data",basename(url))
download.file(url, file)

# ====================================================================================================
# Carga el Csv a un data.frame
# ====================================================================================================
femicidios <- read.table(file = file, header = TRUE, sep = ',', stringsAsFactors = FALSE)

# ====================================================================================================
# Estandarización de los nombres de provincias
# ====================================================================================================
femicidios$hecho_provincia <- gsub('CABA', 'Ciudad de Buenos Aires', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Entre Rios', 'Entre Ríos', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Santa Fé', 'Santa Fe', femicidios$hecho_provincia)
femicidios$hecho_provincia <- gsub('Tucuman', 'Tucumán', femicidios$hecho_provincia)
femicidios_x_provincia <- as.data.frame(table(femicidios$hecho_provincia))

# ====================================================================================================
# Descarga del Shapefile de Argentina
# ====================================================================================================
library(rgdal)
library(leaflet)
library(dplyr)
library(magrittr)

tmp <- tempdir()
url <- "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip"
file <- file.path("..", "data",basename(url))
download.file(url, file)
unzip(file, exdir = tmp)
argentina <- readOGR(dsn = tmp, layer = "ARG_adm1", use_iconv=TRUE, encoding='UTF-8')
  
# Renombrar las columnas para hacer merge
colnames(femicidios_x_provincia) <- c("NAME_1", "Femicidios")
femicidios_x_provincia$NAME_1 <- as.character(femicidios_x_provincia$NAME_1)
# Merge de los datos
argentina@data <- left_join(argentina@data,femicidios_x_provincia, by = c("NAME_1", "Femicidios"))


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
# Descarga de la proyección de población por sexo y provincia
# ====================================================================================================
library(readxl)
url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_prov_2010_2040.xls"
file <- file.path("..", "data",basename(url))
download.file(url, file)

df <- read_excel(file, sheet = 3, col_names = c("year", "total", "varones", "mujeres"))

# Hay una solapa oculta
df_lst <- list() 
for (i in 3:26) {
  read_excel(file, sheet = i, col_names = c("year", "total", "varones", "mujeres")) %>% 
    filter(!is.na(year), year!="Año") %>% 
    mutate(cod_prv = i -2,
           year = as.numeric(year))-> df_lst[[i]]
 
}

df_lst[[3]]
