library(rgdal)
library(leaflet)
library(plyr)
library(readxl)

source("utils.loadShapeFileFromUrl.R")
source("utils.loadCsvFileFromUrl.R")
source("utils.loadProyeccionMujeresFromUrl.R")

tmp.path <- tempdir()
data.path <- file.path(getwd(),"data")

##################################################################################################papa
# Descarga del Shapefile de Argentina
##################################################################################################
argentina <- loadShapeFileFromUrl(url = "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip",
                           layer = "ARG_adm1",
                           path = data.path)

##################################################################################################
# Descarga del registro de femicidio
##################################################################################################
femicidios <- loadCsvFileFromUrl(url="http://datos.jus.gob.ar/dataset/27bb9b2c-521b-406c-bdf9-98110ef73f34/resource/9a06c428-8552-42fe-86e1-487bca9b712c/download/registro-de-femicidios.csv",
                                 path = data.path )

##################################################################################################
# Proyección de la PoblaciÃ³n de Argentina
##################################################################################################
mujeres <- loadProyeccionMujeresFromUrl(url="http://www.indec.gov.ar/bajarCuadroEstadistico.asp?idc=3E17DD2F9318063AAD3E51B564F230E791554FEA85602E9F50376F709AD5B8BFC4CE2FBEFAFA354A",
                                        path=data.path,
                                        year=2017)

# EstandarizaciÃ³n de los nombres de provincias
femicidios$lugar_hecho <- gsub('CABA', 'Ciudad de Buenos Aires', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Entre Rios', 'Entre RÃ�os', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Santa FÃ©', 'Santa Fe', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Tucuman', 'TucumÃ¡n', femicidios$lugar_hecho)
femicidios_x_provincia <- as.data.frame(table(femicidios$lugar_hecho))

# Renombrara columnas para hacer merge
colnames(femicidios_x_provincia) <- c("NAME_1", "Femicidios")

# Merge de los datos
argentina@data <- join(argentina@data,femicidios_x_provincia, by="NAME_1")
argentina@data <- join(argentina@data,mujeres, by="NAME_1")
argentina@data$femPob <- as.integer(argentina@data$TotalMujeres/argentina@data$Femicidios)


pal <- colorQuantile(palette = "Blues", domain = NULL, n = 5, reverse = TRUE)
state_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Femicidio cada </strong>", 
                      argentina$femPob,
                      " <strong>personas</strong>")

m <- leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(femPob), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup)


library(htmlwidgets)
saveWidget(m, file="m.html")