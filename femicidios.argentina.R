library(rgdal)
library(leaflet)
library(plyr)

source("utils.loadShapeFileFromUrl.R")
source("utils.loadCsvFileFromUrl.R")

tmp.path <- tempdir()
data.path = file.path(getwd(),"data")

##################################################################################################
# Descarga del Shapefile de Argentina
##################################################################################################
argentina <- loadShapeFileFromUrl(url = "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip",
                           layer = "ARG_adm1",
                           path = data.path)

##################################################################################################
# Descarga del registro de femicidio
##################################################################################################
femicidios <- loadCsvFileFromUrl(url="http://datos.jus.gob.ar/dataset/27bb9b2c-521b-406c-bdf9-98110ef73f34/resource/9a06c428-8552-42fe-86e1-487bca9b712c/download/registro-de-femicidios.csv",path = data.path )

head(femicidios)

# Población
list <-"NAME_1, Poblacion
Ciudad de Buenos Aires,	2890151
Buenos Aires, 15625084
Catamarca, 367828
Chaco,	1055259
Chubut,	509108
Córdoba, 3308876
Corrientes, 992595
Entre Ríos, 1235994
Formosa, 530162
Jujuy, 673307
La Pampa, 318951
La Rioja, 333642
Mendoza, 1738929
Misiones, 1101593
Neuquén, 551266
Río Negro, 638645
Salta, 1214441
San Juan, 681055
San Luis, 432310
Santa Cruz, 273964
Santa Fe, 3194537
Santiago del Estero, 874006
Tierra del Fuego, 127205
Tucumán, 1448188"

poblacion = as.data.frame(read.table(textConnection(list), header=TRUE, sep=','))

# Estandarización de los nombres de provincias
femicidios$lugar_hecho <- gsub('CABA', 'Ciudad de Buenos Aires', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Entre Rios', 'Entre Ríos', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Santa Fé', 'Santa Fe', femicidios$lugar_hecho)
femicidios$lugar_hecho <- gsub('Tucuman', 'Tucumán', femicidios$lugar_hecho)
femicidios_x_provincia <- as.data.frame(table(femicidios$lugar_hecho))

# Renombrara columnas para hacer merge
colnames(femicidios_x_provincia) <- c("NAME_1", "Femicidios")

# Merge de los datos
argentina@data <- join(argentina@data,femicidios_x_provincia)
argentina@data <- join(argentina@data,poblacion)
argentina@data$femPob <- as.integer(argentina@data$Poblacion/argentina@data$Femicidios)
# dt <- argentina@data
pal <- colorQuantile(palette = "Blues", domain = NULL, n = 5, reverse = TRUE)
state_popup <- paste0("<strong>Provincia: </strong>", 
                      argentina$NAME_1, 
                      "<br><strong>Femicidio cada </strong>", 
                      argentina$femPob,
                      " <strong>personas</strong>")

leaflet(data = argentina) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(femPob), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup)

print("PAsamos")

#head(argentina@data[c(1,2,3,4,5,10,11)])
#head(poblacion)