library(sp)
library(rgdal)
library(leaflet)

tmp <- tempdir()
url <- "http://personal.tcu.edu/kylewalker/data/mexico.zip"
file <- file.path(tempdir(),basename(url))

download.file(url, file)

unzip(file, exdir = tmp)
mexico <- readOGR(dsn = tmp, layer = "mexico", encoding = "UTF-8")


pts <- data.frame(x=runif(100000, mexico@bbox[1,],mexico@bbox[3]), 
                  y=runif(100000, mexico@bbox[2,],mexico@bbox[4])
)

coordinates(pts) <- ~x+y  # pts needs to be a data.frame for this to work
proj4string(pts) <- proj4string(mexico)
estados <- over(pts, mexico)$id      # matching de las coordenadas con los estados

estados <- estados[!is.na(estados)]  # Borramos Na (puntos fuera de cualquier estado)
mexico@data$random <- as.integer(unlist(aggregate(estados, by=list(estados), FUN=sum)[2]))

pal <- colorQuantile("Blues", NULL, n = 10)
state_popup <- paste0("<strong>Estado: </strong>", 
                      mexico$name, 
                      "<br><strong>Valores random para cada estado: </strong>", 
                      mexico$random)

leaflet(data = mexico) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(random), 
                fillOpacity = 0.8, 
                color = "#BDBDC3", 
                weight = 1, 
                popup = state_popup)
