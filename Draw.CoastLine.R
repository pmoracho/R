library("rgdal") # for `ogrInfo()` and `readOGR()`
library("tools") # for `file_path_sans_ext()`
library("dplyr") # for `inner_join()`, `filter()`, `summarise()`, and the pipe operator (%>%)
library("ggplot2") # for `fortify()` and for plotting
library("sp") # for `point.in.polygon()` and `spDists()`
library("tidyr") # for `gather()`
library("ggmap")
library("geosphere")

fortify.shape <- function(x){
    x@data$id <- rownames(x@data)
    x.f <- fortify(x, region = "id")
    x.join <- inner_join(x.f, x@data, by = "id")
}

subset.shape <- function(x, domain){
    x.subset <- filter(x, long > domain[1] & 
                           long < domain[2] & 
                           lat > domain[3] & 
                           lat < domain[4])
    x.subset
}

##############################################################################################
# Descargar y descomprimir el shapefile de líneas costeras
# desde: http://www.naturalearthdata.com/downloads/10m-physical-vectors/
##############################################################################################
tmp.file <- file.path(tempdir(),"ne_10m_coastline.zip")
data.path <- file.path(getwd(),"data")

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", 
                tmp.file)

path.ne.coast <- file.path(data.path,"10m-physical-vectors")
unzip(tmp.file, exdir = path.ne.coast )

##############################################################################################
# Cargar el shapefile
##############################################################################################
fnam.ne.coast <- "ne_10m_coastline.shp"
dat.coast <- readOGR(dsn = path.ne.coast, 
                     layer = file_path_sans_ext(fnam.ne.coast))

# Fortify the shapefile data using `fortify.shape()`:
dat.coast <- fortify.shape(dat.coast) # a 410951x8 dataframe

especies <-data.frame("specie"=c(1:11,1:11), 
               "lat"=c(25.945235,25.945235, 25.945235,25.945235,25.945235,25.945235, 19.744471,18.832071, 19.084574,19.067172, 19.497018,22.216136,18.744251, 21.581057, 20.064758,21.536391,18.85821,18.570462, 18.962724, 21.476433,20.849696,21.170029), 
               "lon"=c(-97.135846, -97.135846,-97.135846, -97.135846, -97.135846, -97.135846, -96.380597, -95.8098,-95.993928,-91.317076,-90.793455,-97.769741, -95.650379,-87.112519,-90.525326,-87.478007,-91.473585,-93.121531,-91.252631, -87.542452,-86.750701,-86.772236))

area <- make_bbox(lon=especies$lon, lat=especies$lat, f=0.1)
dat.coast.mex <- subset.shape(dat.coast, area) 

linea3<-data.frame()
for(i in 1:11){
    linea2<-mutate(dat.coast.mex,dist1=sqrt((long-especies$lon[i])^2+(lat-especies$lat[i])^2),
                   dist2=sqrt((long-especies$lon[i+11])^2+(lat-especies$lat[i+11])^2))
    linea2<-dat.coast.mex[which.min(linea2$dist1):which.min(linea2$dist2),]
    linea2$specie<-i
    linea2$lon<-linea2$long+i*0.1
    linea2$lat<-linea2$lat+i*0.1
    linea3<-rbind(linea3,linea2)
}

map <- get_map(location=area, maptype="satellite", source="google")
ggmap(map) + geom_path(data = linea3, mapping = aes(x = linea3$lon, y = linea3$lat, group=linea3$specie), color = linea3$specie, size=1.2)

##############################################################################################
# Obtenemos punto inicial y final de la especie 1
##############################################################################################
dat.coast.mex$distp1 <- sqrt((dat.coast.mex[, 1]-especies[1, 2])^2+(dat.coast.mex[, 2]-especies[1, 3])^2)
dat.coast.mex$distp2 <- sqrt((dat.coast.mex[, 1]-especies[12, 2])^2+(dat.coast.mex[, 2]-especies[12, 3])^2)

especie <- 4
inicial <- which.min(spDistsN1(as.matrix(dat.coast.mex[,c(1,2)]), as.matrix(especies[c(especie,11+especie), c(3,2)][1,])))
final <- which.min(spDistsN1(as.matrix(dat.coast.mex[,c(1,2)]), as.matrix(especies[c(especie,11+especie), c(3, 2)][2,])))

dat.coast_especie1 <- dat.coast.mex[c(inicial:final),]

xlims <- as.numeric(area[c(1,3)])
ylims <- as.numeric(area[c(2,4)])

##############################################################################################
# Generamos el mapa
##############################################################################################
map <- get_map(location=area, maptype="satellite", source="google")
p0 <- ggmap(map) + 
    geom_path(data = dat.coast_especie1, aes(x = long, y = lat, group = group), color = "blue", size = 1) + 
    coord_map(projection = "mercator") + 
    scale_x_continuous(limits = xlims, expand = c(0, 0)) + 
    scale_y_continuous(limits = ylims, expand = c(0, 0)) + 
    labs(list(title = "", x = "Longitud", y = "Latitud"))

p0
