library("readxl")


##################################################################################################
# Descarga del registro de femicidio
##################################################################################################
this.dir <- dirname(parent.frame(1)$ofile)
setwd(this.dir)

frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
PATH <- dirname(frame_files[[length(frame_files)]])


data.file <- "proyeccion.Rda"
if (!file.exists(data.file)) {
    url <- "http://www.indec.gov.ar/bajarCuadroEstadistico.asp?idc=38455CAC7ADD7A03396FC8EFC3119AE783BD2727BE412BEA291111EEA4CD84F34DBCE9EB7381AE33"
    file <- "file.xls"
    download.file(url, file)
    # Carga el Csv a un data.frame
    proyeccion <- read_excel(file)
    save(proyeccion, file=data.file)
} else {
    proyeccion <- load(file=data.file)
}
proyeccion <- proyeccion[which(proyeccion[1] == 2017),]
head(proyeccion)

system("locate get.proyeccion.poblacion.argentina.R")