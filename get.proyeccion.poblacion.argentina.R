library("readxl")


##################################################################################################
# Descarga del registro de femicidio
##################################################################################################
read.proyecciones <- function(url, path, force.download = FALSE) {

    data.file <- "proyeccion.Rda"
    file <- "file.xls"
    if (!file.exists(data.file)) {
        download.file(url, file)
        proyeccion <- read_excel(file)
        save(proyeccion, file=data.file)
    } else {
        proyeccion <- load(file=data.file)
    }
    proyeccion <- proyeccion[which(proyeccion[1] == 2017),]
}
