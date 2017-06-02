#' Carga un shape file desde una URL y lo salva localmente
#'
#'\code{loadShapeFileFromUrl}
#'
#' @export
#' @name loadShapeFileFromUrl
#' @author Patricio Moracho pmorcho@gmail.com
#' @param url enlace al archivo shapefile.
#' @param layer capa de información a importar
#' @param path carpeta dónde se salvará el archivo descargado o dónde será leído
#' @param force.download Forzar la descarga del archivo
#' @return un ShapeFile
#' @examples
#' loadShapeFileFromUrl(url = "http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip")

require(rgdal)

loadShapeFileFromUrl <- function(url, layer, path, force.download = FALSE) {
    
    fileext = basename(url)
    file = tools::file_path_sans_ext(fileext)
    data.file = file.path(path,file)
    
    if (!file.exists(data.file) || force.download) {
        file <- file.path(data.path,fileext)
        if (!file.exists(file) || force.download) {
            download.file(url, data.file)
        }
        unzip(file, exdir = data.file)
    }
    return(readOGR(dsn = data.file, layer = "ARG_adm1", use_iconv=TRUE, encoding='UTF-8'))
}