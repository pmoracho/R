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


loadShapeFileFromUrl <- function(url, layer, path, force.download = FALSE) {
    
    require(rgdal)
    fileext <- basename(url)
    zipfile <- file.path(path, fileext)
    datapath <- file.path(path, tools::file_path_sans_ext(fileext))
    
    if (!file.exists(datapath) || force.download) {
        if (!file.exists(zipfile) || force.download) {
            download.file(url, zipfile)
        }
        unzip(zipfile, exdir = datapath)
    }
    return(readOGR(dsn = datapath, layer = layer, use_iconv=TRUE, encoding='UTF-8'))
}