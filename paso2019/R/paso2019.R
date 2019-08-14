#' Paso paso2019 Data
#'
#' Data from a Argentinian Paso 2019
#'
#' @docType data
#'
#' @usage data(paso2019)
#'
#' @format An object of class \code{"cross"}; see \code{\link[qtl]{read.cross}}.
#'
#' @keywords datasets
#'
#' @references Moore et al. (2013) Genetics 195:1077-1086
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/23979570}{PubMed})
#'
#' @source \href{https://phenome.jax.org/projects/Moore1b}{QTL Archive}
#'
#' @examples
#' data(paso2019)
#' times <- attr(grav, "time")
#' phe <- grav$pheno
#' \donttest{iplotCurves(phe, times)}
"grav"

# descripcion_postulaciones <- read.table('data/descripcion_postulaciones.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
# descripcion_regiones <- read.table('data/descripcion_regiones.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
# mesas_totales_agrp_politica <- read.table('data/mesas_totales_agrp_politica.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
# mesas_totales_lista <- read.table('data/mesas_totales_lista.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
# mesas_totales <- read.table('data/mesas_totales.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
#
# dir("./data")
#
# save(descripcion_postulaciones, file="data/descripcion_postulaciones.Rda")
# save(descripcion_regiones, file="data/descripcion_regiones.Rda")
# save(mesas_totales_agrp_politica, file="data/mesas_totales_agrp_politica.Rda")
# save(mesas_totales_lista, file="data/mesas_totales_lista.Rda")
# save(mesas_totales, file="data/mesas_totales.Rda")
