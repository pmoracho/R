library(readxl)

loadProyeccionMujeresFromUrl <- function(url, path, year) {

    list <-"NAME_1, Provincia
Ciudad de Buenos Aires,	1
Buenos Aires, 2
Catamarca, 3
C?rdoba, 4
Corrientes, 5
Chaco,6
Chubut,	7
Entre R?os, 8
Formosa, 9
Jujuy, 10
La Pampa, 11
La Rioja, 12
Mendoza, 13
Misiones, 14
Neuqu?n, 15
R?o Negro, 16
Salta, 17
San Juan, 18
San Luis, 19
Santa Cruz, 20
Santa Fe, 21
Santiago del Estero, 22
Tucum?n, 23
Tierra del Fuego, 24"

    provincias <- as.data.frame(read.table(textConnection(list), header=TRUE, sep=','))
    
    #############################################################9#####################################
    # Proyecci?n de la Poblaci?n de Argentina
    ##################################################################################################
    file <- file.path(path,'proyeccion.poblacion.argentina.xls')
    if (!file.exists(file)) {
        download.file(url, file)
    }
    df <- data.frame(Provincia=integer(), TotalMujeres=integer())
    
    # Hay una solapa oculta
    for (i in 3:26) {
        dt <- read_excel(file,sheet = i)
        m <- as.numeric(dt[which(dt[1]==year),4])
        df[nrow(df)+1,] <- c(i-2,m)
    }
    df <- join(df,provincias,by="Provincia")
    return(df)
}


mujeres <- loadProyeccionMujeresFromUrl(url="http://www.indec.gov.ar/bajarCuadroEstadistico.asp?idc=3E17DD2F9318063AAD3E51B564F230E791554FEA85602E9F50376F709AD5B8BFC4CE2FBEFAFA354A",
                                        path=file.path(getwd(),"data"),
                                        year=2017)
