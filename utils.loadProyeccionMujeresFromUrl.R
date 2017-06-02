library(readxl)

loadProyeccionMujeresFromUrl <- function(url, path, year) {

    list <-"NAME_1, Provincia
Ciudad de Buenos Aires,	1
Buenos Aires, 2
Catamarca, 3
Córdoba, 4
Corrientes, 5
Chaco,6
Chubut,	7
Entre Ríos, 8
Formosa, 9
Jujuy, 10
La Pampa, 11
La Rioja, 12
Mendoza, 13
Misiones, 14
Neuquén, 15
Río Negro, 16
Salta, 17
San Juan, 18
San Luis, 19
Santa Cruz, 20
Santa Fe, 21
Santiago del Estero, 22
Tucumán, 23
Tierra del Fuego, 24"

    provincias = as.data.frame(read.table(textConnection(list), header=TRUE, sep=','))
    
    #############################################################9#####################################
    # Proyección de la Población de Argentina
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