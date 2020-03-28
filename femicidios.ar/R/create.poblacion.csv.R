# ====================================================================================================
# Descarga de la proyección de población por sexo y provincia
# ====================================================================================================
library(readxl)
url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/c1_proyecciones_prov_2010_2040.xls"
file <- file.path(".", "data",basename(url))
download.file(url, file)


# Hay una solapa oculta
df_lst <- list() 
for (i in 3:26) {
  read_excel(file, sheet = i, col_names = c("year", "total", "varones", "mujeres")) %>% 
    filter(!is.na(total), year!="Año") %>% 
    mutate(cod_prv = i -2,
           year = as.numeric(year))-> df_lst[[i-2]]
  
}
poblacion <- do.call(rbind, df_lst) 

read.table(text = "provincia, cod_prv
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
Tierra del Fuego, 24", header = TRUE, sep = ",", stringsAsFactors=FALSE) -> nombres

poblacion %>% 
  left_join(nombres, by = "cod_prv") %>% 
  select(provincia, year, total, varones, mujeres) %>%  
  write.csv(file.path(".", "data","poblacion.csv"), row.names = FALSE)
