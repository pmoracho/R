#####################################
### Librerías y carga de archivos ###
#####################################

## En R, una línea o segmento de código no se ejecuta si tiene un signo "#"

## 0. Actualizar versión de R
install.packages("installr")	# Siempre con comillas
library(installr)			# Sin comillas
updateR()				# FALSE si se cuenta con la versión más reciente 			

## 1. Buscar ayuda
## Existen dos formas de buscar ayuda y documentación en R
?criterio_de_busqueda	
??criterio_de_busqueda	

## La primera busca por coincidencia exacta en las librerías instaladas
## La segunda busca por coincidencias aproximadas y también nos permite buscar
## documentación sobre funciones de librerías que no tenemos instaladas
?mean		# Muestra la documentación de la función base mean()	
??mean	# Muestra resultados de funciones similares a mean()

## 2. Entorno de trabajo
## Para conocer cuál es nuestro directorio de trabajo actual, empleamos:
getwd()

## El directorio de trabajo es el lugar en el cual se guardan por default los scripts 
## y archivos que utilizamos y generamos en R. Si queremos cambiar la ubicación de 
## dicho directorio, empleamos:
setwd("path")	# "path" es una ruta definida por el usuario					

## La ventaja de usar el directorio de trabajo es que nos permite obviar
## las rutas de los archivos o scripts que serán utilizados. Por ejemplo, si un archivo
## llamado "archivo.csv" está en el directorio, bastaría con indicar su nombre dentro 
## de una función como read.csv(). De otra forma, tendría que indicarse la ruta más el nombre:
en_dt <- read.csv("archivo.csv")
no_dt <- read.csv("ruta_del_archivo/archivo.csv") 

## 3. El uso de source()
## source() es una función que ejecuta el contenido de un script sin necesidad de abrirlo.
## Esto es útil cuando requerimos cargar librerías y funciones de forma recurrente. 
## El principal argumento de source() es la ruta del archivo, la cual debe incluir la 
## extensión .R: 
source("path/script_a_ejecutar.R")	#path es una ruta definida por el usuario

## Nota: Si el script se guarda en el directorio de trabajo, solo se indica:
source("script_a_ejecutar.R")

## 4. ls()
## Es una función que nos devuelve la lista de objetos en un entorno especificado.
## Si no se indican argumentos, ls() despliega las bases de datos, objetos y las funciones definidas 
## por el usuario en el entorno de trabajo actual. ls() suele emplearse dentro de la función rm
## para limpiar el entorno actual de trabajo y sustituirlo por uno en blanco de la siguiente forma:
rm(list=ls())

## Si deseamos saber si alguna función u objeto se encuentran en el espacio de trabajo:
exists("x")		# FALSE
x <- 5
exists("x")		# TRUE

## 5. Instalación y carga de librerías
install.packages("paquete")						# Individual	
install.packages(c("paquete1", "paquete2", "paquete n"))	# Múltiple
library(paquete)								# Sin comillas

## 5.1 Revisar librerías instaladas
## La función installed.packages() nos regresa una lista detallada de las librerías instaladas. 
View(installed.packages())		# View() nos da una visualización de las librerías en data.frame

## Para verificar si una librería está ha sido descargada, usamos installed.packages()mientras 
## que para checar si se encuentra cargada en el entorno de trabajo, empleamos loadedNamespaces()
is.element(c("tidyverse","zoo"), installed.packages())	#TRUE si están instaladas
is.element(c("tidyverse","zoo"), loadedNamespaces())		#FALSE si no están cargadas

## Ejercicio:
## Crear un script con el siguiente contenido:

rm(list=ls())
library(tidyverse, warn.conflicts=F)
library(zoo, warn.conflicts=F)

## Lo guardamos como "source_demo.R" en el directorio de trabajo 
## y cerramos nuestra sesión de R. Abrimos nuevamente R y verificamos que el 
## espacio de trabajo esté en blanco y que las librerías "tidyverse" y "zoo" estén cargadas:

ls()									# character(0)
is.element(c("tidyverse","zoo"), loadedNamespaces())	# TRUE TRUE

## 4.2 Actualizar librerías
## Las librerías deben actualizarse cuando transitamos de una versión de R a otra
## En ocasiones, no todas las librerías se actualizan de forma automática tras instalar 
## la nueva versión de R 

old.packages() 		#Muestra los paquetes que tienen actualizaciones disponibles
update.packages()		#Actualización manual
update.packages(ask=F)	#Con ask=F, evitamos el cuadro de diálogo que solicita permisos

## 5.3 Cargar librerías de forma individual
library(paquete)	

## 5.4 Cargar múltiples librerías con pacman
install.packages("pacman")		#Si no está instalado
library(pacman)				#Ejecutar librería
p_load(lib_uno, lib_dos, lib_n)	#Sin comillas

## 6. Guardar y cargar objetos
## Existen dos formas principales de guardar objetos en nuestra sesión de R
## Con save.image() que guarda todos los objetos de la sesión actual
save.image(file="archivo.RData")

## Con save() que guarda objetos seleccionados:
save(a, b, file="archivo.RData")

## Para cargar los objetos, empleamos la función load()
load("archivo.RData")

## Para borrar los objetos, utilizamos unlink()
unlink("archivo.RData")

## 7. Lectura de archivos externos

## 7.1 Importar bases de datos de Internet (sitios web)
df <- load(url("http://..."))

## 7.2 Importar utilizando download.file
## download.file es particularmente útil para descargar archivos zip y se complementa 
## con la función unzip. Un flujo estándar de descarga y extracción de archivos zip 
## tendría la siguiente estructura:

download.file("url.zip", "archivo.zip")
archivo_zip <- unzip("archivo.zip")

## En el siguiente ejemplo, descargamos los microdatos de la Ciudad de México correspondientes 
## a la Encuesta Intercensal 2015. La url del archivo se obtiene directamente desde el sitio web
cdmx <- download.file(
	"https://www.inegi.org.mx/contenidos/programas/intercensal/2015/microdatos/eic2015_09_csv.zip",
	"cdmx.zip")	## Al no especificar una ruta, el archivo quedará guardado en nuestro directorio 
	## de trabajo

cdmx_zip <- unzip("cdmx.zip")
str(cdmx_zip)

## Si el zip tiene múltiples archivos, str() nos permite observar cuáles son a fin de 
## seleccionar el deseado. En este caso, se puede observar que el zip contiene los 
## archivos "TR_PERSONA09.CSV" y "TR_VIVIENDA09.CSV"

## 5.3 Lectura de txt y csv
## Para leer archivos de texto y en formato csv (valores separados por comas), empleamos tres funciones
## read.table, read.csv y read.csv2. read.csv2 se emplea en archivos separados por semicolon (;)
## Los argumentos más importantes son header y stringsAsFactors.
## header=T es opción por default así que no requiere indicarse cuando se llama a la función
## Por tanto, si no se quiere conservar los encabezados de las variables, se indica header=F
## stringsAsFactors=T es la opción por default, lo que convierte toda cadena de texto en factores
## Esta opción es útil si se conoce la estructura de la base de datos y los niveles de cada factor
## Si no es el caso, debe indicarse explícitamente stringsAsFactors= F. Con ello, los valores no numéricos
## se coercionan a cadenas de caracteres.

mydf <- read.table("path/archivo.txt",stringsAsFactors=F)			
mydf <- read.csv("path/archivo.csv",stringsAsFactors=F)				
mydf <- read.csv2("path/archivo.csv",stringsAsFactors=F)	

## Empleando el zip de la sección 5.2, seleccionamos el archivo "TR_PERSONA09.csv" y lo cargamos:
cdmx_df <-read.csv("TR_PERSONA09.csv",stringsAsFactors=F)		
View(cdmx_df)	# Para observar la base de datos

## 5.4 Archivos .xls y .xlsx 
## Para este tipo de archivos, empleamos la librería "readxl" y su función clave "read_excel"
## Debe notarse que read_excel "adivina" la extensión del archivo, por lo que se puede optimizar
## la lectura empleando read_xls o read_xlsx si se conoce dicha extensión

## Los argumentos más importantes de la función son: sheet, col_names y na.
## sheet= "string" nos sirve para seleccionar una hoja del libro por su nombre
## sheet= numero nos sirve para seleccionar una hoja por su número
## col_names= T fija el primer renglón como nombre para cada una de las columnas
## na= "" permite establecer una cadena de caracteres para sustituir a los valores NA
## (Por default, la función trata los espacios en blanco como valores perdidos

mydf <- read_excel("path/archivo.xls", sheet=1, col_names=T, na=".")

## 5.5 Archivos de SPSS
## La librería foreign cuenta con funciones para leer archivos de distintos tipos
## Entre ellos se encuentran los de extensión .sav, del programa SPSS
## Para leer esos archivos, se emplea la función read.spss
## Los argumentos más importantes de la función son use.value.labels y to.data.frame
## Ambos son lógicos (TRUE o FALSE).
## use.value.labels= T convierte variables con etiquetas de valor en factores R con esos niveles
## to.data.frame= T transforma el archivo a un data.frame de R 

df <- read.spss("path/archivo.sav", use.value.labels= T, to.data.frame= TRUE)

## 6. Escritura de archivos
## En ocasiones, después de realizar manipulaciones en una BD, deseamos guardar los 
## archivos en formatos útiles para otras personas. 
## Base R nos permite guardar archivos en dos formatos importantes: txt y csv
## Aunque con write.table podemos guardar en ambos formatos, se recomienda usar
## write.csv cuando se trta de bases de datos grandes.
## Los argumentos más importantes en ambas funciones son: el nombre del archivo en el
## espacio de trabajo y el nombre del archivo

write.csv(archivo, "archivo.csv")
write.table(archivo, "archivo.txt")
write.csv2(archivo, "archivo.csv")

## Para guardar archivos en extensiones .xls y .xlsx, una opción es la librería "openxlsx"
## y su función write.xlsx. Los argumentos básicos de esta función son el nombre del
## objeto y el nombre del archivo a guardar. También es posible especificar un nombre
## para la hoja en la que se guardará el archivo. 

write.xlsx(nombre_objeto, "nombre_archivo.xlsx", sheetName="nombre_hoja")