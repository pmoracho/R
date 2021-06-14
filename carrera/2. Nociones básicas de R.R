###########################
## Nociones básicas de R ##
###########################

## 1. Operadores

## :: ::: 		Permiten accesar a variables en un espacio de nombres
## 			Son útiles para llamar a funciones sin cargar librerías en el espacio de trabajo.
			dplyr::filter()		#Ejecuta la función filter() de dplyr	
			tidyr::pivot_wider()	#Ejecuta la función pivot_wider() de tidyr

## $ 			Extracción y creación de componentes 
## [] [[]] 		Indexación
## ^ 			Exponenciación (de derecha a izquierda)
## - +		Suma y resta
## : 			Operador de secuencias
## %any% 		Operadores especiales como %% (resto) y %/% (entero)
## * / 		Multiplicación y división
## < 			Menor que
## >			Mayor que
## <=			Menor o igual que
## >=			Mayor o igual que
## ==			Igual que
## !=			Diferente a 
## ! -		Negación
## & 	|		"y" "o" para comparaciones elemento a elemento
## && || 		"y" "o" utilizado en condicionales if
## ~ 			Operador de fórmulas (Ejemplo: lm(y~x1+x2))
## -> ->> 		Asignación y superasignación a la derecha
## <- <<- 		Asignación y superasignación a la izquierda
## = 			Asignación a la izquierda

## 2. Asignación
## En R, contrario a otros lenguajes de programación, se prefiere el operador "<-"
## en lugar del signo igual "=" para realizar asignaciones. Aunque en general el
## comportamiento de ambos operadores es el mismo en el entorno de trabajo, "<-"
## puede emplearse también como "->" de forma tal que sin importar la dirección 
## de la flecha, se creará una variable con un valor específico.

## Ejecutar
5 -> w;  w	#5
x <- 6;  x	#6
y = 7;   y	#7
8 = z;   z	#Error in 8 = z : lado izquierdo de la asignación inválida (do_set)

## Nota
## La escritura del tipo x <- 6; x es una forma compacta para
## x <- 6
## x 
## Más adelante se muestra otra forma de escritura compacta

## Por otra parte, cuando empleamos "=", las variables no necesariamente se crean
## en el espacio de trabajo, en particular cuando invocamos funciones:
median(test=1:15)	# Error in is.factor(x): el argumento "x" está ausente, sin valor por omisión
test			# Error: objeto 'test' no encontrado

## Ejecutar:
median(test <- 1:15)	# 8
test				# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
## La variable test está definida en el entorno de trabajo

## 3. La función c() y la creación de vectores
## En R se emplea de forma regular la función c() que significa "combine" 
## Esta función permite crear vectores de números, caracteres, variables y 
## observaciones dependiendo del contexto en el que sea utilizada. Los argumentos en 
## c() son coercionados a un tipo común para generar un valor de retorno.
## Los vectores son la estructura de datos básica en R y pueden ser de tipo lógico, numérico
## y caracter. Para verificar el tipo y clase de un vector empleamos las funciones typeof()# y class(). 
## typeof() nos regresa la forma en que un determinado objeto está guardado en el nivel interno del 
## lenguaje. Por su parte, class() se refiere al tipo "abstracto" o "genérico" de un objeto.
## typeof(). Si deseamos saber si un vector pertenece a un tipo específico, empleamos is.
## seguida del atributo de interés. 

## Ejecutar:
num_vec <- c(1:3, 4:9, 10:15)
typeof(num_vec); class(num_vec)	# "numeric"
is.numeric(num_vec)			# TRUE
is.character(num_vec)			# FALSE
is.logical(num_vec)			# FALSE

char_vec <- c(1:10, "perro", "niño")# El vector es coercionado
typeof(char_vec); class(char_vec)	# "character"
is.numeric(num_vec)			# FALSE
is.character(num_vec)			# TRUE
is.logical(num_vec)			# FALSE

logi_vec <- c(T,T,F,F,T)		
typeof(logi_vec); class(logi_vec)	# "logical"
is.numeric(logi_vec)			# FALSE
is.character(logi_vec)			# FALSE
is.logical(logi_vec)			# FALSE

## La coerción de vectores puede hacerse explícitamente empleando as. seguido del atributo de interés

## Ejecutar: 
char_vec_num <- as.numeric(char_vec)	# Notificación de NA introducidos por coercion
char_vec_num					#  [1]  1  2  3  4  5  6  7  8  9 10 NA NA

## Para crear el nuevo vector y visualizar de inmediato su resultado, podemos emplear paréntesis
## de apertura y de cierre de la siguiente manera:
(char_vec_num <- as.numeric(char_vec))

## Ejecutar:
(logi_vec_char <- as.character(logi_vec))	#  "TRUE"  "TRUE"  "FALSE" "FALSE" "TRUE" 
(logi_vec_num <- as.numeric(logi_vec))	# 1 1 0 0 1

## A notar:
## as.numeric() coerciona en NA las cadenas de caracteres en un vector que también contiene números
## as.numeric() transforma un vector lógico en un vector de 0 (para F) y 1 (para T)

## 4. Listas y data.frames
## Las listas pueden contener elementos de distinto tipo incluyendo otras listas.
## Por ese motivo, se les denomina "vectores recursivos". Por sus características, una lista no
## coerciona a los elementos que pertenecen a ella como ocurre con la función c().

## Ejecutar:
list_ej <- list(c(1,4,3), 2, "perro", list=(1:4))
str(list_ej)		# List of 3 $ : num [1:3] 1 4 3 $ : num 2 $ : chr "perro" $ list: int [1:4] 1 2 3 4
typeof(list_ej)		# "list"

vec_ej <- c(c(1,4,3), 2, "perro", list=(1:4))
str(vec_ej)			# Named chr [1:9] "1" "4" "3" "2" "perro" "1" "2" "3" "4"
typeof(vec_ej)		# "character"

## Las listas suelen emplearse en la construcción de funciones y su uso puede observarse
## con la función lm(). Al revisar la documentación de esta función, puede leerse lo siguiente:
## "An object of class "lm" is a list containing at least the following components:" 
?lm

## Una data.frame es un conjunto de listas que pueden diferir en el número de vectores (columnas)
## pero no de elementos (renglones). Puede observarse que la estructura básica de una data.frame
## es justamente una lista como revela la función typeof()

df_ej_uno <- data.frame(a=c(1:3), b=c("perro", "gato", "ratón"), stringsAsFactors=F)
typeof(df_ej_uno)		# "list"
class(df_ej_uno)		# "data.frame"

## Si los números de renglones no coinciden entre variables, se mostrará un error
df_ej_dos <- data.frame(a=c(1:10), b=letters[1:9])
# Error in data.frame(a = c(1:10), b = letters[1:9]): 
#  arguments imply differing number of rows: 10, 9

## 5. Atributos, nombresy factores
## Los atributos permiten alojar metadatos de los objetos mediante una lista de nombres. Se puede
## acceder a ellos mediante la función attributes(). 
attributes(df_ej_uno)
 $names
 [1] "a" "b"

 $class
 [1] "data.frame"

 $row.names
 [1] 1 2 3

## En este caso, uno de los atributos de una data.frame son los nombres (names) de las columnas
## (variables). Los nombres de los vectores columna se asignan al crear una data.frame (como en el
## caso de df_ej_uno) y pueden reasignarse directamente en el objeto, es decir, sin crear una 
## copia modificada del mismo. 
names(df_ej_uno) <- c("uno", "dos")
attributes(df_ej_uno)$names	[1] "uno" "dos"

## (Dado que la función attributes() regresa una lista, podemos acceder a sus elementos
## empleando el operador "$" seguido del atributo que nos interesa verificar). 
x <- attributes(df_ej_uno)
typeof(x)		# "list"
 
## NOTA: La función "names <-" se abordará con mayor detalle en los temas de manipulación de datos.

## Un uso importante de los atributos es la definición de factores. Los factores son vectores que 
## contienen valores predefinidos y son empleados para alojar datos categóricos. 

set.seed(140)	# set.seed permite que el ejemplo sea replicable
num_samp <- rep(c(1,2),10) ; num_samp <- sample(num_samp, rep=F)
fac_ej <- factor(num_samp)

[1] 2 1 2 1 2 2 1 1 2 2 2 2 1 1 2 1 1 1 2 1
Levels: 1 2		# Levels nos muestra el conjunto de valores permitidos para el vector

## Para asignar una jerarquía a los niveles, podems indicar el argumento "levels" directamente
## en la función factor(). 
(fac_ej <- factor(fac_ej, levels=c("2","1")))	# Levels: 2 1

## NOTA: Los factores se verán con mayor profundidad en los temas relativos a manipulación de datos