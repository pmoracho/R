library("tidyverse")

# Todas las mesas
mesas_totales %>%
  distinct(CODIGO_MESA) %>%
  left_join(mesas_totales_agrp_politica %>%
              group_by(CODIGO_MESA) %>%
              summarise(ESCRUTADA = ifelse(sum(VOTOS_AGRUPACION)==0, FALSE, TRUE)),
            by = "CODIGO_MESA") -> mesas

# agrupaciones
descripcion_postulaciones %>%
  select(CODIGO_AGRUPACION, NOMBRE_AGRUPACION) %>%
  distinct() %>%
  arrange(NOMBRE_AGRUPACION) -> agrupaciones

# categorias
descripcion_postulaciones %>%
  select(CODIGO_CATEGORIA, NOMBRE_CATEGORIA ) %>%
  distinct() %>%
  arrange(NOMBRE_CATEGORIA) -> categorias

# listas
descripcion_postulaciones %>%
  select(CODIGO_AGRUPACION, CODIGO_LISTA, NOMBRE_LISTA) %>%
  distinct() %>%
  arrange(NOMBRE_LISTA) -> listas

# Mesas totales x agrupacion
mesas_totales_agrp_politica %>%
  select(CODIGO_AGRUPACION, CODIGO_CATEGORIA, CODIGO_MESA, VOTOS_AGRUPACION) -> agrupacion_categoria_mesas

# listas + categorias + mesas
mesas_totales_lista %>%
  select(CODIGO_LISTA, CODIGO_CATEGORIA, CODIGO_MESA, VOTOS_AGRUPACION) -> agrupacion_listas_mesas


mesas_totales_agrp_politica %>%
  group_by(CODIGO_MESA) %>%
  summarise(ESCRUTADA = ifelse(sum(VOTOS_AGRUPACION)==0, FALSE, TRUE))

mesas_totales_lista %>%
  group_by(CODIGO_MESA) %>%
  summarise(VOTOS = sum(VOTOS_LISTA)) %>%
  filter(VOTOS == 0)


# Total de mesas Escrutadas Si/No
mesas %>%
  group_by(ESCRUTADA) %>%
  summarise(Cantidad = n()) %>%
  mutate(Escrutadas = ifelse(ESCRUTADA, "Si", "No"),
         TotalMesas = nrow(mesas),
         Porcentaje = Cantidad/TotalMesas) %>%
  select(Escrutadas, Cantidad, Porcentaje) %>%
  as.data.frame()


mesas_totales_agrp_politica %>%
  filter(CODIGO_MESA == "0100100001X") %>%
  mutate(URL=get_telegrama_url(CODIGO_MESA)) %>%
  group_by(CODIGO_CATEGORIA, URL) %>%
  summarise(VOTOS=sum(VOTOS_AGRUPACION))



# Mesas totales x lista
mesas_totales_agrp_politica %>%
  select(CODIGO_CATEGORIA,  CODIGO_AGRUPACION, CODIGO_MESA, VOTOS_AGRUPACION) -> mesas_totales_x_agrupacion

# Mesas con votos para Presidente y Vicepresidente de la República
mesas_totales_x_agrupacion %>%
  inner_join(agrupaciones, by = "CODIGO_AGRUPACION") %>%
  inner_join(categorias, by = "CODIGO_CATEGORIA") %>%
  inner_join(mesas, by = "CODIGO_MESA") %>%
  filter(CODIGO_CATEGORIA == '000100000000000',
         ESCRUTADA == TRUE,
         VOTOS_AGRUPACION == 0) %>%
  group_by(NOMBRE_AGRUPACION) %>%
  summarize(Cantidad = n()) %>%
  arrange(Cantidad)

# Mesas con votos 0 para Gobernador y Vice Gobernador Buenos Aires
mesas_totales_x_agrupacion %>%
  inner_join(agrupaciones, by = "CODIGO_AGRUPACION") %>%
  inner_join(categorias, by = "CODIGO_CATEGORIA") %>%
  inner_join(mesas, by = "CODIGO_MESA") %>%
  filter(CODIGO_CATEGORIA == '000402000000000',
         ESCRUTADA == TRUE,
         VOTOS_AGRUPACION == 0) %>%
  group_by(NOMBRE_AGRUPACION) %>%
  summarize(Cantidad = n()) %>%
  arrange(Cantidad)

# Votos por agrupación y categoría
mesas_totales_x_agrupacion %>%
  group_by(CODIGO_AGRUPACION, CODIGO_CATEGORIA) %>%
  summarize(VOTOS_AGRUPACION = sum(VOTOS_AGRUPACION)) %>%
  inner_join(agrupaciones, by = "CODIGO_AGRUPACION") %>%
  inner_join(categorias, by = "CODIGO_CATEGORIA") %>%
  arrange(CODIGO_CATEGORIA, -VOTOS_AGRUPACION)

head(mesas_totales_agrp_politica)

# Mesas con votos para el JUNTOS POR EL CAMBIO en 0
mesas_totales_x_agrupacion %>%
  inner_join(agrupaciones) %>%
  inner_join(categorias) %>%
  inner_join(mesas) %>%
  filter(CODIGO_CATEGORIA == '000100000000000',
         ESCRUTADA == TRUE,
         NOMBRE_AGRUPACION == "JUNTOS POR EL CAMBIO",
         VOTOS_AGRUPACION == 0) %>%
  mutate(url=get_telegrama_url(CODIGO_MESA )) %>%
  View()







mesas_totales_agrp_politica %>%
  left_join(descripcion_postulaciones, by = c("CODIGO_CATEGORIA", "CODIGO_AGRUPACION")) %>%
  inner_join(mesas, by = "CODIGO_MESA") %>%
  filter(ESCRUTADA,
         NOMBRE_AGRUPACION == 'FRENTE DE TODOS',
         VOTOS_AGRUPACION == 0) %>%
  select(CODIGO_MESA) %>%
  distinct() %>%
  mutate(url=get_telegrama_url(CODIGO_MESA )) %>%
  head()
  left_join(descripcion_postulaciones, by = c("CODIGO_CATEGORIA", "CODIGO_AGRUPACION")) %>%

mesas_totales_lista %>%
  filter(CODIGO_MESA == "0600100659X") %>%
  select(CODIGO_LISTA,  NOMBRE_LISTA, VOTOS_LISTA) %>%
  arrange(NOMBRE_AGRUPACION)

  filter(CODIGO_MESA == "0600100659X") %>%



  filter(CODIGO_MESA == "0600100659X") %>%




  inner_join(mesas_totales_agrp_politica) %>%
  filter(CODIGO_MESA == "0600100659X")

str(mesas_totales)
str(mesas_totales_agrp_politica)
str(mesas_totales_lista)
str(descripcion_postulaciones)
str(mesas_totales_lista)
# descripcion_postulaciones <- read.table('data/descripcion_postulaciones.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")
# descripcion_regiones <- read.table('data/descripcion_regiones.dsv', header = TRUE, sep = '|', colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE, quote = "")

mesas_totales_agrp_politica <- read.table('ext-data/mesas_totales_agrp_politica.dsv',
                                          header = TRUE,
                                          sep = '|',
                                          colClasses = c(rep("character",6), "numeric"),
                                          encoding = "UTF-8",
                                          stringsAsFactors = FALSE,
                                          quote = "")
usethis::use_data(mesas_totales_agrp_politica, overwrite = TRUE)

mesas_totales_lista <- read.table('ext-data/mesas_totales_lista.dsv',
                                  header = TRUE,
                                  sep = '|',
                                  colClasses = c(rep("character",7), "numeric"),
                                  encoding = "UTF-8",
                                  stringsAsFactors = FALSE,
                                  quote = "")
str(mesas_totales_lista)
usethis::use_data(mesas_totales_lista, overwrite = TRUE)

mesas_totales <- read.table('ext-data/mesas_totales.dsv',
                            header = TRUE,
                            sep = '|',
                            colClasses = c(rep("character",6), "numeric"),
                            encoding = "UTF-8",
                            stringsAsFactors = FALSE,
                            quote = "")

str(mesas_totales)
usethis::use_data(mesas_totales, overwrite = TRUE)
