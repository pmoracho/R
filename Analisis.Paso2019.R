library("paso2019")
library("tidyverse")

# Meta_Agrupaciones
descripcion_postulaciones %>%
  distinct(NOMBRE_AGRUPACION) %>%
  mutate(id = row_number()) %>%
  select(id_meta_agrupacion = id,
         nombre_meta_agrupacion = NOMBRE_AGRUPACION) -> meta_agrupaciones

# Agrupaciones
descripcion_postulaciones %>%
  distinct(CODIGO_AGRUPACION, NOMBRE_AGRUPACION) %>%
  left_join(meta_agrupaciones, by = c("NOMBRE_AGRUPACION" = "nombre_meta_agrupacion")) %>%
  mutate(id = row_number()) %>%
  select(id_agrupacion = id,
         id_meta_agrupacion,
         codigo_agrupacion = CODIGO_AGRUPACION) -> agrupaciones

# Categorias
descripcion_postulaciones %>%
  distinct(CODIGO_CATEGORIA, NOMBRE_CATEGORIA) %>%
  mutate(id = row_number()) %>%
  select(id_categoria = id,
         codigo_categoria = CODIGO_CATEGORIA,
         nombre_categoria = NOMBRE_CATEGORIA) -> categorias

# Listas
descripcion_postulaciones %>%
  distinct(CODIGO_LISTA, NOMBRE_LISTA, CODIGO_AGRUPACION) %>%
  left_join(agrupaciones, by = c("CODIGO_AGRUPACION" = "codigo_agrupacion")) %>%
  mutate(id = row_number()) %>%
  select(id_lista = id,
         id_agrupacion,
         codigo_lista = CODIGO_LISTA,
         nombre_lista = NOMBRE_LISTA) -> listas

# Distritos
mesas_totales %>%
  distinct(CODIGO_DISTRITO) %>%
  left_join(descripcion_regiones, by = c('CODIGO_DISTRITO' = "CODIGO_REGION")) %>%
  mutate(id = row_number()) %>%
  select(id_distrito = id,
         codigo_distrito = CODIGO_DISTRITO,
         nombre_distrito = NOMBRE_REGION) -> distritos

# Secciones
mesas_totales %>%
  distinct(CODIGO_SECCION) %>%
  left_join(descripcion_regiones, by = c('CODIGO_SECCION' = "CODIGO_REGION")) %>%
  mutate(id = row_number()) %>%
  select(id_seccion = id,
         codigo_seccion = CODIGO_SECCION ,
         nombre_seccion = NOMBRE_REGION) -> secciones

# Circuitos
mesas_totales %>%
  distinct(CODIGO_CIRCUITO) %>%
  left_join(descripcion_regiones, by = c('CODIGO_CIRCUITO' = "CODIGO_REGION")) %>%
  mutate(id = row_number()) %>%
  select(id_circuito = id,
         codigo_circuito = CODIGO_CIRCUITO,
         nombre_circuito = NOMBRE_REGION) -> circuitos

# Mesas
mesas_totales %>%
  distinct(CODIGO_MESA, CODIGO_DISTRITO, CODIGO_SECCION, CODIGO_CIRCUITO) %>%
  left_join(distritos, by = c('CODIGO_DISTRITO' = "codigo_distrito")) %>%
  left_join(secciones, by = c('CODIGO_SECCION' = "codigo_seccion")) %>%
  left_join(circuitos, by = c('CODIGO_CIRCUITO' = "codigo_circuito")) %>%
  mutate(id = row_number(),
         escrutada = NA) %>%
  select(id_mesa = id,
         id_distrito,
         id_seccion,
         id_circuito,
         codigo_mesa = CODIGO_MESA,
         escrutada)  -> mesa

# votos
mesas_totales_lista %>%
  distinct(CODIGO_MESA, CODIGO_CATEGORIA, CODIGO_LISTA, VOTOS_LISTA) %>%
  left_join(categorias, by=c("CODIGO_CATEGORIA" = "codigo_categoria")) %>%
  left_join(listas, by=c("CODIGO_LISTA" = "codigo_lista")) %>%
  left_join(mesas, by=c("CODIGO_MESA" = "codigo_mesa")) %>%
  mutate(id = row_number()) %>%
  select(id_voto = id,
         id_mesa,
         id_categoria,
         id_lista,
         votos = VOTOS_LISTA) -> votos

# Totales de votos por categoria
votos %>%
  group_by(id_categoria) %>%
  summarise(votos = sum(votos)) %>%
  left_join(categorias, by = "id_categoria") %>%
  select(id_categoria,
         codigo_categoria,
         nombre_categoria,
         votos_totales = votos)  %>%
  as.data.frame() -> categorias


glimpse(meta_agrupaciones)
glimpse(agrupaciones)
glimpse(categorias)
glimpse(listas)
glimpse(mesas)
glimpse(votos)
glimpse(distritos)
glimpse(secciones)
glimpse(circuitos)

usethis::use_data(meta_agrupaciones, overwrite = TRUE)
usethis::use_data(agrupaciones, overwrite = TRUE)
usethis::use_data(categorias, overwrite = TRUE)
usethis::use_data(listas, overwrite = TRUE)
usethis::use_data(mesas, overwrite = TRUE)
usethis::use_data(votos, overwrite = TRUE)
usethis::use_data(distritos, overwrite = TRUE)
usethis::use_data(secciones, overwrite = TRUE)
usethis::use_data(circuitos, overwrite = TRUE)





######################

library(paso2019)

mesas %>%
  group_by(escrutada) %>%
  summarize(cantidad=n()) %>%
  mutate(escrutada = reorder(ifelse(escrutada, "Si", "No"), -cantidad),
         prop = round(cantidad / nrow(mesas),2),
         lab.ypos = cumsum(cantidad) - 0.5 * cantidad) %>%
  ggplot(aes(x = "", y = cantidad, fill = escrutada)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "white") +
  #coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_void()

ggplot(aes(x=escrutada, y=cantidad, fill=escrutada)) +
  geom_col() +
  coord_polar()

# Mesas escrutadas con ningun voto en todeas las categorias
mesas %>%
  filter(escrutada == TRUE) %>%
  left_join(votos, by = "id_mesa") %>%
  left_join(listas, by = "id_lista") %>%
  left_join(agrupaciones, by = "id_agrupacion") %>%
  group_by(id_mesa, id_meta_agrupacion, id_categoria) %>%
  summarize(votos = sum(votos)) %>%
  filter(votos == 0) %>%
  ungroup() -> mesas_sin_votos

mesas_sin_votos %>%
  filter(id_meta_agrupacion %in% c(5, 9)) %>%
  group_by(id_mesa, id_meta_agrupacion) %>%
  summarize(mesas_con_0_votos = n())

mesas_sin_votos %>%
  left_join(meta_agrupaciones, by = "id_meta_agrupacion") %>%
  group_by(id_meta_agrupacion, nombre_meta_agrupacion) %>%
  summarize(mesas_con_0_votos = n()) %>%
  filter(id_meta_agrupacion %in% c(5, 9))

mesas %>%
  filter(escrutada == TRUE) %>%
  left_join(votos, by = "id_mesa") %>%
  left_join(listas, by = "id_lista") %>%
  left_join(agrupaciones, by = "id_agrupacion") %>%
  left_join(meta_agrupaciones, by = "id_meta_agrupacion") %>%
  group_by(id_mesa, codigo_mesa, id_meta_agrupacion, nombre_meta_agrupacion) %>%
  summarize(votos = sum(votos)) %>%
  filter(votos == 0, id_meta_agrupacion %in% c(5, 9)) %>%
  mutate(url = get_telegrama_url(codigo_mesa)) %>%
  View()


mesas %>%
  left_join(votos, by = "id_mesa") %>%
  left_join(categorias, by = "id_categoria") %>%
  left_join(listas, by = "id_lista") %>%
  left_join(agrupaciones, by = "id_agrupacion") %>%
  filter(nombre_categoria == "Presidente y Vicepresidente de la República",
         id_meta_agrupacion %in% c(5, 9)) %>%
  group_by(id_mesa, id_meta_agrupacion) %>%
  summarize(votos = sum(votos)) %>%
  filter(votos == 0) %>%
  group_by(id_meta_agrupacion) %>%
  summarize(votos = n())


mesas_totales_agrp_politica %>% glimpse()

mesas_totales_lista %>%
  filter(CODIGO_MESA == "0100100011X") %>%

mesas_totales_agrp_politica %>%
  filter(CODIGO_AGRUPACION %in% c("135", "136") & CODIGO_CATEGORIA == "000100000000000") %>%
  group_by(CODIGO_MESA) %>%
  summarize(FT=sum(ifelse(CODIGO_AGRUPACION == 135, VOTOS_AGRUPACION,0)),
            JxC=sum(ifelse(CODIGO_AGRUPACION == 136, VOTOS_AGRUPACION,0))) %>%
  filter(FT == 0 | JxC ==0) %>%
  ungroup() %>%
  summarize(FT = sum(FT==0), JxC = sum(JxC==0))


mesas_totales_agrp_politica %>%
  summarize(max = max(VOTOS_AGRUPACION))
  filter(CODIGO_AGRUPACION %in% c("135", "136") & CODIGO_CATEGORIA == "000100000000000")

summarize(c=n()) %>%
  filter(c > 1)

filter(CODIGO_MESA == "0200100009X")





  mesas_totales_lista %>%
  filter(CODIGO_MESA == "0100100011X") %>%


  agrupaciones %>%
  View()

descripcion_postulaciones %>%
  glimpse()

mesas_totales_agrp_politica %>%
  filter(CODIGO_AGRUPACION %in% c("01-502", "136")) %>%
  group_by(CODIGO_MESA, CODIGO_AGRUPACION, CODIGO_CATEGORIA) %>%
  summarise(VOTOS_AGRUPACION = sum(VOTOS_AGRUPACION)) %>%
  filter(VOTOS_AGRUPACION == 0, CODIGO_AGRUPACION %in% c("135", "136")) %>%
  View()

agrupaciones %>%
  left_join(meta_agrupaciones) %>%
  filter(codigo_agrupacion == "135")

mesas_totales_agrp_politica %>%
  filter(CODIGO_AGRUPACION %in% c("136", "02-503")) %>%
  filter(CODIGO_MESA == '0200100001X') %>%
  arrange(CODIGO_MESA) %>%
  select(CODIGO_MESA, CODIGO_AGRUPACION, VOTOS_AGRUPACION)

summarize(cant=n())


agrupaciones %>%
  left_join(meta_agrupaciones) %>%
  filter(nombre_meta_agrupacion == "FRENTE DE TODOS")


categorias %>%
  filter(id_categoria == 137)



2^rep(1:10)
