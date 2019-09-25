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
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#0073C2FF", "#868686FF")) +
  theme_void()

mesas %>%
  group_by(escrutada) %>%
  summarize(cantidad=n()) %>%
  mutate(escrutada = reorder(ifelse(escrutada, "Si", "No"), -cantidad),
         prop = round(cantidad / nrow(mesas),2),
         lab.ypos = cumsum(cantidad) - 0.5 * cantidad) %>%
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

votos %>%
  left_join(categorias) %>%
  filter(codigo_categoria == "000100000000000") %>%
  left_join(listas) %>%
  left_join(agrupaciones) %>%
  filter(codigo_agrupacion %in% c("135", "136")) %>%
  group_by(id_mesa) %>%
  summarize(FT=sum(ifelse(codigo_agrupacion == 135, votos,0)),
            JxC=sum(ifelse(codigo_agrupacion == 136, votos,0))) %>%
  filter(FT == 0 | JxC ==0) %>%
  ungroup() %>%
  summarize(FT = sum(FT==0), JxC = sum(JxC==0))


mesas_totales_agrp_politica %>%
  filter(CODIGO_AGRUPACION %in% c("135", "136") & CODIGO_CATEGORIA == "000100000000000") %>%
  group_by(CODIGO_MESA) %>%
  summarize(JxX=sum(ifelse(CODIGO_AGRUPACION == 135, VOTOS_AGRUPACION,0)),
            FdT=sum(ifelse(CODIGO_AGRUPACION == 136, VOTOS_AGRUPACION,0))) %>%
  ungroup() -> m1

votos %>%
  left_join(categorias) %>%
  filter(codigo_categoria == "000100000000000") %>%
  left_join(listas) %>%
  left_join(mesas) %>%
  left_join(agrupaciones) %>%
  filter(codigo_agrupacion %in% c("135", "136")) %>%
  group_by(codigo_mesa) %>%
  summarize(JxC=sum(ifelse(codigo_agrupacion == 135, votos,0)),
            FdT=sum(ifelse(codigo_agrupacion == 136, votos,0))) %>%
  ungroup() -> m2


votos %>%
  left_join(categorias) %>%
  filter(codigo_categoria == "000100000000000") %>%
  left_join(listas) %>%
  left_join(mesas) %>%
  left_join(agrupaciones) %>%
  filter(codigo_agrupacion %in% c("135", "136")) %>%
  filter(codigo_mesa == "0206300917X")

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

mesas %>%
  left_join(votos) %>%
  mutate(escrutada = votos != 0) -> mesas

mesas %>%  head()


mesas %>%
  filter(codigo_mesa == "0206300917X")


mesa %>%  glimpse()
mesas_totales_lista %>%
  filter(CODIGO_MESA == "0206300917X")

mesas_totales_agrp_politica %>%
  filter(CODIGO_MESA == "0206300917X")


mesas_totales_agrp_politica %>%
  filter(CODIGO_AGRUPACION %in% c("135", "136") & CODIGO_CATEGORIA == "000100000000000") %>%
  filter(CODIGO_MESA == "0206300917X")



m1 %>%
  anti_join(m2, by=c("CODIGO_MESA" = "codigo_mesa"))

  group_by(CODIGO_MESA) %>%
  summarise(n=row_number()) %>%
  filter(n> 1)





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

secciones %>% head()

votos %>%
  left_join(mesas, by = "id_mesa") %>%
  left_join(distritos, by = "id_distrito") %>%
  left_join(secciones, by = "id_seccion") %>%
  left_join(listas, by = "id_lista") %>%
  left_join(agrupaciones, by = "id_agrupacion") %>%
  filter(id_categoria == 137) %>%
  group_by(nombre_distrito, nombre_seccion) %>%
  summarize(JxC=sum(ifelse(codigo_agrupacion == 135, votos,0)),
            FdT=sum(ifelse(codigo_agrupacion == 136, votos,0))) %>%
  mutate(diferencia = FdT - JxC) %>%
  arrange(-diferencia) %>%
  ungroup() -> votos_x_distritos_secciones


votos_x_distritos_secciones %>%
  arrange(diferencia) %>%
  mutate(diferencia = abs(diferencia),
         acumulado=cumsum(diferencia)) %>%
  head(10)

votos_x_distritos_secciones %>%
  arrange(-diferencia) %>%
  mutate(diferencia = abs(diferencia),
         acumulado=cumsum(diferencia)) %>%
  head(10)


total <- 0
modelo_nuevo <- c('agrupaciones','categorias','circuitos','distritos','listas','mesas','meta_agrupaciones','secciones','votos')
modelo_original <- c('descripcion_postulaciones','descripcion_regiones','mesas_totales','mesas_totales_lista','mesas_totales_agrp_politica')
for (thing in modelo_original) {
  total <- total + object.size(get(thing))
}
print(total,units='auto')

usethis::use_gpl3_license()
