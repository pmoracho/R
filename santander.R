library(tidyverse)
library(readxl)
library(lubridate)
library(tm)
library(caret)

library(SnowballC)
library(minqa)
library(e1071)
library(fuzzyjoin)

xls <- read_excel("~/Descargas/UltimosMovimientos (11).xls", 
                 col_types = c("skip", "text", "text", 
                               "text", "text", "numeric", "numeric", 
                               "skip", "skip", "skip", "skip", 
                               "skip", "skip", "skip", 
                               "skip", "skip", "skip", 
                               "skip", "skip", "skip", 
                               "skip"), skip = 13)

xls %>% 
    setNames(c('fecha', 'orig', 'des', 'ref', 'monto', 'saldo')) %>% # Renombramos columnas
    mutate(fecha = dmy_hm(fecha)) %>%                                # Fecha a dtm
    filter(!is.na(fecha)) -> um                                      # borramos filas

read_excel("data/santander.grupos.xlsx") %>% 
    mutate(mes=month(dmy_hm(fecha))) %>% 
    select(mes, descripcion, grupo, importe) -> grupos 

DocumentTermMatrix(
    VCorpus(
        VectorSource(grupos$descripcion)),
        control = list(
            tolower = TRUE,
            stopwords = TRUE,
            removePunctuation = TRUE,
            language = "es-419"
            )
        ) %>% 
    as.matrix() %>% 
    as.data.frame() %>% 
    bind_cols(grupos) %>% 
    select(-one_of("importe", "mes", "descripcion")) -> dtm

dtm$grupo <- factor(dtm$grupo)

train <-  filter(dtm, !is.na(grupo))
test <-  filter(dtm, is.na(grupo))
glimpse(train)

fit <- train(grupo ~ ., 
             data = train,
             method = 'glm')
grupos %>% 
    filter(is.na(grupo)) %>% 
    mutate(grupo = predict(fit, test)) %>% 
    View()

