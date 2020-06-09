# 30díasdegráficos con R - día 28 - Chordiagram
# Para #30díasdegráficos y #rstatsES. Día 28: 
  # Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia28.R

library("circlize")
library("tidyverse")

# Para descarfa de los datos acctualizados
# covid.data <- read.csv(file='https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-16")
# saveRDS(covid.data, './data/covid.arg.Rda')
covid.data <- readRDS('./data/covid.arg.Rda')
last_date <- max(covid.data$fecha_apertura, na.rm = TRUE)

covid.data %>% 
  filter(clasificacion_resumen == 'Confirmado',
         !is.na(edad),
         sexo != 'NR') %>% 
  mutate(internado = !is.na(fecha_internacion),
         cui = !is.na(fecha_cui_intensivo),
         arm = replace_na(asistencia_respiratoria_mecanica == "SI",FALSE),
         fallecido = 	replace_na(fallecido == "SI", FALSE),
         sexo = ifelse(sexo == 'M', 'Masculino', 'Femenino'),
         internado = ifelse(fecha_internacion != "", 'Internado', 'Ambulatorio'),
         fallecido = ifelse(fallecido == "SI", 'Fallecido', 'Recuperado')) %>% 
  mutate(clasif_edad = case_when(edad <= 6 ~ '0 a 6',
                                 edad > 6 &  edad <= 14 ~ '7 a 14',
                                 edad > 14 & edad <= 35 ~ '15 a 35',
                                 edad > 35 & edad <= 65 ~ '36 a 65',                          
                                 edad > 65 ~ '>= 66')) %>% 
  select(sexo, edad, clasif_edad, internado, cui, arm, fallecido) %>%
  mutate(clasif_edad = factor(clasif_edad, c('0 a 6', '7 a 14', '15 a 35', '36 a 65', '>= 66')),
         internado = factor(internado, c("Ambulatorio", "Internado")),
         fallecido = factor(fallecido, c("Recuperado", "Fallecido"))
  ) %>% 
  group_by(clasif_edad, sexo, internado, fallecido) %>% 
  summarise(n = n()) -> plot_data

plot_data %>% 
  group_by(clasif_edad, sexo) %>% 
  summarize(n = sum(n)) %>% 
  select(from=clasif_edad, to=sexo, value=n) %>% 
  chordDiagram()
