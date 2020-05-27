# 30díasdegráficos con R - día 16 - Waffle
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + geom_waffle + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 16: Una clasificación por edad de los casos de COVID-19 en Argentina, 
# usamos el paquete waffle y el geom_waffle
# # Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia16.R

library("tidyverse")
library("waffle")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}


covid.data <- read_delim('http://170.150.153.128/covid/covid_19_casos.csv', delim=";")

last_date <- max(covid.data$fis, na.rm = TRUE)

covid.data %>% 
  filter(clasificacion_resumen == 'Confirmado',
         sexo != 'NR') %>% 
  mutate(internado = !is.na(fecha_internacion),
         cui = !is.na(fecha_cui_intensivo),
         arm = replace_na(asist_resp_mecanica == "SI",FALSE),
         fallecido = 	replace_na(fallecido == "SI", FALSE),
         sexo = ifelse(sexo == 'M', 'Masculino', 'Femenino')) %>% 
  mutate(clasif_edad = case_when(edad_actual_anios <= 6 ~ '0 a 6',
                                 edad_actual_anios > 6 &  edad_actual_anios <= 14 ~ '7 a 14',
                                 edad_actual_anios > 14 & edad_actual_anios <= 35 ~ '15 a 35',
                                 edad_actual_anios > 35 & edad_actual_anios <= 65 ~ '36 a 65',                          
                                 edad_actual_anios > 65 ~ '>= 66',
                          TRUE ~ 'Indeterminado')) %>% 
  select(sexo, edad=edad_actual_anios, clasif_edad, internado, cui, arm, fallecido) %>% 
  group_by(sexo, clasif_edad) %>% 
  summarise(n = n()) %>%
  mutate(freq = round(100*(n / sum(n)),0),
         clasif_edad = factor(clasif_edad, c('0 a 6', '7 a 14', '15 a 35', '36 a 65', '>= 66'))) %>%
  ggplot(aes(fill = clasif_edad, values = freq)) +
  geom_waffle(n_rows = 10, size = 0.33, colour = "white", flip = TRUE) +
 
  scale_y_continuous(labels = function(x) x * 10) +
  coord_equal() +
  facet_wrap(~ sexo) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9", "#009E73",  "#0072B2", "#D55E00", "#CC79A7")) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Clasificación etaria de infectados (al: ", last_date, ")\n"), 
       caption = "Fuente: datos.gob.ar",
       y = "",
       x = "") +
  theme(axis.text.x=element_blank())
  