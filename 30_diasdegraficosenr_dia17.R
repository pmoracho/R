# 30díasdegráficos con R - día 16 - Sankey/alluvial
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggalluvial + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 17: Una grafico Sankey o alluvial de como es la evolución de los casos de COVID-19 en Argentina, 
# por grupo etario y sex0.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia17.R

library("tidyverse")
library("ggalluvial")
if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

# covid.data <- read_delim('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv', delim=";")
# covid.data <- read.csv(file="~/Descargas/Covid19Casos.csv", sep=",", nrows = 10)
# 
# covid.casos<- read.csv(file="~/Descargas/Covid19Casos.csv", sep=",", 
#                        colClasses=c("NULL", NA, NA, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", NA, "NULL", NA, NA, "NULL", NA, "NULL", "NULL", "NULL", NA, "NULL", "NULL", "NULL", NA)
#                        )
# glimpse(covid.data)
# saveRDS(covid.casos, './data/covid.casos.Rda')
# 
# cat(ifelse(names(covid.casos) %in% c("clasificacion_resumen", "edad", "ultima_actualizacion", "sexo", "fecha_internacion",
#                                      "fecha_cui_intensivo", "asistencia_respiratoria_mecanica", "fallecido"), NA, '"NULL"'),
#     sep = ", ")


covid.casos = readRDS('./data/covid.casos.Rda')

last_date <- max(covid.casos$ultima_actualizacion, na.rm = TRUE)

covid.casos %>% 
  filter(clasificacion_resumen == 'Confirmado',
         !is.na(edad),
         sexo != 'NR') %>% 
  mutate(internado = !is.na(fecha_internacion),
         cui = !is.na(fecha_cui_intensivo),
         arm = replace_na(asistencia_respiratoria_mecanica == "SI",FALSE),
         fallecido = 	replace_na(fallecido == "SI", FALSE),
         sexo = ifelse(sexo == 'M', 'Masculino', 'Femenino'),
         internado = ifelse(internado, 'Internado', 'Ambulatorio'),
         fallecido = ifelse(fallecido, 'Fallecido', 'Recuperado')) %>% 
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
  ggplot(mapping=aes(y = n,
                     axis1 = fallecido, axis2 = internado, axis3 = sexo, axis4 = clasif_edad)) +
  geom_alluvium(aes(fill = clasif_edad),
                color = "Gray30",
                width = 0, knot.pos = 0.1, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE, color="gray60", linetype = 3) +
  geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:4, labels = c("Fallecido", "Internado", "Sexo", "Edad")) +
  coord_flip() +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Evolución de los caso por edad y sexo (al: ", last_date, ")") , 
         caption = "Fuente: datos.gob.ar", 
       y = "Casos Confirmados", 
       x = ""
  ) +
  scale_fill_manual(values = c("firebrick3", "darkorange", "deepskyblue3", "darkorchid1", "seagreen")) +
  scale_color_manual(values = rep("Black",5)) +
  theme_elegante_std(base_family = "Ralleway")
