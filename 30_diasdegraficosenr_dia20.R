# 30díasdegráficos con R - día 20 - Redes
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggraph + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 20: Un gráfico de redes para observar como se distribuyen las internaciones
# según el rango etario y el sexo.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia20.R

library("tidyverse")
library("ggraph")
library("igraph")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}
# Datos originales
# covid.data <- read.csv(file='https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv', stringsAsFactors = FALSE, fileEncoding = "UTF-16")
# Datos reproducibles
covid.data <- readRDS(url("https://github.com/pmoracho/R/raw/master/data/covid.arg.Rda","rb"))

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
         internado = ifelse(internado, 'Internado', 'Ambulatorio'),
         fallecido = ifelse(fallecido, 'Fallecido', 'Recuperado')) %>% 
  mutate(clasif_edad = case_when(edad <= 6 ~ '0 a 6',
                                 edad > 6 &  edad <= 14 ~ '7 a 14',
                                 edad > 14 & edad <= 35 ~ '15 a 35',
                                 edad > 35 & edad <= 65 ~ '36 a 65',                          
                                 edad > 65 ~ '>= 66')) %>% 
  select(sexo, edad, clasif_edad, internado, cui, arm, fallecido) %>%
  group_by(clasif_edad, sexo, internado, fallecido) %>% 
  summarise(n = n()) -> plot_data


plot_data %>% 
  group_by(sexo, clasif_edad) %>% 
  summarise(n=sum(n)) %>% 
  select(from=sexo, to=clasif_edad, n) -> sexo_edad

plot_data %>% 
  group_by(internado, clasif_edad) %>% 
  summarise(n=sum(n)) %>% 
  select(to=internado, from=clasif_edad, n) -> internado_edad

internado_edad %>%
  rbind(sexo_edad) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = 'stress') +
  geom_edge_link(aes(edge_width=n), show.legend = TRUE, alpha=0.5, color = "#67a9cf") +
  geom_node_point(size=8, color ="#ef8a62") +
  geom_node_text(aes(label = name), repel = TRUE, family="Ralleway") +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("¿Cómo se distribuyen las internaciones entre el sexo y la edad?\nDatos al: ", last_date) , 
       caption = "Fuente: https://datos.gob.ar/", 
       y = "", 
       x = ""
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) 
