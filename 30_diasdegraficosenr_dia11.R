# 30díasdegráficos con R - día 11 - Mapas de calor
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplot2  + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 11 - Un mapa de calor de la evolución de los casos de COVID - 19
# en los distritos de Argentina, son los últimos 14 días, los colores se escalan en función al promedio
# de cada tres días. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia11.R

library("tidyverse")
library("zoo")
library("forcats")
library("scales")
  if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')
# Dependiendo del horario, la última fila puede ser un placeholder si datos
# covid.data <- covid.data[covid.data$dia_inicio != max(covid.data$dia_inicio),]

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))
first_date <- min(as.Date(covid.data$fecha,"%d/%m/%Y"))
ndias <- max(covid.data$dia_inicio)


dias <- expand.grid(distrito = unique(covid.data$osm_admin_level_4),
                    dia = 1:ndias,
                    stringsAsFactors = FALSE)
dias$fecha = first_date + dias$dia - 1
dias$casos = 0

dias_rolling <- 3
dias_ventana <- 14

dias %>% 
  left_join(covid.data, by=c("distrito" = "osm_admin_level_4", "dia" = "dia_inicio")) %>% 
  mutate(casos = replace_na(nue_casosconf_diff, 0)) %>% 
  select(dia, distrito, fecha=fecha.x, casos)  %>% 
  group_by(dia, distrito, fecha) %>% 
  summarise(casos = sum(casos)) %>%
  arrange(distrito, dia) %>% 
  group_by(distrito) %>% 
  filter(dia >= ndias - dias_ventana - dias_rolling,
         distrito != 'Indeterminado') %>% 
  mutate(roll_casos_3 = rollmean(casos, dias_rolling, align='right', fill=0),
         s_roll_casos_3 = replace_na((roll_casos_3-min(roll_casos_3))/(max(roll_casos_3)-min(roll_casos_3)),0),
         label_casos = casos
  ) %>% 
  filter(dia >= ndias - dias_ventana ) %>% 
  ggplot(aes(x = dia, y =   fct_reorder(distrito, casos), fill = s_roll_casos_3)) + 
    geom_tile(colour="gray80", size=0.2) +
    geom_text(aes(label=label_casos, color = s_roll_casos_3 > .7)) +
    scale_color_manual(guide = FALSE, values = c("gray30", "gray90")) +
    scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "white") +
    labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Evolución diaria de los casos por distrito (últimos ", dias_ventana, " días al: ", last_date, ")\n",
                         "Los números son casos diarios, la escala de color en función del promedio de casos de los últimos ", dias_rolling, " días") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       x = "Últimos días"
    ) +
    theme_elegante_std(base_family = "Ralleway") + 
    scale_x_continuous(breaks = seq(ndias - dias_ventana, ndias, by = 1)) +
    theme(axis.title.y=element_blank(),
        legend.position = "none") 
 