# 30díasdegráficos con R - día 23 - Sunburst
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 23: Mi primer "Sunburst" codificado a mano gracias a coord_polar() y a los ejemplos de www.r-graph-gallery.com
# Se muestran la cantidad de casos de COVID-19 en los últimos 7 días en distrititos de interés
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia23.R
# 
library("tidyverse")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

# Para descarga de los datos actualizados
# covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')
# saveRDS(covid.data, './data/covid.casos.arg.Rda')
# Dependiendo del horario, la última fila puede ser un placeholder si datos
# covid.data <- covid.data[covid.data$dia_inicio != max(covid.data$dia_inicio),] 
covid.data <- readRDS('./data/covid.casos.arg.Rda')


last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))

provincias_de_interes <- c('Entre Ríos', 'Chaco', 'Córdoba', 'Santa Fe', 'Río Negro', 'Tucumán')

covid.data %>% 
  mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
  select(dia=dia_inicio, fecha, distrito=osm_admin_level_4, cantidad=nue_casosconf_diff) %>% 
  filter(dia >= max(dia) - 6,
         !(distrito %in% c('Indeterminado'))) %>% 
  arrange(distrito, dia) %>% 
  filter(distrito %in% provincias_de_interes) %>% 
  mutate(cantidad = ifelse(cantidad <= 0, 0, cantidad), nr=row_number()) -> plot_data


plot_data %>% 
  mutate(angle = 90 - 360 * (nr-0.5) / n(),
         hjust = ifelse( angle < -90, 1, 0),
         angle = ifelse(angle < -90, angle+180, angle),
         text = paste0(cantidad),
         text2 = max(dia)-dia) -> label_data

plot_data %>% 
  ggplot(aes(x=nr, y=cantidad+1, fill=distrito)) +
  geom_bar(stat="identity", alpha=0.5, color="black", size=.05) +
  ylim(-25,max(plot_data$cantidad)+2) +
  geom_hline(aes(yintercept=0)) +
  geom_text(data=label_data, aes(x=nr, y=cantidad+1.2, label=text, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle = label_data$angle, inherit.aes = FALSE ) +
  geom_text(data=label_data, aes(x=nr, y=-4, label=text2, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle = label_data$angle, inherit.aes = FALSE ) +
  coord_polar() +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = "COVID-19 en Argentina", 
       subtitle = paste0("Casos en los últimos 7 días por distrito al: ", last_date, "\nEntre Ríos, Chaco, Córdoba, Santa Fe, Río Negro y Tucumán"),  
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData",
       y = "",
       x = "") +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "bottom",
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.line= element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(.2,4), "cm"),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    ) 
