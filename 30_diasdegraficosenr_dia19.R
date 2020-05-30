# 30díasdegráficos con R - día 19 - streamgraph
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggTimeSeries + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 19: Un streamgaph de ggTimeSeries mosrtrando el crecimiento diarios de casos
# en las 6 provincias con mayor número de casos.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia19.R

library("tidyverse")
library("ggTimeSeries")

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

provincias_de_interes <- c('CABA', 'Buenos Aires', 'Chaco','Córdoba', 'Río Negro', 'Tucumán')
dias %>% 
  left_join(covid.data, by=c("distrito" = "osm_admin_level_4", "dia" = "dia_inicio")) %>% 
  mutate(casos = replace_na(nue_casosconf_diff, 0)) %>% 
  select(dia, distrito, casos)  %>% 
  arrange(distrito, dia) %>% 
  filter(distrito %in% provincias_de_interes) %>% 
  ggplot(aes(x = dia, y = casos, group = distrito, fill = distrito)) +
  stat_steamgraph(color="gray30", size=.1, alpha=.8) +
  theme_elegante_std(base_family = "Ralleway") +
  scale_fill_brewer(palette="Set1") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Variación de los casos diarios (al: ", last_date, ")\nLas 6 Provincias con mayor número de casos") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Casos", 
       x = "Número de días desde el 1er caso"
  ) +
  guides(fill = guide_legend(nrow = 1))