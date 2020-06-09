# 30díasdegráficos con R - día 2 - líneas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0
# Para #30díasdegráficos y #rstatsES. Día 2: Una comparativa de la evolución de casos diarios de COVID-19
# en los dos distritos más populosos de Argntina. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia02.R
 
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
covid.data <- readRDS('./data/covid.casos.arg.Rda')
last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))

covid.data %>% 
  filter(osm_admin_level_4 %in% c('CABA', 'Buenos Aires')) %>% 
  mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
  select(dia=dia_inicio, distrito=osm_admin_level_4, cantidad=nue_casosconf_diff) %>% 
  ggplot(mapping=aes(x=dia, color=distrito, y=cantidad)) +
    geom_line() +
    geom_point() +
    geom_smooth(method = 'loess',
                formula = 'y ~ x', alpha = 0.2, size = 1, span = .3, se=FALSE) + 
    labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Variación de los casos diarios en CABA y Buenos Aires por día (al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Casos", 
       x = "Número de días desde el 1er caso"
  ) +
  scale_color_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  theme_elegante_std(base_family = "Ralleway") 
