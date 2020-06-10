# 30díasdegráficos con R - día 30 - Areas polares
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0
# Para #30díasdegráficos y #rstatsES. Día 30: Y el desafío llegó a su fin, en este caso un muy sencillo gráfico de 
# área polar con ggplot puro(muy simple), gracias gente, hemos aprendido mucho.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia30.R

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
  filter(osm_admin_level_4 %in% c('CABA', 'Buenos Aires', 'Chaco', 'Río Negro', 'Córdoba')) %>% 
  group_by(distrito = osm_admin_level_4) %>% 
  summarise(casos = sum(nue_casosconf_diff), fallecidos=sum(nue_fallecidos_diff)) %>% 
  pivot_longer(-distrito) %>% 
  ggplot(aes(x = distrito, y=value, fill = name)) +
  geom_col(width = 1, color = "black") +
  scale_y_sqrt() +
  coord_polar(start=3*pi/2) +
  scale_fill_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Total de casos y fallecidos por distrito (al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "", 
       x = ""
  ) +
  theme(axis.text.y = element_blank())
  