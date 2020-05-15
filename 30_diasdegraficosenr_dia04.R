# 30díasdegráficos con R - día 4- Facetas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0
# Para #30díasdegráficos y #rstatsES. Día 4: Una comparativa de la evolución de casos diarios de COVID-19
# en los 9 distritos, actualmente, con más casos de Argentina.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia04.R

library("tidyverse")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))

covid.data %>% 
  mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
  select(dia=dia_inicio, distrito=osm_admin_level_4, cantidad=nue_casosconf_diff) -> data

data %>% 
  inner_join(data %>% 
               group_by(distrito) %>% 
               summarize(cantidad = sum(cantidad)) %>% 
               arrange(-cantidad) %>% 
               top_n(9), by = c("distrito"), suffix=c("",".y")) %>% 
  ggplot(mapping=aes(x=dia, y=cantidad)) +
  geom_line(color="#67a9cf") +
  geom_point(color="#67a9cf") +
  geom_smooth(method = 'loess',
              formula = 'y ~ x', alpha = 0.2, size = 1, span = .3, se=FALSE, color="#ef8a62") + 
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Variación de casos diarios en los distritos con más casos (al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Cantidades", 
       x = "Número de días desde el 1er caso"
  ) +
  facet_wrap(~distrito,scales="free") +
  theme_elegante_std(base_family = "Ralleway") 
