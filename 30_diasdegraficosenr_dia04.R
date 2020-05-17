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
       y = "Número de casos", 
       x = "Número de días desde el 1er caso"
  ) +
  facet_wrap(~distrito,scales="free") +
  theme_elegante_std(base_family = "Ralleway") 

library("geofaceteAR")

argentina_grid <-  data.frame(
  col = c(1, 3, 5, 1, 2, 1, 3, 4, 2, 2, 4, 1, 3, 3, 4, 1, 2, 2, 1, 1, 2, 1, 1, 1),
  row = c(1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 7, 8, 9, 10),
  code = c("AR-Y", "AR-P", "AR-N", "AR-A", "AR-T", "AR-K", "AR-H", "AR-W", "AR-G", "AR-X", "AR-E", "AR-F", "AR-S", "AR-B", "AR-C", "AR-J", "AR-D", "AR-L", "AR-M", "AR-Q", "AR-R", "AR-U", "AR-Z", "AR-V"),
  name_es = c("Jujuy", "Formosa", "Misiones", "Salta", "Tucumán", "Catamarca", "Chaco", "Corrientes", "Santiago del Estero", "Córdoba", 
              "Entre Ríos", "La Rioja", "Santa Fe", "Buenos Aires", "CABA", "San Juan", "San Luis", "La Pampa", "Mendoza", "Neuquén", "Río Negro", 
              "Chubut", "Santa Cruz", "Tierra del Fuego"),
  stringsAsFactors = FALSE
)
unique(data$distrito)
data %>% 
  filter(distrito!='Indeterminado') %>% 
  ggplot(mapping=aes(x=dia, y=cantidad)) +
  geom_line(color="#67a9cf") +
  geom_point(color="#67a9cf") +
  geom_smooth(method = 'loess',
              formula = 'y ~ x', alpha = 0.2, size = 1, span = .3, se=FALSE, color="#ef8a62") + 
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Variación de casos diarios en los distritos con más casos (al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Número de casos", 
       x = "Número de días desde el 1er caso"
  ) +
  facet_geo(~ distrito, grid = argentina_grid, scales = "free_y") +
  # facet_wrap(~ distrito, scales = "free_y") +
  theme_elegante_std(base_family = "Ralleway") -> p
  ggsave(filename = 'argentina.png', plot = p, device = "png", width = 10, height = 18)
