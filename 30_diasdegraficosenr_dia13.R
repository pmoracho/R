# 30díasdegráficos con R - día 14 - Datos temporales
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggrepel + Algo de dplyr
# Font: Ralleway
# Data: https://apis.datos.gob.ar/series/api/series/?ids=168.1_T_CAMBIOR_D_0_0_26&limit=5000&format=csv
# Para #30díasdegráficos y #rstatsES. Día 13: Hoy hay poca imaginación, voy con un clásico, al menos en Argentina, la cotización
# del dólar en los últimos años, más información de contexto como los cambios de precidencia.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia13.R

library("tidyverse")
library("ggrepel")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

presidencias <- data.frame(fecha = as.Date(c('2019-12-10', '2015-12-10')), presidencia=c('Alberto Fernández', 'Mauricio Macri'))

dolar <- read.csv("https://apis.datos.gob.ar/series/api/series/?ids=168.1_T_CAMBIOR_D_0_0_26&limit=5000&format=csv", na.strings = "", fileEncoding = "UTF-8-BOM",
                       stringsAsFactors = FALSE)
dolar$indice_tiempo = as.Date(dolar$indice_tiempo, format = "%Y-%m-%d")
dolar %>% 
  ggplot(mapping = aes(x=indice_tiempo, y=tipo_cambio_bna_vendedor)) + 
  geom_line(size = 1, color="#67a9cf") +
  geom_vline(data = precidencias,
             mapping = aes(xintercept=fecha),
             color = "#ef8a62",
             linetype="dashed") +
  geom_point(data = presidencias,
             mapping = aes(x=fecha, y=10),
             color = "#ef8a62",
             size = 2) +
  geom_label_repel(data = presidencias,
                   mapping = aes(x=fecha, y=10, label = presidencia), 
                   hjust= -1,
                   color = "#ef8a62",
                   family = "Ralleway", fontface = 'bold',
                   arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first")
  ) +      
  
  
  labs(title = paste("Dólar en Argentina"), 
       subtitle = paste("Cotización del Banco nación entre el", min(dolar$indice_tiempo), "y el" , max(dolar$indice_tiempo)),
       caption = "Fuente: https://datos.gob.ar/", 
       y = "Cotización en $", 
       x = ""
  ) +
  scale_x_date(date_breaks = "12 month", date_labels="%Y-%m") +
  theme_elegante_std(base_family = "Ralleway") 