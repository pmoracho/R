# 30díasdegráficos con R - día 14 - Treemap
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + treemapify + Algo de dplyr
# Font: Ralleway
# Data: https://apis.datos.gob.ar/series/api/series/?ids=168.1_T_CAMBIOR_D_0_0_26&limit=5000&format=csv
# Para #30díasdegráficos y #rstatsES. Día 14: Un treemap usando ggplot + treemapify
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia14.R

library("tidyverse")
library("ggrepel")
library("treemapify")

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

dias %>% 
  left_join(covid.data, by=c("distrito" = "osm_admin_level_4", "dia" = "dia_inicio")) %>% 
  mutate(casos = replace_na(nue_casosconf_diff, 0)) %>% 
  select(distrito, casos)  %>% 
  filter(distrito != 'Indeterminado') %>% 
  group_by(distrito) %>% 
  summarise(casos = sum(casos)) %>% 
  arrange(-casos) %>% 
  mutate(porc = cumsum(casos/sum(casos))) %>% 

  ggplot(aes(area = casos, fill = casos, label=paste0(distrito, ": ", format(casos, big.mark = ".", decimal.mark = ",")),
             subgroup = ifelse(porc <= .85, "85%", ""))) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = .5, colour =
                              "White", fontface = "italic", min.size = 0) +
  geom_treemap_text(place = "middle", grow = T, reflow = T, alpha = 0.8, colour = "black", family= "Ralleway",
                    padding.x = grid::unit(3, "mm"),
                    padding.y = grid::unit(3, "mm")) +
  theme_elegante_std(base_family = "Ralleway") +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Distribución de los casos por distritos al: ", last_date), 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData"
  ) +
  scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "white") +
  theme(legend.position = "none")