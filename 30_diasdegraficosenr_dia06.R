# 30díasdegráficos con R - día 6 - Donuts
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggrepel + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 6: Composición del 95% de los casos de covid-19 en Argentina por distritos 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia06.R

library("tidyverse")
library("ggrepel")

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
break_porc <- .95
covid.data %>% 
  select(distrito=osm_admin_level_4, casos=nue_casosconf_diff) %>% 
  group_by(distrito) %>% 
  summarise(casos=sum(casos, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(porc = casos / sum(casos)) %>%
  arrange(-porc) %>% 
  mutate(cporc = cumsum(porc),
         distrito = ifelse(cporc < break_porc, distrito, 'Resto')) %>% 
  group_by(distrito) %>% 
  summarise(casos = sum(casos),
            porc = sum(round(porc*100,2))) %>% 
  arrange(-porc) -> data

data$porc[data$distrito == 'Resto'] <- 100 - sum(data$porc[data$distrito != 'Resto'])
data %>% 
  mutate( ymax = cumsum(porc),
          ymin = lag(ymax, default=0)
  ) -> data

mac_perc <- sum(data$porc[data$distrito != 'Resto'])
data$distrito <- with(data, reorder(distrito, porc))

fct_reorder(data$distrito, data$porc)

data %>% 
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=distrito)) +
  geom_rect(color="white") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Distribución del ", mac_perc , "% de los casos por distrito\n (Datos al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData"
  ) +
  theme_elegante_std(base_family = "Ralleway") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  geom_label_repel(mapping = aes(x=3.5, y=ymin + (ymax - ymin)/2,
                               #colour =  "white",
                               label = paste0(distrito, ": ", format(porc, digits=2, trim=FALSE), "%\nCasos:", 
                                              format(casos, big.mark = ",", trim=FALSE))),
                 family = "Ralleway", 
                 nudge_y = 1,
                 nudge_x = 1) +
  scale_fill_viridis_d()

