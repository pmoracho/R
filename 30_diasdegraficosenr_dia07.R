# 30díasdegráficos con R - día 4- Facetas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0
# Para #30díasdegráficos y #rstatsES. Día 4: Una comparativa de la evolución de casos diarios de COVID-19
# en los 9 distritos, actualmente, con más casos de Argentina.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia07.R

library("tidyverse")
library("ggridges")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))


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

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))
break_porc <- .95

covid.data %>% 
  mutate(distrito = osm_admin_level_4, 
         casos = nue_casosconf_diff) %>% 
  select(distrito, casos) -> data

data %>% 
  group_by(distrito) %>% 
  summarise(casos=sum(casos)) %>% 
  mutate(porc = casos / sum(casos)) %>%
  arrange(-porc) %>% 
  mutate(cporc = cumsum(porc),
         distrito = ifelse(cporc < break_porc, distrito, 'Resto')) %>% 
  group_by(distrito) %>% 
  summarise(casos = sum(casos),
            porc = sum(round(porc*100,2))) -> principales

perc <- sum(principales$porc[principales$distrito != 'Resto'])

data %>% 
  left_join(principales, by="distrito") %>% 
  mutate(distrito = ifelse(is.na(casos.y), 'Resto', distrito)) %>% 
  select(distrito, casos=casos.x) %>% 
  ggplot(aes(x = casos, y = distrito, fill = distrito)) +
    geom_density_ridges() +
    theme(legend.position = "none") +
    labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Distribución de casos diarios (al: ", last_date, ")\nDetalle en los distritos que suman el ", perc, "% de los casos totales del país") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Distritos", 
       x = "Cantidades de casos diarios"
    ) +
    scale_x_continuous(breaks = c(c(0,5, 10, 20, 50), seq(from=75, to=max(data$casos)+25, by = 25))) +
    theme_elegante_std(base_family = "Ralleway") +
    theme(legend.position = "none") -> p

