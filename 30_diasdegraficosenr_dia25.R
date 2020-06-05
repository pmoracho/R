# 30díasdegráficos con R - día 25 - Gráficas de violín
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 25: Una grafica de violin ue muestra la distribuci{on de los casos de COVID-19 en
# los dos principales distritos de la Argentina
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia25.R

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

covid.data %>% 
  filter(osm_admin_level_4 %in% c('CABA', 'Buenos Aires')) %>% 
  mutate(distrito = factor(osm_admin_level_4)) %>% 
  select(dia=dia_inicio, distrito, cantidad=nue_casosconf_diff) -> plot_data

plot_data %>% 
  ggplot(aes(x=distrito, y=cantidad, fill=distrito)) +
    geom_violin() +
    geom_boxplot(color="gray50", alpha = 0.5, width=0.05) +
    # scale_y_log10() +
    # stat_summary(fun=mean, geom="point", size=2, color = "blue") + 
    stat_summary(fun=median, geom="point", size=2) +
    geom_text_repel(data = plot_data %>% 
                              group_by(distrito) %>%
                              summarise(mediana = median(cantidad)),
                    aes(x = distrito, y = mediana, label=paste("Mediana:",mediana)),
                    family = "Ralleway",
                    vjust= 8,
                    hjust= -2) +
  geom_text_repel(data = plot_data %>% 
                    group_by(distrito) %>%
                    summarise(maximo = max(cantidad)),
                  aes(x = distrito, y = maximo, label=paste("Cantidad máxima en un día:",maximo, "casos")),
                  family = "Ralleway",
                  vjust= -8,
                  hjust= 2) +
    coord_flip() +
    scale_fill_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
    theme_elegante_std(base_family = "Ralleway")  +
    labs(title = paste("COVID-19 en Argentina"), 
         subtitle = paste("Distribución de los casos diarios en CABA y Buenos Aires al: ", last_date) , 
         caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
         y = "Cantidad de casos diarios", 
         x = ""
    ) +
    theme(legend.position = "none")
