# 30díasdegráficos con R - día 9 - Areas apiladas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + ggrepl + Algo de dplyr
# Font: Ralleway
# Data: https://github.com/SistemasMapache/Covid19arData
# Para #30díasdegráficos y #rstatsES. Día 9: Una gráfica de áreas apiladas  con la evolución del 
# COVID - 19 en Argentina (casos/fallcidos).
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia09.R

library("tidyverse")
library("ggrepel")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read_csv('https://docs.google.com/spreadsheets/d/16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA/export?format=csv&id=16-bnsDdmmgtSxdWbVMboIHo5FRuz76DBxsz_BbsEVWA&gid=0')
covid.data %>% 
  mutate(fecha=as.Date(fecha,"%d/%m/%Y")) %>% 
  group_by(dia_inicio,fecha) %>% 
  summarize(casos = sum(nue_casosconf_diff),
         fallecidos = sum(nue_fallecidos_diff)) %>% 
  select(dia = dia_inicio, fecha, casos, fallecidos) %>% 
  pivot_longer(-c("dia", "fecha"), names_to = 'metrica', values_to='cantidades') -> data

data %>% 
  left_join(data %>% 
              group_by(metrica) %>%
              summarise(cantidades = max(cantidades), maximo = TRUE),
            by = c("metrica", "cantidades")
  ) %>% 
  mutate(maximo = ifelse(maximo, paste0("Pico de ", metrica, " de ", cantidades, "\nel ", fecha), NA)) -> data

last_date <- max(as.Date(covid.data$fecha,"%d/%m/%Y"))

ggplot(data, aes(x=dia, y=cantidades, fill=metrica, color=metrica)) + 
  geom_area(alpha=0.6 , size=1) +
  labs(title = paste("COVID-19 en Argentina"), 
       subtitle = paste0("Variación de casos y fallecimientos por día (al: ", last_date, ")") , 
       caption = "Fuente: https://github.com/SistemasMapache/Covid19arData", 
       y = "Cantidades diarias", 
       x = "Número de días desde el 1er caso"
  ) +
  geom_label_repel(mapping = aes(label = maximo),
                   color = "white",
                   segment.color="gray90",
                   family = "Ralleway", 
                   vjust = -1,
                   hjust = 1.5,
                   box.padding = 1,
                   show.legend = FALSE) +
  scale_fill_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  scale_color_discrete(palette = function(x) c("#67a9cf", "#ef8a62")) +
  guides(color = FALSE, label=FALSE) +
  theme_elegante_std(base_family = "Ralleway")
