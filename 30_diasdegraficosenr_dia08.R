# 30díasdegráficos con R - día 1 - barras / columnas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
# Data: https://opendata.ecdc.europa.eu/covid19/casedistribution/csv
# Para #30díasdegráficos y #rstatsES. Día 1: Un gráfico de barras con ggplot2 + tema propio + fuente: Ralleway. 
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia08.R

library("tidyverse")
library("ggrepel")
library("directlabels")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

covid.data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM",
                       stringsAsFactors = FALSE)

last_date <- max(as.Date(covid.data$dateRep,"%d/%m/%Y"))
covid.data %>% 
  # filter(countriesAndTerritories %in% c('Argentina','Brazil', 'Chile', 'Bolivia', 'Paraguay', 'Uruguay')) %>%
  # filter(countriesAndTerritories %in% c('Argentina')) %>%
  filter(countriesAndTerritories %in% c('Brazil')) %>%
  filter(cases > 1) %>%
  mutate(fecha = as.Date(dateRep,"%d/%m/%Y"),
         pais = countriesAndTerritories,
         fallecidos = deaths,
         casos = cumsum(cases)) %>% 
  arrange(pais, fecha) %>% 
  group_by(pais) %>% 
  mutate(dia = row_number()) %>% 
  ungroup() %>% 
  select(dia, pais,  fallecidos, casos) %>% 
  group_by(dia) %>% 
  summarize(casos = casos, fallecidos = fallecidos) %>% 
  # summarize(casos = mean(casos), fallecidos = round(mean(fallecidos),0)) %>% 
  ungroup() %>% 
  ggplot(aes(x = dia, y = casos, z = fallecidos)) + 
    stat_density2d(aes(colour = fallecidos)) +
    # geom_point(alpha=.2) +
    scale_y_log10() +
    geom_dl(aes(label=fallecidos),method="last.points") +
    theme_elegante_std(base_family = "Ralleway") +
    theme(legend.position=c(0,1),
         legend.justification=c(0,1)) 


direct.label(p, method="bottom.pieces")

p <- direct.label(p, list("far.from.others.borders", "calc.boxes", "enlarge.box", 
                          hjust = 1, vjust = 1, box.color = NA, 
                          fill = "transparent", "draw.rects"))
p
    # geom_contour(aes(z=fallecidos)) +
# scale_y_log10() +
# geom_text(aes(label = format(casos, digits=0, big.mark = ',')),  vjust = .6, hjust=1.1,
#           position = position_dodge(width=1)) +
#           
# geom_point(alpha=.2) +
# scale_y_log10() +
# geom_label_repel( mapping = aes(label = round(fallecidos,0)), family = "Ralleway") +
# geom_dl(aes(label=fallecidos), method="bottom.pieces",  stat="contour") +
library(tidyverse)
    
data.frame(casos=c(1, 3, 7, 9, 15, 30, 45),
           dia = c(1, 2, 3, 4 ,5, 6, 7),
           fallecidos = c(0, 1, 2, 3, 4, 3, 6)
           ) %>% 
  ggplot(aes(x = dia, y = casos, z = fallecidos)) + 
  stat_density2d()
    
v <- ggplot(faithfuld, aes(waiting, eruptions, z = density))
v + geom_contour() + scale_x_log10()


m <- ggplot(faithful, aes(x = eruptions, y = waiting)) +
  geom_point() +
  xlim(0.5, 6) +
  ylim(40, 110) + geom_density_2d()

geom_dl(aes(label=variable),method="last.points")

m + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") 

set.seed(4393)
dsmall <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsmall, aes(x, y))
# If you map an aesthetic to a categorical variable, you will get a
# set of contours for each value of that variable
d + geom_density_2d(aes(colour = cut))

# Similarly, if you apply faceting to the plot, contours will be
# drawn for each facet, but the levels will calculated across all facets
d + stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
  facet_grid(. ~ cut) + scale_fill_viridis_c()
# To override this behavior (for instace, to better visualize the density
# within each facet), use after_stat(nlevel)
d + stat_density_2d(aes(fill = after_stat(nlevel)), geom = "polygon") +
  facet_grid(. ~ cut) + scale_fill_viridis_c()

# If we turn contouring off, we can use use geoms like tiles:
d + stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE)
# Or points:
d + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 20, contour = FALSE)
