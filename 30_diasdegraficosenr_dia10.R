# 30díasdegráficos con R - día 10 - Paletas
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplot2 + wesanderson + gridExtra + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 10: Un script para probar paletas, en este caso
# la exelente: https://github.com/karthik/wesanderson
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia10.R

library("tidyverse")
library("wesanderson")
library("gridExtra")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

colores <- 20
paletas <- names(wes_palettes)

data.frame(x=factor(1:colores), y=1) %>% 
  ggplot(aes(x = x, y = y, fill = x)) + 
  geom_col() +
  theme_elegante_std(base_family = "Ralleway") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none")  -> g

plots <- list()
for (p in paletas) {
  plots[[p]] <- g + scale_fill_manual(values = wes_palette(colores, name = p, type = "continuous"), name = "") +
    labs(x = p)
}
grid.arrange(grobs = plots, ncol = 2)