# 30díasdegráficos con R - día 21 - Anotaciones
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + xkcd + Algo de dplyr
# Font: Ralleway
# Para #30díasdegráficos y #rstatsES. Día 21: Va un intento de replicar un clásico meme de xkcd
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia21.R

library("tidyverse")
library("xkcd")

# Para instalar el font xkcd en Linux
# download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf", mode="wb")
# system("mkdir ~/.fonts")
# system("cp xkcd.ttf ~/.fonts")
# font_import(pattern = "[X/x]kcd", prompt=FALSE)
# loadfonts()

xaxis <- c(1950,1960, 1970, 1980, 1990, 2000, 2010)
xaxislbl <- paste0(xaxis, "s")

data.frame(year=seq(from=1940, to=2020, by=10),
           quality=c(1.2, 1.3, 1.4, 1.8, 1.9, 1.8, 1, 1.5, 2)
           ) -> df
df %>% 
  ggplot(mapping=aes(x=year, y=quality)) +
  geom_smooth(method = 'loess',
              color = "black",
              formula = 'y ~ x', alpha = 0.2, size = 1, span = .6, se=FALSE) + 
  labs(title = paste("GENERAL QUALITY OF CHARTS AND\nGRAPHS IN SCIENTIFIC PAPERS\n"), 
       y = "", 
       x = ""
  ) +
  xkcdaxis(c(1945,2020),c(1,2.2)) +
  annotate("rect", xmin = 1989, xmax = 2015, ymin = .98, ymax = 2.2,  alpha = .4) +
  annotate("text", x = 2002, y = 2, label = "POWERPOINT/\nMS PAINT ERA", family="xkcd", size=10,) +
  scale_y_continuous(breaks = c(1,2), labels = c("BAD", "GOOD")) +
  scale_x_continuous(breaks = xaxis, labels = xaxislbl, limits=c(1930, 2020))  +
  theme(plot.title=element_text(vjust=1.25, size=24, hjust = 0.5),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=20)
        )
        
