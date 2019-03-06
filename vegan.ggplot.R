library(ggplot2)
library(gridExtra)
library(tidyverse)
library(purrr)
library (vegan)
library(grid)

data(dune)
data(dune.env)
pool <- with(dune.env, specpool(dune, Management))

# Boxplot
ggplot(dune.env, aes(x=Management, y=specnumber(dune), fill=Management)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y="", x="") +
    theme_light() -> bp1

ggplot(dune.env, aes(x=Management, y=specnumber(dune)/specpool2vect(pool), fill=Management)) +
    geom_boxplot(show.legend = FALSE) +
    labs(y="", x="") + 
    theme_light() -> bp2

# LinePlot
data(BCI)
pool2 <- poolaccum(BCI)

summary(pool2) %>% 
    map(data.frame, check.names = FALSE) %>% 
    map(setNames,c("N", "Value", "lower2.5", "higher97.5", "std")) %>% 
    bind_rows(.id = "ID") -> data

ggplot(data = data, aes(x = N, y = Value, group = ID)) +
    geom_line(aes(y = higher97.5), color="pink") + 
    geom_line(aes(y = Value), color="blue")  +
    geom_line(aes(y = lower2.5), color="pink")  +
    labs(y="Richness", 
         x="Size") + 
    facet_wrap(~ID) +
    theme_light() -> lp

grid.arrange(arrangeGrob(bp1, bp2, ncol = 2),
             lp,
             nrow = 2,
             heights = c(1, 2),
             top = textGrob("Titulo del gráfico",gp=gpar(fontsize=20)))