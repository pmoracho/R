# 30díasdegráficos con R - día 5 - Arcos
# Tema personalizado: devtools::install_github("pmoracho/ggelegant")
# Gráficos: Ggplo2 + Algo de dplyr
# Font: Ralleway
  # Para #30díasdegráficos y #rstatsES. Día 5: Empezamos a salir de la zona de confort de ggplot, un
  # gráfico de arcos, con datos de un proyecto personal de sistematizar la vida musical de L.A.Spinetta.
# Github: https://github.com/pmoracho/R/blob/master/30_diasdegraficosenr_dia05.R

library("tidyverse")
library("tidygraph")
library("ggraph")
library("igraph")

if ("ggelegant" %in% rownames(installed.packages())) {
  library("ggelegant")
} else {
  # devtools::install_github("pmoracho/ggelegant")
  theme_elegante_std <- function(base_family) {}
}

data <- structure(list(persona = structure(c(10L, 10L, 10L, 10L, 10L, 
                                             10L, 6L, 6L, 12L, 12L, 12L, 12L, 8L, 9L, 2L, 7L, 7L, 3L, 3L, 
                                             1L, 11L, 5L, 4L, 4L, 13L), .Label = c("Angel del Guercio", "Carlos Xartuch", 
                                                                                   "Daniel Albertelli", "Edelmiro Molinari", "Eduardo Miró", "Emilio del Guercio", 
                                                                                   "Guido Meda", "Hector Nuñez", "Horacio Soria", "Luis Alberto Spinetta", 
                                                                                   "Ricardo Miró", "Rodolfo Garcia", "Santiago Novoa"), class = "factor"), 
                       grupo = c(" Bundleman", " Los Larkings", " Los Masters", 
                                 " Los Mods", " Los Sbirros", " Almendra (1967-1969)", " Bundleman", 
                                 " Almendra (1967-1969)", " Los Larkings", " Los Masters", 
                                 " Los Mods", " Almendra (1967-1969)", " Los Larkings", " Los Larkings", 
                                 " Los Larkings", " Los Masters", " Los Mods", " Los Masters", 
                                 " Los Mods", " Los Sbirros", " Los Sbirros", " Los Sbirros", 
                                 " Los Sbirros", " Almendra (1967-1969)", " Los Sbirros")), class = "data.frame", row.names = c(NA, 
                                                                                                                                -25L))

data %>% 
  select(from = persona, to = grupo, grupo=grupo, name=persona) -> prepared.data

tbl_graph(edges=prepared.data, directed = TRUE) %>% 
  ggraph(layout = "linear") +
  geom_edge_arc(aes(color=grupo),  edge_width=1.5, edge_alpha = 0.5, fold = TRUE,) +
  geom_node_point(size = 2, color="#67a9cf") +
  geom_node_text(aes(label = str_wrap(name,13)), size = 3, nudge_y =-.7, angle = 90, fontface = "bold",  hjust=.5) +
  coord_cartesian(clip = "off") + 
  theme_elegante_std(base_family = "Ralleway") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none") +
  labs(title = "Historia musical de Luis Albero Spinetta", 
       subtitle = "Los comienzos"
  ) 
