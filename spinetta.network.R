library(igraph)
library(networkD3)
library(tidyverse)
library(xlsx)


grupos <- read.xlsx("spinetta.data.xlsx", sheetName = "grupos", encoding="UTF-8")
grupos_versiones <- read.xlsx("spinetta.data.xlsx", sheetName = "grupos_versiones", encoding="UTF-8")
personas <- read.xlsx("spinetta.data.xlsx", sheetName = "personas", encoding="UTF-8")
personas_grupos_versiones <- read.xlsx("spinetta.data.xlsx", sheetName = "personas_grupos_versiones", encoding="UTF-8")
obras <- read.xlsx("spinetta.data.xlsx", sheetName = "obras", encoding="UTF-8")


# create a dataset:
personas %>% 
  left_join(personas_grupos_versiones, by = "id_persona") %>% 
  left_join(grupos_versiones, by = "id_grupo_version") %>% 
  left_join(grupos, by = "id_grupo") %>% 
  mutate(grupo = paste0(nombre.y, ifelse(is.na(Observaciones), "",  paste0(" (", Observaciones, ")")))) %>% 
  select(persona = nombre.x, 
         grupo) -> data

simpleNetwork(data, height="100px", width="100px",        
              Source = "persona",         # column number of source
              Target = "grupo",           # column number of target
              linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
              charge = -900,              # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
              fontSize = 14,              # size of the node names
              fontFamily = "serif",       # font og node names
              linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
              nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
              opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
              zoom = T                    # Can you zoom on the figure?
)
