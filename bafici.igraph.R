library(tidyverse)
library(igraph)

hitos <- read.csv("E:/pm/repos/bafici.resolver/data/hitos.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
temas <- read.csv("E:/pm/repos/bafici.resolver/data/temas.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
distancias <- read.csv("E:/pm/repos/bafici.resolver/data/distancias.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
ubicaciones <- read.csv("E:/pm/repos/bafici.resolver/data/ubicaciones.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

hitos %>% 
  as_tibble() %>% 
  select(CodigoTema, CodigoUbicacion, Inicio, Fin) %>% 
  left_join(temas, by=c("CodigoTema" = "Codigo")) %>% 
  rename(Pelicula=Nombre) %>% 
  left_join(ubicaciones, by=c("CodigoUbicacion" = "Codigo")) %>% 
  rename(Sede=Nombre) %>% 
  mutate(id=row_number()) %>% 
  select(id, CodigoTema, CodigoUbicacion, Inicio, Fin, Pelicula, Info, Duracion, Sede, Barrio) -> h


h %>% 
  inner_join(temas %>% sample_n(7),
             by = c("CodigoTema" = "Codigo")) -> seleccion

t(combn(seleccion$id, 2)) %>% 
  as_tibble() %>% 
  rename(id.1=1,id.2=2) %>% 
  left_join(seleccion, by = c("id.1" = "id")) %>%
  left_join(seleccion, by = c("id.2" = "id")) %>% 
  mutate(valid = Inicio.y > Inicio.x & CodigoTema.x != CodigoTema.y) %>% 
  filter(valid == TRUE) -> combinaciones


library(igraph)

seleccion %>% 
  distinct(id, CodigoTema) %>% 
  select(id = id, pelicula = CodigoTema) -> nodos 

combinaciones %>% 
  select(Node.1=id.1,Node.2=id.2) -> topology
                                                
g <- graph.data.frame(topology, vertices=nodos, directed=TRUE)

cant.nodos <- nrow(nodos)
cant.tipos <- length(unique(nodos$pelicula))

V(g)$color = rainbow(cant.tipos)[as.numeric(factor(V(g)$pelicula))]

plot.igraph(g, 
            vertex.size = 20, 
            edge.arrow.size = 0.5,
            vertex.label.font=2, 
            vertex.label.color="gray85",
            vertex.label.cex=1.4, 
            edge.color="gray45",
            layout=layout.kamada.kawai)

to <- 1:(cant.nodos-cant.tipos+1)
l <- list(to)
for (n in 1:(cant.nodos-cant.tipos+1)) {
  l[n] <- list(all_simple_paths(g, n, cant.nodos))
}
l <- do.call(c,l)

tipos_path <- 1:length(l)
len_path <- 1:length(l)
for (i in 1:length(l)) {
  tt <- vertex_attr(g, name="pelicula", index = l[[i]])
  tipos_path[i] <- length(unique(tt))
  len_path[i] <- length(tt)
}

posibles <- max(tipos_path)
mas_corto <- min(len_path[which(tipos_path == posibles)])
l[which(tipos_path == posibles & len_path == mas_corto)]


all_simple_paths(g, 1, 5)


library(astar)

nodos$id2 <- LETTERS[1:length(nodos$id)]
combinaciones$id.1.2 <- match(nodos$id,combinaciones$id.1)
LETTERS[factor(as.character(nodos$id))]

nodes <- list(
  A = c(B = 10),
  A = c(C = 10),
  A = c(D = 10),
  B = c(D = 10),
  B = c(C = 10),
  C = c(D = 10)
)

neighbors <- function(node) names(nodes[[node]])
cost_estimate <- function(node, goal) 1
edge_distance <- function(src, dst) nodes[[src]][dst]
is_goal_reached <- function(node, goal) identical(node, goal)

astar('A', 'D', cost_estimate, edge_distance, neighbors, is_goal_reached)
