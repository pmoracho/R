library(foreach)
library(doParallel)
library(magrittr)
library(dplyr)
library(knitr)
library(ggplot2)

registerDoParallel(cores=4)

datos <- data.frame(x=1:5, y=5:1, z=letters[1:5])

tareas <- list(tarea1="select(datos, x) %>% mutate(x=2*x) %>% kable", 
               tarea2="filter(datos, z=='c')", 
               tarea3="ggplot(datos, aes(x=x, y=y)) + 
                        geom_point()",
               tarea4="eval(parse('waffle.r'))"
            )
bar <- foreach(bloque=names(tareas),
               .export = "datos") %dopar% eval(parse(text=tareas[[bloque]]))
bar

tareas[["tarea4"]]
parse("source('/home/pmoracho/Proyectos/R/waffle.R')")
eval("source('waffle.R')")

eval(parse('waffle.R'))
