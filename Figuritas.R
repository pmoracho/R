FIGURITAS_POR_PAQUETE <- 5
FIGURITAS_POR_ALBUM <- 168
CANTIDAD_DE_CORRIDAS <- 1000
PRECIO_POR_PAQUETE <- 15

completar_album <- function(x) {
    total <- FIGURITAS_POR_ALBUM
    completo <- c()
    compras <- 0
    while (length(completo) < total) {
        completo <- union(completo, sample(1:FIGURITAS_POR_ALBUM,FIGURITAS_POR_PAQUETE))
        compras <- compras + 1
    }
    return(compras)
}
paquetes_intentos <- sapply(1:CANTIDAD_DE_CORRIDAS,completar_album)


min(paquetes_intentos)
max(paquetes_intentos)
mean(paquetes_intentos)

# Costo
min(paquetes_intentos) * PRECIO_POR_PAQUETE
max(paquetes_intentos) * PRECIO_POR_PAQUETE
mean(paquetes_intentos) * PRECIO_POR_PAQUETE


library("ggplot2")

mejores <- as.numeric(names(which(max(table(paquetes_intentos)) == table(paquetes_intentos))))
minimo <- min(mejores)
maximo <- max(mejores)
promedio <- mean(paquetes_intentos)
mediana <- median(paquetes_intentos)

qplot(paquetes_intentos, binwidth = 10, geom="histogram", fill=I("blue")) +
    geom_vline(aes(xintercept = minimo, color=paste("Minimo: ",minimo))) +
    geom_vline(aes(xintercept = promedio, color=paste("Promedio: ",promedio))) +
    geom_vline(aes(xintercept = maximo, color=paste("Vaximo: ",maximo))) +
    scale_color_manual(name = "Valores\nmas repetidos", values = c("red", "green", "black")) +
    labs(title="Cantidad de sobres en promedio para llenar 1 album",x = "Sobres", y = "Intentos")
