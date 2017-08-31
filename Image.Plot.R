library(datasets)
library(ggplot2)
library(magick)

rm(list = ls())

area_img_file <- file.path(getwd(),"ozono.jpg")
out_file <- file.path(getwd(), "final.png")
imgmagick_path <- "D:/pm/bin/ImageMagick-7.0.6-10"

convert <- file.path(imgmagick_path, "convert.exe")
composite <- file.path(imgmagick_path, "composite.exe") 

data(airquality)

file1 <- paste0(tempfile(),"_file1.png")
file2 <- paste0(tempfile(),"_file2.png")
file3 <- paste0(tempfile(),"_file3.png")
file4 <- paste0(tempfile(),"_file4.png")
file5 <- paste0(tempfile(),"_file5.png")

# Genero la grafica
plot <- ggplot(airquality, aes(x = Ozone, colour=)) +
        geom_density(fill = "#4271AE", colour = "#CC6666", alpha = 1) +
        scale_x_continuous(name = "Promedio de Ozono en\npartes por billones",
                           breaks = seq(0, 200, 25),
                           limits=c(0, 200)) +
        scale_y_continuous(name = "Density") +
        ggtitle("Gráfico de densidad del promedio de ozono")

ggsave(file1, scale = 1, width = 10, height = NA, units = c("in", "cm", "mm"), dpi = 300)

plot <- plot + geom_density(fill = "#CC6666", colour = "#CC6666", alpha = 1)
ggsave(file2, scale = 1, width = 10, height = NA, units = c("in", "cm", "mm"), dpi = 300)


# Negar imagen
cmd <- paste(convert , file2, "-negate", file2)
system(cmd)

# Flat: me quedo solo con el area
cmd <- paste(composite, file1, file2, " -evaluate-sequence add -colorspace gray -auto-level -threshold 90% -negate ", file3)
system(cmd)

# Largo y ancho del plot
cmd <- paste0(convert, " ", file1, ' -format "%wx%h" info: ')
pplot_dim <- unlist(lapply(strsplit(system(cmd, show.output.on.console=TRUE, intern = TRUE), "x"), as.integer))

# Largo y ancho de la imagen a suporponer
cmd <- paste0(convert, " ", area_img_file, ' -format "%wx%h" info: ')
area_dim <- unlist(lapply(strsplit(system(cmd, show.output.on.console=TRUE, intern = TRUE), "x"), as.integer))

# Redimensiono máscara
cmd <- paste(convert, area_img_file, "-resize", paste0(pplot_dim[1],"x",pplot_dim[2], "!"), file4 )
system(cmd)

# Combinar capas para armar el area con la imagen y el fondo transparente
cmd <- paste(convert, "-gravity Center -geometry +0+0", file4,  "( ", file5, "-gravity Center -geometry +0+0 ) ", "-set colorspace RGB -alpha off -geometry +0+0 -compose copy_opacity -composite -set colorspace sRGB", file5)
system(cmd)

# Combinación final, el grafico original con la imagen superpuesta en el area
cmd <- paste(composite, file1, file5, "-flatten -blur 1x65000", out_file)
system(cmd)

# image.png ( mask.png -gravity Center ) -set colorspace RGB -alpha off -compose copy_opacity -composite -set colorspace sRGB -gravity center salida
# convert prueba3.png -colorspace gray -auto-level -threshold 90% -negate prueba4.png
# composite -compose Dst_In \( prueba4.png -alpha copy \) dolar.jpg -alpha Set  PNG32:salida.png
# composite  salida.png prueba1.png out.png

#' Obtener el tamaño en pixels de una imagen
#'
#' @param imgfile Ptah a la imagen
#' @keywords image imagemagick
#' @export
#' @examples
#' size <- get_imgsize("prueba.jpg")

get_imgsize<-function(imgfile) {

    cmd <- paste0(convert, " ", imgfile, ' -format "%wx%h" info: ')
    ret <- system(cmd, show.output.on.console=TRUE, intern = TRUE)
    return(unlist(lapply(strsplit(ret, "x"), as.integer)))
}

r <- get_imgsize(file3)
r[2]
class(r)


resize<-function(from, to) {
    
}


dolar <- image_crop(image_scale(dolar, "x2100"), "2100x2100")
out <- image_negate(new)
out <- image_flatten(c(dolar, mask))
out <- image_crop("2100x2100+0")

g <- rasterGrob(backimg, interpolate=TRUE)

library(jpeg)
library(grid)
g <- rasterGrob(backimg, interpolate=TRUE)

backimg <- image_read("/home/pmoracho/prueba6.png")

newplot <- plot + annotation_custom(rasterGrob(out, width=unit(1,"npc"), height=unit(1,"npc")), 
                         -Inf, Inf, -Inf, Inf)
plot

myColours = c(1, "steelblue", "#FFBB00", rgb(0.4, 0.2, 0.3))
myColoursAlpha <- add.alpha(myColours, alpha=0.4)

add.alpha <- function(col, alpha=1){
    if(missing(col))
        stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
              rgb(x[1], x[2], x[3], alpha=alpha))  
}

plot <- ggplot(airquality, aes(x = Ozone)) +
    geom_density(fill = "#4271AE", colour = "#4271AE", alpha = 1) +
    scale_x_continuous(name = "Mean ozone in\nparts per billion",
                       breaks = seq(0, 200, 25),
                       limits=c(0, 200)) +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of mean ozone")
