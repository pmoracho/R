library(datasets)
library(ggplot2)
library(magick)

rm(list = ls())
data(airquality)

file1 <- paste0(tempfile(),".png")
file2 <- paste0(tempfile(),".png")
file3 <- paste0(tempfile(),".png")
file4 <- paste0(tempfile(),".png")
file5 <- paste0(tempfile(),".png")

area_img_file <- "/home/pmoracho/dolar.jpg"
out_file <- "/home/pmoracho/final.jpg"

# Genero la gráfica
plot <- ggplot(airquality, aes(x = Ozone, colour=)) +
        geom_density(fill = "#4271AE", colour = "#CC6666", alpha = 1) +
        scale_x_continuous(name = "Mean ozone in\nparts per billion",
                           breaks = seq(0, 200, 25),
                           limits=c(0, 200)) +
        scale_y_continuous(name = "Density") +
        ggtitle("Density plot of mean ozone")

ggsave(file1)
plot <- plot + geom_density(fill = "#CC6666", colour = "#CC6666", alpha = 1) +
ggsave(file2)

img1 <- image_read(file1)
img2 <- image_negate(image_read(file2))

# Finalmente la imagen solo con la curva/area
new <- image_flatten(c(img1, img2), "Add")
image_write(new, file3)

cmd <- paste0("convert ", file3, " -colorspace gray -auto-level -threshold 90% -negate ", 
              file4, ";", "composite -compose Dst_In ", "\\", "( ", file4, " -alpha copy \\) ", 
              area_img_file, " -alpha Set PNG32:", file5, ";", "composite ", file5, " ", 
              file1, " ", out_file)
system(cmd)

# convert prueba3.png -colorspace gray -auto-level -threshold 90% -negate prueba4.png
# composite -compose Dst_In \( prueba4.png -alpha copy \) dolar.jpg -alpha Set  PNG32:salida.png
# composite  salida.png prueba1.png out.png





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
