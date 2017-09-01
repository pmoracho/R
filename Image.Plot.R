add_image_to_color <- function(imgmagick_path = NA, 
                               area_img_file, 
                               out_file, 
                               plot1, 
                               plot2, 
                               width=20, 
                               height=10, 
                               units=c("cm"),
                               dpi=300){
    
    plot_file <- tempfile(pattern="plot_", fileext=".png")
    plot_file_negate <- tempfile(pattern="plot_negate_", fileext=".png")
    mask_file <- tempfile(pattern="mask_", fileext=".png")
    area_file <- tempfile(pattern="area_", fileext=".png")
    
    convert <- ifelse(!is.na(imgmagick_path),file.path(imgmagick_path, "convert"), "convert")

    # Salvamos las dos versiones de los plots    
    ggsave(plot_file, plot = plot1, scale = 1, width = width, height = height, units = units, dpi = dpi)
    ggsave(plot_file_negate, plot = plot2, scale = 1, width = width, height = height, units = units, dpi = dpi)
    
    # Negar imagen
    cmd <- paste(convert , plot_file_negate, "-negate", plot_file_negate)
    system(cmd)

    # Flat: me quedo solo con el area
    cmd <- paste(convert, plot_file, plot_file_negate, "-compose LinearBurn -composite -colorspace gray -auto-level -negate", mask_file)
    system(cmd)
    
    # Largo y ancho del plot
    cmd <- paste0(convert, " ", plot_file, ' -format "%wx%h" info: ')
    pplot_dim <- unlist(lapply(strsplit(system(cmd, intern = TRUE), "x"), as.integer))
    
    # Redimensiono la imagen del area
    cmd <- paste(convert, area_img_file, "-resize", paste0(pplot_dim[1],"x",pplot_dim[2], "!"), area_file )
    system(cmd)
    
    # Combinar capas para armar el area con la imagen y el fondo transparente
    cmd <- paste(convert, "-composite", area_file, plot_file, "( -blur 1x65000 )", mask_file, out_file)
    system(cmd)
    
}

library(datasets)
library(ggplot2)

rm(list = ls())

data(airquality)

area_img_file <- file.path(getwd(),"dolar.jpg")
out_file <- file.path(getwd(), "final.png")
imgmagick_path <- NA
imgmagick_path <- "D:/pm/bin/ImageMagick-7.0.6-10"

# Genero la grafica
plot1 <- ggplot(airquality, aes(x = Ozone)) +
    geom_density(fill = "#4271AE", colour = "#CC6666", alpha = 1) +
    scale_x_continuous(name = "Promedio de Ozono en\npartes por billones",
                       breaks = seq(0, 200, 25),
                       limits=c(0, 200)) +
    scale_y_continuous(name = "Density") +
    ggtitle("Gráfico de densidad del promedio de ozono")

plot2 <- plot1 + geom_density(fill = "#CC6666", colour = "#CC6666", alpha = 1)

add_image_to_color(imgmagick_path, area_img_file, out_file, plot1, plot2)


set.seed(1234)
df <- data.frame(
    sex=factor(rep(c("F", "M"), each=200)),
    weight=round(c(rnorm(200, mean=55, sd=5),
                   rnorm(200, mean=65, sd=5)))
)

plot1 <- ggplot(df, aes(x=weight)) +
    geom_area(stat = "bin", fill = "lightblue", color="darkblue", bins=30)+
    geom_vline(aes(xintercept=mean(weight)),
               color="blue", linetype="dashed", size=1)

plot2 <- ggplot(df, aes(x=weight)) +
    geom_area(stat = "bin", fill = "red", color="darkblue", bins=30)+
    geom_vline(aes(xintercept=mean(weight)),
               color="blue", linetype="dashed", size=1)

add_image_to_color(imgmagick_path, area_img_file, out_file, plot1, plot2)



months <-rep(c("jan", "feb", "mar", "apr", "may", "jun", 
               "jul", "aug", "sep", "oct", "nov", "dec"), 2)
chickens <-c(1, 2, 3, 3, 3, 4, 5, 4, 3, 4, 2, 2)
eggs <-c(0, 8, 10, 13, 16, 20, 25, 20, 18, 16, 10, 8)
values <-c(eggs)
type <-c(rep("eggs", 12))
mydata <-data.frame(months, values)

plot1 <-ggplot(mydata, aes(months, values)) +
    geom_bar(stat = "identity", fill="lightblue", color="darkblue", aes(fill = type)) 

plot2 <-ggplot(mydata, aes(months, values)) +
    geom_bar(stat = "identity", fill="darkblue", color="darkblue", aes(fill = type)) 

add_image_to_color(imgmagick_path, area_img_file, out_file, plot1, plot2, width = 30, height = 15)
