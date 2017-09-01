add_image_to_color <- function(imgmagick_path, area_img_file, out_file, plot1, plot2){
    
    file1 <- paste0(tempfile(),"_file1.png")
    file2 <- paste0(tempfile(),"_file2.png")
    file3 <- paste0(tempfile(),"_file3.png")
    file4 <- paste0(tempfile(),"_file4.png")
    file5 <- paste0(tempfile(),"_file5.png")
    
    convert <- ifelse(!is.na(imgmagick_path),file.path(imgmagick_path, "convert"), "convert")
    
    ggsave(file1, plot = plot1, scale = 1, width = 10, height = NA, units = c("in", "cm", "mm"), dpi = 300)
    ggsave(file2, plot = plot2, scale = 1, width = 10, height = NA, units = c("in", "cm", "mm"), dpi = 300)
    
    # Negar imagen
    cmd <- paste(convert , file2, "-negate", file2)
    system(cmd)

    # Flat: me quedo solo con el area
    cmd <- paste(convert, file1, file2, " -evaluate-sequence add -colorspace gray -auto-level -threshold 90% -negate ", file3)
    system(cmd)
    
    # Largo y ancho del plot
    cmd <- paste0(convert, " ", file1, ' -format "%wx%h" info: ')
    pplot_dim <- unlist(lapply(strsplit(system(cmd, intern = TRUE), "x"), as.integer))
    
    # Redimensiono máscara
    cmd <- paste(convert, area_img_file, "-resize", paste0(pplot_dim[1],"x",pplot_dim[2], "!"), file4 )
    system(cmd)
    
    # Combinar capas para armar el area con la imagen y el fondo transparente
    cmd <- paste(convert, "-gravity Center -geometry +0+0", file4,  "\\(", file3, "-gravity Center -geometry +0+0 \\)", "-set colorspace RGB -alpha off -geometry +0+0 -compose copy_opacity -composite -set colorspace sRGB", file5)
    system(cmd)
    
    # Combinación final, el grafico original con la imagen superpuesta en el area
    cmd <- paste(convert, file1, file5, "-flatten -blur 1x65000", out_file)
    system(cmd)        
}

library(datasets)
library(ggplot2)

rm(list = ls())

data(airquality)

area_img_file <- file.path(getwd(),"dolar.jpg")
out_file <- file.path(getwd(), "final.png")
imgmagick_path <- NA

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
    geom_area(stat = "bin", fill = "lightblue", color="darkblue")+
    geom_vline(aes(xintercept=mean(weight)),
               color="blue", linetype="dashed", size=1)

plot2 <- ggplot(df, aes(x=weight)) +
    geom_area(stat = "bin", fill = "red", color="darkblue")+
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
    geom_bar(stat = "identity", fill="red", color="darkblue", aes(fill = type)) 

plot2 <-ggplot(mydata, aes(months, values)) +
    geom_bar(stat = "identity", fill="darkblue", color="darkblue", aes(fill = type)) 

add_image_to_color(imgmagick_path, area_img_file, out_file, plot1, plot2)
