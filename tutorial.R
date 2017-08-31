library(e1071)

data.file <- "../ML_for_Hackers/02-Exploration/data/01_heights_weights_genders.csv"
heights.weights<-read.csv(data.file)
head(heights.weights)
summary(heights.weights)

heights <- with(heights.weights,Height)
weights <- with(heights.weights,Weight)


quantile(heights)

mean(heights.weights$Weight)

range(heights)
IQR(heights)
var(heights)
sd(heights)

" Esta es una prueba
  de un string multilinea que hace las veces
  de un comentario
"
skewness(heights)
kurtosis(heights)


hist(heights,nclass=20, main="Histograma de las Alturas", xlab="alturas",ylab="Frecuencias", col="red")
plot(density(heights), main="Densidad", xlab="alturas",ylab="Densidad estimada", col="red",type="p")
plot(heights, weights, main="Gráfica entre las dos variables", xlab="Alturas",ylab="Pesos", col="green",type="p")


f<-ecdf(heights)
range(heights)


#Construcción de gráfico con histogramas marginales
x <- pmin(3, pmax(-3, rnorm(200)))
y <- pmin(3, pmax(-3, rnorm(200)))
xhist <- hist(x, breaks=seq(-3,3,0.5), plot=FALSE)
yhist <- hist(y, breaks=seq(-3,3,0.5),plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(-3,3)
yrange <- c(-3,3)
b<-matrix(c(2,0,1,3),2,2,byrow=TRUE)
nf <- layout(b, widths=c(3,1), heights=c(1,3), respect=TRUE)
par(mar=c(4,4,1,1))
plot(x, y, xlim=xrange, ylim=yrange, xlab="x", ylab="y",type="p")
par(mar=c(0,4,1,1))
barplot(xhist$counts,axes=FALSE, ylim=c(0, top), space=0,col="2")
title(main='Scatterplot con histogramas marginales',font=2)
par(mar=c(4,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top),space=0, horiz=TRUE,col="3")

library(ggplot2)
# Construcción de un histograma con ggplot
ggplot(heights.weights, aes(x = Height)) + geom_histogram(col="2")

#Construcción de gráfico de las densidades por género
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density() + facet_grid(Gender ~ .)
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density()


#Construcción de gráfica scatterplot
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + geom_smooth()


# Transformamos los datos para analizar como se comporta y graficamos la recta que divide a entre géneros
# Primero transformamos los datos
heights.weights <- transform(heights.weights,Male = ifelse(Gender == 'Male', 1, 0))
# Ahora claculamos una regresión lineal general
logit.model <- glm(Male ~ Weight + Height,data = heights.weights,family = binomial(link = 'logit'))
# Por último contruimos el gráfico
ggplot(heights.weights, aes(x = Height, y = Weight)) +
    geom_point(aes(color = Gender, alpha = 0.25)) +
    scale_alpha(guide = "none") + 
    scale_color_manual(values = c("Male" = "black", "Female" = "blue")) +
    theme_bw() +
    abline(intercept = -coef(logit.model)[1] / coef(logit.model)[2],
                slope = - coef(logit.model)[3] / coef(logit.model)[2],
                geom = 'abline',
                color = 'red')




#Cargamos los datos
temperaturas.global<-scan("data/globtemp.dat")
x=temperaturas.global[45:142]
#Asignamos los tiempos de nuestros datos
t=1900:1997
#Estimamos la recta que describe "mejor" nuestros datos
fit=lm(x~t)
plot(t,x,type="o",col="2",xlab="Años",ylab="Temperaturas globales",main="Ejemplo de Regresión Lineal")
abline(fit)


library(ggplot2)
#Cargamos los datos
edades<-read.csv("../ML_for_Hackers/05-Regression/data/longevity.csv")
#Les cambiamos el nombre a las columnas

edades$Fumadores <- as.character(edades$Fumadores)
edades$Fumadores[edades$Fumadores == "1"] <- "Fumador"
edades$Fumadores[edades$Fumadores == "0"] <- "No fumador"
names(edades)<-c("Fumadores","Edad_de_Muerte")
#Graficamos explorando el comportamiento de la densidad entre hombres y mujeres.
ggplot(edades, aes(x = Edad_de_Muerte, fill = factor(Fumadores)))+geom_density() + facet_grid(Fumadores ~ .)

mean(edades[edades$Fumadores == "Fumador",2])
mean(edades[edades$Fumadores == "No fumador",2])

aggregate(edades[, 2], list(edades$Fumadores), mean)

edades[edades$Fumadores == "Fumador",2]

predicion<-73
with(edades,mean(Edad_de_Muerte-predicion)^2)
