set.seed(100)
n <- 100
x <- seq(n)
y <- rev(rnorm(n, 50 + 40 * x^(-0.3), 1))
Data <- data.frame(var1=x, var2=y)
plot(y ~ x, Data)
smoothingSpline = smooth.spline(Data$var1, Data$var2, spar=0.7)
lines(smoothingSpline, col='red', lwd=3)


set.seed(100)
n <- 100
x <- seq(n)
y <- rev(rnorm(n, 50 + 40 * x^(-0.3), 1))
Data <- data.frame(var1=x, var2=y)
lines(Data$var1, predict(loess(var2 ~ var1, Data)), col = "blue",  lwd = 3)


library(ggplot2)
ggplot(Data, aes(var1, var2)) + geom_point() + geom_smooth()







lista <- list()
for(i in 1:5){
    lista[i]<-i
}
lista[[1]]

variables <- c("a", "b", "c", "d", "e")
for(i in 1:5){
    assign(variables[i], i)
}
a




