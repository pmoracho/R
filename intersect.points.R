# Temperaturas medidas
medidas <- read.table(text="height Temp
2541 293.35
2558 292.75
2591 291.65
2625 290.65
2654 290.15
2683 289.65
2693 289.15
2702 288.65
2708 288.36", sep=' ', header=TRUE)   
medidas <- medidas[, c(2,1)]

# Temperaturas de función lineal
lineales <- read.table(text="height Tmax Tmin
2500 290.8035 283.9201
2550 290.3135 283.4301
2600 289.8235 282.9401
2650 289.3335 282.4501
2700 288.8435 281.9601
2750 288.3535 281.4701", 
sep=' ', header=TRUE)

# Solo vamos a buscar la intersección con Tmax
lineales <- lineales[, c(2,1)]  

m1 <- lm(height~Tmax, lineales)
m2 <- lm(height~Temp, medidas)
cm <- rbind(coef(m1),coef(m2)) 
# Calculamos intersección
punto.inter <- c(-solve(cbind(cm[,2],-1)) %*% cm[,1])
punto.inter

# Graficamos
plot(lineales)
points(medidas)
grid(5, 5, lwd = 2)
lines(smooth.spline(lineales, spar=0), col='red', lwd=3)
lines(smooth.spline(medidas, spar=1), col='green', lwd=1)
lines(smooth.spline(medidas, spar=0), col='blue', lwd=3)
points(punto.inter[1],punto.inter[2],pch = 8)




library("rgeos")
library("sp")
l1 <- SpatialLines(list(Lines(list(Line(cbind(lineales$Tmax, lineales$height))), 1)))
l2 <- SpatialLines(list(Lines(list(Line(cbind(medidas$Temp, medidas$height))), 1)))
punto.inter <- coordinates(gIntersection(l1, l2))
punto.inter


plot(lineales)
points(medidas)
grid(5, 5, lwd = 2)
lines(l1, lwd = 3, col='red')
lines(l2, lwd = 3, col='blue')
points(punto.inter[1],punto.inter[2],pch = 8)
