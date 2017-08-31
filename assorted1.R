library(cluster)
library(knnGarden)
good<-0
for(i in 1:150){
    test <- iris[i,-5]
    train <- iris[-i,]
    result <- knnVCN(train[,-5], train[,5], test, K = 3, ShowObs=T,"maximum")
    
    if((result[,5])==(as.character(iris[i,5]))){
        good<-good+1
    }
}
print(paste0(good, " aciertos de 150"))
library(devtools)
install_github("cjgb/caRtociudad")
library(caRtociudad)

dire <- "Calle Alcalá 145, Madrid"
class(dire)
df <- cartociudad_geocode(dire, max_results = 1)
df <- geo(dire, max_results = 1)

df <- obtener_datos(dire)

df <- cartociudad_geocode("Calle Alcalá 145, Madrid", max_results = 1)

obtener_datos <- function(param){
    return(cartociudad_geocode(param))
}

class(cartociudad_geocode)
dire <- c("Calle Alcalá 145, Madrid")
df <- obtener_datos("Calle Alcalá 145, Madrid")

require(Lock5Data)
data("Cereal")

Cereales20 <- function(Cereal, name) {
    return(Cereal[Cereal$Name == name,c("Name","Company","Calories")])
}

Cereales20(Cereal, name = "AppleJacks")


Cereal[Cereal$Name == "AppleJacks",]


bases <- c("A","G","C","T")
P <- runif(1,0,1) # Valor de probabilidad de que ocurra un base, único para 
ADN <- sample(bases,size = 21,replace = TRUE,prob = c(P,P,P,P))
length(ADN)

runif(10,0,1)
set.seed(1)
round(runif(10,1,100))

runif(c("A","G","C","T"))

aggregate(ADN)

nueva_version <- function(adn, bases, porcentaje) {
    nuevo_adn <- adn
    l <- length(adn)
    change <- as.integer(runif(as.integer(l*porcentaje/100),1,l))
    for (i in change) {
        posibles = bases[!bases == adn[i]]
        nuevo_adn[i] <- sample(posibles,1)
    }
    return(nuevo_adn)
}

print(ADN)
print(nueva_version(ADN, bases,10))

print(nueva_version(ADN, bases))
print(nueva_version(ADN, bases))
print(nueva_version(ADN, bases))

library(FactoClass)
data(ColorAdjective)
FC.col <-FactoClass(ColorAdjective, dudi.coa)
library("data.table")
DF = data.frame(A=1:3, B=c("foo","A,Name","baz"))
fwrite(df, file="test.csv", quote=TRUE)
write.csv(DF, file="test.csv", row.names=FALSE, quote=FALSE)

df <- fread(file="test.csv")

df <- fread(file="test.csv", fill=TRUE)
head(df)

df <- fread(file="test.csv", fill=TRUE, quote="\"")

fwrite(Cereal, file="test.csv", col.names=TRUE, quote=FALSE)