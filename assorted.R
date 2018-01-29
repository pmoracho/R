data_min <- data.frame("cond"=c("a","b","c"),"min"=c(1,3,1))
data <- data.frame("cond"=c("a","b","b","a","c"),"val"=c(0,2,4,7,0))

data
data_min

merge(data,data_min, by="cond")[data$val>data_min$min,]

data[data_min[match(data$cond, data_min$cond),]$min <= data$val,]



# Defining my long function that checks if each string in a
# vector contains a substring with a "bad" string in it.
# If it does, that whole substring is replaced with a new string:
library(stringr)
mystring_replace = function(strings_vector, badstring, newstring){
    with_string <- grepl(badstring, strings_vector)  # what elements contain badstring?
    mysplits <- str_split(string = test$a[with_string], pattern = ', ') # split those elements with badstring based on ', '
    for (i in 1:length(mysplits)) {   # loop through the list of splits:
        allstrings <- mysplits[[i]]
        for (ii in 1:length(allstrings)) {  # loop through substrings
            if (grepl(badstring, allstrings[ii])) mysplits[[i]][ii] <- newstring
        }
    }
    for (i in seq_along(mysplits)) {  # merge the split elements back together
        mysplits[[i]] <- paste(mysplits[[i]], collapse = ", ")
    }
    strings_vector[with_string] <- unlist(mysplits)
    return(strings_vector)
}


# Test
mystring_replace(test$a, badstring = '_aaa', newstring = "NEW")


# Create an example data frame:
test <- data.frame(a = c("str1_element_1_aaa, str1_element_2",
                         "str2_element_1",
                         "str3_element_1, str3_element_2_aaa, str3_element_3"),
                   stringsAsFactors = F)
test
str(test)


badstring <- '_aaa'
newstring <- "NEW"

test$strs <- sapply(test$a, FUN=function(x) strsplit(x, ','))

test$strs <- sapply(sapply(test$a, FUN=function(x) strsplit(x, ',')), FUN = function(x) ifelse(grepl(badstring, x, fixed = TRUE), newstring, x))

test$strs <- lapply(test$strs, FUN = function(x) x[grep("_aaa", x)] <- "NEW")

str(test)

lista <- c("2", "1")
lista
new <- lapply(lista, FUN = function(x) ifelse(grep("1", x), "new", x))
new



lista[grep("1", lista)] <- "new"

grep

m<-matrix(c(1,4,6,7,0,0,1,1,0,0,3,2),3,4, byrow=TRUE)
m
m[1,c(1,2)]
m[1,c(3,4)]
m[c(2,3),c(1,2)]
m[c(2,3),c(3,4)]

c <- matrix(c(1,1,2,2,3,3,4,4,3,3,4,4),3,4, byrow=TRUE)
c

g <-lapply(split(m, c), matrix, nr=2)

g[[1]] * g[[3]]

class(g[[1]])


split(m, rep(1:2, each = 2))


h <- matrix(1:64, 8) 

k <- kronecker(matrix(1:16, 4, byrow = TRUE), matrix(1, 2, 2))
g <- lapply(split(h, k), matrix, nr = 2)


df <- data.frame(timestamp=c("09/10/2017 00:00:00:000000", "09/10/2017 00:01:00:000000", "09/10/2017 00:02:00:000000", 
                             "09/10/2017 00:03:00:000000", "09/10/2017 00:04:00:000000", "09/10/2017 00:05:00:000000",
                             "09/10/2017 00:06:00:000000", "09/10/2017 00:07:00:000000", "09/10/2017 00:08:00:000000", 
                             "09/10/2017 00:09:00:000000", "09/10/2017 00:10:00:000000", "09/10/2017 00:00:00:000000", 
                             "09/10/2017 00:01:00:000000", "09/10/2017 00:02:00:000000", "09/10/2017 00:03:00:000000", 
                             "09/10/2017 00:04:00:000000", "09/10/2017 00:05:00:000000", "09/10/2017 00:06:00:000000", 
                             "09/10/2017 00:07:00:000000", "09/10/2017 00:08:00:000000", "09/10/2017 00:09:00:000000", 
                             "09/10/2017 00:10:00:000000"), b=c (1:22))

str(df)
df[as.character(timestamp) >= "09/10/2017 00:10:00"]

str(df)

df$sexo <- as.factor(ifelse(as.character(df$sexo)=="H", "0", "1"))

levels(df)

df$sexo <- as.character(df$sex)
df$sex <- sapply(df$sexo, FUN=function(x) {factor(ifelse(as.character(x)=="H", 0, 1), levels=c(0,1), labels=c("Hombre", "Mujer"))})
str(df)

as.numeric(df$sexo)-1

NA * 1:10
n <- data.frame(sexo=factor(NA * 1:10, levels=c(0,1), labels=c("H", "M")))
n$sexo <-  as.factor(df$sex)
str(n)

df <- data.frame(sexo=sample(c("H","M"),10, replace = TRUE))
dput(df)
levels(df$sexo) <- c("Hombre", "Mujer") # Respetar el orden
as.numeric(df$sexo)-1
df$sex <- factor(as.character(df$sexo), levels=c(1, 0))
str(df)

df$sexo <- as.numeric(df$sexo== "Mujer")
df$sexo <- as.factor(levels = df$sexo, labels=c("Hombre", "Mujer"))
str(df)

structure(
    c("H", "M"),
    levels = c(1,0),
    class = c("ordered", "factor")
)
dput(df)

structure(list(sexo = structure(c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 
                                  1L, 0L, 1L), .Label = c("H", "M"), class = "factor")), .Names = "sexo", row.names = c(NA, 
                                                                                                                        -10L), class = "data.frame")

datos<-datos%>%rowwise(.)%>%mutate(Rend_mas_menos=
                ifelse(T,
                       (acum*acum2)^(1/(dias360(dmy(fpri_entrada),dmy(datos$Date[numr]))/360))-1,
                       acum^(1/(dias360(dmy(fpri_entrada),dmy(datos$Date[numr]))/360))-1
    +      ))

mutate

ifelse(T, 1, 2)


library(dplyr)
library(lubridate)
iris <- head(iris)

T <- 1
T

iris<-iris%>%rowwise(.)%>%mutate(fecha=ifelse(T, 1, 0))

mutate(iris, var = ifelse(NA, NA, NA))

A <- read.table(text = "ID    Var1    Var2    Var3
1       0       3       4
                2       1       5       0
                3       1       6       7", header=TRUE)

B <- read.table(text = "ID    Var1    Var2    Var3    
                1       2       4       2
                2       2       1       1
                3       0       2       1
                4       1       0       3", header=TRUE)

rbind(A,B)


setNames(A, tolower(names(A)))

aggregate(. ~ ID, data=rbind(A,B), sum)

Sys.setenv(JAVA_HOME='C:\Program Files\Java\jre7') # for 64-bit version
Sys.setenv(JAVA_HOME='C:\Program Files (x86)\Java\jre7') # for 32-bit version

library(rJava)

sessionInfo()


datf <- read.table(text = "ID   Sex Mor SITE
                   110   F   W    1
                   111   M   W    2
                   112   M   B    4
                   135   F   W    3
                   556   M   B    1
                   557   U   B    1
                   558   M   W    2",
                   header = TRUE)

# Remove rows with Sex = 'U' and droplevel
datf <- droplevels(datf[!datf$Sex == 'U',])
str(datf)

# Remove any row where ID in vector
datf <- datf[!(datf$ID %in% c(110, 558)),]
datf


smallestN<- function(a,b) {
    i<- 1
    repeat {
        if (all(i%%a:b == 0)) {
            break
        } else {
            i<-i+1
        }
    }
    return(i)
}

smallestN(1,6)
c(2:13)
seq.int(1, 13, length.out=13)
v <- c(2:13)
merge(v,v,all=TRUE)
crossprod(v,v)
expand.grid(v, v, v)
seq(c(2:13), )
m
m <- sapply(v, FUN=function(x) (x-1)*v)
table(m)
intersect(m, m)

library(numbers)
library(pracma)
pracma::Lcm(1, 13)
pra
mLCM(1:45)

lcm(3,4)

lcm <- function(x, y) {
    # choose the greater number
    if(x > y) {
        greater = x
    } else {
        greater = y
    }
    
    while(TRUE) {
        if((greater %% x == 0) && (greater %% y == 0)) {
            lcm = greater
            break
        }
        greater = greater + 1
    }
    return(lcm)
}

Lcm(1,13)

sort(rownames(mtcars))[1:20]

ancho <- 20
m <- matrix(rep(0,ancho * ancho), ancho, ancho)

m[sample(1:ancho, 1), sample(1:ancho,1)] <- 1

require("Caret")

df <- read.table(text = '
                 Quarter Coupon      Total
                 1   "Dec 06"  25027.072  132450574
                 2   "Dec 07"  76386.820  194154767
                 3   "Dec 08"  79622.147  221571135
                 4   "Dec 09"  74114.416  205880072
                 5   "Dec 10"  70993.058  188666980
                 6   "Jun 06"  12048.162  139137919
                 7   "Jun 07"  46889.369  165276325
                 8   "Jun 08"  84732.537  207074374
                 9   "Jun 09"  83240.084  221945162
                 10  "Jun 10"  81970.143  236954249
                 11  "Mar 06"   3451.248  116811392
                 12  "Mar 07"  34201.197  155190418
                 13  "Mar 08"  73232.900  212492488
                 14  "Mar 09"  70644.948  203663201
                 15  "Mar 10"  72314.945  203427892
                 16  "Mar 11"  88708.663  214061240
                 17  "Sep 06"  15027.252  121285335
                 18  "Sep 07"  60228.793  195428991
                 19  "Sep 08"  85507.062  257651399
                 20  "Sep 09"  77763.365  215048147
                 21  "Sep 10"  62259.691  168862119', header=TRUE)

newdf <- data.frame(Valor=numeric(), XXX=character(), YYY=character(), ZZZ=character())
for (col in colnames(df)) {
    newdf <- rbind(newdf, merge(df[, col], t(c(unlist(strsplit(col, "_"))))))
}
newdf

df <- read.table(text = "CYT_CD40L_exp1 MEMB_QVD_exp1 ORG_FasL_exp3
                 0 2 4
                 1 3 5", header = TRUE)


#Cargo la librería
library(tidyverse)
# Creo el df foo con los datos el ejemplo.
tribble(~CYT_CD40L_exp1,~MEMB_QVD_exp1, ~ORG_FasL_exp3, 
        0,             2,              4,
        1,             3,              5) -> foo

#En dos pasos cambio la estructura.
gather(foo) %>% 
    separate(key, into=c("XXX", "YYY", "ZZZ")) 



c(unlist(strsplit(colnames(df), "_")))

unlist(df)

df <- read.table(text = 'Cobranza F1 F2 F3 F4
                 100 200 100 600 800
                 200 400 200 600 1100
                 300 600 300 1100 1000
                 100 200 100 1200 800
                 100 200 100 500 400', header=TRUE)
model <- lm(Cobranza ~  F3, data=df)
summary(model)
predict.glm()



new.df <- data.frame(F1=c(500), F2=c(1200), F3=c(900), F4=c(400))
predict(model, new.df)
lm

library(dplyr)
boletin <-
    
    df %>%
    group_by_all() %>%
    mutate(prop = Cobranza / 523)
sum() %>%
    
    df %>%
    summarise_all(.funs = sum)

mutate(prop = Cobranza / 523)


fit <- train(Cobranza ~ F1 + F2, data = df, method = "glm", family = "binomial")

predict.nls



df <- read.table(text = 'Cobranza Facturacion
                 100 200
                 200 400
                 300 600
                 100 220
                 100 210', header=TRUE)

cobranza.lm <- nls (Cobranza ~  Facturacion, data=df)
newdata <-data.frame(Facturacion = c(150))
predict.poly(cobranza.lm, newdata, interval="predict") 

library(dplyr)
web_main <- data.frame(a=sample(1:2,size = 100, replace = T),
                       b=sample(1:100,size = 100, replace = T),
                       c=sample(1:50,size = 100, replace = T),
                       d=sample(1:25,size = 100, replace = T),
                       e=sample(1:10,size = 100, replace = T),
                       f=sample(1:100,size = 100, replace = T),
                       g=sample(1:50,size = 100, replace = T),
                       h=sample(1:25,size = 100, replace = T),
                       i=sample(1:10,size = 100, replace = T),
                       j=sample(1:2,size = 100, replace = T),
                       k=sample(1:2,size = 100, replace = T),
                       l=sample(1:5,size = 100, replace = T),
                       m=sample(1:5,size = 100, replace = T),
                       n=sample(1:2,size = 100, replace = T),
                       o=sample(1:2,size = 100, replace = T),
                       p=sample(1:25,size = 100, replace = T),
                       q=sample(1:50,size = 100, replace = T),
                       r=sample(1:5,size = 100, replace = T))

boletin <- web_main %>%
    group_by(a) %>%
    count() %>%
    mutate(prop = n / dim(web_main)[1])


df <- read.table(text = 'Dato Valor
                 uno 1
                 dos 2', header=TRUE)

pairs(df)


regmatches(tags, gregexpr("\\<.*?\\>", tags))


Sys.Date()

meteo <- data.frame(date=as.factor(c("01/01/2017 23:45")))

str(meteo)
strftime(meteo$date, format = "%d/%m/%Y %H:%M")

as.POSIXct(strftime(meteo$date, format = "%d/%m/%Y %H:%M"))
as.Date(p)


datos <- rbind(NA, 3,8,2,NA,1,2,NA,3,8,9,8)
l <- split(datos, cumsum(as.numeric(is.na(datos))))
d <- sapply(l, "[", i = seq_len(max(sapply(l, length))))
class(d)

cumsum(as.numeric(is.na(datos)))
https://github.com/cran/car/blob/master/R/outlierTest.R

library(ggplot2)
getMethod(aes_auto)

methods(ggplot)

methods(ggplot)
getAnywhere('aes')
methods(class="ggplot.data.frame")

TestString <- "white tiger roars.12.03.001-fast horse runs13.15.01.001-cat is useless 11.01.09.001-dog barks22.07.01.001"
gsub("[^a-z ]", "", TestString) 

ls(.GlobalEnv)[ls(.GlobalEnv) %!in% c("d","n")]

subset(ls(.GlobalEnv), !(ls(.GlobalEnv) %in% c("d","n")))
car:::outlierTest.lm


Books <- data.frame(Title = c("To kill a mockingbird (1960)", 
                              "Harry Potter and the order of the phoenix (2003)", 
                              "Of mice and men (something something) (1937)"))
str(Books)


Books$Year <- gsub("[\\(\\)]", "", regmatches(Books$Title, gregexpr("\\([0-9]{4}\\)", Books$Title)))

gsub("[\\(]", "", regmatches(Books$Title, gregexpr("(.*?) \\(", Books$Title)))

regmatches(Books$Title, gregexpr("(.*?) \\(", Books$Title))


Books$Year <- 
    lst <- list(A = c('aa','bb','B', 'cc', 'dd', 'C', 'ee', 'ff'), D = (c('aa', 'bb')))


ID<-c("‘3CS3PJ478’, ‘N6LQ1CMW1’, ‘4J9GNYHC8’, ‘6H8D3A1P0’")
paste("SELECT TOP 100 [AxiomaDate]
      ,[RiskModelID] ,[IID],[Factor1],[Factor2],[Factor3],[Factor4]FROM [PortfolioAnalytics].[Data_Axioma].[SecurityExposures]
      Where AxiomaDate IN (
      SELECT   MAX(AxiomaDate)
      FROM     [Portfol
      ioAnalytics].[Data_Axioma].[FactorReturns]
      GROUP BY MONTH(AxiomaDate), YEAR(AxiomaDate)
      AND  IID IN (", paste(ID, collapse = ", "), ")")


summary()

# generamos 1000 observaciones de 1 a 100
data <- as.integer(runif(min=0, max=100, n=1000))

quantile(data)
quantile(data, prob=c(0,0.25,0.5,0.75,1))

# deciles
quantile(data, prob=seq(0, 1, length = 101))

3[1]
v <-c(2,3)
A = matrix(c(1,-2,3,14),2,2)
class(A)
class(A[1,])
class(v)