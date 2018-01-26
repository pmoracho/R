# ############################################################################
# Funciones utiles
# ############################################################################
percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# x <- cob.filter(df, 201712, 3015, "LEG", 5)

cob.filter <- function(df, periodo, cc, depto, cant=5) {

    train.df <- head(df[df$Tipo == "R" & df$PeriodoId < periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,], 12*cant)
    train.df <- train.df[order(train.df$PeriodoId),]
    train.df$np <- 1:nrow(train.df)

    test.df <- df[df$Tipo == "P" & df$PeriodoId == periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,]
    test.df$CobTotalReal <- df[df$Tipo == "R" & df$PeriodoId == periodo & df$ConceptoCuentaId == cc & df$Departamento == depto, c("CobTotal")]
    test.df$CobTotalProyectadaADM <- test.df$CobTotal
    test.df$CobTotal = NA
    test.df$np <- nrow(train.df) + 1

    return(list(train.df=train.df,test.df=test.df))
}

test.models <- function(df, model.grid) {
    resultados <- data.frame()
    for (i in 1:nrow(model.grid)) {
        mt <- model.grid[i,]
        print(paste("Processing", mt$modelo, "pass", i, "of", nrow(model.grid)))
        data = cob.filter(df, mt$pp, mt$cc, mt$depto, mt$cant)
        ff <- as.formula(as.character(mt$ff))
        model <- train(ff, data=data$train.df, method = mt$modelo)
        data$test.df$CobTotal <- predict(model, newdata=data$test.df)

        if(ncol(resultados) == 0){
            resultados <- cbind(mt, data$test.df[, c("CobTotalProyectadaADM", "CobTotalReal", "CobTotal")])
        } else {
            resultados <- rbind(resultados, cbind(mt, data$test.df[, c("CobTotalProyectadaADM", "CobTotalReal", "CobTotal")]))

        }
    }
    resultados$DesvioADM <- abs((resultados$CobTotalReal-resultados$CobTotalProyectadaADM)*100/resultados$CobTotalReal)
    resultados$DesvioNuevo <- abs((resultados$CobTotalReal-resultados$CobTotal)*100/resultados$CobTotalReal)
    resultados$Status <- ifelse(resultados$DesvioADM > resultados$DesvioNuevo, "*", "")
    resultados$MontoDesvioADM <-  resultados$CobTotalReal * resultados$DesvioADM/100
    resultados$MontoDesvioNuevo <-  resultados$CobTotalReal * resultados$DesvioNuevo/100

    return(resultados)
}

# ############################################################################
# Recupero datos de la BD
# ############################################################################
library(RODBC)

SQL <- "SELECT 	PeriodoId,
                Mes,
                ConceptoCuentaId,
                Departamento,
                MonedaConsolidacion,
                CobTotal,
                Fact0,
                Fact1,
                Fact2,
                Fact3,
                Fact4,
                Fact5,
                Tipo
                FROM PresupuestoCobranzasMatrizCalculo
                ORDER BY PeriodoId DESC"


start.time <- Sys.time()
cn<-odbcDriverConnect("DRIVER={SQL Server};SERVER=momdb2test;Database=Contable_DB;uid=mecanus;pwd=mecanus")
df <- sqlQuery(cn, SQL)
close(cn)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

save(df, file="cobranza.Rda")
load("cobranza.Rda")

df <- read.csv("cobranza.mom.csv", as.is=T)

# ##################################################################################################################################
# CARET
# ##################################################################################################################################
library("caret")

models <- c("rpart", "rpart2", "qrf", "treebag", "gbm")
#models <- c("rpart")
model.formula <- CobTotal ~ np + Fact0 + Fact1 + Fact2 + Fact3 + Fact4 + Fact5
model.grid <- expand.grid(pp=c(201708, 201709, 201710, 201711, 201712),
                  #cc=c(3013,3014,3015),
                  cc=c(3015),
                  depto=c("LEG", "MAR", "PAT"),
                  modelo=models,
                  cant=4,
                  ff=Reduce(paste, deparse(model.formula)))
resultados <- test.models(df, model.grid)



model.grid <- as.data.frame(rbind(cbind("qrf", 3013, "LEG"),
                                  cbind("rpart", 3013, "MAR"),
                                  cbind("treebag", 3013, "PAT"),
                                  cbind("qrf", 3014, "LEG"),
                                  cbind("qrf", 3014, "MAR"),
                                  cbind("treebag", 3014, "PAT"),
                                  cbind("gbm", 3015, "LEG"),
                                  cbind("gbm", 3015, "MAR"),
                                  cbind("gbm", 3015, "PAT")
))
model.grid <- merge(x=model.grid, y=c(201708, 201709, 201710, 201711, 201712))
names(model.grid) = c("modelo", "cc", "depto", "pp")
model.grid$cant = 5
model.grid$ff=Reduce(paste, deparse(model.formula))

resultados <- test.models(df, model.grid)
resultados2 <- resultados

colSums(resultados[resultados$cc==3015,c("MontoDesvioADM","MontoDesvioNuevo")])
colSums(resultados[resultados$cc!=3015,c("MontoDesvioADM","MontoDesvioNuevo")])

aggregate(cbind(MontoDesvioADM,MontoDesvioNuevo, MontoDesvioADM-MontoDesvioNuevo) ~ ifelse(cc==3015, "Exterior","Local"), resultados, sum)

modres <- aggregate(cbind(MontoDesvioADM,MontoDesvioNuevo) ~ modelo + cc + depto, resultados, sum)
modres$status <- ifelse(modres$MontoDesvioADM>modres$MontoDesvioNuevo, '*', '')
modres

modres4 <- modres

df[df$ConceptoCuentaId==3015 & df$Departamento=="LEG",]


library("corrplot")

ndf <- head(df[df$Tipo == "R" &
            df$PeriodoId < 201801 &
            df$ConceptoCuentaId == 3013 &
            df$Departamento == "LEG",
            c(1,2,6,7,8,9,10,11,12)], 12*5)


plot(ndf[, c("nper", "CobTotal")]) +
abline(lm(CobTotal ~ nper, ndf))

m <- cor(ndf[, -c(1,2)])
corrplot(m, method="number")



ndf <- ndf[order(ndf[,1]),]
rownames(ndf) <- 1:nrow(ndf)
ndf$nper <- 1:nrow(ndf)
+

plot
+ Fact1

m <- cor(ndf)
corrplot(m, method="number")
head(df)

l.model <- lm(CobTotal ~ Fact0, ndf)
summary(l.model)

predict(l.model, data.frame(Fact0=417609.2))

library("rpart")

data = cob.filter(df, 201708, 3015, "LEG")
data$train.df <- head(data$train.df,12)
model <- rpart(model.formula, data=data$train.df)
data$test.df$CobTotal <- predict(model, newdata=data$test.df)
data$test.df