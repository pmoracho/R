# load("~/Downloads/cobranza.Rda")
library(rpart)
gen.data <- function(df, periodo, cc, depto) {
    
    train.df <- df[df$Tipo == "R" & df$PeriodoId < periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,]
    test.df <- df[df$Tipo == "P" & df$PeriodoId == periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,]
    test.df$CobTotalReal <- df[df$Tipo == "R" & df$PeriodoId == periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,c("CobTotal")]
    test.df$CobProyectadaADM <- df[df$Tipo == "P" & df$PeriodoId == periodo & df$ConceptoCuentaId == cc & df$Departamento == depto,c("CobTotal")]
    return(list(train.df=train.df, test.df=test.df))
}

##############################################################################################################################
# Recursive Partitioning and Regression Trees
##############################################################################################################################
ret <- gen.data(df, 201609, 3014, "LEG")
model.formula <- CobTotal ~ Fact0
model <- rpart(model.formula, method="anova", data=ret$train.df)
ret$test.df$CobTotal <- predict(model,ret$test.df)
format(ret$test.df[, c("CobTotal", "CobTotalReal", "CobProyectadaADM")], decimal.mark=".", big.mark=",", scientific=FALSE)

