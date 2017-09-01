percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

check.model <- function(df.train, n.periodos) {

        
    
    df.test <- df[df$PeriodoId == periodo,]
    num <- df.train[which(df.train$PeriodoId == df.test$PeriodoId), 1] + 1 
    df.window <- df.train[seq.int(from=num, to=num+n.periodos-1), ]
    
    # ############################################################################
    # Tomo como predictoras, los montos mensuales de cobranzas y 
    # las últimas 5 facturaciones
    # ############################################################################
    model <- lm(CobPeriodo ~  Cob1 + Cob2 + Cob3 + Cob4 + Cob5 + Cob6 + Cob7 + Cob8 + Cob9 + Cob10 + Cob11 + Cob12 + Fact1 + Fact2 + Fact3 + Fact4 +  Fact5 + Fact6 + Fact7 + Fact8 + Fact9 + Fact10 + Fact11 + Fact12, data = df.window)
    
    df.ret <- df.test[,c("Num", "PeriodoId", "Departamento", "Origen", "CobPeriodo")]
    
    return(predict(model, df.test))
}
    


sapply(df, )

check.model(201701, df, 36)


# ############################################################################
# 
# ############################################################################
library(RODBC) 
dbhandle <- odbcDriverConnect('driver={SQL Server};server=momdb2test;database=Newmigra;trusted_connection=true')

query = "select * 
                from    PresupuestoCobranzasMatrizCalculo2
                WHERE   1 = 1
                        AND Origen = 'Todos'
                        AND Departamento = 'Todos'
                ORDER BY PeriodoId DESC, Departamento, Origen"

df.completo <- sqlQuery(dbhandle, query)

df <- df.completo[df.completo$PeriodoId > 201403,]

model <- glm(CobPeriodo ~  Cob1 * Cob2 * Fact1, data = df)

predict(model, type="response") # predicted values
residuals(model, type="deviance") # residuals

confint(model)
exp(coef(model))
df["ProyectadoR"] <- predict(model, df)
df$DesvioR <- abs((df$ProyectadoR - df$CobPeriodo)/df$CobPeriodo)
df$DesvioRStr <- percent(df$DesvioR)
data.frame( Item=c("Promedio", "Máximo"), 
            Regresion=c(percent(mean(df$DesvioR)),percent(max(df$DesvioR)))
          )

# anova(model)
# summary.aov(model)
# summary(model)
# head(df)
# summary(df)

cor(
    as.matrix(df[, c("CobPeriodo", "Fact1", "Fact2")])
)


plot(grasas$edad, grasas$grasas, xlab = "Edad", ylab = "Grasas")
abline(regresion)

# plot(model)
# 

n <- df[, c("CobPeriodo", "Cob1", "Cob2", "Cob3", "Cob4", "Cob5", "Cob6", "Cob7", "Cob8", "Cob9", "Cob10", "Cob11", "Cob12", "Fact1", "Fact2", "Fact3", "Fact4", "Fact5", "Fact6", "Fact7", "Fact8", "Fact9", "Fact10", "Fact11", "Fact12")]

library("PerformanceAnalytics")
chart.Correlation(n, histogram=TRUE, pch=19)


library(corrplot)
M <- cor(n)
corrplot(M, method="circle")
corrplot(M, order="hclust", addrect=3)
corrplot(M, order="AOE", cl.pos="b", tl.pos="d", tl.srt=60)


