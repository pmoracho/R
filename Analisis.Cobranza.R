# ############################################################################
# Test de regresión lineal
# ############################################################################
percent <- function(x, digits = 2, format = "f", ...) {
    paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

df.completo <- read.table("./data/cobranza.txt")
df <- df.completo[df.completo$Departamento == "Todos" & 
                      df.completo$Origen == "Todos" &
                      df.completo$PeriodoId >= 201402,]



# ############################################################################
# Tomo como predictoras, los montos mensuales de cobranzas y 
# las últimas 5 facturaciones
# ############################################################################
# model <- lm(Total ~ Mes + Mes.0 + Mes.1 + Mes.2 + Mes.3 + Mes.4 + Mes.5 + Fact0 + Fact1 + Fact2 + Fact3 + Fact4 + Fact5 , data = df)
# model <- lm(CobTotal ~ Mes + Cob1 + Cob2 + Cob3 + Cob4 + Cob5 + Fact1 + Fact2 + Fact3 + Fact4 + Fact5, data = df)
model <- lm(CobTotal ~ Mes +  Fact1 + Fact2 + Fact3 + Fact4 + Fact5, data = df)

cor(df[, c("CobTotal")], 
    df[, c("Mes", "Cob1","Cob2","Cob3","Cob4","Cob5","Cob6","Fact1","Fact2","Fact3","Fact4","Fact5")])

# head(df)

df["Proyectado"] <- predict(model, df)
df$Desvio  <- abs((df$Proyectado - df$CobTotal)/df$CobTotal)
df$DesvioStr  <- percent((df$Proyectado - df$CobTotal)/df$CobTotal)

df[, c("Periodo", "Total", "Proyectado", "DesvioStr")]
percent(mean(as.numeric(df$Desvio)))
percent(max(as.numeric(df$Desvio)))


plot(model)


c <- df[, c("CobTotal", "Cob0","Cob1","Cob2","Cob3","Cob4","Cob5","Fact0","Fact1","Fact2","Fact3","Fact4","Fact5")]

library(corrplot)
corrplot.mixed(cor(df[, c("CobTotal", "Mes", "Cob0","Cob1","Cob2","Cob3","Cob4","Cob5","Fact0","Fact1","Fact2","Fact3","Fact4","Fact5")]), order="hclust", tl.col="black")

library("PerformanceAnalytics")
chart.Correlation(c, histogram=TRUE, pch=19)