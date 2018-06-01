df <- read.fwf(file="utlfile/padr/SELE-SAL-CONSTA.p20out2.20180331.tmp",widths = c(10,1))
head(df)
str(df)


muestra <- df[sample(1:nrow(df),10000),]
names(muestra) <- c("NR", "DV")
muestra$DV <- factor(muestra$DV)
muestra$NR <- as.character(muestra$NR)

muestra$DV <- as.character(muestra$DV)
muestra$NR <- as.character(muestra$NR)
str(muestra)
head(muestra)


muestra <- data.frame(DV=muestra$DV,
           NR=muestra$NR,
           do.call(rbind,strsplit(as.character(muestra$NR), ""))
)

data <- split(muestra, sample(c(0,1), size = nrow(muestra), prob = c(0.8, 0.2), replace = TRUE))
names(data) <- c("train", "test")

library(caret)

formula <- DV ~ NR
model <- train(formula, data=data$train, method="gbm")

data$test$DV2 <- NA
data$test$DV2 <- predict(model, data$test)
table(data$test$DV2, data$test$DV)

