library(readxl)
filename <- "~/Descargas/UltimosMovimientos.xls"
ultimos <- read_excel(filename, sheet = "Ultimos Movimientos", skip = 5)
deldia <- read_excel(filename, sheet = "Movimientos del Dia", skip = 5)

df <- as.data.frame(rbind(ultimos, deldia))
rm(list=c('ultimos', 'deldia'))
colnames(df) <- c('Fecha', 'Sucursal', 'Descripcion', 'Ref', 'Importe')
df$ingreso <- ifelse(df$Importe>0, TRUE, FALSE)

#movimientos <- df
#save(movimientos, file = "movimientos.banco.Rda")
load("movimientos.banco.Rda")

match[df$Ref %in% movimientos]

library(dplyr)
anti_join(df, movimientos)

str(movimientos)
movimientos<-movimientos[-3,]
str(df)
str(merge(movimientos, df))
