c_dupli <- data.frame(id=c(1,1,2), value=c(10,20,30))
c_dupli$num <- ave(c_dupli$id, c_dupli$id, FUN = seq_along)
reshape(c_dupli, idvar="id", timevar="num", direction="wide")
aggregate(value ~ id, c_dupli, c)

c_dupli$id2 <- c_dupli$id
c_dupli <- c_dupli[c(1,3,2)]

grouped <- aggregate(value ~ id, c_dupli, length)
cols <- max(grouped$value)
rows <- length(grouped$value)
m <- matrix(0, ncol = cols + 1, nrow = rows)

aggregate( value ~ id, c_dupli, c)
table(rep(1:nrow(c_dupli), sapply(c_dupli$id, length)), 
      unlist(c_dupli$id, use.names=FALSE))

summary(df)
str(df)
y <- lapply(df, class)
unlist(y, use.names = TRUE)
str(df)
reshape(c_dupli, idvar="id", timevar="id2",   direction="wide")

t(test)
unstack(c_dupli)
unstack(c_dupli)

as.list(c_dupli)

require(reshape2)

dcast(c_dupli, "id", value.var="value")

reshape(c_dupli, timevar=c("value"), idvar=c("id"), direction="wide")

data.frame(unique(c_dupli[1]), 
           as.data.frame.matrix(xtabs(value ~ do.call(paste, c_dupli[1]), c_dupli)))

df.out <- aggregate(value ~ id, data=c_dupli, c)

str(df.out)

aggregate(c_dupli, by = list(c_dupli$id), FUN = c)

library(data.table)
dcast(c_dupli, id ~ rowid(id, prefix = "value"), value.var = "value")


df<-data.frame(id = c(rep("A",3),rep("B",2)),amount = c(10,54,23,34,76))
newdf<-cbind(data.frame(id = unique(df$id)),matrix(as.numeric(unlist(tapply(df$amount,df$id,identity))),nrow=length(unique(df$id)),byrow=T))

class(df.out)
df.out[2]

df.sec <- cbind(data.frame(id = unique(df$id)), 
                matrix(as.numeric(unlist(tapply(df$amount, df$id, identity))), 
                       nrow = length(unique(df$id)), byrow = T))



tmp <- aggregate(value ~ id, data=c_dupli, c)
within(tmp, 
       out<-data.frame(do.call('cbind', strsplit(as.character(tmp), ',', fixed=FALSE))))


df.out <- aggregate(value ~ id, data=c_dupli, c)

df1 <- c_dupli[with(c_dupli, order(id)), ]
idx <- c_dupli$id[!duplicated(c_dupli$id)]
df.dwin <- cbind(data.frame(id=idx), 
                 as.data.frame(matrix(df1$value, 
                                      nrow=length(idx), byrow=TRUE)))




list <- "id,mes,personas
199,1,2
199,2,263066
199,3,266504
199,4,177196
199,5,263066
199,6,266504
199,7,177196
199,8,263066
199,9,266504
199,10,177196
199,11,3
199,12,4
4,1,9
4,2,2
4,3,266504
4,4,177196
4,5,263066
4,6,266504
4,7,177196
4,8,263066
4,9,266504
4,10,177196
4,11,1333
4,12,3
57,1,4
57,2,4
57,3,266504
57,4,177196
57,5,263066
57,6,266504
57,7,177196
57,8,263066
57,9,266504
57,10,177196
57,11,4444
57,12,4"

df = as.data.frame(read.table(textConnection(list), header=TRUE, sep=','))
x = state.x77
aggregate(personas ~ id, data=df[df$mes %in% c(1,2,12),], sum)

aggregate(x=df[df$mes %in% c(1,2,12),], by=list(df$id), FUN=sum)
aggregate()
chickwts

aggregate(by="id", subset=personas, data=df[df$mes %in% c(1,2,12),], FUN=sum)
state.region
aggregate(state.x77, list(Region = state.region), mean)



library(ggplot2)

# Here's some data I had lying around
tb <- structure(list(region = c("Africa", "Asia", "Latin America", 
                                "Other", "US-born"), ncases = c(36L, 34L, 56L, 2L, 44L)), .Names = c("region", 
                                                                                                     "ncases"), row.names = c(NA, -5L), class = "data.frame")


# A bar chart of counts
ggplot(tb, aes(x = region, weight = ncases, fill = region)) +
    geom_bar()

# Pie chart.  Forgive me, Hadley, for I must sin.
ggplot(tb, aes(x = factor(1), weight = ncases, fill = region)) +
    geom_bar(width = 1) +
    coord_polar(theta = "y") +
    labs(x = "", y = "")

# Percentage pie.
ggplot(tb, aes(x = factor(1), weight = ncases/sum(ncases), fill = region)) +
    geom_bar() +
    scale_y_continuous(formatter = 'percent') +
    coord_polar(theta = "y") +
    labs(x = "", y = "")


# Waffles
# How many rows do you want the y axis to have?
ndeep <- 5

# I need to convert my data into a data.frame with uniquely-specified x
# and y coordinates for each case
# Note - it's actually important to specify y first for a
# horizontally-accumulating waffle
# One y for each row; then divide the total number of cases by the number of
# rows and round up to get the appropriate number of x increments
tb4waffles <- expand.grid(y = 1:ndeep,
                          x = seq_len(ceiling(sum(tb$ncases) / ndeep)))

# Expand the counts into a full vector of region labels - i.e., de-aggregate
regionvec <- rep(tb$region, tb$ncases)

# Depending on the value of ndeep, there might be more spots on the x-y grid
# than there are cases - so fill those with NA
tb4waffles$region <- c(regionvec, rep(NA, nrow(tb4waffles) - length(regionvec)))

# Plot it
ggplot(tb4waffles, aes(x = x, y = y, fill = region)) + 
    geom_tile(color = "white") + # The color of the lines between tiles
    scale_fill_manual("Region of Birth",
                      values = RColorBrewer::brewer.pal(5, "Dark2"))


###
###
waffle <- function(x, rows, cols = seq_along(x), ...) {
    xx <- rep(cols, times = x)
    lx <- length(xx)
    m <- matrix(nrow = rows, ncol = (lx %/% rows) + (lx %% rows != 0))
    m[1:length(xx)] <- xx
    
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    
    par(list(...))
    plot.new()
    o <- cbind(c(row(m)), c(col(m))) + 1
    plot.window(xlim = c(0, max(o[, 2]) + 1), ylim = c(0, max(o[, 1]) + 1),
                asp = 1, xaxs = 'i', yaxs = 'i')
    rect(o[, 2], o[, 1], o[, 2] + .85, o[, 1] + .85, col = c(m), border = NA)
    
    invisible(list(m = m, o = o))
}


cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
m <- waffle(c(80, 30, 20, 10), rows = 8, cols = cols, mar = c(0,0,0,7),
            bg = 'cornsilk')
legend('right', legend = LETTERS[1:4], pch = 15, col = cols, pt.cex = 2,
       bty = 'n')


data(Lock5Data)
library(Lock5Data)


bases <- c("A","G","C","T")
ADN <- sample(bases,size = 2000,replace = TRUE)

# Crear un vector con una base aleatoria y que sea distinta a la original
ADN2 = sapply(ADN, function(x){sample(bases[bases!=x],1)})

# Seleccionar cuales elementos seran reemplazados con prob 10%
reemplazar = runif(n = length(ADN)) < 0.1

# Reemplazar por el valor alternativo
ADN[reemplazar] = ADN2[reemplazar]

ice_root = "/home/pmoracho/Tmp/H2o"
train.csv <- tempfile(pattern = "train", tmpdir = ice_root, fileext = ".csv")
write.csv(train, file=train.csv)

library(h2o)
h2o.init(nthreads=-1, max_mem_size = "1g", ice_root = "/home/pmoracho/Tmp/H2o") 

TRAIN_H20 <- h2o.importFile(train.csv, sep=",")

n <- 10
train <- do.call("rbind", replicate(n, train, simplify = FALSE))
format(object.size(train), units="Mb")

h2o.importFile(path = )
TRAIN_H20 <- as.h2o(train) 




f<-function(x) merge(x,df1[agrep(x$Name[1],df2$codigo),],all=TRUE)
do.call(rbind,by(df1,df1$Name,f))

df1
df2





match
df3



df3[agrep(df3$codigo, df3$Name),]

# I slightly modified your data to test multiple matches    
a<-data.frame(aID=c("1234","1234","4567","6789","3645"),aInfo=c("blue","blue2","green","goldenrod","cerulean"))
b<-data.frame(bID=c("4567","(1234)","6789","23645","63528973"), bInfo=c("apple","banana","kiwi","pomegranate","lychee"))

f<-function(x) merge(x,b[agrep(x$aID[1],b$bID),],all=TRUE)
do.call(rbind,by(a,a$aID,f))
a
b

df3 <-merge(df2,df1,by=NULL)
df3[grep(df3$codigo, df3$Name),]

df3[which(grep(df3[,1],df3[,4])),]

lapply(df3, function {return grep(df3$codigo, df3$Name)})

sapply(df3, function(x){sample(bases[bases!=x],1)})

library(data.table)

txt1 <- "Name,V1,V2
ABCD,L21A,J32F
JKLM,P21B,R52L
OPQR,K31A,L23P
AXXX,L21A,J32F
AXJK,L21A,J32F
"

txt2 <- "codigo,V1,V2
B.*C,verde,mediano
J.*K,azul,chico
Q.*R,Morado,grande"

df1 <- as.data.frame(read.table(textConnection(txt1), header=TRUE, sep=','))
df2 <- as.data.frame(read.table(textConnection(txt2), header=TRUE, sep=','))

df3 <- merge(df1,df2,by=NULL)
df3[apply(df3, 1, function(x) grepl(x[4], x[1])),]
