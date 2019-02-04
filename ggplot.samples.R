library("ggplot2")

#########################################################################
## Line Graph
#########################################################################
## Sencillo
#########################################################################
df <- data.frame(x=1:10,y=c(10,11,13,16,17,15,16,19,18,17))
ggplot(data=df, aes(x=x, y=y)) +
    geom_line() +
    geom_point()

#########################################################################
## Dos grupos
#########################################################################
df <- data.frame(x=rep(1:10,2),
                 y=c(10,11,13,16,17,15,16,19,18,17,6,5,6,7,8,9,11,10,12,9), 
                 z=factor(rep(1:2,c(10,10))))

ggplot(data=df, aes(x=x, y=y, group=z, color=z)) +
    geom_line() +
    geom_point()

#########################################################################
# SpLine Graph
# The Spline chart type is a Line graph that plots a fitted curve through 
# each data point in a series. Line Charts show how a particular data 
# changes at equal intervals of time.
#########################################################################
df <- data.frame(x=rep(1:10,2),
                 y=c(10,11,13,16,17,15,16,19,18,17,6,5,6,7,8,9,11,10,12,9), 
                 z=factor(rep(1:2,c(10,10))))

ggplot(data=df, aes(x=x, y=y, group=z, color=z)) +
    geom_point() +
    geom_smooth(se=F, span=0.2)

#########################################################################
# StepLine Graph
# A stepped line graph (also called step chart) is a chart similar to a 
# line graph, but with the line forming a series of steps between data 
# points. A stepped line chart can be useful when you want to show the 
# changes that occur at irregular intervals. For example, price rise in 
# milk products, petrol, tax rate, interest rates, etc
#########################################################################
df <- data.frame(x=rep(1:10,2),
                 y=c(10,11,13,16,17,15,16,19,18,17,6,5,6,7,8,9,11,10,12,9), 
                 z=factor(rep(1:2,c(10,10))))

ggplot(data=df, aes(x=x, y=y, group=z, color=z)) +
    geom_point() +
    geom_step()


#########################################################################
# Area Graph
# An Area Chart or area graph are basically a line graph with the area 
# below the lined filled with colors or textures. Like line graphs area 
# charts are  used to represent the development of quantitative values 
# over a time period. It can also be used to compare two or more categories 
# and is similar to the Stacked Area Chart.
# Area charts often used to show overall trends over time rather than 
# specific values.
#########################################################################
df <- data.frame(x=rep(1:10,2),
                 y=c(10,11,13,16,17,15,16,19,18,17,6,5,6,7,8,9,11,10,12,9), 
                 z=factor(rep(1:2,c(10,10))))

ggplot(data=df, aes(x=x, y=y, group=z, color=z)) +
    geom_area(aes(x=x, y=y, fill=z),stat="identity",alpha=0.6)
    
