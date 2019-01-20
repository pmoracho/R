dummy.var <- function(type="i", rows=10, x, from=Sys.Date()) {
    
    if (type=="d") {
        f <- list(fun=sample, args=list(x=as.Date(from) + x, size=as.name("rows"), replace=TRUE))
    } else if (type=="n") {
        f <- list(fun=runif, args=list(min=min(x), max=max(x), n=as.name("rows")))
    } else if (type=="f") {
        f <- list(fun=sample, args=list(x=as.factor(x), size=as.name("rows"), replace=TRUE))
    } else if (type=="b") {
        f <- list(fun=sample, args=list(x=c(TRUE, FALSE), size=as.name("rows"), replace=TRUE))
    } else {
        f <- list(fun=sample, args=list(x=x, size=as.name("rows"), replace=TRUE))
    }
    
    do.call(f$fun, f$args)
}

df <- data.frame(
    date = dummy.var("d", 100, 1:1000, from='2015-01-01'),
    Parasitemia =  dummy.var("n", 100, 1:5),
    Genera = dummy.var("f", 100, c('Plasmodium','Haemoproteus')),
    Prevalence = dummy.var("n", 100, 0:55),
    Year = dummy.var("i", 100, c(2016, 2017))
)

date 

dummy.df <- function(rows, col, colclass) {
    
}