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

dummy.var(type='b', rows=10, x=100)

#' sample_date_time
#' Retorna un vector de fechas/hora random de una longitud dada
#' Basada en: https://stackoverflow.com/a/14721124/6836377
#'
#' @param n         : Cantidad de valores de la muestra
#' @param from_date : Desde que fecha (solo Fecha, por defecto la del sistema)
#' @param to_date   : Hasta que fecha (solo Fecha, por defecto la del sistema + 1 día)
#' @param sorted    : Resultados ordenados (Por defecto no)
#'
#' @return un vector del tipo POSIXct
#' @export
#'
#' @examples
#' 
#' sample_date_time(10)
#' sample_date_time(10, '2018-01-01', '2019-01-01')
#' class(sample_date_time(10, '2018-01-01', '2019-01-01', sorted = FALSE))
#' 
sample_date_time <- function(n, from_date=Sys.Date(), to_date=Sys.Date()+1, sorted = FALSE) {
    
    st <- as.POSIXct(as.Date(from_date))
    et <- as.POSIXct(as.Date(to_date))
    dt <- as.numeric(difftime(et, st, unit="sec"))
    if (sorted == TRUE) {ev <- sort(runif(n, 0, dt))} else {ev <- runif(n, 0, dt)}
    
    st + ev
}


#' sample_date
#' Retorna un vector de fechas random de una longitud dada
#' Basada en: https://stackoverflow.com/a/14721124/6836377
#'
#' @param n         : Cantidad de valores de la muestra
#' @param from_date : Desde que fecha (solo Fecha, por defecto la del sistema)
#' @param to_date   : Hasta que fecha (solo Fecha, por defecto la del sistema + 100 días)
#' @param sorted    : Resultados ordenados (Por defecto No)
#'
#' @return un vector del tipo POSIXct
#' @export
#'
#' @examples
#' 
#' sample_date(10)
#' sample_date(10, '2018-01-01', '2019-01-01')
#' class(sample_date(10, '2018-01-01', '2019-01-01', sorted = FALSE))
#' 
sample_date <- function(n, from_date=Sys.Date(), to_date=Sys.Date()+100, sorted = FALSE) {
    
    trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;
    
    st <- as.POSIXct(as.Date(from_date))
    et <- as.POSIXct(as.Date(to_date))
    dt <- as.numeric(difftime(et, st, unit="days"))
    if (sorted == TRUE) {ev <- sort(runif(n, 0, dt))} else {ev <- runif(n, 0, dt)}
    
    as.POSIXct(st + trunc(ev,0)*86400)
}


sample_logical <- function(n, sorted = FALSE, prob=NULL) {

    if (sorted == TRUE) {
        sort(sample(x=c(TRUE,FALSE), size=n, replace=TRUE, prob=prob))
    } else {
        sample(x=c(TRUE,FALSE), size=n, replace=TRUE, prob=prob)
    }
}


sample_character <- function(n, charsize = 10, sorted = FALSE) {
    
    vec <- unlist(lapply(1:n, FUN = function(x) paste(sample(LETTERS, charsize), collapse="")))
    if (sorted == TRUE) {sort(vec)} else {vec}
        
}

sample_character_numbers <- function(n, charsize = 10, sorted = FALSE) {
    
    NUMBERS <- c('1','2','3','4','5','6','7','8','9','0')
    data <- c(LETTERS, NUMBERS, NUMBERS, NUMBERS)
    vec <- unlist(lapply(1:n,FUN = function(x) paste(sample(data, charsize), collapse="")))
    if (sorted == TRUE) {sort(vec)} else {vec}
    
}