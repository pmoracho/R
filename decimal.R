library(bit64)

decimal <- function(x, prec=2) {
  
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  if (length(prec) != 1 || prec %% 1 != 0) stop("'prec' must be an escalar integer")
  
  exp <- (10 ^ prec)
  
  i64  <- as.integer64(x * exp)
  cat(i64, sep ="\n")
  i64 <- i64 + as.integer64((x %% 1) * exp)
  cat(i64, sep ="\n")

  structure(.Data = as.numeric(i64/exp), 
            class = "decimal", 
            prec = prec, 
            x_int = i64
            )
}

decimal(12345.678,2)

print.decimal <- function(x, ...) {
  prec <- attr(x, "prec")
  values <- unclass(x) / 10 ^ prec
  cat(paste0("decimal(", prec, "): "), trimws(format(values, nsmall=prec)))
}

decimal.as.numeric <- function(x) {
  if (!is.decimal(x)) return(as.numeric(x))
  prec <- attr(x, "prec")
  as.numeric(unclass(x) / (10 ^ prec))
}

is.decimal <- function(x) {
  class(x) == "decimal"
}

fun.decimal <- function(x, fun, ...) {
  if (!is.numeric(x))  stop("x must be a numeric vectors")
  prec <- attr(x, "prec")
  values <- fun((unclass(x) / 10 ^ prec), ...)
  decimal(values, prec)
}

op.add.decimal <- function(x, y, op) {
  
  if (is.decimal(x)) {
    prec <- attr(x, "prec")
    x <- unclass(x) / (10 ^ prec)
  }
  if (is.decimal(y)) {
    prec <- attr(y, "prec")
    y <- unclass(y) / (10 ^ prec)
  }
  # cat(x, sep='\n')
  # cat(y, sep='\n')
  values <- op(x, y)
  decimal(values, prec)
  
}

op.mul.decimal <- function(x, y, op) {
  
  prec <- attr(x, "prec")
  x <- unclass(x)
  if (is.decimal(y)) y <- unclass(y) / (10 ^ prec)
  # cat(x, sep='\n')
  # cat(y, sep='\n')
  values <- op(x, y) / (10 ^ prec)
  decimal(values, prec)
}

op.exp.decimal <- function(x, y, op) {
  
  prec <- attr(x, "prec")
  x <- unclass(x) / (10 ^ prec)
  if (is.decimal(y)) y <- unclass(y) / (10 ^ prec)
  # cat(x, sep='\n')
  # cat(y, sep='\n')
  values <- op(x, y) 
  decimal(values, prec)
}

op.div.decimal <- function(x, y, op) {
  
  prec <- attr(x, "prec")
  x <- unclass(x)
  
  if (is.decimal(y))  y <- y * (10 ^ prec) else y <- y * (10 ^ prec)
  # cat(x, sep='\n')
  # cat(y, sep='\n')
  values <- op(x, y)
  decimal(values , prec)
}

sum.decimal <- function(x, ...) {fun.decimal(x, sum, ...)}
mean.decimal <- function(x, ...) {fun.decimal(x, mean, ...)}

`+.decimal` <- function(x, y) {op.add.decimal(x, y, `+`)}
`-.decimal` <- function(x, y) {op.add.decimal(x, y, `-`)}
`*.decimal` <- function(x, y) {op.mul.decimal(x, y, `*`)}
`/.decimal` <- function(x, y) {op.mul.decimal(x, y, `/`)}

`^.decimal` <- function(x, y) {op.exp.decimal(x, y, `^`)}
`%%.decimal` <- function(x, y) {op.add.decimal(x, y, `%%`)}
`%/%.decimal` <- function(x, y) {op.div.decimal(x, y, `%/%`)}

##############################################3
x <- decimal(2,2)
y <- decimal(2,5)
x + y

x - y
x * y
x / y
x ^ y
x %% y
x %/% y

300 %/% 200
300 %/% 200
3 %/% 2


x <- decimal(c(10,103,1.3, 8.567, NA), 2)

z <- 2.05 * (10 ^ 2)
z == as.integer(z)

decimal(2.05) - 1
decimal(2.05) + 1
decimal(2.05) - decimal(1)
decimal(2.05) + decimal(1)
decimal(2.05) * 2
decimal(2.05) ^ 2

2 ^ decimal(2)
2.05 ^ 2

decimal(2.05) %% 2
205 %% 200

unclass()

decimal(0.05)
mean(x)

sum(x)
as.numeric(x)

y1 <- c(1,1)
y2 <- c(1,1)
y1 + y2

class(x)

sum(x)

2.05 * 10 ^ 2

x / 100
print(x)
sloop::s3_dispatch(x)

identical(is.na, `^`)

205 + 200
205 - 200
205 * 2

library(glue)


check <- function(FUN, x, y) { 
  ret <- FUN(x, y)
  paste(decimal.as.numeric(x), substitute(FUN), decimal.as.numeric(y), "=", ret)
}


check(`+`, x, y)
check(`-`, x, y)
check(`*`, x, y)
check(`/`, x, y)

decimal.as.numeric(x)

decimal.as.numeric(2.0)

df <- data.frame(v = decimal(runif(10),2))
df

minusclass <- function (class, whichclass) 
{
  if (length(class)) {
    i <- whichclass == class
    if (any(i)) 
      class[!i]
    else class
  }
  else class
}

as.data.frame.decimal <- function (x, ...) 
{
  cl <- oldClass(x)
  on.exit(setattr(x, "class", cl))
  setattr(x, "class", minusclass(cl, "decimal"))
  ret <- as.data.frame(x, ...)
  k <- length(ret)
  for (i in 1:k) setattr(ret[[i]], "class", cl)
  ret
}

library(zoo)
library(tidyverse)

inte

as.decimal = function(x, p=2L) structure(as.integer64(x*10^p), class="decimal", precision=p)
print.decimal = function(x) print(as.double(x))
as.integer64.decimal = function(x) structure(x, class="integer64", precision=NULL) # this should not be exported
as.double.decimal = function(x) as.integer64(x)/(10^attr(x, "precision"))
is.decimal = function(x) inherits(x, "decimal")
"==.decimal" = function(e1, e2) `==`(as.integer64(e1), as.integer64(e2))
"+.decimal" = function(e1, e2) `+`(as.integer64(e1), as.integer64(e2))

d = as.decimal(12.69)
is.decimal(d)
#[1] TRUE
print(d)
#[1] 12.69
as.double(d)
#[1] 12.69
d + as.decimal(0.9)
#[1] 13.59
0.1 + 0.2 == 0.3
#[1] FALSE
as.decimal(0.1) + as.decimal(0.2) == as.decimal(0.3)
#[1] TRUE
#
library(Rmpfr)

x <- mpfr(2.34, 2)

x
mpfr(pi, 120)
