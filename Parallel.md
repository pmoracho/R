# Paralel

## Dividir 

* Por Tarea
* Por Datos

## Modelos

* Memoria compartida
* Memoria distribuida

* Master - Worker: Hay un proceso maestro que crea y corrdina workers que son los que procesan los datos
* Map - Reduce -> Hadoop/Spark Data distribuida


## Paquetes

R packages
* Core package: parallel
* Parallel support for big data:
    - sparklyr, iotools
    - pbdR
* Embarrassingly parallel, master-worker model:
    - foreach, future.apply
    - snow, snowFT, snowfall
    - future


## Package parallel

```r
library(parallel)
ncores <- detectCores(logical = FALSE)
cl <- makeCluster(ncores)
(cl, x = ncores:1, fun = rnorm)
stopCluster(cl)
```

## Inicialización de los nodos

¿Por qué conviene inicializar los nodos?

* Cada nodo se inicia con un entorno limpio
* La comuniocación entr los nodos y el proceso maestro es costosa
* Buena práctica: inicializar los workers con todo lo que puedes se constante o un consumo innecesarios de tiempo:
    * Datos estáticos
    * Librerías
    * Funciones globales

Funciones para inicializar workers:

* `clusterCall()`:Evalua la misma función con los mismoa argumentos en todos los nodos

    ```r
    cl <- makeCluster(2)
    clusterCall(cl, function() library(janeaustenr))
     
    clusterCall(cl, function(i) emma[i], 20)
     
    [[1]]
    [1] "She was the youngest of the two daughters of a most affectionate,"

    [[2]]
    [1] "She was the youngest of the two daughters of a most affectionate,"
    ```

* `clusterEvalQ()`: Evalua una expresión en todos los nodos

    ```r
    cl <- makeCluster(2)
    clusterEvalQ(cl, {
        library(janeaustenr)
        library(stringr)
        get_books <- function() austen_books()$book %>% unique %>% as.character
    })

    clusterCall(cl, function(i) get_books()[i], 1:3)
 
    [[1]]
    [1] "Sense & Sensibility" "Pride & Prejudice"   "Mansfield Park"     

    [[2]]
    [1] "Sense & Sensibility" "Pride & Prejudice"   "Mansfield Park"
    ```

* `RclusterExport()` exporta determinados objetos en todos los nodos

    ```r
    books <- get_books()
    cl <- makeCluster(2)
    clusterExport(cl, "books")

    clusterCall(cl, function() print(books))
 
    [[1]]
    [1] "Sense & Sensibility" "Pride & Prejudice"   "Mansfield Park"     
    [4] "Emma"                "Northanger Abbey"    "Persuasion"         

    [[2]]
    [1] "Sense & Sensibility" "Pride & Prejudice"   "Mansfield Park"     
    [4] "Emma"                "Northanger Abbey"    "Persuasion"
    ```
    

## foreach, future.apply and Load Balancing


What is foreach for?
* Developed by Rich Calaway and Steve Weston.
* Provides a new looping construct for repeated execution.
* Supports running loops in parallel.
* Unified interface for sequential and parallel processing.
* Greatly suited for embarrassingly parallel applications.


foreach looping construct

library(foreach)
foreach(n = rep(5, 3)) %do% rnorm(n)
foreach(n = rep(5, 3), m = 10^(0:2)) %do% rnorm(n, mean = m)


Combining results
foreach(n = rep(5, 3), .combine = rbind) %do% rnorm(n)
foreach(n = rep(5, 3), .combine = '+') %do% rnorm(n)

List comprehension

foreach(x = sample(1:1000, 10), .combine = c) %:% 
    when(x %% 3 == 0 || x %% 5 == 0) %do% x
 

### foreach and parallel backends

Popular backends

* doParallel (parallel)
* doFuture (future)
* doSEQ (for consisent sequential interface)


Package doParallel (Rich Calaway et al.)

* Interface between foreach and parallel
* Must register via registerDoParallel() with cluster info

Quick registration:

library(doParallel)
registerDoParallel(cores = 3)

using multicore functionality for Unix-like systems (fork)
using snow functionality for Windows systems


Register by passing a cluster object:

library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)
will use snow functionality



### Using doParallel

Sequential:

    ```r
    library(foreach)
    foreach(n = rep(5, 3)) %do% rnorm(n)
    ```

Parallel:

    ```r
    library(doParallel)
    cl <- makeCluster(3)
    registerDoParallel(cl)

    foreach(n = rep(5, 3)) %dopar% rnorm(n)
    ```

### Package doFuture (Henrik Bengtsson)

* On top of the future package
* How to plan the future:
    *  sequential
    *  cluster
    *  multicore
    *  multiprocess

* future.batchtools: run processes on HPC clusters (Torque, Slurm, SGE etc.)


library(doFuture)
registerDoFuture()

* Cluster plan:

plan(cluster, workers = 3)
foreach(n = rep(5, 3)) %dopar% rnorm(n)


* Multicore plan:
plan(multicore)
foreach(n = rep(5, 3)) %dopar% rnorm(n)


```r
# Load the package
library("foreach")

# foreach() %do% construct
result <- foreach(let = letters, .combine = c) %do% 
                max_frequency(let, words = words, min_length=5)
                
# Plot results 
barplot(result, las = 2)


# foreach()%do% construct with 2 iterators
result <- foreach(let = letters, n = c(rep(2, 13), rep(6, 13)), .combine = c) %do%
                max_frequency(let, words = words, min_length=n)
            
# Plot results
barplot(result, las=2)

# Register doParallel with 3 cores
registerDoParallel(cores=3)

# foreach()%dopar% loop
res <- foreach(r = rep(1000, 100), .combine = rbind, 
            .packages = "extraDistr") %dopar% myrdnorm(r)
            
# Dimensions of res
dim_res <- dim(res)

# Function for doParallel foreach
freq_doPar <- function(cores, min_length = 5) {
    # Register a cluster of size cores
    registerDoParallel(cores = cores)
    
    # foreach loop
    foreach(let = chars, .combine = c, 
            .export = c("max_frequency", "select_words", "words"),
            .packages = c("janeaustenr", "stringr")) %dopar%
        max_frequency(let, words = words, min_length = min_length)
}

# Run on 2 cores
freq_doPar(2)

# Function for doFuture foreach
freq_doFut <- function(cores, min_length = 5) {
    # Register and set plan
    registerDoFuture()
    plan(cluster, workers = cores)
    
    # foreach loop
    foreach(let = chars, .combine = c) %dopar% 
        max_frequency(let, words = words, min_length = min_length)
}
```


### future and future.apply


Package future
Developed by Henrik Bengtsson (now also funded by R Consortium)
Uniform way to evaluate R expressions asynchronously
Provides a unified API for sequential and parallel processing of R expressions
Processing via a construct called future
An abstraction for a value that may be available at some point in the future    

What is a future?

Example in plain R:

x <- mean(rnorm(n, 0, 1))
y <- mean(rnorm(n, 10, 5))
print(c(x, y))

Via implicit futures:

x %<-% mean(rnorm(n, 0, 1))
y %<-% mean(rnorm(n, 10, 5))
print(c(x, y))


Via explicit futures:

x <- future(mean(rnorm(n, 0, 1)))
y <- future(mean(rnorm(n, 10, 5)))


Sequential and parallel futures

Sequential:

plan(sequential)
x %<-% mean(rnorm(n, 0, 1))
y %<-% mean(rnorm(n, 10, 5))
print(c(x, y))

Parallel:

plan(multicore)

x %<-% mean(rnorm(n, 0, 1))
y %<-% mean(rnorm(n, 10, 5))
print(c(x, y))

Package future.apply
Developed by Henrik Bengtsson
Provide parallel API for all the apply functions in base R using futures
Sibling to foreach
Functions: future_lapply(), future_sapply(), future_apply(), ...


Using lapply():

lapply(1:10, rnorm)
Using future_lapply() sequentially:

plan(sequential)
future_lapply(1:10, rnorm)

Using future_lapply() on a cluster:

plan(cluster, workers = 4)
future_lapply(1:10, rnorm)