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
Interface between foreach and parallel
Must register via registerDoParallel() with cluster info
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

Cluster plan:
plan(cluster, workers = 3)
foreach(n = rep(5, 3)) %dopar% rnorm(n)


Multicore plan:
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
```
