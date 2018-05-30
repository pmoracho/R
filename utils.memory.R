ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
        fn(get(x, pos = pos)))
    
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
    obj.dim <- t(napply(names, function(x)
        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size,obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    out <- out[c("Type", "PrettySize", "Rows", "Columns")]
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (head)
        out <- head(out, n)
    out
}

showMemoryUse <- function(sort="size", decreasing=FALSE, limit) {
    
    objectList <- ls(parent.frame())
    
    oneKB <- 1024
    oneMB <- 1048576
    oneGB <- 1073741824
    
    memoryUse <- sapply(objectList, function(x) as.numeric(object.size(eval(parse(text=x)))))
    
    memListing <- sapply(memoryUse, function(size) {
        if (size >= oneGB) return(paste(round(size/oneGB,2), "GB"))
        else if (size >= oneMB) return(paste(round(size/oneMB,2), "MB"))
        else if (size >= oneKB) return(paste(round(size/oneKB,2), "kB"))
        else return(paste(size, "bytes"))
    })
    
    memListing <- data.frame(objectName=names(memListing),memorySize=memListing,row.names=NULL)
    
    if (sort=="alphabetical") memListing <- memListing[order(memListing$objectName,decreasing=decreasing),] 
    else memListing <- memListing[order(memoryUse,decreasing=decreasing),] #will run if sort not specified or "size"
    
    if(!missing(limit)) memListing <- memListing[1:limit,]
    
    print(memListing, row.names=FALSE)
    return(invisible(memListing))
}

showMemoryUse()