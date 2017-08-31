#############################################################
# Gráfico tipo waffle
#############################################################
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
    return(p)
}


cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")
waffle(c(80, 30, 20, 10), rows = 8, cols = cols, mar = c(0,0,0,7),
            bg = 'cornsilk')
c <- legend('right', legend = LETTERS[1:4], pch = 15, col = cols, pt.cex = 2,
       bty = 'n')


