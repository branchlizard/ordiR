bparrows <- function(x, myscale = 1, matrix.name, ...) {
    arrows(x0=0, y0=0, x1=myscale*x[,1], y1=myscale*x[,2], col='blue')
    text(myscale*x, labels=as.factor(names(matrix.name)))
}
