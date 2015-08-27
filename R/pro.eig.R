pro.eig <- function(pca.object) {
    eig.values <- vector()

    for (i in pca.object[8]$CA$eig) {
        eig.values <- append(eig.values, i)
    }

    eig.sum <- sum(eig.values)

    pc.1.prop <- eig.values[1]/eig.sum
    pc.2.prop <- eig.values[2]/eig.sum

    pc.axes <- c(pc.1.prop, pc.2.prop)
    pc.axes
}
