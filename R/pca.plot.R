pca.plot <- function(pca.object, col.factor, pch.factor, matrix.name,  scaling = 1, main = NULL, myscale = 0.25, cex = 1, circle=TRUE) {

    #Get scores for species and sites to use as coloring and labeling factors
    pca.scores <- scores(pca.object, display='sites', scaling=scaling)
    pca.species <- scores(pca.object, display='species', scaling=scaling)

    #Setup plot such that all of the axes will appear
    par(mar=c(5,6,4,2)+0.1)

    #Plot the biplot
    plot(pca.object, display='sites', scaling=scaling, type='n', main=paste(main),
         xlab=paste('PC1 \n Proportion Explained = ', round(pro.eig(pca.object)[1]*100, 2), '%'),
         ylab=paste('PC2 \n Proportion Explained = ', round(pro.eig(pca.object)[2]*100, 2), '%'))
    points(pca.scores, col=unclass(col.factor) + 1, pch=unclass(pch.factor) + 15, cex=cex)
    bparrows(pca.species, myscale=myscale, matrix.name)
    legend('topright', paste(levels(pch.factor)), pch=levels.to.num(pch.factor)+15)
    legend('topleft', paste(levels(col.factor)), col=levels.to.num(col.factor) + 1, pch=15)

    #Circle of Equlibrium
    if (circle == TRUE) {
        radius <- sqrt(2/nrow(matrix.name))*myscale
        xx <- radius*cos(seq(0,2*pi, length.out=1000))
        yy <- radius*sin(seq(0,2*pi, length.out=1000))
        lines(xx,yy, col='red')
    }

}
