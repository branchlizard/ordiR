#Custom NMDS Plotting Function#
nmds.plot <- function(nmds.object, col.factor, pch.factor, main = NULL, myscale = 1, cex = 1){
    nmds.scores <- nmds.object$points
    ordiplot(nmds.object, display='sites', type='n', main=paste(main, '\n Stress = ', nmds.object$stress))
    points(nmds.scores, col=unclass(col.factor) +1, pch=unclass(pch.factor) +15, cex=cex)
    legend('topright', paste(levels(pch.factor)), pch=levels.to.num(pch.factor)+15)
    legend('topleft', paste(levels(col.factor)), col=levels.to.num(col.factor) + 1, pch=15)
}
