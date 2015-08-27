pca.reg2 <- function(pca.object, sp.name, group, plot=TRUE) {

    ###Create Scores DF
    pca.scores <- scores(pca.object, display='sites', scaling=1)
    reg.df <- data.frame(pca.scores)
    reg.df$group <- group

    ###Get slopes for each group and plot reg line if plot=TRUE
    group.slopes <- vector()
    group.r.sq <- vector()

    for (i in levels(reg.df$group)) {
        group.subset <- reg.df[reg.df$group == i,]
        group.model <- lm(PC2 ~ PC1, data=group.subset)
        r.sq <- summary(group.model)$r.squared


        if (plot == TRUE) {
            abline(group.model, lty=5, col='orange')
        }

        group.slope <- as.numeric(group.model$coef[2])
        group.slopes <- append(group.slopes, group.slope)
        group.r.sp <- append(group.r.sq, r.sq)
    }

    ###Get Angle of Inclination (radians) for each group
    group.angles <- atan(group.slopes)
    
    ###Get Species Vector Slope
    pca.sp.df <- data.frame(scores(pca.object, display='species', scaling=1))
    sp.row.num <-grep(sp.name, row.names(pca.sp.df))
    sp.vector <- pca.sp.df[sp.row.num,]
    sp.slope <- sp.vector[2]/sp.vector[1]

    ###Get Species Vector Angle of Inclination (radians)
    sp.angle <- atan(sp.slope)

    ###Find difference between group angles and species vector angle
    group.diff <- unlist(sapply(group.angles, function(x) (x - sp.angle)))
        
    ###Find correlation between the group regression lines and the species vector
    cors <- cos(group.diff)
    cors.df <- data.frame(levels(group), cors)
    names(cors.df) <- c('Group', paste(sp.name, 'Cor'))
    cors.df

}
