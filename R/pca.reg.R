pca.reg <- function(pca.object, group, matrix.name, myscale, plot=TRUE) {
    
    ###Create Scores DF
    pca.scores <- scores(pca.object, display='sites', scaling=1)
    reg.df <- data.frame(pca.scores)
    reg.df$group <- group

    ###Create Species DF
    pca.sp.df <- data.frame(scores(pca.object, display='species', scaling=1))

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

    ###Circle of Equilibrium
    radius <- sqrt(2/nrow(matrix.name))*myscale
    sig.sp <- vector()
    sp.row.num <- vector()
    
    for (i in 1:nrow(pca.sp.df)) {
        magnitude <- sqrt((pca.sp.df[i,][2]*myscale)^2 + (pca.sp.df[i,][1]*myscale)^2)

        if (magnitude > radius) {
            sig.sp <- append(sig.sp,row.names(pca.sp.df[i,]))
            sp.row.num <- append(sp.row.num, i)
        }
    }

    ###If statement for Significant Species
    if (length(sig.sp) > 0) {

        ###Get Species Vector Slope
        sp.df <- data.frame(pca.sp.df[sp.row.num,])
        sp.df$slope <- sp.df[2]/sp.df[1]
        names(sp.df) <- c('PC1', 'PC2', 'slope')
        
        ###Get Species Vector Angle of Inclination (radians)
        sp.df$angle <- atan(sp.df$slope)

        ###Find difference between group angles and sig species vector angles
        diff.df <- data.frame(sapply(group.angles, function(x) (x - sp.df$angle)))
        cos.diff <- apply(diff.df, 2, cos)

        if (length(cos.diff) == 2) {
            cors.df <- data.frame(sig.sp, cos.diff[1], cos.diff[2])
            names(cors.df) <- c('Descriptors', levels(group))
            return(cors.df)
        }

        else {
            ###Find correlation between the group regression lines and the species vector
            cors.df <- data.frame(sig.sp, apply(diff.df, 2, cos))
            names(cors.df) <- c('Descriptors', levels(group))
            return(cors.df)
        }


    }
    
    else {
        return('No Significant Species Found')
    }
    
}
