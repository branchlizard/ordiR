levels.to.num <- function(x) {
    as.numeric(levels(as.factor(unclass(x))))
}
