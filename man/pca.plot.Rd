% Generated by roxygen2 (4.1.1.9000): do not edit by hand
% Please edit documentation in R/pca.plot.R
\name{pca.plot}
\alias{pca.plot}
\title{Custom PCA Biplotting Function}
\usage{
pca.plot(pca.object, col.factor, pch.factor, matrix.name, scaling = 1,
  main = NULL, myscale = 0.25, cex = 1, circle = TRUE)
}
\arguments{
\item{pca.object}{PCA object from rda function (vegan).}

\item{col.factor}{Name of vector to be used to color the points.}

\item{pch.factor}{Name of vector to be used to create symbols.}

\item{matrix.name}{Name of the matrix containing the data.}

\item{scaling}{R-mode or Q-mode Defaults to 1.}

\item{main}{Title of plot.}

\item{myscale}{Scaling size for bparrows Defaults to 0.25.}

\item{cex}{Point size Defaults to 1.}

\item{circle}{Draw circle of equilibrium Defaults to TRUE}
}
\description{
A custom PCA biplotting function which allows the user to define the
color and type of the points based on the groups examined. Currently,
this function is limited to only two groups, but multiple levels of each
group is supported.
}
\examples{
data(ffg.cast)
ffg.hel <- decostand(ffg.cast[,-1:-4], 'hellinger')
pca.hel <- rda(ffg.hel)
pca.plot(pca.hel, ffg.cast$CU, ffg.cast$CU, ffg.hel, cex=1.5, main='Plot 1')
}
\keyword{vegan}

