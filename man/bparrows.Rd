\name{bparrows}
\alias{bparrows}
\title{Biplot Arrows Function}
\usage{
bparrows(x, myscale = 1, matrix.name, ...)
}
\arguments{
\item{x}{pca species scores object.}

\item{myscale}{scale of arrows to be plotted Defaults to 1.}

\item{matrix.name}{name of data matrix.}
}
\description{
Biplot Arrows Function
}
\examples{
data(ffg.cast)
ffg.hel <- decostand(ffg.cast[,-1:-4], 'hellinger')
pca.hel <- rda(ffg.hel)
pca.scores <- scores(pca.hel, display='sites', scaling=1)
pca.species <- scores(pca.hel, display='species', scaling=1)
plot(pca.hel, display='sites', scaling=1, type='n', main='PCA Biplot')
points(pca.scores)
bparrows(pca.species, myscale=0.6, matrix.name=ffg.cast)
}
\keyword{vegan}

