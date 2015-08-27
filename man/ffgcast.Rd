\name{ffg.cast}
\alias{ffg.cast}
\docType{data}
\title{
Functional Feeding Group Matrix
}
\description{
This dataset consists of macroinvertebrate functional feeding group data
collected from Slab Camp Creek in Kentucky. 
}
\usage{data("ffg.cast")}
\format{
  A data frame with 6 observations on the following 9 variables.
  \describe{
    \item{\code{SAMPID}}{a character vector}
    \item{\code{STREAM}}{a factor with levels \code{SC}}
    \item{\code{CU}}{a factor with levels \code{pool} \code{riffle}}
    \item{\code{YR.POSTREST}}{a factor with levels \code{1} \code{2} \code{3}}
    \item{\code{COLLECTOR.FILTERER}}{a numeric vector}
    \item{\code{COLLECTOR.GATHERER}}{a numeric vector}
    \item{\code{HERBIVORE}}{a numeric vector}
    \item{\code{PREDATOR}}{a numeric vector}
    \item{\code{SHREDDER}}{a numeric vector}
  }
}
\examples{
data(ffg.cast)
}
\keyword{datasets}
