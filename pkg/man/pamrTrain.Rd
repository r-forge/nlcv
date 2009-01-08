\name{pamrTrain}
\alias{pamrTrain}
\title{Function providing a formula interface to pamr.train}
\description{
  Function that provides a classical R modelling interface, 
  using a \code{formula} and \code{data} argument
}
\usage{
pamrTrain(formula, data, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{formula}
  \item{data}{data frame}
  \item{\dots}{further arguments to be passed to \code{pamr.train}}
}
\value{
  Object that is perfectly identical to the object
  returned by \code{pamr.train}
}
\author{Tobias Verbeke}
\seealso{\code{\link[pamr]{pamr.train}}}
\examples{
  set.seed(120)
  x <- matrix(rnorm(1000*20), ncol=20)
  y <- sample(c(1:4), size=20, replace=TRUE)
  alldf <- cbind.data.frame(t(x), y)
  pamrTrain(y ~ ., alldf)
}
\keyword{models}
