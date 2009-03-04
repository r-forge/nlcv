\name{pamrI}
\alias{pamrI}
\title{Instance of a learnerSchema for pamr models}
\description{
  This object is an instance of the learnerSchema object
  and will be typically used as the \code{.method} argument
  of an \code{MLearn} call.
}
\usage{
pamrI
}
\author{Tobias Verbeke}
\seealso{\code{\link[MLInterfaces]{MLearn}}}
\examples{
  set.seed(120)
  x <- matrix(rnorm(1000*20), ncol=20)
  y <- sample(c(1:4), size=20, replace=TRUE)
  alldf <- cbind.data.frame(t(x), y)

  # assure it is a factor (otherwise error message)
  alldf$y <- factor(alldf$y) 

  (mlobj <- MLearn(y ~ .,
      data = alldf,
      .method = pamrI,
      trainInd = 1:15))
}
\keyword{models}
