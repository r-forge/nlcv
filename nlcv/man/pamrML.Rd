\name{pamrML}
\alias{pamrML}
\alias{print.pamrML}
\alias{predict.pamrML}
\title{Wrapper function around the pamr.* functions}
\description{
  The pamrML functions are wrappers around \code{pamr.train} and
  \code{pamr.predict} that provide a more classical R modelling
  interface than the original versions.
}
\usage{
pamrML(formula, data, ...)
\method{print}{pamrML}(x, ...)
\method{predict}{pamrML}(object, newdata, ...)
}
\arguments{
  \item{formula}{model formula}
  \item{data}{data frame}
  \item{x}{object of class \code{pamrML}}
  \item{object}{object of class \code{pamrML}}
  \item{newdata}{data frame containing new observations for which predicted
    values will be estimated based on the model contained in \code{object}}
  \item{\dots}{further arguments to the \code{print} method}
}
\details{
  The name of the response variable is kept as an attribute in the
  \code{pamrML} object to allow for predict methods that can be
  easily used for writing converter functions for use in the 
  \code{MLInterfaces} framework. 
}
\value{
  For \code{pamrML} an object of class \code{pamrML} which
  adds an attribute to the original object returned by
  \code{pamr.train} (or \code{pamrTrain}).
  
  The \code{print} method lists the names of the different
  components of the \code{pamrML} object.
  
  The \code{predict} method returns a vector of predicted values
}
\author{Tobias Verbeke}
\seealso{\code{\link[pamr]{pamr.train}}, \code{\link[pamr]{pamr.predict}}}
\examples{
  set.seed(120)
  x <- matrix(rnorm(1000*20), ncol=20)
  y <- sample(c(1:4), size=20, replace=TRUE)
  # for original pam
  mydata <- list(x=x, y=y)
  mytraindata <- list(x=x[,1:15],y=factor(y[1:15]))
  mytestdata <-  list(x = x[,16:20], y = factor(y[16:20]))

  # for formula-based methods including pamrML
  alldf <- cbind.data.frame(t(mydata$x), y)
  traindf <- cbind.data.frame(t(mytraindata$x), y = mytraindata$y)
  testdf <- cbind.data.frame(t(mytestdata$x), y = mytestdata$y)

  ### create pamrML object
  pamrMLObj <- pamrML(y ~ ., traindf)
  pamrMLObj

  ### test predict method
  predict(object = pamrMLObj, newdata = testdf, 
      threshold = 1) # threshold compulsory
}
\keyword{models}
