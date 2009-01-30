\name{confusionMatrix}
\alias{confusionMatrix}
\alias{print.nlcvConfusionMatrix}
\alias{confusionMatrix.nlcv}
\title{compute a confusion matrix for the optimal number of features
       for a given technique used in the nested loop cross validation}
\description{
   The observed and predicted classes are cross-tabulated for a given
   classification technique used in the nested loop cross validation.
   The predicted class that is used to construct the confusion matrix 
   is the class that was predicted most of the time (\eqn{>= 50\%}{>= 50\%}) across 
   all runs of the nested loop.
}
\usage{
confusionMatrix(x, ...)
\method{confusionMatrix}{nlcv}(x, tech, proportions = TRUE)
\method{print}{nlcvConfusionMatrix}(x, ...)
}
\arguments{
  \item{x}{object for which a confusionMatrix should be produced, e.g.
    one produced by the \code{nlcv} function; for the print method, it
    is the object to be printed}
  \item{tech}{string indicating the classification technique for which
    the confusion matrix should be returned}
  \item{proportions}{logical indicating whether the cells of the matrix
    should contain proportions (\code{TRUE}) or raw counts (\code{FALSE})}
  \item{\dots}{Dots argument to pass additional parameters to the
               \code{confusionMatrix} or \code{print} methods}
}
\value{
  \code{confusionMatrix} produces an object of class \code{confusionMatrix}
  which directly inherits from the \code{ftable} class (representing the 
  confusion matrix)
}
\author{W. Talloen and T. Verbeke}
\keyword{manip}
