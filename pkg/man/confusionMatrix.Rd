\name{confusionMatrix}
\alias{confusionMatrix}
\alias{print.confusionMatrix}
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
confusionMatrix(nlcvObj, tech, proportions = TRUE)
\method{print}{confusionMatrix}(x, ...)
}
\arguments{
  \item{nlcvObj}{object as produced by the \code{nlcv} function}
  \item{tech}{string indicating the classification technique for which
    the confusion matrix should be returned}
  \item{proportions}{logical indicating whether the cells of the matrix
    should contain proportions (\code{TRUE}) or raw counts (\code{FALSE})}
  \item{x}{Object of class 'confusionMatrix' as produced by the function of the 
            same name}
  \item{\dots}{Dots argument to pass additional parameters to the
               \code{print} method}
}
\value{
  \code{confusionMatrix} produces an object of class \code{confusionMatrix}
  which directly inherits from the \code{ftable} class (representing the 
  confusion matrix)
}
\author{W. Talloen and T. Verbeke}
\keyword{manip}
