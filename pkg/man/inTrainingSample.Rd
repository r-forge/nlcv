\name{inTrainingSample}
\alias{inTrainingSample}
\title{Function to define a learning sample based on balanced sampling}
\description{
  This function takes in a factor with class labels of the total
  dataset, draws a sample (balanced with respect to the different
  levels of the factor) and returns a logical vector indicating whether
  the observation is in the learning sample (\code{TRUE}) or 
  not (\code{FALSE}).
}
\usage{
inTrainingSample(y, propTraining = 2/3)
}
\arguments{
  \item{y}{factor with the class labels for the total data set}
  \item{propTraining}{proportion of the data that should be in a 
    training set; the default value is 2/3.}
}
\value{
  logical vector indicating for each observation in \code{y} whether
  the observation is in the learning sample (\code{TRUE}) or 
  not (\code{FALSE})
} % TODO add examples
\author{W. Talloen and T. Verbeke}
\keyword{manip} % TODO change keyword
