\name{rankDistributionPlot}
\alias{rankDistributionPlot}
\title{Plot the Distribution of Ranks of Features Across nlcv Runs}
\description{
  This plot offers an overview of the distribution of the ranks of 
  the n best-ranked features. The order of the features is determined
  by the median rank of the feature across all nlcv runs.
}
\usage{
rankDistributionPlot(nlcvObj, n = 5, ...)
}
\arguments{
  \item{nlcvObj}{object of class \code{nlcv} as produced by the \code{nlcv} function}
  \item{n}{number of features for whicht the distribution should be displayed}
  \item{\dots}{additional arguments to the boxplot functions (such as \code{main},
          \code{sub}, etc.}
}
\value{
  For each of the n features, a boxplot is displayed.
}
\author{Willem Talloen and Tobias Verbeke}
\examples{
  data(nlcvRF_SS)
  rankDistributionPlot(nlcvRF_SS, n = 9)
}
\keyword{manip}
