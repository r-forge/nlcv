\name{topFeatures}
\alias{topFeatures}
\title{Extract the Top n Features from a nlcv Object}
\description{
	Function to extract the top n features from a nlcv object
}
\usage{
topFeatures(nlcvObj, n = 5, method = "percentage")
}
\arguments{
  \item{nlcvObj}{object of class \code{nlcv}, as produced 
                 by function \code{nlcv}}
  \item{n}{number of features to extract and rank}
  \item{method}{method used to rank the features; one of \code{percentage}
     (percentage of runs the feature is selected in the top n), 
     \code{meanrank} (mean rank of the feature across runs) or \code{medianrank} 
     (median rank of the feature across runs)}
}
\details{
  The top n features are extracted across all runs
  of the nested loop cross-validation. After ranking
  on their frequency of selection, the top n are retained 
  and returned.
}
\value{
  a data frame of one column (\texttt{percentage})
  with percentages reflecting the frequency of selection 
  of a feature in the top n across all runs; 
  the features are sorted on decreasing frequency.
}
\examples{
  data(nlcvRF_SS)
  topFeatures(nlcvRF_SS, n = 7, method = "medianrank")
}
\author{W. Talloen and T. Verbeke}
\keyword{manip}
