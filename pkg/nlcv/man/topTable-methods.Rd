\name{topTable-methods}
\docType{methods}
\alias{topTable}
\alias{topTable-methods}
\alias{topTable,nlcv-method}
\title{Methods for topTable}
\description{
  Methods for topTable. topTable extracts the top n most important features
  for a given classification or regression procedure. 
}
\section{Methods}{
\describe{

nlcv

\item{fit = "nlcv"}{nlcv objects are produced by \code{nlcv}}
}
}
\arguments{
  \item{fit}{object resulting from a classification or regression procedure}
  \item{n}{number of features that one wants to extract from a table that
    ranks all features according to their importance in the classification
    or regression model}
  \item{method}{method used to rank the features; one of \code{percentage}
     (percentage of runs the feature is selected in the top n), 
     \code{meanrank} (mean rank of the feature across runs) or \code{medianrank} 
     (median rank of the feature across runs); \code{percentage} is the default
     method}
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
\author{W. Talloen and T. Verbeke}
\examples{
  data(nlcvRF_SS)
  topTable(nlcvRF_SS, n = 7, method = "medianrank")
}
\keyword{methods}
\keyword{manip}
