\name{limmaTwoGroups}
\alias{limmaTwoGroups}
\title{Wrapper around limma for the comparison of two groups}
\description{
  Wrapper around limma for the comparison of two groups
}
\usage{
limmaTwoGroups(object, group)
}
\arguments{
  \item{object}{object of class ExpressionSet}
  \item{group}{string indicating the variable defining the two 
    groups to be compared}
}
\details{
  Basically, the wrapper combines the \code{lmFit}, \code{eBayes}
  and \code{topTable} steps
}
\value{
  \code{topTable} output for the second (i.e. slope) coefficient
  of the linear model.
}
\references{
  Smyth, G. K. (2004). Linear models and empirical Bayes methods
    for assessing differential expression in microarray experiments.
    \emph{Statistical Applications in Genetics and Molecular Biology},
    Vol. 3, No. 1, Article 3.
    
  \url{http://www.bepress.com/sagmb/vol3/iss1/art3}
}
\author{Tobias Verbeke}
% TODO add example{}
\keyword{models}
\keyword{regression}
