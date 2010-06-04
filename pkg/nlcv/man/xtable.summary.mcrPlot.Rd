\name{xtable.summary.mcrPlot}
\alias{xtable.summary.mcrPlot}
\title{xtable method for summary.mcrPlot objects}
\description{
  xtable method for summary.mcrPlot objects
}
\usage{
\method{xtable}{summary.mcrPlot}(x, caption = NULL, label = NULL, align = NULL,
  digits = NULL, display = NULL, ...)
}
\arguments{
  \item{x}{object of class 'summary.mcrPlot' as produced 
    by the \code{summary.mcrPlot}}
  \item{caption}{LaTeX caption, see the \code{xtable} help page}
  \item{label}{LaTeX label, see the \code{xtable} help page}
  \item{align}{alignment specification, see the \code{xtable} help page}
  \item{digits}{number of digits to display, see the \code{xtable} help page}
  \item{display}{format of the columns, see the \code{xtable} help page}
  \item{\dots}{additional arguments to be passed to \code{xtable}}
}
\value{
  LaTeX table representing the summary of the mcrPlot output, i.e.
  the optimal number of features, the mean MCR and the standard deviation
  on the MCR for each of the classification methods used.
}
\author{Willem Talloen and Tobias Verbeke}
\seealso{\code{\link{summary.mcrPlot}}, \code{\link{mcrPlot}}, \code{\link[xtable]{xtable}}}
\examples{
  data(nlcvRF_SS)
  mp <- mcrPlot(nlcvRF_SS, plot = FALSE)
  smp <- summary(mp)
  xtable(smp)
}
\keyword{manip}
