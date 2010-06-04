\name{xtable.confusionMatrix}
\alias{xtable.confusionMatrix}
\title{xtable method for confusionMatrix objects}
\description{
  xtable method for confusionMatrix objects
}
\usage{
\method{xtable}{confusionMatrix}(x, caption = NULL, label = NULL, align = NULL,
  digits = NULL, display = NULL, ...)
}
\arguments{
  \item{x}{object of class 'confusionMatrix' as produced 
    by the \code{confusionMatrix}}
  \item{caption}{LaTeX caption, see the \code{xtable} help page}
  \item{label}{LaTeX label, see the \code{xtable} help page}
  \item{align}{alignment specification, see the \code{xtable} help page}
  \item{digits}{number of digits to display, see the \code{xtable} help page}
  \item{display}{format of the columns, see the \code{xtable} help page}
  \item{\dots}{additional arguments to be passed to \code{xtable}}
}
\value{
  LaTeX table representing the confusion matrix
}
\author{Willem Talloen and Tobias Verbeke}
\seealso{\code{\link{confusionMatrix}}, \code{\link[xtable]{xtable}}}
\keyword{manip}
