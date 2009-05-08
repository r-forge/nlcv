\name{nlcvData}
\alias{nlcvRF_SS}
\alias{nlcvTT_SS}
\alias{nlcvRF_WS}
\alias{nlcvTT_WS}
\alias{nlcvRF_WHS}
\alias{nlcvTT_WHS}
\alias{nlcvRF_R}
\alias{nlcvTT_R}
\alias{nlcvRF_SHS}
\alias{nlcvTT_SHS}
\docType{data}
\title{Simulated Datasets to Demonstrate nlcv Functionality}
\description{
  Four different datasets were simulated using different types and
  strengths of signal. For each of these datasets a nested loop 
  cross validation procedure is run using each of random forest and
  t test feature selection.
}
\usage{data(nlcvRF_R)}
\format{
  The given data sets are delivered in the form of objects of
  class \code{nlcv} as produced by the \code{nlcv} function
}
\details{
  The objects were created using the code given in the examples 
  section.
}
\examples{
\dontrun{
### create datasets
set.seed(415)
EsetStrongSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 10,
    nNoEffectCols = 0, betweenClassDifference = 2, withinClassSd = 0.5)
EsetWeakSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5,
    nNoEffectCols = 0, betweenClassDifference = 1, withinClassSd = 0.6)
EsetWeakHeteroSignal <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 5,
    nNoEffectCols = 5, betweenClassDifference = 1, withinClassSd = 0.6)
EsetRandom <- simulateData(nCols = 40, nRows = 1000, nEffectRows = 0, 
    nNoEffectCols = 0)

### run nested loop cross validation
nlcvRF_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = 10, 
    fsMethod = "randomForest", verbose = TRUE)
nlcvTT_SS <- nlcv(EsetStrongSignal, classVar = "type", nRuns = 10, 
    fsMethod = "t.test", verbose = TRUE)
nlcvRF_WS <- nlcv(EsetWeakSignal, classVar = "type", nRuns = 10, 
    fsMethod = "randomForest", verbose = TRUE)
nlcvTT_WS <- nlcv(EsetWeakSignal, classVar = "type", nRuns = 10, 
    fsMethod = "t.test", verbose = TRUE)
nlcvRF_WHS <- nlcv(EsetWeakHeteroSignal, classVar = "type", nRuns = 10, 
    fsMethod = "randomForest", verbose = TRUE)
nlcvTT_WHS <- nlcv(EsetWeakHeteroSignal, classVar = "type", nRuns = 10, 
    fsMethod = "t.test", verbose = TRUE)
nlcvRF_R <- nlcv(EsetRandom, classVar = "type", nRuns = 10, 
    fsMethod = "randomForest", verbose = TRUE)
nlcvTT_R <- nlcv(EsetRandom, classVar = "type", nRuns = 10, 
    fsMethod = "t.test", verbose = TRUE)
}
}
\keyword{datasets}
