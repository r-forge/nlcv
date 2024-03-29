\name{rocPlot}
\alias{rocPlot}
\title{
  Produce a ROC plot for a classification model belonging to a given technique 
  and with a given number of features.
}
\description{
  Produce a ROC plot for a classification model belonging to a given technique 
  and with a given number of features.
}
\usage{
rocPlot(nlcvObj, tech, nfeat, main = NULL, globalAUCcol = "#FF9900", ...)
}
\arguments{
  \item{nlcvObj}{
  object of class 'nlcv' as produced by the nlcv function
}
  \item{tech}{
  technique; character of length one; one of 'dlda', 'lda', 'nlda', 'qda', 'glm', 
            'randomForest', 'bagg', 'pam', 'svm' or 'ksvm' 
}
  \item{nfeat}{
   number of features used in the classification model; numeric of length one
}
  \item{main}{
  main title to be used for the ROC plot
}
  \item{globalAUCcol}{
  color for the global AUC (defaults to '#FF9900')
}
  \item{\dots}{
  further arguments for the plot call (such as sub e.g.)
}
}
\value{
  A ROC plot is drawn to the current device
}
\author{
 Tobias Verbeke
}
