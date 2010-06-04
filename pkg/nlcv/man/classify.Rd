\name{classify}
\alias{classify}
\title{Wrapper to run all classifiers for one run}
\description{
  Wrapper function that will run all classification algorithms using
  a given split in training and test data (corresponding to one run
  in the outer loop of the double cross validation).
}
\usage{
classify(eset, trainingSample, testSample, classVar = "type")
}
\arguments{
  \item{eset}{Expression set on which the classifiers will be run}
  \item{trainingSample}{matrix with for each run the indices of the observations
    included in the training (or learning) sample}
  \item{testSample}{matrix with for each run the indices of the 
    observations included in the test (or validation) sample}
  \item{classVar}{String giving the name of the variable containing the
                  observed class labels} 
}
\value{
  A list with the following components
  \item{dlda}{estimated misclassification rate using 
      diagonal linear discriminant analysis}
  \item{svm}{estimated misclassification rate using 
      support vector machines}
  \item{randomForest}{estimated misclassification rate using 
      a random forest}
  \item{bagg}{estimated misclassification rate using 
      bagging} 
  \item{pam}{estimated misclassification rate using 
      the pam algorithm}
  \item{dlda.predic}{predicted values by linear discriminant analysis}
  \item{svm.predic}{predicted values by support vector machines}
  \item{randomForest.predic}{predicted values by random forests}
  \item{bagg.predic}{predicted values by bagg}
  \item{pam.predic}{predicted values by the pam algorithm}
}

\author{Willem Talloen and Tobias Verbeke}
\note{
  The different classification algorithms are called via the uniform interfaces
  of the \code{MLInterfaces} package.
} % TODO add examples{}
\keyword{data}
