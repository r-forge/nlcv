\name{nlcv}
\alias{nlcv}
\title{Nested Loop Cross-Validation}
\description{
  This function first proceeds to a feature selection and then
  applies five different classification algorithms. % TODO add more content, Willem
}
\usage{
nlcv(eset, classVar = "type", nRuns = 2, propTraining = 2/3, classdist = c("balanced", "unbalanced"), 
  nFeatures = c(2, 3, 5, 7, 10, 15, 20, 25, 30, 35), fsMethod = c("randomForest", "t.test", "limma", "none"), 
  classifMethods = c("dlda", "randomForest", "bagg", "pam", "svm"), fsPar = NULL, initialGenes = seq(length.out = nrow(eset)), 
  geneID = "ID",
  storeTestScores = FALSE, verbose = FALSE)
}
\arguments{
  \item{eset}{ExpressionSet object containing the genes to classify}
  \item{classVar}{String giving the name of the variable containing the
                  observed class labels}
  \item{nRuns}{Number of runs for the outer loop of the cross-validation}
  \item{propTraining}{Proportion of the observations to be assigned to the
    training set. By default \code{propTraining = 2/3}.}
  \item{classdist}{distribution of classes; allows to indicate whether 
    your distribution 'balanced' or 'unbalanced'. The sampling strategy
    for each run is adapted accordingly.}
  \item{nFeatures}{Numeric vector with the number of features to be selected
    from the features kept by the feature selection method. For each number n
    specified in this vector the classification algorithms will be run using 
    only the top n features.}
  \item{fsMethod}{Feature selection method; one of \code{"randomForest"}, \code{"t.test"},
                 \code{"limma"} or \code{"none"}.}
  \item{classifMethods}{character vector with the classification methods to be
    used in the analysis; elements can be chosen among \code{"dlda"}, \code{"lda"}, \code{"nlda"}, \code{"dlda"}, 
    \code{"randomForest"}, \code{"bagg"}, \code{"pam"}, \code{"svm"}, \code{"ksvm"}}
  \item{fsPar}{List of further parameters to pass to the feature selection method;
    currently the default for \code{"randomForest"} is an empty \code{list()} 
    whereas for \code{"t.test"}, one can specify the particular test to be
    used (the default being \code{list(test = "f"}).}
  \item{geneID}{string representing the name of the gene ID variable in the
    fData of the expression set to use; this argument was added for people who
    use e.g. both Entrez IDs and Ensemble gene IDs}
  \item{initialGenes}{Initial subset of genes in the ExpressionSet on which
    to apply the nested loop cross validation procedure. By default all genes
    are selected.}
  \item{storeTestScores}{should the test scores be stored in the \code{nlcv}
    object? Defaults to \code{FALSE}}
  \item{verbose}{Should the output be verbose (\code{TRUE}) or not (\code{FALSE}).}
}
\details{
  TODO
}
\value{
  The result is an object of class 'nlcv'. It is a list with 
  two components, \code{output} and \code{features}.
  
  De \code{output} component is a list of five components, one
  for each classification algorithm used. Each of these components
  has as many components as there are elements in the \code{nFeatures}
  vector. These components contain both the error rates for each run
  (component \code{errorRate}) and the predicted labels for each run
  (character matrix \code{labelsMat}). 
  
  The \code{features} list is a list with as many components as there
  are runs. For each run, a named vector is given with the variable 
  importance measure for each gene. For t test based feature selection,
  P-values are used; for random forest based feature selection the
  variable importance measure is given.
}
\author{Willem Talloen and Tobias Verbeke}
\note{ 
  The variable importance measure used is the third column of the
  output returned by the \code{randomForest} function.
}
\keyword{htest}
