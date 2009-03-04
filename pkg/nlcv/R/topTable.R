setGeneric("topTable", function(fit, n, ...){
      standardGeneric("topTable")
})    

setOldClass("nlcv")

#' function to extract the top n features
#' @param fit object of class 'nlcv', as produced by function 'nlcv'
#' @param n number of features to extract and rank
#' @param method method used to rank the features; one of \code{percentage}
#' (percentage of runs the feature is selected in the top n), \code{meanrank}
#' (mean rank of the feature across runs) or \code{medianrank} (median rank of
#' the feature across runs)
#' @value returns a data frame with percentages reflecting
#' the frequency of selection of a feature in the top n
#' across all runs; the features are sorted on decreasing
#' frequency
#' @export
setMethod("topTable", "nlcv", 
    function(fit, n = 5, method = "percentage"){
  if (!inherits(fit, "nlcv")) 
    stop("The object is not of class 'nlcv'")
  if (!(method %in% c("percentage", "meanrank", "medianrank")))
  
  n <- match.arg(n)
  switch(method,
      percentage = {
        nRuns <- length(fit$features)
        selectedFeatures <- table(unlist(lapply(fit$features,
              function(x){names(x)[1:n]})))
        selectedFeatures <- selectedFeatures[order(selectedFeatures,
                decreasing = TRUE)][1:n]     
        selectedFeatures <- 100 * (selectedFeatures / nRuns)
        res <- data.frame(percentage = selectedFeatures)
      },
      meanrank = {
        orderedFeatureMatrix <- do.call("cbind", lapply(fit$features, function(x) rank(x[order(names(x))])))
        meansByFeature <- apply(orderedFeatureMatrix, 1, mean, na.rm = TRUE)
        selectedFeatures <- sort(meansByFeature)[1:n]
      res <- data.frame(meanrank = selectedFeatures)
      },
      medianrank = {
        orderedFeatureMatrix <- do.call("cbind", lapply(fit$features, function(x) rank(x[order(names(x))])))
        mediansByFeature <- apply(orderedFeatureMatrix, 1, median, na.rm = TRUE)
        selectedFeatures <- sort(mediansByFeature)[1:n]
        res <- data.frame(medianrank = selectedFeatures)
  })
  return(res)
})


