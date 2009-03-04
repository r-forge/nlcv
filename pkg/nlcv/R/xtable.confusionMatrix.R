#' xtable method for confusionMatrix objects, as produced by confusionMatrix methods
#' @param object object of class 'confusionMatrix'
#' @param '...' additional arguments for xtable, such as caption etc.
#' @S3method xtable confusionMatrix
xtable.confusionMatrix <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  tech <- attr(x, "tech")
  nFeaturesOptim <- attr(x, "nFeaturesOptim")
  mat <- x[1:2, 1:2] # assumes two-class problems
  dimnames(mat) <- list(observed = paste("observed", attr(x, "row.vars")[[1]]),
      predicted = paste("predicted", attr(x, "col.vars")[[1]])) 
  
  if(is.null(caption)) caption <- paste("Confusion matrix for classifier: ", tech,  
        "; Optimal number of features: ", nFeaturesOptim, sep = "")
  xtable:::xtable.matrix(mat, caption = caption, label = label, align = align,
      digits = digits, display = display, ...)
}
