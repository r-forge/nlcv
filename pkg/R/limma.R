#' Wrapper for the limma function for the comparison of two groups
#' 
#' @param object object of class ExpressionSet
#' @param group string indicating the variable defining the two groups
#'               to be compared

limmaTwoGroups <- function(object, group){
  f <- as.numeric(as.factor(pData(object)[, group]))
  if (length(unique(f)) != 2)
    stop("Use 'limmaTwoGroups' only with a 'group' variable having two group levels")
  design <- model.matrix(~ f)
  fit <- lmFit(object, design)
  fit <- eBayes(fit)
  sortedResults <- topTable(fit, coef = 2, number = dim(object)["Features"])
  # coef = 2 because we are not interested whether the intercept is significant 
  # but whether group 2 is significantly different from group 1
  return(sortedResults)
}


