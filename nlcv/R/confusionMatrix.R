confusionMatrix <- function(x, ...){
  UseMethod("confusionMatrix")
}

confusionMatrix.nlcv <- function(x, tech, proportions = TRUE){
  mcrObj <- mcrPlot(x, plot = FALSE)
  mcrSummary <- summary(mcrObj)
  # number of features: always optimal number of features
  nFeaturesOptim <- mcrSummary[tech, "nFeat_optim"]
  
  scoresObj <- scoresPlot(x, tech = tech, nfeat = nFeaturesOptim,
      plot = FALSE)
  observedClasses <- attr(x, "classVar")                      
  levelsObserved <- levels(observedClasses)
  oppObservedClasses <- factor(levelsObserved[(as.numeric(observedClasses) %% 2)+1],
      levels = levelsObserved)
  predictedClasses <- factor(levelsObserved[ifelse(scoresObj >= 0.5, 
              observedClasses, oppObservedClasses)], levels = levelsObserved)
  res <- ftable(predictedClasses ~ observedClasses, add.margins = TRUE)
  if (proportions) res <- prop.table(res)   
  # res <- addmargins(res)
  attr(res, "tech") <- tech
  attr(res, "nFeaturesOptim") <- nFeaturesOptim
  class(res) <- c("nlcvConfusionMatrix", "confusionMatrix", class(res)) # extends ftable
  return(res)                      
}

print.nlcvConfusionMatrix <- function(x, ...){
  
  tech <- attr(x, "tech")
  nFeaturesOptim <- attr(x, "nFeaturesOptim")
  
  cat("Confusion matrix for classifier: ", tech, "\n")
  cat("Optimal number of features: ", nFeaturesOptim, "\n\n")
  NextMethod(x, ...)
}
