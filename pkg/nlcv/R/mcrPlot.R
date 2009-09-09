# contains 
#   o mcrPlot
#   o summary.mcrPlot
#   o print.summary.mcrPlot


# misclassification rate plot

# plots for each classification technique and a given number of features
# used the mean misclassification rate (mcr)
# and its standard error across all runs of the nested loop cross-validation
#  
mcrPlot <- function(nlcvObj, # object of class 'nlcv' such as produced by the nlcv function
    plot = TRUE, 
    optimalDots = TRUE,
    rescale = FALSE, 
    layout = TRUE,
    ...){ # additional graphical parameters for plot(), e.g. main
  
  # compute quantities to plot
  meanfun <- function(x) sapply(lapply(x, function(y) y$errorRate), mean)
  sdfun <- function(x) sapply(lapply(x, function(y) y$errorRate), sd)
  
  meanMatrix <- sapply(nlcvObj$output, meanfun)
  sdMatrix <- sapply(nlcvObj$output, sdfun)
  
  # optimal number of features for each technique
  nFeatures <- attr(nlcvObj, "nFeatures")
  optimFeatures <- nFeatures[apply(meanMatrix, 2, which.min)] # which min takes first, features ordered
  
  if (plot){
    def.par <- par(no.readonly = TRUE)
    ### layout of the plot
    if (optimalDots){
      if (layout) layout(matrix(1:2, ncol=1), heights=c(6, 1))
      
      ### upper plot
      # set up upper plot
      op <- par(mar = c(1, 4, 4, 2) + 0.1)  
    }
    if (rescale) plotmax <- max(meanMatrix,na.rm=TRUE) else plotmax=1
    plot(c(min(nFeatures), max(nFeatures)), c(0, plotmax), 
        type = "n", axes = FALSE,
        xlab = "",  
        ylab = "Misclassification Rate", ...)
    mtext(side = 1, line = 2, text = "Number of features")
    axis(1, at = c(0, nFeatures))
    axis(2, las = 2)
    
    # add content to upper plot
    plotColours <- c("green", "blue", "red", "orange", "purple") # , "pink")
    offsets <- c(-0.2, -0.1, 0, 0.1, 0.2)
    for (iTechnique in seq(ncol(meanMatrix))){
      points(nFeatures + offsets[iTechnique], meanMatrix[, iTechnique], 
          col = plotColours[iTechnique], pch = 20)  
      segments(nFeatures + offsets[iTechnique], meanMatrix[, iTechnique] + sdMatrix[, iTechnique],
          nFeatures + offsets[iTechnique], meanMatrix[, iTechnique] - sdMatrix[, iTechnique],
          col = plotColours[iTechnique])
      lines(nFeatures + offsets[iTechnique], meanMatrix[, iTechnique], col = plotColours[iTechnique])
    }  
    
    legend('topright', colnames(meanMatrix),
        col = plotColours, pch = 1, lty = 1)
    
    # setup
    if (optimalDots){
      par(mar = c(0, 4, 0, 2) + 0.1)
      plot(c(0, 1), xlim = c(min(nFeatures), max(nFeatures)), ylim = c(0, 1), 
          type = "n", axes = FALSE, bty = "o", ylab = "")
      dotex <- 2
      dotpch <- 20
      
      ### optimal features bar (second plot)    
      uniqOptimFeatures <- unique(optimFeatures)
      
      # individual dots
      for (iOptim in seq(along = uniqOptimFeatures)){
        whichi <- which(optimFeatures == uniqOptimFeatures[iOptim])
        subsetOptim <- optimFeatures[whichi]
        
        if (length(subsetOptim) == 1){
          points(x = uniqOptimFeatures[iOptim],
              y = 0.5, 
              col = plotColours[whichi],
              cex = dotex,
              pch = dotpch)       
        } else if (length(subsetOptim) ==  2){
          points(x = rep(uniqOptimFeatures[iOptim], 2), 
              y = c(0.45, 0.55),
              col = plotColours[whichi],
              cex = dotex,
              pch = dotpch)     
        } else if (length(subsetOptim) ==  3){
          points(x = uniqOptimFeatures[iOptim] + c(-0.2, 0, 0.2), 
              y = c(0.45, 0.55, 0.45),
              col = plotColours[whichi],
              cex = dotex,
              pch = dotpch)
        } else if (length(subsetOptim) ==  4){
          points(x = uniqOptimFeatures[iOptim] + c(-0.2, -0.2, 0.2, 0.2), 
              y = c(0.45, 0.55, 0.45, 0.55),
              col = plotColours[whichi],
              cex = dotex,
              pch = dotpch)
        } else if (length(subsetOptim) ==  5){
          points(x = uniqOptimFeatures[iOptim] + c(-0.2, 0, 0, 0, 0.2), 
              y = c(0.5, 0.45, 0.5, 0.55, 0.5),
              col = plotColours[whichi],
              cex = dotex,
              pch = dotpch)
        }    
      }
      par(op)
    }
  }
  res <- list(meanMcrMatrix = meanMatrix, sdMcrMatrix = sdMatrix)
  class(res) <- "mcrPlot"
  attr(res, "nFeatures") <- nFeatures # for summary method
  invisible(res)
  par(def.par)
}

summary.mcrPlot <- function(object, ...){
  nFeatures <- attr(object, "nFeatures")
  meanMcrMatrix <- object$meanMcrMatrix
  posOptim <- apply(meanMcrMatrix, 2, which.min)
  optimFeatures <- nFeatures[posOptim]
  nc <- ncol(meanMcrMatrix)
  means <- numeric(nc)
  sds <- numeric(nc)
  for (iCol in seq(nc)){
    means[iCol] <- meanMcrMatrix[posOptim[iCol], iCol]
    sds[iCol] <- object$sdMcrMatrix[posOptim[iCol], iCol]
  }
  res <- cbind(optimFeatures, means, sds)
  rownames(res) <- colnames(meanMcrMatrix)
  colnames(res) <- c("nFeat_optim", "mean_MCR", "sd_MCR")
  class(res) <- "summary.mcrPlot"
  return(res)
}

print.summary.mcrPlot <- function(x, digits = 2, ...){
  print.default(unclass(x), digits = digits, ...)
}

