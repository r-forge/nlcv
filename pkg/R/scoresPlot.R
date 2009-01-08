scoresPlot <- function(nlcvObj,   # output object from nlcv 
    tech,      # name of the classification technique
    nfeat,     # number of features for which you would like to see
    # the scores plot
    plot = TRUE,
    barPlot = FALSE, # barplot or MCREstimate-type scores plot
    layout = TRUE, 
    main = NULL,
    sub = NULL,
    ...){ # additional arguments such as main, sub etc.
  
  ### error checking
  if (!inherits(nlcvObj, "nlcv"))
    stop("nlcvObj is not of class 'nlcv'")
  
  if (!(tech %in% c("dlda", "lda", "nlda", "qda", "glm", 
            "randomForest", "bagg", "pam", "svm", "ksvm"))){
    stop("Invalid classification technique in 'tech' argument")
  }
  ### compare predicted with observed class labels (for all runs)
  resout <- compareOrig(nlcvObj = nlcvObj, tech = tech)
  
  ### compute proportions of correct classification over all runs    
  resoutProp <- lapply(resout, colMeans, na.rm = TRUE)              
  
  ### plot results for a given number of features
  nfeatName <- paste("nfeat", nfeat, sep = "")
  classVar <- nlcvObj$trueClasses # attr(nlcvObj, "classVar")
  classVarLevels <- levels(classVar)
  
  plotData <- resoutProp[[nfeatName]] # vector of proportions
  names(plotData) <- names(classVar)
  classVar <- as.factor(classVar) # after using names
  
  ##  data values are grouped according to their observed labels
  ##  (e.g. responders / non responders)
  plotData <- plotData[order(classVar)] 
  classVar <- classVar[order(classVar)] # order the labels as well
  
  if (plot){
    if (!barPlot) {
      ### layout
      if (layout) layout(matrix(1:2, ncol = 1), heights = c(6, 1))
      
      ### upper plot
      plot(x = seq(along = plotData), # indices (names) of the samples 
          y = plotData,   # vector of proportions of misclassification for each sample
          ylim = c(0, 1),
          type = "n", las =  3, ann = FALSE, axes = FALSE, ...)
      
      axis(1, las = 3, at = seq(along = plotData), 
          labels = names(plotData), cex = 0.6)
      axis(2, las = 2)
      
      # draw grid first...
      abline(h = 0.5, col = "grey")
      abline(v = seq(length(plotData)), lty = "dashed", col = "grey")
      
      # ... then add dots  
      points(x = seq(along = plotData), y = plotData,
          pch = ifelse(plotData >= 0.5, 19, 17), cex = 2,
          col = ifelse(plotData >= 0.5, "darkblue", "orange"))
      
      title(main = if(is.null(main)){ 
                paste("Scores Plot (", tech, ", ", nfeat, " features)", sep = "")
              } else {
                main
              })
      
      ### add observed class membership
      nClasses <- length(classVarLevels)
      classColors <- brewer.pal(nClasses+3,"YlGn")[2:(nClasses+1)] # from MCREstimate
      
      sampleLocations <- seq(along = classVar)
      rect(xleft = sampleLocations-0.5, ybottom = -0.5, 
          xright = sampleLocations + 0.5, ytop = -0.015, 
          col = classColors[as.numeric(classVar)], 
          border = classColors[as.numeric(classVar)])
      abline(v = sampleLocations, lty = 2, col = "grey")
      
      ### lower plot (with legends)
      op <- par(mar = c(0,4,0,2))
      plot(c(0, 1), type = "n", ann = FALSE, axes = FALSE)
      legend("left", legend = classVarLevels, fill = classColors,
          bty = "n")
      legend("right", legend = c("0.5 <= score <=   1", 
              "   0 <= score <  0.5"), 
          pch = c(19, 17), pt.cex = 1.5, col = c("darkblue", "orange"), 
          bty = "n")
      par(op)
      
    } else {
      barplot(height = plotData, col = ifelse(plotData >= 0.5, "green", "red"),  
          las = 3, ...)
      abline(h = 0.5, col = "grey")    
    }
  }
  
  invisible(plotData) # named vector of proportion correctly classified
}

