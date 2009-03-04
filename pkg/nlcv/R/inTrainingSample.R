### make random partitions
# returns a logical vector indicating whether the observation
# 
inTrainingSample <- function(y, propTraining = 2/3, classdist = c("balanced", "unbalanced")) # was: RandomPartition
{
  classdist <- match.arg(classdist)
  if (!(classdist %in% c("balanced", "unbalanced")))
    stop("'classdist' should be one of 'balanced' or 'unbalanced'")
  
  # sample sizes
  nTotalSample <- length(y)
  nTrainingSample <- round(propTraining * nTotalSample)
  nTestSample <- nTotalSample - nTrainingSample

  if (nlevels(factor(y) != 2))
    stop("'nlcv' currently only works for two-class problems")
  
  if (classdist == "balanced"){ 
    
    K <- nlevels(factor(y))
    trainingSampleRun <- NULL
    props    <- round(nTrainingSample / nTotalSample * table(y))
    props[1] <- nTrainingSample - sum(props[2:K])
    
    for (k in 1:K){
      y.num  <- as.numeric(factor(y))
      trainingSampleRun <- c(trainingSampleRun, sample(which(y.num == k))[1:props[k]])
    }
    
    res <- rep(FALSE, length = nTotalSample)
    res[trainingSampleRun] <- TRUE 
  } else {
    
    smallestClass <- names(sort(table(y)))[1]
    nSmallest <- sum(y == smallestClass)
   
    nSmallestTrain <- round(propTraining * nSmallest)
    nBiggestTrain <- nSmallestTrain
    nSmallestTest <- nSmallest - nSmallestTrain
    nBiggestTest <- nTotalSample - (nSmallestTest + nSmallestTrain + nBiggestTrain)
    
    
    # split up in smallest class indices and biggest class indices
    smallestIndices <- which(y == smallestClass)
    biggestIndices <- seq(along = y)[-smallestIndices]
    
    sampleSmallestTrain <- sample(smallestIndices, nSmallestTrain)
    # sampleSmallestTest <- smallestIndices[-sampleSmallestTrain]
    
    sampleBiggestTrain <- sample(biggestIndices, nBiggestTrain)
    # sampleBiggestTest <- biggestIndices[-sampleBiggestTrain]
    
    trainingSampleRun <- c(sampleSmallestTrain, sampleBiggestTrain)
    res <- rep(FALSE, length = nTotalSample)
    res[trainingSampleRun] <- TRUE
    
  }
  return(res)
}

