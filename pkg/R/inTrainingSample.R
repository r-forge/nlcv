### make random partitions
# returns a logical vector indicating whether the observation
# 
inTrainingSample <- function(y, propTraining = 2/3) # was: RandomPartition
{
  # sample sizes
  nTotalSample <- length(y)
  nTrainingSample <- round(propTraining * nTotalSample)
  nTestSample <- nTotalSample - nTrainingSample

  if (nlevels(factor(y) != 2))
    stop("'nlcv' currently only works for two-class problems")
  
  K <- nlevels(factor(y))
  LearnSampRun <- NULL
  props    <- round(nTrainingSample / nTotalSample * table(y))
  props[1] <- nTrainingSample - sum(props[2:K])
  
  for (k in 1:K){
    y.num  <- as.numeric(factor(y))
    LearnSampRun <- c(LearnSampRun, sample(which(y.num == k))[1:props[k]])
  }
  
  res <- rep(FALSE, length = nTotalSample)
  res[LearnSampRun] <- TRUE 
  return(res)
}

