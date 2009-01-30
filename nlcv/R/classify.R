### function randiv: a wrapper code for ONE random division
classify <- function(eset, trainingSample, testSample, classVar = "type")  # TV added arguments two last arguments
{
  
  ## Classification with DLDA
  trainingSample <- as.integer(trainingSample)
  
  dlda.predic  <- stat.diag.daB(eset, classVar, trainingSample)
  
  conf.matrix  <- confuMat(dlda.predic)
  error.rate   <- function(cm) 1 - sum(diag(cm)) / sum(cm)
  dlda.error   <- error.rate(conf.matrix)
  
  ## Classification with Nearest Neighbors 
  # knn.error <- numeric(3)
  # for(k in c(1,3,5))
  #   {
  # i            <- ((k-1)/2)+1
  #   knn.predic   <- knnB(eset, classVar, LearnSampRun, k = k, prob = FALSE)
  #   knn.error[i] <- error.rate(confuMat(knn.predic))
  #       }
  
  ## Classification with random forest 
  randomForest.predic <- randomForestB(eset, classVar, trainingSample, ntree = 10000)
  randomForest.error  <- error.rate(confuMat(randomForest.predic))
  
  ## Classification with bagging 
  bagg.predic <- baggingB(eset, classVar, trainingSample)
  bagg.error <- error.rate(confuMat(bagg.predic))
  
  ## Classification with PAM 
  pam.predic <- pamrB(eset, classVar, trainingSample)
  pam.error <- error.rate(confuMat(pam.predic))
  
  ## Classification with Support Vector Machines
  svm.predic <- svmB(eset, classVar, trainingSample, kernel = "linear")
  svm.error <- error.rate(confuMat(svm.predic))
  
  #SVM with different cost and gamma settings
  #svm.error <- matrix(0, nrow = 3, ncol = 3)
  #for(cost in 0:2)
  #  {
  # for(gamma in (-1):1)
  #   {
  #     i       <- cost+1
  #     j       <- gamma+2
  #     svm.fit <- svmB(eset, classVar, LearnSampRun, 
  #         cost=2^cost, 
  #         gamma = 2^gamma/nrow(exprs(eset)),
  #         type="C-classification")
  #     svm.error[i,j] <- error.rate(confuMat(svm.fit))
  #   }
  #  }
  
  ## Output
  res <- list(dlda = dlda.error, randomForest = randomForest.error, bagg = bagg.error, 
      pam = pam.error, svm = svm.error,
      
      dlda.predic = dlda.predic, randomForest.predic = randomForest.predic, bagg.predic = bagg.predic, 
      pam.predic = pam.predic, svm.predic = svm.predic)
  
  return(res)
}
