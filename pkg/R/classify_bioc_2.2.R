classify_bioc_2.2 <- function(eset, trainingSample, testSample, classVar = "type",
    classifMethodList){
  
  trainingSample <- as.integer(trainingSample)
  
  error.rate   <- function(cm) 1 - sum(diag(cm)) / sum(cm)
  
  ## Classification with DLDA
  if (classifMethodList$dlda){ # can be NULL
    dlda.predic  <- stat.diag.daB(eset, classVar, trainingSample)
    
    conf.matrix  <- confuMat(dlda.predic)
    dlda.error   <- error.rate(conf.matrix)
  }
  ## Classification with Nearest Neighbors 
  # knn.error <- numeric(3)
  # for(k in c(1,3,5))
  #   {
  # i            <- ((k-1)/2)+1
  #   knn.predic   <- knnB(eset, classVar, LearnSampRun, k = k, prob = FALSE)
  #   knn.error[i] <- error.rate(confuMat(knn.predic))
  #       }
  
  ## Classification with random forest
  if (classifMethodList$randomForest){
    randomForest.predic <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")), 
                        data = eset, 
                        trainInd = trainingSample, 
                        method = randomForestI, 
                        ntree = 10000)
    
    randomForest.error  <- error.rate(confuMat(randomForest.predic))
  }
  ## Classification with bagging 
  if (classifMethodList$bagg){
    bagg.predic <- baggingB(eset, classVar, trainingSample)
    bagg.error <- error.rate(confuMat(bagg.predic))
  }
  
  ## Classification with PAM 
  if (classifMethodList$pam){
    pam.predic <- pamrB(eset, classVar, trainingSample)
    pam.error <- error.rate(confuMat(pam.predic))
  }
  
  ## Classification with Support Vector Machines
  if (classifMethodList$svm){
    svm.predic <- svmB(eset, classVar, trainingSample, kernel = "linear")
    svm.error <- error.rate(confuMat(svm.predic))
  }
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
  res <- vector(mode = "list", length = 2 * length(classifMethodList))
  if (classifMethodList$dlda){
    res$dlda <- dlda.error
    res$dlda.predic <- dlda.predic
  }
  if (classifMethodList$randomForest){
    res$randomForest <- randomForest.error
    res$randomForest.predic <- randomForest.predic
  }
  if (classifMethodList$bagg){
    res$bagg <- bagg.error
    res$bagg.predic <- bagg.predic
  }
  if (classifMethodList$pam){
    res$pam <- pam.error
    res$predic <- pam.predic
  }
  if (classifMethodList$svm){
    res$svm <- svm.error
    res$svm.predic <- svm.predic
  }    
  return(res)
}
