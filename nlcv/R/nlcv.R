nlcv <- function(eset,
    classVar = "type",
    nRuns = 2,          # total number of runs i.e. number of splits in training and test set
    propTraining = 2/3, # proportion of data in a training set
    classdist = c("balanced", "unbalanced"), 
    nFeatures = c(2, 3, 5, 7, 10, 15, 20, 25, 30, 35), #, 40),
    fsMethod = c("randomForest", "t.test", "limma", "none"),
    classifMethods = c("dlda", "randomForest", "bagg", "pam", "svm"),
    fsPar = NULL,
    initialGenes = seq(length.out = nrow(eset)),
    geneID = "ID",
    storeTestScores = FALSE,
    verbose = FALSE){
  
  fsMethod <- match.arg(fsMethod)
  if (!(fsMethod %in% c("randomForest", "t.test", "limma", "none")))
    stop("argument fsMethod should be one of 'randomForest', 't.test', 'limma' or 'none'")
  
  classifMethodList <- list(dlda = "dlda" %in% classifMethods,
      lda = "lda" %in% classifMethods,
      nlda = "nlda" %in% classifMethods,
      qda = "qda" %in% classifMethods,
      glm = "glm" %in% classifMethods,
      randomForest = "randomForest" %in% classifMethods,
      bagg = "bagg" %in% classifMethods,
      pam = "pam" %in% classifMethods,
      svm = "svm" %in% classifMethods,
      ksvm = "ksvm" %in% classifMethods)
  
  # check on classVar
  if (!is.factor(pData(eset)[,classVar]))
    stop("'classVar' should be a factor variable")
  
  classdist <- match.arg(classdist)
  
  # sort nFeatures (assumption of mcrPlot)
  nFeatures <- sort(nFeatures)
  
  # sample sizes (total, training, test)
  
  ## initialize matrices (total, training, test)
  ## depends on classdist argument
  
  respVar <- pData(eset)[, classVar]
  nTotalSample <- length(respVar)  # total sample size
  
  if (classdist == "balanced"){
     
    nTrainingSample <- round(propTraining * nTotalSample) # sample size of training (=learning) set 
    nTestSample <- nTotalSample - nTrainingSample  #   "            validation (=test) set
     
  } else {
    
    smallestClass <- names(sort(table(respVar)))[1]
    nSmallest <- sum(respVar == smallestClass)
    
    nSmallestTrain <- round(propTraining * nSmallest)
    nBiggestTrain <- nSmallestTrain
    nSmallestTest <- nSmallest - nSmallestTrain
    nBiggestTest <- nTotalSample - (nSmallestTest + nSmallestTrain + nBiggestTrain)
    
    nTrainingSample <- nSmallestTrain + nBiggestTrain
    nTestSample <- nSmallestTest + nBiggestTest
  
  }
  
  totalSampleAllRuns <- matrix(0, nrow = nRuns, ncol = nTotalSample)
  trainingSampleAllRuns <- matrix(0, nrow = nRuns, ncol = nTrainingSample)
  testSampleAllRuns   <- matrix(0, nrow = nRuns, ncol = nTestSample)
  
  indicesTrainingSample <- matrix(NA, nrow = nRuns, ncol = nTotalSample)
  
  for (irun in 1:nRuns) {
    indicesTrainingSample[irun, ] <- inTrainingSample(pData(eset)[, classVar],
        propTraining = propTraining, classdist = classdist)
    trainingSampleAllRuns[irun, ] <- which(indicesTrainingSample[irun, ])
    testSampleAllRuns[irun, ]   <- which(!indicesTrainingSample[irun, ])
  }
  
  ### output data structure for features used by the classifiers
  feat.outp <- vector(length = nRuns, mode = "list")
  
  ### output data structure for errors and predicted classes
  
  ## list nested in first level
  featuresList <- vector(length = length(nFeatures), mode = "list")
  names(featuresList) <- paste("nfeat", nFeatures, sep = "")
  
  ## list nested in second level
  resultList <- if (storeTestScores){
    list(labelsMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
    , testScoresMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
    , errorRate = rep(NA, nRuns)
    , AUC = rep(NA, nRuns)
    , ROC = vector(mode = "list", length = nRuns)) # for performance objects
  } else  {
    list(labelsMat = matrix(NA, nrow = nRuns, ncol = nTotalSample)
        , errorRate = rep(NA, nRuns)
        , AUC = rep(NA, nRuns)
        , ROC = vector(mode = "list", length = nRuns)) # for performance objects
  }
  ## nest second level in first level
  featuresList <- lapply(featuresList, function(x){x <- resultList; return(x)})
  
  ## create output data structure
  output <- vector(length = length(classifMethods), mode = "list") 
  names(output) <- classifMethods
  output <- lapply(output, function(x){x <- featuresList; return(x)})
  
  for (irun in 1:nRuns){
    
    trainingSampleRun <- trainingSampleAllRuns[irun, ]
    testSampleRun <- testSampleAllRuns[irun, ]
    
    ### Feature Selection
    
    switch(fsMethod,
        none = {
          
          orderedEset <- eset
          fNames <- featureNames(eset)
          featRF <- rep(1, length(fNames))
          names(featRF) <- fNames
          feat.outp[[irun]] <- featRF
          
          # nothing is done
        },
        randomForest = {

          # allow for customized use
          mtry <- if (is.null(fsPar$mtry)) as.integer(sqrt(length(initialGenes)))
              else fsPar$mtry
#          if (mtry > max(nFeatures))
#            stop("'mtry' component of 'fsPar' cannot exceed max(nFeatures)")
           ntree <- if (is.null(fsPar$ntree)) 500
              else fsPar$ntree
          
          
          rf <- randomForest::randomForest(x = t(exprs(eset[initialGenes, ])),
              y = pData(eset)[,classVar], 
              mtry= mtry,
              importance=TRUE)
          
          importanceRf <- randomForest::importance(rf)
        
          # TV: MLearn method blows up memory for some reason :-(
      
#          rf <- MLearn(formula = as.formula(paste(classVar, "~ .", sep = " ")),
#              data = eset[initialGenes, ], 
#              trainInd = as.integer(trainingSampleRun),
#              .method = randomForestI,
#              importance = TRUE,
#              ntree = ntree,
#              mtry = mtry)
          
          # order features from highest to lowest variable importance
          orderedGenesRF <- order(importanceRf[, 3], decreasing = TRUE)
          orderedEset <- eset[orderedGenesRF, ]
          featRF <- importanceRf[orderedGenesRF, 3]
          feat.outp[[irun]] <- featRF},
        
        t.test = {
          
          fsPar$test <- if (is.null(fsPar$test)) "f" else fsPar$test
          
          yl.num      <- as.numeric(factor(pData(eset)[trainingSampleRun, classVar])) - 1
          xl.mtt      <- exprs(eset[, trainingSampleRun])
          f.stat      <- mt.teststat(xl.mtt, yl.num, fsPar$test) # "f" by default
          orderedGenesTT <- order(f.stat, decreasing = TRUE)
          
          orderedEset <- eset[orderedGenesTT, ]
          featTT <- f.stat[orderedGenesTT]
          names(featTT) <- rownames(exprs(eset))[orderedGenesTT]
          feat.outp[[irun]] <- featTT},
        
        limma = {
          limmaTopTable <- limmaTwoGroups(eset[, trainingSampleRun],
                                      group = classVar)
          limmaTopTableGeneIDs <- limmaTopTable[,geneID]                                   
          orderedEset <- eset[limmaTopTableGeneIDs, ] # features sorted by increasing P-values
          featLimma <- limmaTopTable$P.Value
          names(featLimma) <- limmaTopTableGeneIDs
          feat.outp[[irun]] <- featLimma
      }) # end of switch
    
    ### Classification    
    for (iNumber in seq(along = nFeatures)){
      
      ## use first nFeatures[iNumber]] rows (i.e. features)
      nFeaturesEset <- orderedEset[1:nFeatures[iNumber], ] 
      
      ## wrapper for all classification algorithms      
      results  <- 
      # make conditional on BioC release    
          classify_bioc_2.3(nFeaturesEset, 
          trainingSample = trainingSampleRun,
          testSample = testSampleRun,
          classVar = classVar,
          classifMethodList = classifMethodList)
  
      ## extract errors, predicted classes and (for continuous outputs) predicted values
      classVarLevels <- levels(pData(eset)[[classVar]])
      
      for (iMethod in classifMethods){      
         # MCR
         output[[iMethod]][[iNumber]][["errorRate"]][irun] <- results[[iMethod]]
         
         # predicted values
         iPredic <- paste(iMethod, "predic", sep = ".")
         if (!(iPredic %in% c("glm.predic", "nlda.predic"))){
           output[[iMethod]][[iNumber]][["labelsMat"]][irun,!indicesTrainingSample[irun, ]] <-
               as.character(testPredictions(results[[iPredic]]))
         } else {
           output[[iMethod]][[iNumber]][["labelsMat"]][irun,!indicesTrainingSample[irun, ]] <-
               classVarLevels[as.numeric(testPredictions(results[[iPredic]]))]
         
           # test scores  
           if (storeTestScores){
           output[[iMethod]][[iNumber]][["testScoresMat"]][irun,!indicesTrainingSample[irun, ]] <-
               testScores(results[[iPredic]])
           }
           
           # AUC
           iAUC <- paste(iMethod, "AUC", sep = ".")
           output[[iMethod]][[iNumber]][["AUC"]][irun] <- results[[iAUC]]
           
           # ROC curve (performance object)
           iROC <- paste(iMethod, "ROC", sep = ".")
           output[[iMethod]][[iNumber]][["ROC"]][irun] <- results[[iROC]]
         }
         
      }  
    } 
  }
  
  res <- list(output = output, features = feat.outp, trueClasses = pData(eset)[[classVar]])
  attr(res, "nFeatures") <- nFeatures
  tmpClassVar <- pData(eset)[, classVar]
  names(tmpClassVar) <- sampleNames(eset)
  attr(res, "classVar") <- tmpClassVar 
  class(res) <- "nlcv"
  return(res)
}
