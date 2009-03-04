MLIConverter.nlda = function(obj, data, trainInd) {
  teData = data[-trainInd,]
  trData = data[trainInd,]
  tepr = predict(obj, teData)$class
  trpr = predict(obj, trData)$class
  tesco =  predict(obj, teData)$posterior[,2] # 
  names(tepr) = rownames(teData)
  names(trpr) = rownames(trData)
  new("classifierOutput", testPredictions=factor(tepr),
      trainPredictions=factor(trpr), testScores=tesco, RObject=obj)
}

nldaI <- new("learnerSchema", packageName = "MASS", 
    mlFunName = "lda", converter = MLIConverter.nlda)

