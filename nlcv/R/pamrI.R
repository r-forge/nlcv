# pamr interface for MLInterfaces

# version of pamr.train with formula interface

#setGeneric("pamrTrain", function(formula, data, ...){
#      standardGeneric("pamrTrain")
#})


pamrTrain <- function(formula, data, ...){
  x <- model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(x)){
    x <- x[,-which(colnames(x) %in% "(Intercept)")]
  }
  x <- t(x)
  mf <- model.frame(formula, data)
  resp <- model.response(mf)
  inputList <- list(x = x, y = resp)
  pamrObj <- pamr.train(data = inputList, ...)
  return(pamrObj)
}

pamrML <- function(formula, data, ...){
  
  # TV: quick and (especially) dirty
  responseName <- as.character(formula[[2]])
  if (class(data) == "ExpressionSet"){
    data <- as.data.frame(t(exprs(data)))
    data <- cbind(data, pData(data)[[responseName]])
  }
  
  fit <- pamrTrain(formula, data, ...)
  
  attr(fit, "responseName") <- responseName # for the predict method
  class(fit) <- "pamrML"
  fit
}

predict.pamrML <- function(object, newdata, ...){
  
  if (class(newdata) == "ExpressionSet"){
    newx <- exprs(newdata)
  } else { # data frame
    # remove response if present (for an easy MLIConverter)
    responseName <- attr(object, "responseName")
    responsePos <- which(names(newdata) == responseName)
    if (length(responsePos)) newdata <- newdata[,-responsePos]
    newx <- t(data.matrix(newdata))
  }
  
  parList <- list(...)
  parList$fit <- object
  parList$newx <- newx
  
  if (is.null(parList$threshold)) 
    parList$threshold <- 1
  if (is.null(parList$type)) parList$type <- 
        c("class", "posterior", "centroid", "nonzero")
  if (is.null(parList$prior)) parList$prior <- object$prior
  if (is.null(parList$threshold.scale)) parList$threshold.scale <-
        object$threshold.scale
        
  res <- do.call("pamr.predict", parList)
  return(res)
}

print.pamrML <- function(x, ...){
  cat("pamrML S3 instance. components:\n")
  print(names(x), ...)
}

#' @param obj object as returned by pamrML i.e. of class \code{pamrML} 
#' @param data original data used as input for MLearn
#' @param trainInd training indices used as input to MLearn
#' @returnType classifierOutput 
#' @return object of class \code{classifierOutput}
pamrIconverter <- function(obj, data, trainInd){
  teData <- data[-trainInd,]
  trData <- data[trainInd,]
  tepr <- predict(obj, teData)
  trpr <- predict(obj, trData)
  names(tepr) <- rownames(teData)
  names(trpr) <- rownames(trData)
  res <- new("classifierOutput", testPredictions = factor(tepr), 
      trainPredictions = factor(trpr), RObject = obj)
  return(res)
}

pamrI <- new("learnerSchema", packageName = "nlcv", 
    mlFunName = "pamrML", converter = pamrIconverter)
