
## function to compare the original matrix of correct classes to
## each component of the output object for a certain classifier
##
## the return value is a list with for each number of features selected,
## a matrix of logical values indicating whether the classifier results
## correspond (TRUE) or not (FALSE) to the original values to be classified


compareOrig <- function(nlcvObj,  # object as produced by the nlcv function
        techn  # string giving the technique for which the comparison to
               # correct classes should be made
        ){ 
    
    # matrix with all rows corresponding to the correct classes
    # for the samples
    nRuns <- length(nlcvObj$features)
    origmat <- matrix(rep(as.character(attr(nlcvObj, "classVar")), 
                    nRuns),  # number of runs
            ncol = length(attr(nlcvObj, "classVar")), byrow = TRUE)
    
    technList <- nlcvObj$output[[techn]]
    compmatList <- lapply(technList, function(x){
                res <- x$labelsMat == origmat
                return(res)
            })
    return(compmatList)
}



