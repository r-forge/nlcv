xtable.summary.mcrPlot <- function(x, caption = NULL, label = NULL, align = NULL, 
    digits = NULL, display = NULL, ...){
  smpMat <- as.matrix(unclass(x))
  xtable(smpMat, caption = caption, label = label, align = align, 
         digits = digits, display = display, ...)
}

