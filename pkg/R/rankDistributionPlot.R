

rankDistributionPlot <- function(nlcvObj, n = 5, ...){
  
  ### create a big matrix à la boot.ranks, i.e.
  # all genes (from first to last) in rows and
  # all ranks (for all runs) in columns 
  
  # transform variable importance into rank
  importanceRank <- lapply(nlcvObj$features, function(x) rank(-x))
  namesRank <- lapply(importanceRank, function(x) x[order(names(x))])
  ranksByRun <- do.call("cbind", namesRank)
  
  ### compute median rank across runs (in order to rank the genes) and take top n
  ranksByRunSelection <- as.data.frame(ranksByRun[order(apply(ranksByRun, 1, median)),])[1:n,]
  
  ### prepare data for boxplotting (from wide to long)
  ranksByRunSelection <- cbind(feature = rownames(ranksByRunSelection), 
      ranksByRunSelection)
  
  boxplotData <- reshape(ranksByRunSelection, idvar="feature", 
      varying = list(names(ranksByRunSelection)[2:ncol(ranksByRunSelection)]),
      direction="long")
  
  boxplotData$feature <- factor(boxplotData$feature, 
      levels = as.character(ranksByRunSelection$feature)) # fix order of levels
  
  ### draw boxplots  
  boxplot(V1 ~ feature, data = boxplotData, ylab = "rank", xlab = "feature", 
      axes = FALSE, ...)
  axis(1)    
  axis(2, las = 2)
  
#    p <- ggplot(boxplotData, aes(y=V1, x=feature))    
#    p + geom_boxplot() ### becomes a mess with many features to be inspected
}

