#' Calculate SWAMP PHAB metrics
#'
#' @param phabdata A data frame of SWAMP formatted PHAB data
#' @param verbose 0 (default) is silent, 1 prints SampleIDs, 2 prints metrics
#' @return A data frame with the metric values, standard deviations, and
#' sample size for all successfully calculated metrics
#' @export


metricsPHAB <- function(phabdata, verbose = 0){
  metricFun <- function (data) {
    
    
    metrics <- c("algae", "bankMorphology", "channelMorphology", "habitatComplexity",
                 "humanDisturbance", "riparianVegetation", "slope", 
                 "substrate", "flow", "misc") 
    
    resultList <- lapply(metrics, function(f){
      if(verbose >= 2)print(f)
      fun <- get(f)
      tryCatch(fun(data), error = function(e){
        print(paste("Problem with", 
                    unique(data$SampleID),
                    "in", f, "metrics; discarding"))
        NULL})})
    names(resultList) <- metrics
    resultList <- resultList[sapply(resultList, is.data.frame)]
    result <- rbind.fill(resultList)
    result
  }
  datasplit <- split(phabdata, phabdata$SampleID)
  results <- lapply(datasplit, function(d){
    if(verbose >= 1)print(unique(d$SampleID))
    metricFun(d)})

  resultsFinal <- rbind.fill(results)
  resultsFinal[, 1:5]
}







